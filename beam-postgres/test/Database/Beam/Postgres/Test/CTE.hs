{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Rendering, type-safety, and PostgreSQL integration tests for common table
-- expressions. Deliberately ill-typed expressions live in
-- "Database.Beam.Postgres.Test.CTENegative" so this module retains normal type
-- checking.
module Database.Beam.Postgres.Test.CTE (unitTests, integrationTests) where

import Control.Exception (ErrorCall, TypeError, evaluate, try)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Kind (Type)
import Data.List (isInfixOf, isPrefixOf)
import Data.Text (Text)

import Database.Beam
import Database.Beam.Postgres
import qualified Database.Beam.Postgres.Full as Pg
import qualified Database.Beam.Query.CTE as CTE
import Database.Beam.Postgres.Syntax
  ( PgDeleteSyntax(..)
  , PgInsertSyntax(..)
  , PgSelectSyntax(..)
  , PgUpdateSyntax(..)
  , PostgresInaccessible
  , pgRenderSyntaxScript
  )
import Database.PostgreSQL.Simple (execute_)

import Test.Tasty
import Test.Tasty.HUnit

import Database.Beam.Postgres.Test
import qualified Database.Beam.Postgres.Test.CTENegative as Negative

data CteRowT f = CteRow
  { cteId    :: C f Int32
  , cteValue :: C f Text
  } deriving (Generic, Beamable)

deriving instance Show (CteRowT Identity)
deriving instance Eq (CteRowT Identity)

-- A legal Haskell projection shape with no fields. PostgreSQL has no
-- corresponding zero-column SELECT or RETURNING relation, so the CTE builders
-- must reject it before rendering SQL.
data EmptyCteT (f :: Type -> Type) = EmptyCte
  deriving (Generic, Beamable)

instance Table CteRowT where
  data PrimaryKey CteRowT f = CteRowKey (C f Int32)
    deriving (Generic, Beamable)
  primaryKey = CteRowKey . cteId

newtype CteDb entity = CteDb
  { dbCteRows :: entity (TableEntity CteRowT)
  } deriving (Generic, Database Postgres)

cteDb :: DatabaseSettings Postgres CteDb
cteDb = defaultDbSettings

unitTests :: TestTree
unitTests = testGroup "Common table expression tests"
  [ renderingTests
  , typeSafetyTests
  , projectionValidationTests
  ]

integrationTests :: IO ByteString -> TestTree
integrationTests getConn = testGroup "Common table expression integration tests"
  [ testMixedCteBodies getConn
  , testWithDmlConsumers getConn
  ]

renderingTests :: TestTree
renderingTests = testGroup "Common table expression rendering tests"
  [ testMixedCteRendering
  , testNestedSelectCteRendering
  , testRecursiveSelectThenDeleteRendering
  , testEmptyDataModifyingCtes
  , testWithDmlConsumerRendering
  , testRecursiveInsertWithRendering
  , testTopLevelOnlyDmlConsumerRendering
  , testEmptyDmlConsumers
  , testReturningAfterDmlConsumers
  ]

-- These tests force expressions compiled with deferred type errors in the
-- isolated negative-fixture module. Checking fragments of GHC's error ensures
-- an unrelated deferred error cannot make a test pass accidentally.
typeSafetyTests :: TestTree
typeSafetyTests = testGroup "Common table expression type-safety tests"
  [ testCase "rejects a DELETE CTE inside pgSelectWith" $
      assertPlacementTypeError Negative.invalidNestedDelete
  , testCase "rejects an INSERT CTE inside pgSelectWith" $
      assertPlacementTypeError Negative.invalidNestedInsert
  , testCase "rejects an UPDATE CTE inside pgSelectWith" $
      assertPlacementTypeError Negative.invalidNestedUpdate
  , testCase "rejects SELECT followed by DELETE inside pgSelectWith" $
      assertPlacementTypeError Negative.invalidNestedSelectThenDelete
  , testCase "rejects DELETE followed by SELECT inside pgSelectWith" $
      assertPlacementTypeError Negative.invalidNestedDeleteThenSelect
  , testCase "conservatively rejects an empty INSERT inside pgSelectWith" $
      assertPlacementTypeError Negative.invalidNestedEmptyInsert
  , testCase "conservatively rejects an identity UPDATE inside pgSelectWith" $
      assertPlacementTypeError Negative.invalidNestedIdentityUpdate
  , testCase "placement cannot be bypassed with coerce" $
      assertPlacementTypeError Negative.invalidCoercedPlacement
  , testCase "rejects a recursively self-referencing INSERT CTE" $
      assertDeferredTypeErrorContaining
        ["CteTopLevelOnly", "CteNestedAllowed"]
        Negative.invalidRecursiveInsert
  ]

projectionValidationTests :: TestTree
projectionValidationTests = testGroup "Common table expression projection validation tests"
  [ testCase "rejects a zero-column SELECT CTE" $
      assertEmptyProjectionError emptySelectProjection
  , testCase "rejects a zero-column data-modifying CTE" $
      assertEmptyProjectionError emptyDeleteProjection
  ]

assertPlacementTypeError :: SqlSelect Postgres a -> Assertion
assertPlacementTypeError =
  assertDeferredTypeErrorContaining ["CteTopLevelOnly", "CteNestedAllowed"]

assertDeferredTypeErrorContaining
  :: [String]
  -> SqlSelect Postgres a
  -> Assertion
assertDeferredTypeErrorContaining expectedFragments sql = do
  result <- try (evaluate (BL.length (renderSelectBytes sql)))
  case result of
    Left (err :: TypeError) ->
      let message = show err
      in mapM_ (assertFragment message) expectedFragments
    Right _ ->
      assertFailure "expected the expression to contain a deferred type error"
  where
    assertFragment message fragment =
      assertBool ("mentions " ++ fragment) (fragment `isInfixOf` message)

assertEmptyProjectionError :: SqlSelect Postgres a -> Assertion
assertEmptyProjectionError sql = do
  result <- try (evaluate (BL.length (renderSelectBytes sql)))
  case result of
    Left (err :: ErrorCall) ->
      assertBool "explains the non-empty projection requirement"
        ("at least one column" `isInfixOf` show err)
    Right _ ->
      assertFailure "expected the zero-column CTE projection to be rejected"

-- A single top-level WITH block may freely mix SELECT and data-modifying CTE
-- bodies. Besides checking the individual keywords, this guards against
-- accidentally nesting a second WITH while combining the syntax fragments.
testMixedCteRendering :: TestTree
testMixedCteRendering = testCase "renders mixed SELECT, INSERT, UPDATE, and DELETE CTEs" $ do
  let sql = renderSelect mixedCteSelect
  assertBool "renders one top-level WITH" ("WITH " `isPrefixOf` sql)
  assertBool "does not render a nested WITH keyword" (not ("WITH WITH" `isInfixOf` sql))
  assertBool "renders INSERT" ("INSERT INTO" `isInfixOf` sql)
  assertBool "renders UPDATE" ("UPDATE" `isInfixOf` sql)
  assertBool "renders DELETE" ("DELETE FROM" `isInfixOf` sql)
  assertEqual "renders three RETURNING clauses" 3 (length (filter (== "RETURNING") (words sql)))

-- pgSelectWith remains available for its original purpose: embedding a
-- SELECT-only WITH block as a subquery.
testNestedSelectCteRendering :: TestTree
testNestedSelectCteRendering = testCase "SELECT CTEs remain valid inside pgSelectWith" $ do
  let sql = renderSelect nestedSelectCteSelect
  assertBool "renders an inner WITH" ("FROM (WITH " `isInfixOf` sql)

-- Closing the recursive SELECT portion with toTopLevel should preserve WITH
-- RECURSIVE while allowing a later DELETE CTE in the same top-level block.
testRecursiveSelectThenDeleteRendering :: TestTree
testRecursiveSelectThenDeleteRendering = testCase "recursive SELECT can feed a top-level DELETE CTE" $ do
  let sql = renderSelect recursiveSelectThenDeleteCteSelect
  assertBool "renders WITH RECURSIVE" ("WITH RECURSIVE " `isPrefixOf` sql)
  assertBool "renders DELETE" ("DELETE FROM" `isInfixOf` sql)

-- Value-level empty operations must not leave behind an empty or partial WITH
-- clause when the final SELECT is rendered.
testEmptyDataModifyingCtes :: TestTree
testEmptyDataModifyingCtes = testCase "omits empty INSERT and identity UPDATE CTEs" $ do
  let sql = renderSelect emptyDataModifyingCteSelect
  assertBool "does not render WITH" (not ("WITH " `isPrefixOf` sql))
  assertBool "does not render INSERT" (not ("INSERT INTO" `isInfixOf` sql))
  assertBool "does not render UPDATE" (not ("UPDATE" `isInfixOf` sql))

-- Each PostgreSQL DML consumer must place the WITH block before, rather than
-- inside, its terminal statement. These rendering checks cover the three
-- independent Sql* wrappers reconstructed by the public functions.
testWithDmlConsumerRendering :: TestTree
testWithDmlConsumerRendering = testCase "renders WITH before terminal INSERT, UPDATE, and DELETE" $ do
  assertWithTerminal "INSERT INTO" (renderInsert insertWithStatement)
  assertWithTerminal "UPDATE" (renderUpdate updateWithStatement)
  assertWithTerminal "DELETE FROM" (renderDelete deleteWithStatement)

-- A recursive SELECT CTE is legal before a terminal DML statement. This makes
-- sure pgInsertWith preserves the recursive flag collected by With.
testRecursiveInsertWithRendering :: TestTree
testRecursiveInsertWithRendering = testCase "renders WITH RECURSIVE before a terminal INSERT" $ do
  sql <- requireRenderedStatement (renderInsert recursiveInsertWithStatement)
  assertBool "starts with WITH RECURSIVE" ("WITH RECURSIVE " `isPrefixOf` sql)
  assertBool "renders terminal INSERT" (" INSERT INTO" `isInfixOf` sql)

-- Top-level DML consumers may accept the stronger CteTopLevelOnly placement.
-- A data-modifying CTE followed by DELETE exercises that fact at compile time
-- as well as checking the resulting SQL shape.
testTopLevelOnlyDmlConsumerRendering :: TestTree
testTopLevelOnlyDmlConsumerRendering = testCase "accepts a modifying CTE before terminal DELETE" $ do
  sql <- requireRenderedStatement (renderDelete topLevelOnlyDeleteWithStatement)
  assertBool "renders DELETE as the CTE body"
    ("AS (DELETE FROM" `isInfixOf` sql)
  assertBool "renders DELETE as the terminal statement"
    (") DELETE FROM" `isInfixOf` sql)

-- An empty INSERT and identity UPDATE have no terminal statement. PostgreSQL
-- cannot execute a bare WITH clause, so their consumers must retain the
-- existing no-op representation and discard the accumulated definitions.
testEmptyDmlConsumers :: TestTree
testEmptyDmlConsumers = testCase "keeps empty INSERT and identity UPDATE as no-ops" $ do
  assertEqual "empty INSERT has no syntax" Nothing
    (renderInsert emptyInsertWithStatement)
  assertEqual "identity UPDATE has no syntax" Nothing
    (renderUpdate identityUpdateWithStatement)

-- The consumers deliberately return the existing Sql* wrappers. Their
-- PgReturning instances must therefore remain usable without a parallel
-- pgInsertReturningWith/pgUpdateReturningWith/pgDeleteReturningWith API.
testReturningAfterDmlConsumers :: TestTree
testReturningAfterDmlConsumers = testCase "supports RETURNING after each terminal DML consumer" $ do
  assertReturning "INSERT" (renderInsertReturning (Pg.returning insertWithStatement id))
  assertReturning "UPDATE" (renderUpdateReturning (Pg.returning updateWithStatement id))
  assertReturning "DELETE" (renderDeleteReturning (Pg.returning deleteWithStatement id))

-- Rendering alone cannot verify PostgreSQL's execution and snapshot semantics.
-- This integration case checks both the RETURNING rows and the final table
-- state after all three modifying CTEs execute.
testMixedCteBodies :: IO ByteString -> TestTree
testMixedCteBodies getConn = testCase "SELECT and data-modifying CTEs can be mixed" $
  withTestPostgres "mixed_cte_bodies" getConn $ \conn -> do
    execute_ conn "CREATE TABLE cte_rows (id INT PRIMARY KEY, value TEXT NOT NULL)"
    execute_ conn "INSERT INTO cte_rows VALUES (1, 'selected'), (3, 'before-update'), (4, 'deleted')"

    result <- runBeamPostgres conn $ runSelectReturningList mixedCteSelect

    assertEqual "rows returned by each CTE"
      [ ( CteRow 1 "selected"
        , CteRow 2 "inserted"
        , CteRow 3 "updated"
        , CteRow 4 "deleted"
        )
      ]
      result

    remaining <- runBeamPostgres conn $ runSelectReturningList $ select $
      orderBy_ (asc_ . cteId) $ all_ (dbCteRows cteDb)
    assertEqual "data modifications were applied"
      [ CteRow 1 "selected"
      , CteRow 2 "inserted"
      , CteRow 3 "updated"
      ]
      remaining

-- Execute each terminal DML consumer against PostgreSQL. The three statements
-- use SELECT CTEs to choose or construct their affected rows, proving that the
-- reusable names remain visible to INSERT, UPDATE, and DELETE.
testWithDmlConsumers :: IO ByteString -> TestTree
testWithDmlConsumers getConn = testCase "WITH can terminate in INSERT, UPDATE, or DELETE" $
  withTestPostgres "with_dml_consumers" getConn $ \conn -> do
    execute_ conn "CREATE TABLE cte_rows (id INT PRIMARY KEY, value TEXT NOT NULL)"
    execute_ conn "INSERT INTO cte_rows VALUES (1, 'source'), (3, 'before-update'), (4, 'delete-me')"

    runBeamPostgres conn $ do
      runInsert insertWithStatement
      runUpdate updateWithStatement
      runDelete deleteWithStatement

    remaining <- runBeamPostgres conn $ runSelectReturningList $ select $
      orderBy_ (asc_ . cteId) $ all_ (dbCteRows cteDb)
    assertEqual "all terminal DML statements used their CTE rows"
      [ CteRow 1 "source"
      , CteRow 2 "inserted-with"
      , CteRow 3 "updated-with"
      ]
      remaining

-- Exercise the main user-facing flow: bind a normal SELECT CTE, perform each
-- supported data modification, then join all four reusable results in the final
-- SELECT. The placement of the complete block is inferred as top-level-only.
mixedCteSelect
  :: SqlSelect Postgres
       ( CteRowT Identity
       , CteRowT Identity
       , CteRowT Identity
       , CteRowT Identity
       )
mixedCteSelect = selectWith $ topLevelOnly $ do
  selected <- selecting $ do
    row <- all_ (dbCteRows cteDb)
    guard_ (cteId row ==. val_ 1)
    pure row

  inserted <- Pg.cteInsertReturning
    (dbCteRows cteDb)
    (insertValues [CteRow 2 "inserted"])
    Pg.onConflictDefault
    id

  updated <- Pg.cteUpdateReturning
    (dbCteRows cteDb)
    (\row -> cteValue row <-. val_ "updated")
    (\row -> cteId row ==. val_ 3)
    id

  deleted <- Pg.cteDeleteReturning
    (dbCteRows cteDb)
    (\row -> cteId row ==. val_ 4)
    id

  case (inserted, updated) of
    (Just inserted', Just updated') -> pure $ do
      selectedRow <- reuse selected
      insertedRow <- reuse inserted'
      updatedRow <- reuse updated'
      deletedRow <- reuse deleted
      pure (selectedRow, insertedRow, updatedRow, deletedRow)
    _ -> error "Expected non-empty INSERT and UPDATE CTEs"

nestedSelectCteSelect :: SqlSelect Postgres (CteRowT Identity)
nestedSelectCteSelect = select $ Pg.pgSelectWith $ nestedAllowed $ do
  selected <- selecting $ do
    row <- all_ (dbCteRows cteDb)
    guard_ (cteId row ==. val_ 1)
    pure row
  pure (reuse selected)

-- PostgreSQL permits a recursive SELECT CTE to feed a later modifying CTE, but
-- not a modifying CTE to recursively reference itself. 'toTopLevel' closes the
-- recursive SELECT knot before the DELETE is added.
recursiveSelectThenDeleteCteSelect :: SqlSelect Postgres (CteRowT Identity)
recursiveSelectThenDeleteCteSelect = selectWith $ do
  recursiveIds <- toTopLevel $ mdo
    ids <- selecting $
      pure (as_ @Int32 (val_ 1)) `unionAll_` do
        previousId <- reuse ids
        guard_ (previousId <. val_ 2)
        pure (previousId + 1)
    pure ids

  deleted <- Pg.cteDeleteReturning
    (dbCteRows cteDb)
    (\row -> exists_ $ do
      recursiveId <- reuse recursiveIds
      guard_ (cteId row ==. recursiveId)
      pure recursiveId)
    id

  pure (reuse deleted)

-- Empty INSERT values and identity UPDATE assignments do not produce SQL.
-- Their wrappers return Nothing, leaving selectWith to render the final query
-- without an empty WITH clause.
emptyDataModifyingCteSelect :: SqlSelect Postgres Int32
emptyDataModifyingCteSelect = selectWith $ do
  inserted <- Pg.cteInsertReturning
    (dbCteRows cteDb)
    SqlInsertValuesEmpty
    Pg.onConflictDefault
    id
  updated <- Pg.cteUpdateReturning
    (dbCteRows cteDb)
    (const mempty)
    (const (val_ True))
    id
  case (inserted, updated) of
    (Nothing, Nothing) -> pure finalQuery
    _ -> error "Expected empty INSERT and UPDATE CTEs"
  where
    finalQuery :: Q Postgres CteDb QBaseScope (QExpr Postgres QBaseScope Int32)
    finalQuery = pure (val_ 1)

-- Both expressions below are valid Beam projection shapes, but contain no
-- fields from which SQL columns could be built. They exercise the shared
-- validation for SELECT and data-modifying CTE bodies respectively.
emptySelectProjection :: SqlSelect Postgres (EmptyCteT Identity)
emptySelectProjection = selectWith $ do
  rows <- selecting
    (pure (EmptyCte :: EmptyCteT (QExpr Postgres CTE.QAnyScope)))
  pure (reuse rows)

emptyDeleteProjection :: SqlSelect Postgres (EmptyCteT Identity)
emptyDeleteProjection = selectWith $ do
  rows <- Pg.cteDeleteReturning
    (dbCteRows cteDb)
    (const (val_ False))
    (const (EmptyCte :: EmptyCteT (QExpr Postgres PostgresInaccessible)))
  pure (reuse rows)

-- Copy one row selected by the CTE into a new row. insertFrom is what exposes
-- the reusable query to the terminal INSERT source.
insertWithStatement :: SqlInsert Postgres CteRowT
insertWithStatement = Pg.pgInsertWith $ do
  source <- selecting $ do
    row <- all_ (dbCteRows cteDb)
    guard_ (cteId row ==. val_ 1)
    pure row
  pure $ Pg.insert
    (dbCteRows cteDb)
    (insertFrom $ do
      row <- reuse source
      pure (CteRow (cteId row + 1) (val_ "inserted-with")))
    Pg.onConflictDefault

-- Select the target key independently, then reference it through EXISTS in
-- the terminal UPDATE predicate.
updateWithStatement :: SqlUpdate Postgres CteRowT
updateWithStatement = Pg.pgUpdateWith $ do
  targets <- selecting $ do
    row <- all_ (dbCteRows cteDb)
    guard_ (cteId row ==. val_ 3)
    pure (cteId row)
  pure $ update
    (dbCteRows cteDb)
    (\row -> cteValue row <-. val_ "updated-with")
    (\row -> exists_ $ do
      targetId <- reuse targets
      guard_ (cteId row ==. targetId)
      pure targetId)

-- The DELETE form uses the same reusable-key pattern as UPDATE, exercising
-- the third terminal syntax wrapper.
deleteWithStatement :: SqlDelete Postgres CteRowT
deleteWithStatement = Pg.pgDeleteWith $ do
  targets <- selecting $ do
    row <- all_ (dbCteRows cteDb)
    guard_ (cteId row ==. val_ 4)
    pure (cteId row)
  pure $ delete (dbCteRows cteDb) $ \row -> exists_ $ do
    targetId <- reuse targets
    guard_ (cteId row ==. targetId)
    pure targetId

-- Recursion is completed while the block is still nested-safe. The terminal
-- INSERT then consumes the recursive result at top level.
recursiveInsertWithStatement :: SqlInsert Postgres CteRowT
recursiveInsertWithStatement = Pg.pgInsertWith $ mdo
  ids <- selecting $
    pure (as_ @Int32 (val_ 1)) `unionAll_` do
      previousId <- reuse ids
      guard_ (previousId <. val_ 2)
      pure (previousId + 1)
  pure $ Pg.insert
    (dbCteRows cteDb)
    (insertFrom $ do
      rowId <- reuse ids
      pure (CteRow rowId (val_ "recursive")))
    Pg.onConflictDefault

-- Adding a modifying CTE fixes the block to CteTopLevelOnly. pgDeleteWith is
-- a top-level consumer, so this remains well-typed.
topLevelOnlyDeleteWithStatement :: SqlDelete Postgres CteRowT
topLevelOnlyDeleteWithStatement = Pg.pgDeleteWith $ do
  _ <- Pg.cteDeleteReturning
    (dbCteRows cteDb)
    (\row -> cteId row ==. val_ 99)
    id
  pure $ delete
    (dbCteRows cteDb)
    (\row -> cteId row ==. val_ 100)

emptyInsertWithStatement :: SqlInsert Postgres CteRowT
emptyInsertWithStatement = Pg.pgInsertWith $ do
  _ <- selecting $ all_ (dbCteRows cteDb)
  pure $ Pg.insert
    (dbCteRows cteDb)
    SqlInsertValuesEmpty
    Pg.onConflictDefault

identityUpdateWithStatement :: SqlUpdate Postgres CteRowT
identityUpdateWithStatement = Pg.pgUpdateWith $ do
  _ <- selecting $ all_ (dbCteRows cteDb)
  pure $ update
    (dbCteRows cteDb)
    (const mempty)
    (const (val_ True))

assertWithTerminal
  :: String
  -> Maybe String
  -> Assertion
assertWithTerminal terminal rendered = do
  sql <- requireRenderedStatement rendered
  assertBool "starts with WITH" ("WITH " `isPrefixOf` sql)
  assertBool ("renders terminal " ++ terminal) ((" " ++ terminal) `isInfixOf` sql)

requireRenderedStatement
  :: Maybe String
  -> IO String
requireRenderedStatement rendered =
  case rendered of
    Nothing -> assertFailure "expected a PostgreSQL statement" >> pure ""
    Just sql -> pure sql

renderInsert :: SqlInsert Postgres table -> Maybe String
renderInsert SqlInsertNoRows = Nothing
renderInsert (SqlInsert _ (PgInsertSyntax syntax)) =
  Just (BL.unpack (pgRenderSyntaxScript syntax))

renderUpdate :: SqlUpdate Postgres table -> Maybe String
renderUpdate SqlIdentityUpdate = Nothing
renderUpdate (SqlUpdate _ (PgUpdateSyntax syntax)) =
  Just (BL.unpack (pgRenderSyntaxScript syntax))

renderDelete :: SqlDelete Postgres table -> Maybe String
renderDelete (SqlDelete _ (PgDeleteSyntax syntax)) =
  Just (BL.unpack (pgRenderSyntaxScript syntax))

assertReturning :: String -> Maybe String -> Assertion
assertReturning command rendered = do
  sql <- requireRenderedStatement rendered
  assertBool (command ++ " retains its WITH prefix") ("WITH " `isPrefixOf` sql)
  assertBool (command ++ " renders RETURNING") (" RETURNING " `isInfixOf` sql)

renderInsertReturning :: Pg.PgInsertReturning a -> Maybe String
renderInsertReturning Pg.PgInsertReturningEmpty = Nothing
renderInsertReturning (Pg.PgInsertReturning syntax) =
  Just (BL.unpack (pgRenderSyntaxScript syntax))

renderUpdateReturning :: Pg.PgUpdateReturning a -> Maybe String
renderUpdateReturning Pg.PgUpdateReturningEmpty = Nothing
renderUpdateReturning (Pg.PgUpdateReturning syntax) =
  Just (BL.unpack (pgRenderSyntaxScript syntax))

renderDeleteReturning :: Pg.PgDeleteReturning a -> Maybe String
renderDeleteReturning (Pg.PgDeleteReturning syntax) =
  Just (BL.unpack (pgRenderSyntaxScript syntax))

renderSelect :: SqlSelect Postgres a -> String
renderSelect = BL.unpack . renderSelectBytes

renderSelectBytes :: SqlSelect Postgres a -> BL.ByteString
renderSelectBytes (SqlSelect (PgSelectSyntax syntax)) =
  pgRenderSyntaxScript syntax

topLevelOnly
  :: With be db 'CteTopLevelOnly a
  -> With be db 'CteTopLevelOnly a
topLevelOnly = id

nestedAllowed
  :: With be db 'CteNestedAllowed a
  -> With be db 'CteNestedAllowed a
nestedAllowed = id
