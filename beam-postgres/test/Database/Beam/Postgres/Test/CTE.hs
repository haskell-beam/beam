{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Rendering, type-safety, and PostgreSQL integration tests for common table
-- expressions. Deliberately ill-typed expressions live in
-- "Database.Beam.Postgres.Test.CTENegative" so this module retains normal type
-- checking.
module Database.Beam.Postgres.Test.CTE (unitTests, integrationTests) where

import Control.Exception (TypeError, evaluate, try)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Kind (Type)
import Data.List (isInfixOf, isPrefixOf, sortOn)
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

import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
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

-- A legal Haskell projection shape with no fields. PostgreSQL represents this
-- degree-zero relation by omitting the CTE column-alias list. Data-modifying
-- CTEs with RETURNING use a private physical sentinel to satisfy PostgreSQL's
-- grammar while retaining this zero-field shape at Beam's public boundary.
data EmptyCteT (f :: Type -> Type) = EmptyCte
  deriving (Generic, Beamable)

deriving instance Show (EmptyCteT Identity)
deriving instance Eq (EmptyCteT Identity)

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
  ]

integrationTests :: IO ByteString -> TestTree
integrationTests getConn = testGroup "Common table expression integration tests"
  [ testMixedCteBodies getConn
  , testSideEffectOnlyCtes getConn
  , testMaterializationExecution getConn
  , testLiftedWithExecution getConn
  , testWithDmlConsumers getConn
  , testCteParameterOrdering getConn
  , testDataModifyingCteModel getConn
  , testSideEffectOnlyCteModel getConn
  , testWithDmlConsumerModel getConn
  , testRecursiveCteModel getConn
  , testDegreeZeroSelects getConn
  , testDegreeZeroDataModifyingCtes getConn
  , testDegreeZeroRepeatedReuse getConn
  ]

renderingTests :: TestTree
renderingTests = testGroup "Common table expression rendering tests"
  [ testMixedCteRendering
  , testMaterializationRendering
  , testNestedMaterializedCteRendering
  , testSideEffectOnlyRendering
  , testSideEffectNoOps
  , testLiftedWithNameSupply
  , testNestedSelectCteRendering
  , testRecursiveSelectThenDeleteRendering
  , testEmptyDataModifyingCtes
  , testWithDmlConsumerRendering
  , testRecursiveInsertWithRendering
  , testTopLevelOnlyDmlConsumerRendering
  , testEmptyDmlConsumers
  , testReturningAfterDmlConsumers
  , testDegreeZeroSelectRendering
  , testDegreeZeroDataModifyingRendering
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
  , testCase "rejects a side-effect-only DELETE inside pgSelectWithNested" $
      assertPlacementTypeError Negative.invalidNestedSideEffectDelete
  , testCase "placement cannot be bypassed with coerce" $
      assertPlacementTypeError Negative.invalidCoercedPlacement
  , testCase "rejects a recursively self-referencing INSERT CTE" $
      assertDeferredTypeErrorContaining
        ["No instance", "MonadFix", "PgCteTopLevelOnly"]
        Negative.invalidRecursiveInsert
  , testCase "side-effect-only CTE results cannot be reused" $
      assertDeferredTypeErrorContaining
        ["ReusableQ"]
        Negative.invalidReuseSideEffect
  ]

assertPlacementTypeError :: SqlSelect Postgres a -> Assertion
assertPlacementTypeError =
  assertDeferredTypeErrorContaining ["PgCteTopLevelOnly", "PgCteNestedAllowed"]

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
      assertBool
        ("mentions " ++ fragment ++ "\nDeferred error was:\n" ++ message)
        (fragment `isInfixOf` message)

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

-- Materialization is an explicit PostgreSQL 12+ spelling choice. Check the
-- complete token rather than a loose MATERIALIZED substring, since the latter
-- would make the NOT MATERIALIZED case pass the positive assertion too.
testMaterializationRendering :: TestTree
testMaterializationRendering = testCase "renders every SELECT CTE materialization policy" $ do
  let defaultSql = renderSelect (materializationSelect Pg.PgCteDefault)
      materializedSql = renderSelect (materializationSelect Pg.PgCteMaterialized)
      notMaterializedSql = renderSelect (materializationSelect Pg.PgCteNotMaterialized)
  assertBool "default omits MATERIALIZED"
    (not (" MATERIALIZED (" `isInfixOf` defaultSql))
  assertBool "default omits NOT MATERIALIZED"
    (not (" NOT MATERIALIZED (" `isInfixOf` defaultSql))
  assertBool "renders AS MATERIALIZED"
    (" AS MATERIALIZED (" `isInfixOf` materializedSql)
  assertBool "renders AS NOT MATERIALIZED"
    (" AS NOT MATERIALIZED (" `isInfixOf` notMaterializedSql)

-- pgSelectWithNested is the safe nested consumer for PostgreSQL-specific
-- SELECT CTE features. This complements the compatibility test for the older
-- pgSelectWith API below.
testNestedMaterializedCteRendering :: TestTree
testNestedMaterializedCteRendering = testCase "embeds a materialized PgWith block in a subquery" $ do
  let sql = renderSelect nestedMaterializedCteSelect
  assertBool "renders the nested WITH"
    ("FROM (WITH " `isInfixOf` sql)
  assertBool "retains the materialization modifier"
    (" AS MATERIALIZED (" `isInfixOf` sql)

-- DML without RETURNING is still executed by PostgreSQL but forms no temporary
-- table and exposes no reusable result. Its CTE name must therefore have no
-- empty column-alias list.
testSideEffectOnlyRendering :: TestTree
testSideEffectOnlyRendering = testCase "renders side-effect-only INSERT, UPDATE, and DELETE CTEs" $ do
  let sql = renderSelect sideEffectOnlyCteSelect
  assertBool "renders INSERT" ("INSERT INTO" `isInfixOf` sql)
  assertBool "renders UPDATE" ("UPDATE" `isInfixOf` sql)
  assertBool "renders DELETE" ("DELETE FROM" `isInfixOf` sql)
  assertBool "does not render RETURNING" (not ("RETURNING" `isInfixOf` sql))
  assertBool "does not render an empty alias list" (not ("() AS" `isInfixOf` sql))

-- Empty inserts and identity updates should consume neither a name nor a CTE
-- slot. With no other CTEs, the final query must not acquire an empty WITH.
testSideEffectNoOps :: TestTree
testSideEffectNoOps = testCase "omits side-effect-only empty INSERT and identity UPDATE" $ do
  let sql = renderSelect sideEffectNoOpSelect
  assertBool "does not render WITH" (not ("WITH " `isPrefixOf` sql))
  assertBool "does not render INSERT" (not ("INSERT INTO" `isInfixOf` sql))
  assertBool "does not render UPDATE" (not ("UPDATE" `isInfixOf` sql))

-- Lifting a complete portable helper must not restart its State Int name
-- supply. Four definitions from native/lifted/native construction should be
-- allocated exactly once as cte0 through cte3.
testLiftedWithNameSupply :: TestTree
testLiftedWithNameSupply = testCase "shares CTE names across lifted and native builders" $ do
  let sql = renderSelect liftedWithSelect
  mapM_ (\name -> assertBool ("renders " ++ name) (("\"" ++ name ++ "\"") `isInfixOf` sql))
    ["cte0", "cte1", "cte2", "cte3"]
  assertBool "does not allocate cte4" (not ("\"cte4\"" `isInfixOf` sql))

-- pgSelectWith remains available for its original purpose: embedding a
-- SELECT-only WITH block as a subquery.
testNestedSelectCteRendering :: TestTree
testNestedSelectCteRendering = testCase "SELECT CTEs remain valid inside pgSelectWith" $ do
  let sql = renderSelect nestedSelectCteSelect
  assertBool "renders an inner WITH" ("FROM (WITH " `isInfixOf` sql)

-- Closing the recursive SELECT portion with pgToTopLevel should preserve WITH
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
-- inside, its terminal statement. These rendering checks cover INSERT, UPDATE,
-- and DELETE while retaining Beam's existing Sql* result types.
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

-- Top-level DML consumers may accept the stronger PgCteTopLevelOnly placement.
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

-- PostgreSQL's syntax for a degree-zero CTE has no column-alias parentheses.
-- Cover both the portable selecting renderer and the native renderer, including
-- nested and explicit materialization forms, because they enter PostgreSQL
-- syntax through different code paths.
testDegreeZeroSelectRendering :: TestTree
testDegreeZeroSelectRendering = testCase "renders reusable degree-zero SELECT CTEs" $ do
  let portableSql = renderSelect emptySelectProjection
      nativeSql = renderSelect
        (emptyNativeSelectProjection Pg.PgCteDefault)
      materializedSql = renderSelect
        (emptyNativeSelectProjection Pg.PgCteMaterialized)
      nestedSql = renderSelect nestedEmptySelectProjection

  mapM_ assertDegreeZeroSelect
    [portableSql, nativeSql, materializedSql, nestedSql]
  assertBool "retains explicit materialization"
    (" AS MATERIALIZED (" `isInfixOf` materializedSql)
  assertBool "remains valid in a nested SELECT"
    ("FROM (WITH " `isInfixOf` nestedSql)
  where
    assertDegreeZeroSelect sql = do
      assertBool "does not render an empty CTE alias list"
        (not ("\"cte0\"()" `isInfixOf` sql))
      assertBool "the CTE body projects no columns"
        ("SELECT  FROM" `isInfixOf` sql || "SELECT FROM" `isInfixOf` sql)
      assertBool "the consumer projects no columns"
        ("SELECT  FROM \"cte0\"" `isInfixOf` sql ||
         "SELECT FROM \"cte0\"" `isInfixOf` sql)

-- INSERT, UPDATE, and DELETE share the sentinel path but have independent
-- RETURNING renderers. Assert every spelling, including that the physical
-- sentinel is declared once and is not selected by the zero-field consumer.
testDegreeZeroDataModifyingRendering :: TestTree
testDegreeZeroDataModifyingRendering =
  testCase "renders reusable degree-zero data-modifying CTEs" $
    mapM_ assertDegreeZeroDml
      [ ("INSERT", renderSelect (emptyInsertProjection [CteRow 1 "one"]))
      , ("UPDATE", renderSelect (emptyUpdateProjection 1 "updated"))
      , ("DELETE", renderSelect (emptyDeleteProjection 1))
      ]
  where
    assertDegreeZeroDml (command, sql) = do
      assertBool (command ++ " declares one physical sentinel")
        ("\"cte0\"(\"res0\") AS" `isInfixOf` sql)
      assertBool (command ++ " appends a valid RETURNING expression")
        (" RETURNING NULL::boolean" `isInfixOf` sql)
      assertEqual (command ++ " emits one RETURNING keyword")
        1
        (length (filter (== "RETURNING") (words sql)))
      assertBool (command ++ " does not expose the sentinel")
        ("SELECT  FROM \"cte0\"" `isInfixOf` sql ||
         "SELECT FROM \"cte0\"" `isInfixOf` sql)

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

-- PostgreSQL executes a modifying CTE exactly once even when it has no
-- RETURNING clause and the terminal SELECT does not reference it. Verify that
-- all three commands affect the final database state, not merely that their
-- syntax parses.
testSideEffectOnlyCtes :: IO ByteString -> TestTree
testSideEffectOnlyCtes getConn = testCase "unreferenced side-effect-only CTEs execute once" $
  withTestPostgres "side_effect_only_ctes" getConn $ \conn -> do
    execute_ conn "CREATE TABLE cte_rows (id INT PRIMARY KEY, value TEXT NOT NULL)"
    execute_ conn "INSERT INTO cte_rows VALUES (1, 'unchanged'), (2, 'before-update'), (3, 'delete-me')"

    marker <- runBeamPostgres conn $
      runSelectReturningOne sideEffectOnlyCteSelect
    assertEqual "terminal SELECT still runs" (Just (1 :: Int32)) marker

    remaining <- runBeamPostgres conn $ runSelectReturningList $ select $
      orderBy_ (asc_ . cteId) $ all_ (dbCteRows cteDb)
    assertEqual "all expected side effects were applied"
      [ CteRow 1 "unchanged"
      , CteRow 2 "after-update"
      , CteRow 4 "inserted"
      ]
      remaining

-- Planner choices are deliberately not asserted because they may vary across
-- PostgreSQL releases. Successful execution and equal results validate the two
-- explicit PostgreSQL 12+ spellings without coupling the test to EXPLAIN.
testMaterializationExecution :: IO ByteString -> TestTree
testMaterializationExecution getConn = testCase "MATERIALIZED and NOT MATERIALIZED execute" $
  withTestPostgres "cte_materialization" getConn $ \conn -> do
    execute_ conn "CREATE TABLE cte_rows (id INT PRIMARY KEY, value TEXT NOT NULL)"
    execute_ conn "INSERT INTO cte_rows VALUES (1, 'one'), (2, 'two')"

    materialized <- runBeamPostgres conn $ runSelectReturningList $
      materializationSelect Pg.PgCteMaterialized
    notMaterialized <- runBeamPostgres conn $ runSelectReturningList $
      materializationSelect Pg.PgCteNotMaterialized
    assertEqual "both policies preserve query results" materialized notMaterialized

-- Rendering checks the shared name supply; execution additionally proves that
-- ReusableQ values returned by a lifted multi-CTE helper retain their meaning.
testLiftedWithExecution :: IO ByteString -> TestTree
testLiftedWithExecution getConn = testCase "a lifted multi-CTE helper remains reusable" $
  withTestPostgres "lifted_with_execution" getConn $ \conn -> do
    lifted <- runBeamPostgres conn $ runSelectReturningList liftedWithSelect
    assertEqual "a lifted multi-CTE helper remains reusable"
      [(1, 11, 12)]
      lifted

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

-- PostgreSQL receives Beam values separately from the rendered placeholders.
-- Generate distinct values at each syntactic level so any disagreement between
-- syntax construction order and parameter collection order becomes observable
-- in the returned rows, rather than merely producing valid-looking SQL.
testCteParameterOrdering :: IO ByteString -> TestTree
testCteParameterOrdering getConn = testCase "preserves parameter order across CTE bodies and the terminal query" $
  withTestPostgres "cte_parameter_ordering_property" getConn $ \conn -> do
    passes <- Hedgehog.check . Hedgehog.property $ do
      baseId <- Hedgehog.forAll (Gen.int (Range.linear (-100000) 100000))
      firstOffset <- Hedgehog.forAll (Gen.int (Range.linear 1 1000))
      secondOffset <- Hedgehog.forAll (Gen.int (Range.linear 1 1000))
      payload <- Hedgehog.forAll (Gen.text (Range.linear 0 24) Gen.alphaNum)

      let first = CteRow (fromIntegral baseId) ("first:" <> payload)
          second = CteRow
            (fromIntegral (baseId + firstOffset))
            ("second:" <> payload)
          terminal = CteRow
            (fromIntegral (baseId + firstOffset + secondOffset))
            ("terminal:" <> payload)

      actual <- Hedgehog.evalIO $ runBeamPostgres conn $
        runSelectReturningList (parameterOrderingSelect first second terminal)

      actual Hedgehog.=== [(first, second, terminal)]

    assertBool "CTE parameter-ordering property failed" passes

-- Model a statement containing all three kinds of data-modifying CTE. The
-- operations use disjoint keys, avoiding PostgreSQL's deliberately unspecified
-- ordering when sibling modifying CTEs affect the same row. Both RETURNING
-- values and durable table state are compared with the pure expected result.
testDataModifyingCteModel :: IO ByteString -> TestTree
testDataModifyingCteModel getConn = testCase "data-modifying CTEs agree with a pure table model" $
  withTestPostgres "data_modifying_cte_model_property" getConn $ \conn -> do
    execute_ conn "CREATE TABLE cte_rows (id INT PRIMARY KEY, value TEXT NOT NULL)"

    passes <- Hedgehog.check . Hedgehog.property $ do
      baseId <- Hedgehog.forAll (Gen.int (Range.linear (-100000) 96000))
      payload <- Hedgehog.forAll (Gen.text (Range.linear 0 24) Gen.alphaNum)

      let inserted = CteRow (fromIntegral baseId) ("inserted:" <> payload)
          beforeUpdate = CteRow (fromIntegral (baseId + 1)) ("before-update:" <> payload)
          updated = CteRow (cteId beforeUpdate) ("updated:" <> payload)
          deleted = CteRow (fromIntegral (baseId + 2)) ("deleted:" <> payload)
          untouched = CteRow (fromIntegral (baseId + 3)) ("untouched:" <> payload)
          initial = [beforeUpdate, deleted, untouched]
          expectedFinal = [inserted, updated, untouched]

      Hedgehog.evalIO $ do
        execute_ conn "TRUNCATE TABLE cte_rows"
        runBeamPostgres conn $ runInsert $
          insert (dbCteRows cteDb) (insertValues initial)

      returned <- Hedgehog.evalIO $ runBeamPostgres conn $
        runSelectReturningList $
          dataModifyingCteModelSelect inserted (cteId updated) (cteValue updated) (cteId deleted)

      finalRows <- Hedgehog.evalIO $ runBeamPostgres conn $
        runSelectReturningList $ select $
          orderBy_ (asc_ . cteId) $ all_ (dbCteRows cteDb)

      returned Hedgehog.=== [(inserted, updated, deleted)]
      finalRows Hedgehog.=== expectedFinal

    assertBool "data-modifying CTE model property failed" passes

-- Repeat the three-operation model without RETURNING. This catches parameter
-- ordering or accidental omission in the side-effect-only path by
-- comparing durable state over generated inputs.
testSideEffectOnlyCteModel :: IO ByteString -> TestTree
testSideEffectOnlyCteModel getConn = testCase "side-effect-only CTEs agree with a pure table model" $
  withTestPostgres "side_effect_only_cte_model_property" getConn $ \conn -> do
    execute_ conn "CREATE TABLE cte_rows (id INT PRIMARY KEY, value TEXT NOT NULL)"

    passes <- Hedgehog.check . Hedgehog.property $ do
      baseId <- Hedgehog.forAll (Gen.int (Range.linear (-100000) 96000))
      payload <- Hedgehog.forAll (Gen.text (Range.linear 0 24) Gen.alphaNum)

      let inserted = CteRow (fromIntegral baseId) ("inserted:" <> payload)
          beforeUpdate = CteRow (fromIntegral (baseId + 1)) ("before-update:" <> payload)
          updated = CteRow (cteId beforeUpdate) ("updated:" <> payload)
          deleted = CteRow (fromIntegral (baseId + 2)) ("deleted:" <> payload)
          untouched = CteRow (fromIntegral (baseId + 3)) ("untouched:" <> payload)
          initial = [beforeUpdate, deleted, untouched]
          expectedFinal = [inserted, updated, untouched]

      Hedgehog.evalIO $ do
        execute_ conn "TRUNCATE TABLE cte_rows"
        runBeamPostgres conn $ runInsert $
          insert (dbCteRows cteDb) (insertValues initial)

      marker <- Hedgehog.evalIO $ runBeamPostgres conn $
        runSelectReturningOne $
          sideEffectCteModelSelect inserted (cteId updated) (cteValue updated) (cteId deleted)
      finalRows <- Hedgehog.evalIO $ runBeamPostgres conn $
        runSelectReturningList $ select $
          orderBy_ (asc_ . cteId) $ all_ (dbCteRows cteDb)

      marker Hedgehog.=== Just (1 :: Int32)
      finalRows Hedgehog.=== expectedFinal

    assertBool "side-effect-only CTE model property failed" passes

-- Exercise each top-level WITH consumer with independently generated values.
-- RETURNING results prove that the existing PostgreSQL execution instances can
-- still consume the Sql* wrappers, while the final table comparison checks the
-- combined INSERT, UPDATE, and DELETE behavior against a pure model.
testWithDmlConsumerModel :: IO ByteString -> TestTree
testWithDmlConsumerModel getConn = testCase "WITH DML consumers agree with a pure table model" $
  withTestPostgres "with_dml_consumer_model_property" getConn $ \conn -> do
    execute_ conn "CREATE TABLE cte_rows (id INT PRIMARY KEY, value TEXT NOT NULL)"

    passes <- Hedgehog.check . Hedgehog.property $ do
      baseId <- Hedgehog.forAll (Gen.int (Range.linear (-100000) 95000))
      payload <- Hedgehog.forAll (Gen.text (Range.linear 0 24) Gen.alphaNum)

      let source = CteRow (fromIntegral baseId) ("source:" <> payload)
          inserted = CteRow (fromIntegral (baseId + 1)) ("inserted:" <> payload)
          beforeUpdate = CteRow (fromIntegral (baseId + 2)) ("before-update:" <> payload)
          updated = CteRow (cteId beforeUpdate) ("updated:" <> payload)
          deleted = CteRow (fromIntegral (baseId + 3)) ("deleted:" <> payload)
          untouched = CteRow (fromIntegral (baseId + 4)) ("untouched:" <> payload)
          initial = [source, beforeUpdate, deleted, untouched]
          expectedFinal = [source, inserted, updated, untouched]

      Hedgehog.evalIO $ do
        execute_ conn "TRUNCATE TABLE cte_rows"
        runBeamPostgres conn $ runInsert $
          insert (dbCteRows cteDb) (insertValues initial)

      (insertedRows, updatedRows, deletedRows) <- Hedgehog.evalIO $
        runBeamPostgres conn $ do
          insertedRows <- Pg.runPgInsertReturningList $ Pg.returning
            (modelInsertWithStatement (cteId source) inserted) id
          updatedRows <- Pg.runPgUpdateReturningList $ Pg.returning
            (modelUpdateWithStatement (cteId updated) (cteValue updated)) id
          deletedRows <- Pg.runPgDeleteReturningList $ Pg.returning
            (modelDeleteWithStatement (cteId deleted)) id
          pure (insertedRows, updatedRows, deletedRows)

      finalRows <- Hedgehog.evalIO $ runBeamPostgres conn $
        runSelectReturningList $ select $
          orderBy_ (asc_ . cteId) $ all_ (dbCteRows cteDb)

      insertedRows Hedgehog.=== [inserted]
      updatedRows Hedgehog.=== [updated]
      deletedRows Hedgehog.=== [deleted]
      finalRows Hedgehog.=== expectedFinal

    assertBool "WITH DML consumer model property failed" passes

-- Generate a bounded recursive sequence, use it to drive a DELETE CTE, and
-- compare both the returned rows and remaining table against the corresponding
-- Haskell lists. This executes the recursive SELECT, its pgToTopLevel promotion,
-- and the following modifying CTE rather than checking only rendered keywords.
testRecursiveCteModel :: IO ByteString -> TestTree
testRecursiveCteModel getConn = testCase "recursive CTE execution agrees with a bounded sequence model" $
  withTestPostgres "recursive_cte_model_property" getConn $ \conn -> do
    execute_ conn "CREATE TABLE cte_rows (id INT PRIMARY KEY, value TEXT NOT NULL)"

    passes <- Hedgehog.check . Hedgehog.property $ do
      start <- Hedgehog.forAll (Gen.int (Range.linear (-100000) 95000))
      count <- Hedgehog.forAll (Gen.int (Range.linear 1 25))
      payload <- Hedgehog.forAll (Gen.text (Range.linear 0 24) Gen.alphaNum)

      let startId = fromIntegral start
          endId = fromIntegral (start + count - 1)
          recursiveRows =
            [ CteRow (fromIntegral rowId) ("recursive:" <> payload)
            | rowId <- [start .. start + count - 1]
            ]
          untouched = CteRow (fromIntegral (start + count)) ("untouched:" <> payload)

      Hedgehog.evalIO $ do
        execute_ conn "TRUNCATE TABLE cte_rows"
        runBeamPostgres conn $ runInsert $
          insert (dbCteRows cteDb) (insertValues (recursiveRows ++ [untouched]))

      deletedRows <- Hedgehog.evalIO $ runBeamPostgres conn $
        runSelectReturningList (recursiveCteModelSelect startId endId)
      finalRows <- Hedgehog.evalIO $ runBeamPostgres conn $
        runSelectReturningList $ select $
          orderBy_ (asc_ . cteId) $ all_ (dbCteRows cteDb)

      sortOn cteId deletedRows Hedgehog.=== recursiveRows
      finalRows Hedgehog.=== [untouched]

    assertBool "recursive CTE model property failed" passes

-- A row need not contain a projected value. PostgreSQL still returns one
-- zero-field result for every source row, and postgresql-simple must decode the
-- final rows without expecting any result fields. Exercise both the portable
-- and native builders and both explicit materialization policies.
testDegreeZeroSelects :: IO ByteString -> TestTree
testDegreeZeroSelects getConn = testCase "degree-zero SELECT CTEs preserve source cardinality" $
  withTestPostgres "degree_zero_select_ctes" getConn $ \conn -> do
    execute_ conn "CREATE TABLE cte_rows (id INT PRIMARY KEY, value TEXT NOT NULL)"

    emptySummary <- runBeamPostgres conn $ runSelectReturningOne $
      emptySelectSummary
    assertEqual "an empty degree-zero relation has count zero and is not present"
      (Just (0, False))
      emptySummary

    execute_ conn "INSERT INTO cte_rows VALUES (1, 'one'), (2, 'two'), (3, 'three')"

    portable <- runBeamPostgres conn $
      runSelectReturningList emptySelectProjection
    native <- runBeamPostgres conn $ runSelectReturningList $
      emptyNativeSelectProjection Pg.PgCteDefault
    materialized <- runBeamPostgres conn $ runSelectReturningList $
      emptyNativeSelectProjection Pg.PgCteMaterialized
    notMaterialized <- runBeamPostgres conn $ runSelectReturningList $
      emptyNativeSelectProjection Pg.PgCteNotMaterialized
    populatedSummary <- runBeamPostgres conn $ runSelectReturningOne $
      emptySelectSummary

    let expected = replicate 3 EmptyCte
    assertEqual "portable selecting preserves cardinality" expected portable
    assertEqual "native default preserves cardinality" expected native
    assertEqual "MATERIALIZED preserves cardinality" expected materialized
    assertEqual "NOT MATERIALIZED preserves cardinality" expected notMaterialized
    assertEqual "aggregates and EXISTS observe degree-zero rows"
      (Just (3, True))
      populatedSummary

-- Each modifying command has a separate RETURNING renderer. Besides validating
-- all three, this checks the boundary cases of several affected rows and no
-- affected rows, verifies that the private sentinel is not passed to the row
-- decoder, and compares the resulting durable table state.
testDegreeZeroDataModifyingCtes :: IO ByteString -> TestTree
testDegreeZeroDataModifyingCtes getConn =
  testCase "degree-zero modifying CTEs preserve affected-row cardinality" $
    withTestPostgres "degree_zero_modifying_ctes" getConn $ \conn -> do
      execute_ conn "CREATE TABLE cte_rows (id INT PRIMARY KEY, value TEXT NOT NULL)"
      execute_ conn "INSERT INTO cte_rows VALUES (1, 'one'), (2, 'two'), (3, 'three')"

      inserted <- runBeamPostgres conn $ runSelectReturningList $
        emptyInsertProjection [CteRow 4 "four", CteRow 5 "five"]
      updated <- runBeamPostgres conn $ runSelectReturningList $
        emptyUpdateProjection 2 "updated"
      deleted <- runBeamPostgres conn $ runSelectReturningList $
        emptyDeleteProjection 3
      deletedNone <- runBeamPostgres conn $ runSelectReturningList $
        emptyDeleteProjection 99

      assertEqual "INSERT retains two affected rows"
        (replicate 2 EmptyCte) inserted
      assertEqual "UPDATE retains two affected rows"
        (replicate 2 EmptyCte) updated
      assertEqual "DELETE retains three affected rows"
        (replicate 3 EmptyCte) deleted
      assertEqual "a command affecting no rows returns an empty relation"
        [] deletedNone

      remaining <- runBeamPostgres conn $ runSelectReturningList $ select $
        orderBy_ (asc_ . cteId) $ all_ (dbCteRows cteDb)
      assertEqual "all modifying commands applied their side effects"
        [CteRow 1 "updated", CteRow 2 "updated"]
        remaining

-- Reusing a degree-zero modifying CTE twice is a useful stress case: no field
-- can carry cardinality through the query, so the nine rows show that both
-- references read the same three-row RETURNING result. The final table state
-- separately confirms that the DELETE removed every source row.
testDegreeZeroRepeatedReuse :: IO ByteString -> TestTree
testDegreeZeroRepeatedReuse getConn =
  testCase "repeated degree-zero reuse preserves relational cardinality" $
    withTestPostgres "degree_zero_repeated_reuse" getConn $ \conn -> do
      execute_ conn "CREATE TABLE cte_rows (id INT PRIMARY KEY, value TEXT NOT NULL)"
      execute_ conn "INSERT INTO cte_rows VALUES (1, 'one'), (2, 'two'), (3, 'three')"

      summary <- runBeamPostgres conn $ runSelectReturningOne $
        emptyDeleteSummary
      assertEqual "COUNT and EXISTS observe every returned DELETE row"
        (Just (3, True))
        summary

      execute_ conn "INSERT INTO cte_rows VALUES (1, 'one'), (2, 'two'), (3, 'three')"
      products <- runBeamPostgres conn $ runSelectReturningList $
        repeatedEmptyDeleteProjection
      assertEqual "two references form the expected Cartesian product"
        (replicate 9 EmptyCte)
        products

      remaining <- runBeamPostgres conn $ runSelectReturningList $ select $
        all_ (dbCteRows cteDb)
      assertEqual "the modifying CTE deletes every source row"
        []
        remaining

materializationSelect
  :: Pg.PgCteMaterialization
  -> SqlSelect Postgres (CteRowT Identity)
materializationSelect materialization = Pg.pgSelectWithTopLevel $ do
  rows <- Pg.pgSelectingWith materialization $ all_ (dbCteRows cteDb)
  pure (reuse rows)

nestedMaterializedCteSelect :: SqlSelect Postgres (CteRowT Identity)
nestedMaterializedCteSelect = select $ Pg.pgSelectWithNested $ do
  rows <- Pg.pgSelectingWith Pg.PgCteMaterialized $
    all_ (dbCteRows cteDb)
  pure (reuse rows)

sideEffectOnlyCteSelect :: SqlSelect Postgres Int32
sideEffectOnlyCteSelect = Pg.pgSelectWithTopLevel $ do
  Pg.cteInsert
    (dbCteRows cteDb)
    (insertValues [CteRow 4 "inserted"])
    Pg.onConflictDefault
  Pg.cteUpdate
    (dbCteRows cteDb)
    (\row -> cteValue row <-. val_ "after-update")
    (\row -> cteId row ==. val_ 2)
  Pg.cteDelete
    (dbCteRows cteDb)
    (\row -> cteId row ==. val_ 3)
  pure finalMarkerQuery

sideEffectNoOpSelect :: SqlSelect Postgres Int32
sideEffectNoOpSelect = Pg.pgSelectWithTopLevel $ do
  Pg.cteInsert
    (dbCteRows cteDb)
    SqlInsertValuesEmpty
    Pg.onConflictDefault
  Pg.cteUpdate
    (dbCteRows cteDb)
    (const mempty)
    (const (val_ True))
  pure finalMarkerQuery

finalMarkerQuery
  :: Q Postgres CteDb QBaseScope (QExpr Postgres QBaseScope Int32)
finalMarkerQuery = pure (val_ 1)

-- A complete two-CTE portable helper is lifted as one action. Its internal
-- dependency also proves that lifting preserves ReusableQ values, not only the
-- emitted syntax fragments.
portableWithHelper
  :: With Postgres CteDb
       (ReusableQ Postgres CteDb (QExpr Postgres CTE.QAnyScope Int32))
portableWithHelper = do
  first <- selecting $ pure (as_ @Int32 (val_ 10))
  selecting $ do
    value <- reuse first
    pure (value + 1)

liftedWithSelect
  :: SqlSelect Postgres
       (Int32, Int32, Int32)
liftedWithSelect = Pg.pgSelectWithTopLevel $ do
  nativeBefore <- Pg.pgSelecting $ pure (as_ @Int32 (val_ 1))
  lifted <- Pg.pgLiftWith portableWithHelper
  nativeAfter <- Pg.pgSelecting $ do
    value <- reuse lifted
    pure (value + 1)
  pure $ do
    before <- reuse nativeBefore
    middle <- reuse lifted
    after <- reuse nativeAfter
    pure (before, middle, after)

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
mixedCteSelect = Pg.pgSelectWithTopLevel $ do
  selected <- Pg.pgSelecting $ do
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

-- Place values in two dependent CTE bodies and in the terminating SELECT. The
-- dependency prevents the second CTE from becoming an unrelated test fragment,
-- while the result exposes every bound value for exact comparison.
parameterOrderingSelect
  :: CteRowT Identity
  -> CteRowT Identity
  -> CteRowT Identity
  -> SqlSelect Postgres
       (CteRowT Identity, CteRowT Identity, CteRowT Identity)
parameterOrderingSelect first second terminal = selectWith $ do
  firstRows <- selecting $ pure (cteRowValues_ @CTE.QAnyScope first)
  secondRows <- selecting $ do
    _ <- reuse firstRows
    pure (cteRowValues_ @CTE.QAnyScope second)
  pure $ do
    firstRow <- reuse firstRows
    secondRow <- reuse secondRows
    pure
      ( firstRow
      , secondRow
      , cteRowValues_ @QBaseScope terminal
      )

cteRowValues_
  :: forall scope. CteRowT Identity -> CteRowT (QExpr Postgres scope)
cteRowValues_ row = CteRow (val_ (cteId row)) (val_ (cteValue row))

-- The returned relation exposes the result of every modifying CTE. Keeping
-- their keys disjoint gives the property a deterministic reference model while
-- still exercising mixed syntax assembly and PostgreSQL execution semantics.
dataModifyingCteModelSelect
  :: CteRowT Identity
  -> Int32
  -> Text
  -> Int32
  -> SqlSelect Postgres
       (CteRowT Identity, CteRowT Identity, CteRowT Identity)
dataModifyingCteModelSelect inserted updateId updateValue deleteId =
  Pg.pgSelectWithTopLevel $ do
    insertedRows <- Pg.cteInsertReturning
      (dbCteRows cteDb)
      (insertValues [inserted])
      Pg.onConflictDefault
      id
    updatedRows <- Pg.cteUpdateReturning
      (dbCteRows cteDb)
      (\row -> cteValue row <-. val_ updateValue)
      (\row -> cteId row ==. val_ updateId)
      id
    deletedRows <- Pg.cteDeleteReturning
      (dbCteRows cteDb)
      (\row -> cteId row ==. val_ deleteId)
      id

    case (insertedRows, updatedRows) of
      (Just insertedRows', Just updatedRows') -> pure $ do
        insertedRow <- reuse insertedRows'
        updatedRow <- reuse updatedRows'
        deletedRow <- reuse deletedRows
        pure (insertedRow, updatedRow, deletedRow)
      _ -> error "Expected non-empty INSERT and UPDATE CTEs"

sideEffectCteModelSelect
  :: CteRowT Identity
  -> Int32
  -> Text
  -> Int32
  -> SqlSelect Postgres Int32
sideEffectCteModelSelect inserted updateId updateValue deleteId =
  Pg.pgSelectWithTopLevel $ do
    Pg.cteInsert
      (dbCteRows cteDb)
      (insertValues [inserted])
      Pg.onConflictDefault
    Pg.cteUpdate
      (dbCteRows cteDb)
      (\row -> cteValue row <-. val_ updateValue)
      (\row -> cteId row ==. val_ updateId)
    Pg.cteDelete
      (dbCteRows cteDb)
      (\row -> cteId row ==. val_ deleteId)
    pure finalMarkerQuery

-- The source key is selected in a CTE, then used to derive the inserted key.
-- This keeps both the CTE and terminal INSERT semantically relevant.
modelInsertWithStatement
  :: Int32
  -> CteRowT Identity
  -> SqlInsert Postgres CteRowT
modelInsertWithStatement sourceId inserted = Pg.pgInsertWith $ do
  sourceIds <- Pg.pgSelecting $ do
    row <- all_ (dbCteRows cteDb)
    guard_ (cteId row ==. val_ sourceId)
    pure (cteId row)
  pure $ Pg.insert
    (dbCteRows cteDb)
    (insertFrom $ do
      selectedId <- reuse sourceIds
      pure $ CteRow
        (selectedId + val_ (cteId inserted - sourceId))
        (val_ (cteValue inserted)))
    Pg.onConflictDefault

modelUpdateWithStatement
  :: Int32
  -> Text
  -> SqlUpdate Postgres CteRowT
modelUpdateWithStatement updateId updateValue = Pg.pgUpdateWith $ do
  targetIds <- Pg.pgSelecting $ do
    row <- all_ (dbCteRows cteDb)
    guard_ (cteId row ==. val_ updateId)
    pure (cteId row)
  pure $ update
    (dbCteRows cteDb)
    (\row -> cteValue row <-. val_ updateValue)
    (\row -> exists_ $ do
      targetId <- reuse targetIds
      guard_ (cteId row ==. targetId)
      pure targetId)

modelDeleteWithStatement
  :: Int32
  -> SqlDelete Postgres CteRowT
modelDeleteWithStatement deleteId = Pg.pgDeleteWith $ do
  targetIds <- Pg.pgSelecting $ do
    row <- all_ (dbCteRows cteDb)
    guard_ (cteId row ==. val_ deleteId)
    pure (cteId row)
  pure $ delete (dbCteRows cteDb) $ \row -> exists_ $ do
    targetId <- reuse targetIds
    guard_ (cteId row ==. targetId)
    pure targetId

recursiveCteModelSelect
  :: Int32
  -> Int32
  -> SqlSelect Postgres (CteRowT Identity)
recursiveCteModelSelect startId endId = Pg.pgSelectWithTopLevel $ do
  recursiveIds <- Pg.pgToTopLevel $ mdo
    ids <- Pg.pgSelecting $
      pure (as_ @Int32 (val_ startId)) `unionAll_` do
        previousId <- reuse ids
        guard_ (previousId <. val_ endId)
        pure (previousId + 1)
    pure ids

  deletedRows <- Pg.cteDeleteReturning
    (dbCteRows cteDb)
    (\row -> exists_ $ do
      recursiveId <- reuse recursiveIds
      guard_ (cteId row ==. recursiveId)
      pure recursiveId)
    id

  pure (reuse deletedRows)

nestedSelectCteSelect :: SqlSelect Postgres (CteRowT Identity)
nestedSelectCteSelect = select $ Pg.pgSelectWith $ do
  selected <- selecting $ do
    row <- all_ (dbCteRows cteDb)
    guard_ (cteId row ==. val_ 1)
    pure row
  pure (reuse selected)

-- PostgreSQL permits a recursive SELECT CTE to feed a later modifying CTE, but
-- not a modifying CTE to recursively reference itself. 'pgToTopLevel' closes the
-- recursive SELECT knot before the DELETE is added.
recursiveSelectThenDeleteCteSelect :: SqlSelect Postgres (CteRowT Identity)
recursiveSelectThenDeleteCteSelect = Pg.pgSelectWithTopLevel $ do
  recursiveIds <- Pg.pgToTopLevel $ mdo
    ids <- Pg.pgSelecting $
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
-- Their wrappers return Nothing, leaving pgSelectWithTopLevel to render the
-- final query without an empty WITH clause.
emptyDataModifyingCteSelect :: SqlSelect Postgres Int32
emptyDataModifyingCteSelect = Pg.pgSelectWithTopLevel $ do
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

-- The portable builder reaches PostgreSQL through the SQL99-shaped compatibility
-- instance. Each input table row contributes one row to the reusable
-- degree-zero relation even though the projection contains no values.
emptySelectProjection :: SqlSelect Postgres (EmptyCteT Identity)
emptySelectProjection = selectWith $ do
  rows <- selecting $ do
    _ <- all_ (dbCteRows cteDb)
    pure (EmptyCte :: EmptyCteT (QExpr Postgres CTE.QAnyScope))
  pure (reuse rows)

-- The native SELECT path additionally carries PostgreSQL's materialization
-- policy. Its logical result is identical for all three policies.
emptyNativeSelectProjection
  :: Pg.PgCteMaterialization
  -> SqlSelect Postgres (EmptyCteT Identity)
emptyNativeSelectProjection materialization = Pg.pgSelectWithTopLevel $ do
  rows <- Pg.pgSelectingWith materialization $ do
    _ <- all_ (dbCteRows cteDb)
    pure (EmptyCte :: EmptyCteT (QExpr Postgres CTE.QAnyScope))
  pure (reuse rows)

-- SELECT CTEs remain nestable when their relation has degree zero.
nestedEmptySelectProjection :: SqlSelect Postgres (EmptyCteT Identity)
nestedEmptySelectProjection = select $ Pg.pgSelectWithNested $ do
  rows <- Pg.pgSelecting $ do
    _ <- all_ (dbCteRows cteDb)
    pure (EmptyCte :: EmptyCteT (QExpr Postgres CTE.QAnyScope))
  pure (reuse rows)

-- COUNT(*) and EXISTS do not need a projected field, so they are natural
-- consumers of a degree-zero relation. Both references share one CTE body.
emptySelectSummary :: SqlSelect Postgres (Int32, Bool)
emptySelectSummary = Pg.pgSelectWithTopLevel $ do
  rows <- Pg.pgSelecting $ do
    _ <- all_ (dbCteRows cteDb)
    pure (EmptyCte :: EmptyCteT (QExpr Postgres CTE.QAnyScope))
  pure $ do
    count <- aggregate_ (const (as_ @Int32 countAll_)) (reuse rows)
    pure (count, exists_ (reuse rows))

-- The next three fixtures deliberately return no logical values. PostgreSQL's
-- RETURNING grammar is satisfied internally, while the outer SELECT exposes no
-- physical columns and retains one row per affected table row.
emptyInsertProjection
  :: [CteRowT Identity]
  -> SqlSelect Postgres (EmptyCteT Identity)
emptyInsertProjection values = Pg.pgSelectWithTopLevel $ do
  rows <- Pg.cteInsertReturning
    (dbCteRows cteDb)
    (insertValues values)
    Pg.onConflictDefault
    (const (EmptyCte :: EmptyCteT (QExpr Postgres PostgresInaccessible)))
  case rows of
    Just rows' -> pure (reuse rows')
    Nothing -> error "Expected non-empty INSERT values"

emptyUpdateProjection
  :: Int32
  -> Text
  -> SqlSelect Postgres (EmptyCteT Identity)
emptyUpdateProjection maximumId value = Pg.pgSelectWithTopLevel $ do
  rows <- Pg.cteUpdateReturning
    (dbCteRows cteDb)
    (\row -> cteValue row <-. val_ value)
    (\row -> cteId row <=. val_ maximumId)
    (const (EmptyCte :: EmptyCteT (QExpr Postgres PostgresInaccessible)))
  case rows of
    Just rows' -> pure (reuse rows')
    Nothing -> error "Expected a non-identity UPDATE"

emptyDeleteProjection
  :: Int32
  -> SqlSelect Postgres (EmptyCteT Identity)
emptyDeleteProjection minimumId = Pg.pgSelectWithTopLevel $ do
  rows <- Pg.cteDeleteReturning
    (dbCteRows cteDb)
    (\row -> cteId row >=. val_ minimumId)
    (const (EmptyCte :: EmptyCteT (QExpr Postgres PostgresInaccessible)))
  pure (reuse rows)

-- Two references to the same modifying CTE must multiply its row cardinality,
-- not execute the DELETE twice or expose its private sentinel.
repeatedEmptyDeleteProjection :: SqlSelect Postgres (EmptyCteT Identity)
repeatedEmptyDeleteProjection = Pg.pgSelectWithTopLevel $ do
  rows <- Pg.cteDeleteReturning
    (dbCteRows cteDb)
    (const (val_ True))
    (const (EmptyCte :: EmptyCteT (QExpr Postgres PostgresInaccessible)))
  pure $ do
    _ <- reuse rows
    _ <- reuse rows
    pure (EmptyCte :: EmptyCteT (QExpr Postgres QBaseScope))

-- Aggregating the reusable DELETE result verifies that the private physical
-- sentinel supplies relational rows without becoming a Beam expression.
emptyDeleteSummary :: SqlSelect Postgres (Int32, Bool)
emptyDeleteSummary = Pg.pgSelectWithTopLevel $ do
  rows <- Pg.cteDeleteReturning
    (dbCteRows cteDb)
    (const (val_ True))
    (const (EmptyCte :: EmptyCteT (QExpr Postgres PostgresInaccessible)))
  pure $ do
    count <- aggregate_ (const (as_ @Int32 countAll_)) (reuse rows)
    pure (count, exists_ (reuse rows))

-- Copy one row selected by the CTE into a new row. insertFrom is what exposes
-- the reusable query to the terminal INSERT source.
insertWithStatement :: SqlInsert Postgres CteRowT
insertWithStatement = Pg.pgInsertWith $ do
  source <- Pg.pgSelecting $ do
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
  targets <- Pg.pgSelecting $ do
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
  targets <- Pg.pgSelecting $ do
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
recursiveInsertWithStatement = Pg.pgInsertWith recursiveInsertWith

recursiveInsertWith
  :: Pg.PgWith CteDb 'Pg.PgCteNestedAllowed (SqlInsert Postgres CteRowT)
recursiveInsertWith = mdo
  ids <- Pg.pgSelecting $
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

-- Adding a modifying CTE fixes the block to PgCteTopLevelOnly. pgDeleteWith is
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
  _ <- Pg.pgSelecting $ all_ (dbCteRows cteDb)
  pure $ Pg.insert
    (dbCteRows cteDb)
    SqlInsertValuesEmpty
    Pg.onConflictDefault

identityUpdateWithStatement :: SqlUpdate Postgres CteRowT
identityUpdateWithStatement = Pg.pgUpdateWith $ do
  _ <- Pg.pgSelecting $ all_ (dbCteRows cteDb)
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
