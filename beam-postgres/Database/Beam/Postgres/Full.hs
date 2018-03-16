{-# LANGUAGE LambdaCase #-}

-- | Module providing (almost) full support for Postgres query and data
-- manipulation statements. These functions shadow the functions in
-- "Database.Beam.Query" and provide a strict superset of functionality. They
-- map 1-to-1 with the underlying Postgres support.
module Database.Beam.Postgres.Full where

import           Database.Beam hiding (insertValues)
import           Database.Beam.Query.Internal
import           Database.Beam.Backend.SQL
import           Database.Beam.Schema.Tables

import           Database.Beam.Postgres.Types
import           Database.Beam.Postgres.Syntax

import           Data.Monoid ((<>))
import qualified Data.Text as T

insert :: DatabaseEntity Postgres db (TableEntity table)
       -> SqlInsertValues PgInsertValuesSyntax table
       -> PgInsertOnConflict table
       -> SqlInsert PgInsertSyntax
insert tbl values onConflict_ =
  case insertReturning tbl values onConflict_
         (Nothing :: Maybe (table (QExpr PgExpressionSyntax PostgresInaccessible) -> QExpr PgExpressionSyntax PostgresInaccessible Int)) of
    PgInsertReturning a ->
      SqlInsert (PgInsertSyntax a)
    PgInsertReturningEmpty ->
      SqlInsertNoRows

data PgInsertReturning a
  = PgInsertReturning PgSyntax
  | PgInsertReturningEmpty

insertReturning :: Projectible PgExpressionSyntax a
                => DatabaseEntity Postgres be (TableEntity table)
                -> SqlInsertValues PgInsertValuesSyntax table
                -> PgInsertOnConflict table
                -> Maybe (table (QExpr PgExpressionSyntax PostgresInaccessible) -> a)
                -> PgInsertReturning (QExprToIdentity a)

insertReturning _ SqlInsertValuesEmpty _ _ = PgInsertReturningEmpty
insertReturning (DatabaseEntity (DatabaseTable tblNm tblSettings))
                (SqlInsertValues (PgInsertValuesSyntax insertValues_))
                (PgInsertOnConflict mkOnConflict)
                returning =
  PgInsertReturning $
  emit "INSERT INTO " <> pgQuotedIdentifier tblNm <>
  emit "(" <> pgSepBy (emit ", ") (allBeamValues (\(Columnar' f) -> pgQuotedIdentifier (_fieldName f)) tblSettings) <> emit ") " <>
  insertValues_ <> emit " " <> fromPgInsertOnConflict (mkOnConflict tblFields) <>
  (case returning of
     Nothing -> mempty
     Just mkProjection ->
         emit " RETURNING "<>
         pgSepBy (emit ", ") (map fromPgExpression (project (mkProjection tblQ) "t")))
   where
     tblQ = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (\_ -> fieldE (unqualifiedField (_fieldName f))))) tblSettings
     tblFields = changeBeamRep (\(Columnar' f) -> Columnar' (QField True tblNm (_fieldName f))) tblSettings

data PgUpdateReturning a
  = PgUpdateReturning PgSyntax
  | PgUpdateReturningEmpty

updateReturning :: Projectible PgExpressionSyntax a
                => DatabaseEntity Postgres be (TableEntity table)
                -> (forall s. table (QField s) -> [ QAssignment PgFieldNameSyntax PgExpressionSyntax s ])
                -> (forall s. table (QExpr PgExpressionSyntax s) -> QExpr PgExpressionSyntax s Bool)
                -> (table (QExpr PgExpressionSyntax PostgresInaccessible) -> a)
                -> PgUpdateReturning (QExprToIdentity a)
updateReturning table@(DatabaseEntity (DatabaseTable _ tblSettings))
                mkAssignments
                mkWhere
                mkProjection =
  case update table mkAssignments mkWhere of
    SqlUpdate pgUpdate ->
      PgUpdateReturning $
      fromPgUpdate pgUpdate <>
      emit " RETURNING " <>
      pgSepBy (emit ", ") (map fromPgExpression (project (mkProjection tblQ) "t"))

    SqlIdentityUpdate -> PgUpdateReturningEmpty
  where
    tblQ = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (pure (fieldE (unqualifiedField (_fieldName f)))))) tblSettings

newtype PgDeleteReturning a = PgDeleteReturning PgSyntax

deleteReturning :: Projectible PgExpressionSyntax a
                => DatabaseEntity Postgres be (TableEntity table)
                -> (forall s. table (QExpr PgExpressionSyntax s) -> QExpr PgExpressionSyntax s Bool)
                -> (table (QExpr PgExpressionSyntax PostgresInaccessible) -> a)
                -> PgDeleteReturning (QExprToIdentity a)
deleteReturning table@(DatabaseEntity (DatabaseTable _ tblSettings))
                mkWhere
                mkProjection =
  PgDeleteReturning $
  fromPgDelete pgDelete <>
  emit " RETURNING " <>
  pgSepBy (emit ", ") (map fromPgExpression (project (mkProjection tblQ) "t"))
  where
    SqlDelete pgDelete = delete table mkWhere
    tblQ = changeBeamRep (\(Columnar' f) -> Columnar' (QExpr (pure (fieldE (unqualifiedField (_fieldName f)))))) tblSettings

newtype PgInsertOnConflict (tbl :: (* -> *) -> *) =
    PgInsertOnConflict (tbl (QField PostgresInaccessible) -> PgInsertOnConflictSyntax)
newtype PgInsertOnConflictTarget (tbl :: (* -> *) -> *) =
    PgInsertOnConflictTarget (tbl (QExpr PgExpressionSyntax PostgresInaccessible) -> PgInsertOnConflictTargetSyntax)
newtype PgConflictAction (tbl :: (* -> *) -> *) =
    PgConflictAction (tbl (QField PostgresInaccessible) -> PgConflictActionSyntax)

onConflictDefault :: PgInsertOnConflict tbl
onConflictDefault = PgInsertOnConflict (\_ -> PgInsertOnConflictSyntax mempty)

onConflict :: Beamable tbl
           => PgInsertOnConflictTarget tbl
           -> PgConflictAction tbl
           -> PgInsertOnConflict tbl
onConflict (PgInsertOnConflictTarget tgt) (PgConflictAction update_) =
  PgInsertOnConflict $ \tbl ->
  let exprTbl = changeBeamRep (\(Columnar' (QField _ _ nm)) ->
                                 Columnar' (QExpr (\_ -> fieldE (unqualifiedField nm))))
                              tbl
  in PgInsertOnConflictSyntax $
     emit "ON CONFLICT " <> fromPgInsertOnConflictTarget (tgt exprTbl)
                         <> fromPgConflictAction (update_ tbl)

anyConflict :: PgInsertOnConflictTarget tbl
anyConflict = PgInsertOnConflictTarget (\_ -> PgInsertOnConflictTargetSyntax mempty)

conflictingFields :: Projectible PgExpressionSyntax proj
                  => (tbl (QExpr PgExpressionSyntax PostgresInaccessible) -> proj)
                  -> PgInsertOnConflictTarget tbl
conflictingFields makeProjection =
  PgInsertOnConflictTarget $ \tbl ->
  PgInsertOnConflictTargetSyntax $
  pgParens (pgSepBy (emit ", ") (map fromPgExpression (project (makeProjection tbl) "t"))) <> emit " "

conflictingFieldsWhere :: Projectible PgExpressionSyntax proj
                       => (tbl (QExpr PgExpressionSyntax PostgresInaccessible) -> proj)
                       -> (tbl (QExpr PgExpressionSyntax PostgresInaccessible) ->
                           QExpr PgExpressionSyntax PostgresInaccessible Bool)
                       -> PgInsertOnConflictTarget tbl
conflictingFieldsWhere makeProjection makeWhere =
  PgInsertOnConflictTarget $ \tbl ->
  PgInsertOnConflictTargetSyntax $
  pgParens (pgSepBy (emit ", ") (map fromPgExpression (project (makeProjection tbl) "t"))) <>
  emit " WHERE " <>
  pgParens (let QExpr mkE = makeWhere tbl
                PgExpressionSyntax e = mkE "t"
            in e) <>
  emit " "

conflictingConstraint :: T.Text -> PgInsertOnConflictTarget tbl
conflictingConstraint nm =
  PgInsertOnConflictTarget $ \_ ->
  PgInsertOnConflictTargetSyntax $
  emit "ON CONSTRAINT " <> pgQuotedIdentifier nm <> emit " "

onConflictDoNothing :: PgConflictAction tbl
onConflictDoNothing = PgConflictAction $ \_ -> PgConflictActionSyntax (emit "DO NOTHING")

onConflictUpdateSet :: Beamable tbl
                    => (tbl (QField PostgresInaccessible) ->
                        tbl (QExpr PgExpressionSyntax PostgresInaccessible)  ->
                        [ QAssignment PgFieldNameSyntax PgExpressionSyntax PostgresInaccessible ])
                    -> PgConflictAction tbl
onConflictUpdateSet mkAssignments =
  PgConflictAction $ \tbl ->
  let assignments = mkAssignments tbl tblExcluded
      tblExcluded = changeBeamRep (\(Columnar' (QField _ _ nm)) -> Columnar' (QExpr (\_ -> fieldE (qualifiedField "excluded" nm)))) tbl

      assignmentSyntaxes = do
        QAssignment assignments' <- assignments
        (fieldNm, expr) <- assignments'
        pure (fromPgFieldName fieldNm <> emit "=" <> pgParens (fromPgExpression expr))
  in PgConflictActionSyntax $
     emit "DO UPDATE SET " <> pgSepBy (emit ", ") assignmentSyntaxes

onConflictUpdateInstead :: (Beamable tbl, Projectible T.Text proj)
                        => (tbl (QExpr T.Text PostgresInaccessible) -> proj)
                        -> PgConflictAction tbl
onConflictUpdateInstead mkProj =
  onConflictUpdateSet $ \tbl _ ->
  let tblFields = changeBeamRep (\(Columnar' (QField _ _ nm)) -> Columnar' (QExpr (\_ -> nm))) tbl
      proj = project (mkProj tblFields) "t"

  in map (\fieldNm -> QAssignment [ (unqualifiedField fieldNm, fieldE (qualifiedField "excluded" fieldNm)) ]) proj

onConflictSetAll :: (Beamable tbl, Projectible T.Text (tbl (QExpr T.Text PostgresInaccessible)))
                 => PgConflictAction tbl
onConflictSetAll = onConflictUpdateInstead id
