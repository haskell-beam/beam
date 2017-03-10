module Database.Beam.Backend.SQL where

import Database.Beam.Backend.Types
import Database.Beam.SQL.Types

import Data.Maybe

sqlBooleanOpts :: BeamSqlBackend be => SQLExpr be -> Maybe (SQLExpr be)
sqlBooleanOpts (SQLBinOpE "AND" (SQLValE false) _)
  | sqlIsFalse false = Just (SQLValE (toBackendLiteral False))
sqlBooleanOpts (SQLBinOpE "AND" _ (SQLValE false))
  | sqlIsFalse false = Just (SQLValE (toBackendLiteral False))
sqlBooleanOpts (SQLBinOpE "AND" (SQLValE true) q)
  | sqlIsTrue true = Just q
sqlBooleanOpts (SQLBinOpE "AND" q (SQLValE true))
  | sqlIsTrue true = Just q

sqlBooleanOpts (SQLBinOpE "OR" q (SQLValE false))
  | sqlIsFalse false = Just q
sqlBooleanOpts (SQLBinOpE "OR" (SQLValE false) q)
  | sqlIsFalse false = Just q
sqlBooleanOpts (SQLBinOpE "OR" (SQLValE true1) (SQLValE true2))
    | sqlIsTrue true1, sqlIsTrue true2 = Just (SQLValE (toBackendLiteral True))

sqlBooleanOpts _ = Nothing

sqlIsFalse, sqlIsTrue :: BeamSqlBackend be =>
  BackendLiteral be -> Bool
sqlIsFalse = maybe False not . fromBackendLiteral
sqlIsTrue = fromMaybe False . fromBackendLiteral

class ( BeamBackend be
      , FromBackendLiterals be Bool, FromBackendLiteral be Bool ) =>
      BeamSqlBackend be where
  sqlExprOptimizations ::
    Monad m => SQLExpr be -> m (Maybe (SQLExpr be))
  sqlExprOptimizations e = pure (sqlBooleanOpts e)
