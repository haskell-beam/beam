module Database.Beam.Backend.SQL where

import Database.Beam.Backend.Types

import Data.Proxy

-- sqlBooleanOpts :: BeamSqlBackend be => SQLExpr be -> Maybe (SQLExpr be)
-- sqlBooleanOpts (SQLBinOpE "AND" (SQLValE false) _)
--   | sqlValueIsFalse false = Just (SQLValE (SQLValue False))
-- sqlBooleanOpts (SQLBinOpE "AND" _ (SQLValE false))
--   | sqlValueIsFalse false = Just (SQLValE (SQLValue False))
-- sqlBooleanOpts (SQLBinOpE "AND" (SQLValE true) q)
--   | sqlValueIsTrue true = Just q
-- sqlBooleanOpts (SQLBinOpE "AND" q (SQLValE true))
--   | sqlValueIsTrue true = Just q

-- sqlBooleanOpts (SQLBinOpE "OR" q (SQLValE false))
--   | sqlValueIsFalse false = Just q
-- sqlBooleanOpts (SQLBinOpE "OR" (SQLValE false) q)
--   | sqlValueIsFalse false = Just q
-- sqlBooleanOpts (SQLBinOpE "OR" (SQLValE true1) (SQLValE true2))
--   | sqlValueIsTrue true1, sqlValueIsTrue true2 = Just (SQLValE (SQLValue True))

-- sqlBooleanOpts _ = Nothing

class ( BeamBackend be
--      , FromBackendLiterals be SQLNull, FromBackendLiteral be SQLNull
      ) =>
      BeamSqlBackend be where

--  sqlExprOptimizations ::
--    Monad m => SQLExpr be -> m (Maybe (SQLExpr be))
--  sqlExprOptimizations e = pure (sqlBooleanOpts e)
