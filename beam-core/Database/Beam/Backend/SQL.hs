module Database.Beam.Backend.SQL
  ( module Database.Beam.Backend.SQL.SQL99
  , module Database.Beam.Backend.SQL.Types
  , module Database.Beam.Backend.Types ) where

import Database.Beam.Backend.SQL.SQL99
import Database.Beam.Backend.SQL.Types
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

--  sqlExprOptimizations ::
--    Monad m => SQLExpr be -> m (Maybe (SQLExpr be))
--  sqlExprOptimizations e = pure (sqlBooleanOpts e)
