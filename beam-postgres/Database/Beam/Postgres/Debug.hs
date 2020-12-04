{-# LANGUAGE PolyKinds #-}
module Database.Beam.Postgres.Debug where

import           Database.Beam.Query
import           Database.Beam.Postgres.Types (Postgres(..))
import           Database.Beam.Postgres.Connection
  ( Pg
  , liftIOWithHandle
  , pgRenderSyntax )
import           Database.Beam.Postgres.Full
  ( PgInsertReturning(..)
  , PgUpdateReturning(..)
  , PgDeleteReturning(..) )
import Database.Beam.Postgres.Syntax
  ( PgSyntax
  , PgSelectSyntax(..)
  , PgInsertSyntax(..)
  , PgUpdateSyntax(..)
  , PgDeleteSyntax(..) )

import qualified Data.ByteString.Char8 as BC

import qualified Database.PostgreSQL.Simple as Pg

-- | Type class for @Sql*@ types that can be turned into Postgres
-- syntax, for use in the following debugging functions
--
-- These include
--
--    * 'SqlSelect'
--    * 'SqlInsert'
--    * 'SqlUpdate'
--    * 'SqlDelete'
--    * 'PgInsertReturning'
--    * 'PgUpdateReturning'
--    * 'PgDeleteReturning'
class PgDebugStmt statement where
  pgStmtSyntax :: statement -> Maybe PgSyntax

instance PgDebugStmt (SqlSelect Postgres a) where
  pgStmtSyntax (SqlSelect (PgSelectSyntax e)) = Just e
instance PgDebugStmt (SqlInsert Postgres a) where
  pgStmtSyntax SqlInsertNoRows = Nothing
  pgStmtSyntax (SqlInsert _ (PgInsertSyntax e)) = Just e
instance PgDebugStmt (SqlUpdate Postgres a) where
  pgStmtSyntax SqlIdentityUpdate = Nothing
  pgStmtSyntax (SqlUpdate _ (PgUpdateSyntax e)) = Just e
instance PgDebugStmt (SqlDelete Postgres a) where
  pgStmtSyntax (SqlDelete _ (PgDeleteSyntax e)) = Just e
instance PgDebugStmt (PgInsertReturning a) where
  pgStmtSyntax PgInsertReturningEmpty = Nothing
  pgStmtSyntax (PgInsertReturning e) = Just e
instance PgDebugStmt (PgUpdateReturning a) where
  pgStmtSyntax PgUpdateReturningEmpty = Nothing
  pgStmtSyntax (PgUpdateReturning e) = Just e
instance PgDebugStmt (PgDeleteReturning a) where
  pgStmtSyntax (PgDeleteReturning e) = Just e

pgTraceStmtIO :: PgDebugStmt statement => Pg.Connection -> statement -> IO ()
pgTraceStmtIO conn s =
  BC.putStrLn =<< pgTraceStmtIO' conn s

pgTraceStmtIO' :: PgDebugStmt statement => Pg.Connection -> statement -> IO BC.ByteString
pgTraceStmtIO' conn stmt =
  let syntax = pgStmtSyntax stmt
  in maybe (return (BC.pack "(no statement)")) (pgRenderSyntax conn) syntax

pgTraceStmt :: PgDebugStmt statement => statement -> Pg ()
pgTraceStmt stmt =
  liftIOWithHandle (flip pgTraceStmtIO stmt)

