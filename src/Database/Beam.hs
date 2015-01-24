{-# LANGUAGE GADTs #-}
module Database.Beam
     ( module Database.Beam.Types
     , module Database.Beam.SQL
     , module Database.Beam.Query
     , module Database.Beam.Schema
     , module Database.Beam.Schema.Database
     , module Database.Beam.Backend

     , Typeable, Generic

     , justOne ) where

import Database.Beam.Types
import Database.Beam.SQL
import Database.Beam.Query
import Database.Beam.Schema
import Database.Beam.Schema.Database
import Database.Beam.Backend

import Control.Monad.Trans

import Data.Typeable
import Data.Conduit

import GHC.Generics

justOne :: (MonadIO m, FromSqlValues a) => Query q a -> Beam m -> m (Maybe a)
justOne q beam =
  do let justOneSink = await >>= \x ->
                       case x of
                         Nothing -> return Nothing
                         Just  x -> noMoreSink x
         noMoreSink x = await >>= \nothing ->
                        case nothing of
                          Nothing -> return (Just x)
                          Just  _ -> return Nothing
     src <- runQuery q beam
     case src of
       Left err -> return Nothing
       Right  x -> x $$ justOneSink