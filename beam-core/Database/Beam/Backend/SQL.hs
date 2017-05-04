module Database.Beam.Backend.SQL
  ( module Database.Beam.Backend.SQL.SQL2003
  , module Database.Beam.Backend.SQL.Types
  , module Database.Beam.Backend.Types

  , MonadBeam(..) ) where

import Database.Beam.Backend.SQL.SQL2003
import Database.Beam.Backend.SQL.Types
import Database.Beam.Backend.Types

import Control.Monad.IO.Class

-- * MonadBeam class

class (BeamBackend be, Monad m, MonadIO m, Sql92SanityCheck syntax) =>
  MonadBeam syntax be handle m | m -> syntax be handle, be -> m, handle -> m where

  withDatabaseDebug :: (String -> IO ())
                    -> handle
                    -> m a -> IO a
  withDatabase :: handle -> m a -> IO a
  withDatabase = withDatabaseDebug (\_ -> pure ())

  runReturningMany :: FromBackendRow be x => syntax -> (m (Maybe x) -> m a) -> m a

  runNoReturn :: syntax -> m ()
  runNoReturn cmd =
      runReturningMany cmd $ \(_ :: m (Maybe ())) -> pure ()

  runReturningOne :: FromBackendRow be x => syntax -> m (Maybe x)
  runReturningOne cmd =
      runReturningMany cmd $ \next ->
        do a <- next
           case a of
             Nothing -> pure Nothing
             Just x -> do
               a' <- next
               case a' of
                 Nothing -> pure (Just x)
                 Just _ -> pure Nothing

  runReturningList :: FromBackendRow be x => syntax -> m [x]
  runReturningList cmd =
      runReturningMany cmd $ \next ->
          let collectM acc = do
                a <- next
                case a of
                  Nothing -> pure (acc [])
                  Just x -> collectM (acc . (x:))
          in collectM id
