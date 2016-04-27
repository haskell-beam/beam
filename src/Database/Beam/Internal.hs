module Database.Beam.Internal where


import {-# SOURCE #-} Database.Beam.Schema.Tables
import {-# SOURCE #-} Database.Beam.SQL.Types

import Control.Applicative
import Control.Monad
import Control.Arrow
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.Identity

import Data.Text (Text, unpack)
import Data.Time
import Data.Proxy
import Data.String
import Data.Typeable
import Data.Data

import Database.HDBC

data DBSchemaComparison be = Migration [MigrationAction be]
                           | Unknown

data MigrationAction be where
    MACreateTable :: Table table => Text -> TableSettings be table -> MigrationAction be

data BeamValue = BeamInteger Integer
               | BeamString String
               | BeamUTCTime UTCTime
               | BeamNull
                 deriving (Show, Eq, Ord)

class (Eq (BeamBackendValue be), Data (BeamBackendValue be), Show (BeamBackendValue be), Data be) => BeamBackend be where
    data BeamBackendValue be :: *

    openBeam :: (MonadIO m, Database d) => DatabaseSettings be d -> be -> m (Beam be d m)

    sqlNull :: BeamBackendValue be
    sqlInteger :: Integer -> BeamBackendValue be
    sqlString :: String -> BeamBackendValue be
    sqlUTCTime :: UTCTime -> BeamBackendValue be
    sqlBool :: Bool -> BeamBackendValue be

    fromBackendValue :: BeamBackendValue be -> Maybe BeamValue
    backendAsBool :: BeamBackendValue be -> Bool

data Beam be d m = Beam
                 { beamDbSettings :: DatabaseSettings be d
                 , beamDebug :: Bool

                 , closeBeam :: m ()

                 , compareSchemas :: ReifiedDatabaseSchema -> DatabaseSettings be d -> DBSchemaComparison be
                 , adjustColDescForBackend :: SQLColumnSchema -> SQLColumnSchema

                 , getLastInsertedRow :: Text -> m [BeamBackendValue be]

                 , beamExecute :: String -> [BeamBackendValue be] -> m (Either String (IO (Maybe [BeamBackendValue be])))
                 , beamDescribeDatabase :: m ReifiedDatabaseSchema
                 , beamCommit :: m ()
                 , beamRollback :: m () }

newtype BeamT be e d m a = BeamT { runBeamT :: Beam be d m -> m (BeamResult e a) }

data BeamResult e a = Success a
                    | Rollback (BeamRollbackReason e)
                      deriving Show

data BeamRollbackReason e = InternalError String
                          | UserError e
                            deriving Show
instance Error (BeamRollbackReason e) where
    strMsg = InternalError


-- transBeam :: Functor m => (forall a. (s -> m (a, Maybe b)) -> n a) -> (forall a. n a -> s -> m (a, b)) -> Beam be d m -> Beam be d n
-- transBeam lift lower beam = beam
--                           { closeBeam = lift (const ((,Nothing) <$> closeBeam beam))
--                           , getLastInsertedRow = \s -> lift (const ((, Nothing) <$> getLastInsertedRow beam s)) }

instance Monad m => Monad (BeamT be e d m) where
    a >>= mkB = BeamT $ \beam ->
                do x <- runBeamT a beam
                   case x of
                     Success x -> runBeamT (mkB x) beam
                     Rollback e -> return (Rollback e)
    return = BeamT . const . return . Success

instance Monad m => Functor (BeamT be e d m) where
    fmap  = liftM

instance Monad m => Applicative (BeamT be e d m) where
    pure  = return
    (<*>) = ap

instance MonadIO m => MonadIO (BeamT be e d m) where
    liftIO = lift . liftIO

instance MonadTrans (BeamT be e d) where
    lift x = BeamT $ \_ ->
             do res <- x
                return (Success res)
