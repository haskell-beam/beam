module Database.Beam.Internal where


import Database.Beam.Schema.Tables
import Database.Beam.SQL.Types

import Control.Applicative
import Control.Monad
import Control.Arrow
import Control.Monad.Error
import Control.Monad.Reader

import Data.Text (Text, unpack)
import Data.Proxy
import Data.String
import Data.Typeable

import Database.HDBC

data DBSchemaComparison = Migration [MigrationAction]
                        | Unknown
                          deriving Show

data MigrationAction where
    MACreateTable :: Table table => Text -> Proxy table -> MigrationAction

instance Show MigrationAction where
    show (MACreateTable name t) = concat ["MACreateTable ", unpack name , " ", "(Proxy :: ", show (typeOf t), ")"]

class BeamBackend backendSettings where
    openBeam :: (MonadIO m, Database d) => DatabaseSettings d -> backendSettings -> m (Beam d m)

data Beam d m = Beam
              { beamDbSettings :: DatabaseSettings d
              , beamDebug :: Bool

              , closeBeam :: m ()

              , compareSchemas :: ReifiedDatabaseSchema -> DatabaseSettings d -> DBSchemaComparison
              , adjustColDescForBackend :: SQLColumnSchema -> SQLColumnSchema

              , getLastInsertedRow :: Text -> m [SqlValue]

              , withHDBCConnection :: forall a. (forall conn. IConnection conn => conn -> m a) -> m a }

newtype BeamT e d m a = BeamT { runBeamT :: Beam d m -> m (BeamResult e a) }

data BeamResult e a = Success a
                    | Rollback (BeamRollbackReason e)
                      deriving Show

data BeamRollbackReason e = InternalError String
                          | UserError e
                            deriving Show
instance Error (BeamRollbackReason e) where
    strMsg = InternalError


transBeam :: Functor m => (forall a. (s -> m (a, Maybe b)) -> n a) -> (forall a. n a -> s -> m (a, b)) -> Beam d m -> Beam d n
transBeam lift lower beam = beam
                          { closeBeam = lift (const ((,Nothing) <$> closeBeam beam))
                          , getLastInsertedRow = \s -> lift (const ((, Nothing) <$> getLastInsertedRow beam s))
                          , withHDBCConnection = \f -> lift (\s -> second Just <$> withHDBCConnection beam (flip lower s . f)) }

instance Monad m => Monad (BeamT e d m) where
    a >>= mkB = BeamT $ \beam ->
                do x <- runBeamT a beam
                   case x of
                     Success x -> runBeamT (mkB x) beam
                     Rollback e -> return (Rollback e)
    return = BeamT . const . return . Success

instance Monad m => Functor (BeamT e d m) where
    fmap  = liftM

instance Monad m => Applicative (BeamT e d m) where
    pure  = return
    (<*>) = ap

instance MonadIO m => MonadIO (BeamT e d m) where
    liftIO = lift . liftIO

instance MonadTrans (BeamT e d) where
    lift x = BeamT $ \_ ->
             do res <- x
                return (Success res)
