module Database.Beam.Internal where


import Database.Beam.Schema.Tables
import Database.Beam.SQL.Types

import Control.Monad
import Control.Arrow
import Control.Monad.Except

import Data.Text (Text, unpack)
import Data.Proxy
import Data.Typeable
import Data.Monoid ((<>))

import Database.HDBC

data DBSchemaComparison = Migration [MigrationAction]
                        | Unknown
                          deriving Show

instance Monoid DBSchemaComparison where
    mappend (Migration a) (Migration b) = Migration (a <> b)
    mappend _ Unknown = Unknown
    mappend Unknown _ = Unknown

    mempty = Migration []

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

transBeam :: Functor m => (forall a. (s -> m (a, Maybe b)) -> n a) -> (forall a. n a -> s -> m (a, b)) -> Beam d m -> Beam d n
transBeam liftB lower beam = beam
                          { closeBeam = liftB (const ((,Nothing) <$> closeBeam beam))
                          , getLastInsertedRow = \s -> liftB (const ((, Nothing) <$> getLastInsertedRow beam s))
                          , withHDBCConnection = \f -> liftB (\s -> second Just <$> withHDBCConnection beam (flip lower s . f)) }

instance Monad m => Monad (BeamT e d m) where
    a >>= mkB = BeamT $ \beam -> runBeamT a beam >>= \case
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
