{-# LANGUAGE RankNTypes, GADTs, TupleSections #-}
module Database.Beam.Types where

import Database.Beam.Schema.Tables
import Database.Beam.SQL.Types

import Control.Applicative
import Control.Monad
import Control.Arrow
import Control.Monad.Error
import Control.Monad.Reader

import Data.Text (Text, unpack)
import Data.Time.Clock (UTCTime)
import Data.Proxy
import Data.String
import Data.Conduit
import Data.Typeable
import Data.List

import GHC.Generics

import Database.HDBC

data DBSchemaComparison = Migration [MigrationAction]
                        | Unknown
                          deriving Show

data MigrationAction where
    MACreateTable :: Table table => Proxy table -> MigrationAction

instance Show MigrationAction where
    show (MACreateTable t) = concat ["MACreateTable ", unpack (dbTableName t)]

data BeamResult e a = Success a
                    | Rollback (BeamRollbackReason e)
                      deriving Show

data BeamRollbackReason e = InternalError String
                          | UserError e
                            deriving Show
instance Error (BeamRollbackReason e) where
    strMsg = InternalError

newtype BeamT e m a = BeamT { runBeamT :: Beam m -> m (BeamResult e a) }

data Beam m = Beam
            { closeBeam :: m ()

            , compareSchemas :: forall d. Database d => DatabaseSchema -> Proxy d -> DBSchemaComparison
            , adjustColDescForBackend :: SQLColumnSchema -> SQLColumnSchema

            , withHDBCConnection :: forall a. (forall conn. IConnection conn => conn -> m a) -> m a }

class BeamBackend dbSettings where
    openBeam :: MonadIO m => dbSettings -> m (Beam m)

-- * BeamT Instances

transBeam :: Functor m => (forall a. (s -> m (a, Maybe b)) -> n a) -> (forall a. n a -> s -> m (a, b)) -> Beam m -> Beam n
transBeam lift lower beam = beam
                          { closeBeam = lift (const ((,Nothing) <$> closeBeam beam))
                          , withHDBCConnection = \f -> lift (\s -> ((id *** Just) <$> withHDBCConnection beam (flip lower s . f))) }

beamNoErrors :: BeamT () m a -> BeamT () m a
beamNoErrors = id

instance Monad m => Monad (BeamT e m) where
    a >>= mkB = BeamT $ \beam ->
                do x <- runBeamT a beam
                   case x of
                     Success x -> runBeamT (mkB x) beam
                     Rollback e -> return (Rollback e)
    return = BeamT . const . return . Success

instance Monad m => Functor (BeamT e m) where
    fmap  = liftM

instance Monad m => Applicative (BeamT e m) where
    pure  = return
    (<*>) = ap

instance MonadIO m => MonadIO (BeamT e m) where
    liftIO = lift . liftIO

instance MonadTrans (BeamT e) where
    lift x = BeamT $ \_ ->
             do res <- x
                return (Success res)