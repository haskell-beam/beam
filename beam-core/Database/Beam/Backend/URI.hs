{-# LANGUAGE CPP #-}

-- | Convenience methods for constructing backend-agnostic applications
module Database.Beam.Backend.URI where

import           Database.Beam.Backend.SQL

import           Control.Exception

import qualified Data.Map as M
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif

import           Network.URI

data BeamResourceNotFound = BeamResourceNotFound deriving Show
instance Exception BeamResourceNotFound

data BeamOpenURIInvalid = BeamOpenURIInvalid deriving Show
instance Exception BeamOpenURIInvalid

data BeamOpenURIUnsupportedScheme = BeamOpenURIUnsupportedScheme String deriving Show
instance Exception BeamOpenURIUnsupportedScheme

data BeamURIOpener c where
  BeamURIOpener :: MonadBeam be hdl m
                => c be hdl m
                -> (URI -> IO (hdl, IO ()))
                -> BeamURIOpener c
newtype BeamURIOpeners c where
  BeamURIOpeners :: M.Map String (BeamURIOpener c) -> BeamURIOpeners c

instance Semigroup (BeamURIOpeners c) where
  (<>) = mappend

instance Monoid (BeamURIOpeners c) where
  mempty = BeamURIOpeners mempty
  mappend (BeamURIOpeners a) (BeamURIOpeners b) =
    BeamURIOpeners (mappend a b)

data OpenedBeamConnection c where
  OpenedBeamConnection
    :: MonadBeam be hdl m
    => { openedBeamDatabase :: c be hdl m
       , openedBeamHandle   :: hdl
       , closeBeamConnection :: IO ()
     } -> OpenedBeamConnection c

mkUriOpener :: MonadBeam be hdl m
            => String
            -> (URI -> IO (hdl, IO ()))
            -> c be hdl m
            -> BeamURIOpeners c
mkUriOpener schemeNm opener c = BeamURIOpeners (M.singleton schemeNm (BeamURIOpener c opener))

withDbFromUri :: forall c a
               . BeamURIOpeners c
              -> String
              -> (forall be hdl m. MonadBeam be hdl m => c be hdl m -> m a)
              -> IO a
withDbFromUri protos uri actionWithDb =
  withDbConnection protos uri (\c hdl -> withDatabase hdl (actionWithDb c))

withDbConnection :: forall c a
                  . BeamURIOpeners c
                 -> String
                 -> (forall be hdl m. MonadBeam be hdl m =>
                      c be hdl m -> hdl -> IO a)
                 -> IO a
withDbConnection protos uri actionWithDb =
  bracket (openDbConnection protos uri) closeBeamConnection $
  \(OpenedBeamConnection c hdl _) -> actionWithDb c hdl

openDbConnection :: forall c
                  . BeamURIOpeners c
                 -> String
                 -> IO (OpenedBeamConnection c)
openDbConnection protos uri = do
  (parsedUri, BeamURIOpener c openURI) <- findURIOpener protos uri
  (hdl, closeHdl) <- openURI parsedUri
  pure (OpenedBeamConnection c hdl closeHdl)

findURIOpener :: BeamURIOpeners c -> String -> IO (URI, BeamURIOpener c)
findURIOpener (BeamURIOpeners protos) uri =
  case parseURI uri of
    Nothing -> throwIO BeamOpenURIInvalid
    Just parsedUri ->
      case M.lookup (uriScheme parsedUri) protos of
        Nothing -> throwIO (BeamOpenURIUnsupportedScheme (uriScheme parsedUri))
        Just opener -> pure (parsedUri, opener)
