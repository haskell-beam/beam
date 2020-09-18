{-# LANGUAGE CPP #-}

-- | Convenience methods for constructing backend-agnostic applications
module Database.Beam.Backend.URI where

import           Control.Exception

import qualified Data.Map as M

import           Network.URI

data BeamResourceNotFound = BeamResourceNotFound deriving Show
instance Exception BeamResourceNotFound

data BeamOpenURIInvalid = BeamOpenURIInvalid deriving Show
instance Exception BeamOpenURIInvalid

data BeamOpenURIUnsupportedScheme = BeamOpenURIUnsupportedScheme String deriving Show
instance Exception BeamOpenURIUnsupportedScheme

data BeamURIOpener c where
  BeamURIOpener :: c be hdl m
                -> (forall a. hdl -> m a -> IO a)
                -> (URI -> IO (hdl, IO ()))
                -> BeamURIOpener c
newtype BeamURIOpeners c where
  BeamURIOpeners :: M.Map String (BeamURIOpener c) -> BeamURIOpeners c

instance Semigroup (BeamURIOpeners c) where
  BeamURIOpeners a <> BeamURIOpeners b =
    BeamURIOpeners (a <> b)

instance Monoid (BeamURIOpeners c) where
  mempty = BeamURIOpeners mempty
  mappend = (<>)

data OpenedBeamConnection c where
  OpenedBeamConnection
    :: { beamRunner          :: (forall a. hdl -> m a -> IO a)
       , openedBeamDatabase  :: c be hdl m
       , openedBeamHandle    :: hdl
       , closeBeamConnection :: IO ()
     } -> OpenedBeamConnection c

mkUriOpener :: (forall a. hdl -> m a -> IO a)
            -> String
            -> (URI -> IO (hdl, IO ()))
            -> c be hdl m
            -> BeamURIOpeners c
mkUriOpener runner schemeNm opener c = BeamURIOpeners (M.singleton schemeNm (BeamURIOpener c runner opener))

withDbFromUri :: forall c a
               . BeamURIOpeners c
              -> String
              -> (forall be hdl m. (forall r. hdl -> m r -> IO r) -> c be hdl m -> m a)
              -> IO a
withDbFromUri protos uri actionWithDb =
  withDbConnection protos uri (\runner c hdl -> runner hdl (actionWithDb runner c))

withDbConnection :: forall c a
                  . BeamURIOpeners c
                 -> String
                 -> (forall be hdl m. (forall r. hdl -> m r -> IO r) ->
                      c be hdl m -> hdl -> IO a)
                 -> IO a
withDbConnection protos uri actionWithDb =
  bracket (openDbConnection protos uri) closeBeamConnection $
  \(OpenedBeamConnection runner c hdl _) -> actionWithDb runner c hdl

openDbConnection :: forall c
                  . BeamURIOpeners c
                 -> String
                 -> IO (OpenedBeamConnection c)
openDbConnection protos uri = do
  (parsedUri, BeamURIOpener c runner openURI) <- findURIOpener protos uri
  (hdl, closeHdl) <- openURI parsedUri
  pure (OpenedBeamConnection runner c hdl closeHdl)

findURIOpener :: BeamURIOpeners c -> String -> IO (URI, BeamURIOpener c)
findURIOpener (BeamURIOpeners protos) uri =
  case parseURI uri of
    Nothing -> throwIO BeamOpenURIInvalid
    Just parsedUri ->
      case M.lookup (uriScheme parsedUri) protos of
        Nothing -> throwIO (BeamOpenURIUnsupportedScheme (uriScheme parsedUri))
        Just opener -> pure (parsedUri, opener)
