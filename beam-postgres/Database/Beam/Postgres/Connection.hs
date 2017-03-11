module Database.Beam.Postgres.Connection
  ( Pg.Connection
  , Pg.ResultError(..), Pg.SqlError(..)
  , Pg.ExecStatus(..)


  , Pg.ConnectInfo(..), Pg.defaultConnectInfo

  , Pg.postgreSQLConnectionString

  , Pg.connectPostgreSQL, Pg.connect
  , Pg.close

  , query ) where

import           Control.Monad.Free.Church
import           Control.Monad.IO.Class

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC

import           Database.Beam
import           Database.Beam.Backend.Types
import           Database.Beam.Query.Internal

import           Database.Beam.Postgres.Syntax
import           Database.Beam.Postgres.Types

import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Database.PostgreSQL.LibPQ as Pg hiding
  (Connection, escapeStringConn, escapeIdentifier, escapeByteaConn)
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.Internal as Pg
  ( Field(..)
  , withConnection, escapeStringConn, escapeIdentifier, escapeByteaConn)
import qualified Database.PostgreSQL.Simple.Ok as Pg

import           Data.ByteString.Builder (toLazyByteString, byteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Conduit as C
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy

query :: forall q syntax m db s a.
         ( IsQuery q, FromBackendRow Postgres (QExprToIdentity a)
         , MonadIO m, Functor m, Projectible PgSyntax a ) =>
         Pg.Connection -> q PgSyntax db () a
      -> C.Source m (QExprToIdentity a)
query conn q = do
  let (_, _, x) = buildSql92Query (Proxy @PgSyntax) (toQ q) 0

  success <- liftIO $ do
    syntax <- pgRenderSyntax conn x
    putStrLn ("Going to run " <> BC.unpack syntax)
    Pg.withConnection conn (\conn -> Pg.sendQuery conn syntax)

  if success
    then do
      singleRowModeSet <- liftIO (Pg.withConnection conn Pg.setSingleRowMode)
      if singleRowModeSet then streamResults Nothing
         else fail "Could not enable single row mode"
    else do
      errMsg <- fromMaybe "No libpq error provided" <$> liftIO (Pg.withConnection conn Pg.errorMessage)
      fail (show errMsg)

  where
    streamResults fields = do
      nextRow <- liftIO (Pg.withConnection conn Pg.getResult)
      case nextRow of
        Nothing -> pure ()
        Just row -> do
          fields' <- liftIO (maybe (getFields row) pure fields)
          parsedRow <- liftIO (runPgRowReader conn row fields' fromBackendRow)
          case parsedRow of
            Nothing -> liftIO (bailEarly row "Could not read row")
            Just parsedRow -> do
              C.yieldOr parsedRow (liftIO bailAfterParse)
              streamResults (Just fields')

    bailEarly row error = do
      Pg.unsafeFreeResult row
      cancelQuery
      fail error

    bailAfterParse = cancelQuery

    cancelQuery =
      Pg.withConnection conn $ \conn -> do
      cancel <- Pg.getCancel conn
      case cancel of
        Nothing -> pure ()
        Just cancel -> do
          res <- Pg.cancel cancel
          case res of
            Right () -> pure ()
            Left err -> fail ("Could not cancel: " <> show err)

getFields :: Pg.Result -> IO [Pg.Field]
getFields res = do
  Pg.Col colCount <- Pg.nfields res

  let getField col =
        Pg.Field res (Pg.Col col) <$> Pg.ftype res (Pg.Col col)

  mapM getField [0..colCount - 1]

pgRenderSyntax ::
  Pg.Connection -> PgSyntax -> IO ByteString
pgRenderSyntax conn (PgSyntax mkQuery) =
  renderBuilder <$> runF mkQuery finish step mempty
  where
    renderBuilder = BL.toStrict . toLazyByteString

    step (EmitBuilder b next) a = next (a <> b)
    step (EmitByteString b next) a = next (a <> byteString b)
    step (EscapeString b next) a = do
      res <- wrapError "EscapeString" (Pg.escapeStringConn conn b)
      next (a <> byteString res)
    step (EscapeBytea b next) a = do
      res <- wrapError "EscapeBytea" (Pg.escapeByteaConn conn b)
      next (a <> byteString res)
    step (EscapeIdentifier b next) a = do
      res <- wrapError "EscapeIdentifier" (Pg.escapeIdentifier conn b)
      next (a <> byteString res)

    finish x a = pure a

    wrapError step go = do
      res <- go
      case res of
        Right res -> pure res
        Left res -> fail (step <> ": " <> show res)

runPgRowReader ::
  Pg.Connection -> Pg.Result -> [Pg.Field] -> FromBackendRowM Postgres a -> IO (Maybe a)
runPgRowReader conn res fields readRow =
  Pg.nfields res >>= \(Pg.Col colCount) ->
  runF readRow finish step 0 colCount fields
  where
    step (ParseOneField _) _ _ [] = pure Nothing
    step (ParseOneField _) curCol colCount _
      | curCol >= colCount = pure Nothing
    step (ParseOneField next) curCol colCount fields@(_:fields') =
      let next' Nothing _ _ _ = pure Nothing
          next' (Just x) _ _ [] = fail "Internal error"
          next' (Just x) curCol colCount (_:fields) = next x (curCol + 1) colCount fields'
      in step (PeekField next') curCol colCount fields

    step (PeekField next) curCol colCount [] = next Nothing curCol colCount []
    step (PeekField next) curCol colCount fields
      | curCol >= colCount = next Nothing curCol colCount fields
    step (PeekField next) curCol colCount fields@(field:_) =
      do fieldValue <- Pg.getvalue res (Pg.Row 0) (Pg.Col curCol)
         res <- Pg.runConversion (Pg.fromField field fieldValue) conn
         case res of
           Pg.Errors _ -> next Nothing curCol colCount fields
           Pg.Ok x -> next (Just x) curCol colCount fields

    finish x _ _ _ = pure (Just x)
