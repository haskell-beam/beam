module Database.Beam.Postgres.Connection
  ( Pg.Connection
  , Pg.ResultError(..), Pg.SqlError(..)
  , Pg.ExecStatus(..)


  , Pg.ConnectInfo(..), Pg.defaultConnectInfo

  , Pg.postgreSQLConnectionString

  , Pg.connectPostgreSQL, Pg.connect
  , Pg.close
  ) where

import           Data.ByteString (ByteString)

import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.LibPQ as Pg

import qualified Data.Conduit as C

query :: forall q syntax m db s a.
         ( IsQuery q, PgExtensionsSyntax syntax
         , MonadIO m, Functor m, Projectible syntax a) =>
         Pg.Connection -> (forall s. q syntax db s a) -> Source m (QExprToIdentity a)
query conn q = do
  let (_, _, x) = buildSql92Query (Proxy @syntax) (toQ q) 0

  syntax <- pgRenderSyntax conn x
  res <- exec conn syntax
  case res of
    Nothing -> do
      -- TODO better error reporting
      errMsg <- fromMaybe "No libpq error provided" <$> Pg.errorMessage conn
      fail (show errMsg)
    Just res -> do
      execStatus <- resultStatus res
      case execStatus of
        TuplesOk -> do
          Pg.Row tupleCount  <- ntuples res
          Pg.Col columnCount <- nfields res

          

        -- TODO better error reporting
        _ -> fail "Error"
