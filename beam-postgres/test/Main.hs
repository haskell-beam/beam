module Main where

import Data.ByteString (ByteString)
import Data.Text (unpack)
import qualified Data.Text.Lazy as TL

import Test.Tasty
import qualified TestContainers.Tasty as TC

import qualified Database.Beam.Postgres.Test.Copy as Copy
import qualified Database.Beam.Postgres.Test.CTE as CTE
import qualified Database.Beam.Postgres.Test.DataTypes as DataType
import qualified Database.Beam.Postgres.Test.Marshal as Marshal
import qualified Database.Beam.Postgres.Test.Migrate as Migrate
import qualified Database.Beam.Postgres.Test.Select as Select
import qualified Database.Beam.Postgres.Test.Select.PgNubBy as Select.PgNubBy
import qualified Database.Beam.Postgres.Test.TempTable as TempTable
import qualified Database.Beam.Postgres.Test.Windowing as Windowing
import Database.PostgreSQL.Simple (ConnectInfo(..), defaultConnectInfo)
import qualified Database.PostgreSQL.Simple as Postgres

main :: IO ()
main = defaultMain $ testGroup "beam-postgres tests"
  -- Rendering and compile-negative tests do not need Docker, so keep them
  -- outside the Testcontainers resource and available as fast unit tests.
  [ CTE.unitTests
  , TC.withContainers setupTempPostgresDB $ \getConnStr ->
      testGroup "PostgreSQL integration tests"
        [ Marshal.tests getConnStr
        , CTE.integrationTests getConnStr
        , Select.tests getConnStr
        , Select.PgNubBy.tests getConnStr
        , DataType.tests getConnStr
        , Migrate.tests getConnStr
        , TempTable.tests getConnStr
        , Windowing.tests getConnStr
        , Copy.tests getConnStr
        ]
  ]


setupTempPostgresDB :: TC.MonadDocker m => m ByteString
setupTempPostgresDB = do
  let user = "postgres"
      password = "root"
      db = "testdb"

  -- Pin the server version so normal CI runs are reproducible.
  postgresContainer <- TC.run $
    TC.containerRequest (TC.fromTag "postgres:18.4")
      TC.& TC.setExpose [5432]
      TC.& TC.setEnv
        [ ("POSTGRES_USER", user)
        , ("POSTGRES_PASSWORD", password)
        , ("POSTGRES_DB", db)
        ]
      TC.& TC.setWaitingFor
        (TC.waitForLogLine TC.Stderr
          ("database system is ready to accept connections" `TL.isInfixOf`))

  pure $ Postgres.postgreSQLConnectionString defaultConnectInfo
    { connectHost = "localhost"
    , connectUser = unpack user
    , connectPassword = unpack password
    , connectDatabase = unpack db
    , connectPort = fromIntegral $ TC.containerPort postgresContainer 5432
    }
