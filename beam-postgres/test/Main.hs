module Main where

import Data.ByteString ( ByteString )
import Data.Text ( unpack )
import qualified Data.Text.Lazy as TL

import Test.Tasty
import qualified TestContainers.Tasty as TC

import qualified Database.Beam.Postgres.Test.Select as Select
import qualified Database.Beam.Postgres.Test.Marshal as Marshal
import qualified Database.Beam.Postgres.Test.DataTypes as DataType
import qualified Database.Beam.Postgres.Test.Migrate as Migrate
import qualified Database.Beam.Postgres.Test.Windowing as Windowing
import Database.PostgreSQL.Simple ( ConnectInfo(..), defaultConnectInfo )
import qualified Database.PostgreSQL.Simple as Postgres

main :: IO ()
main = defaultMain
     $ TC.withContainers setupTempPostgresDB
     $ \getConnStr ->
        testGroup "beam-postgres tests"
          [ Marshal.tests getConnStr
          , Select.tests getConnStr
          , DataType.tests getConnStr
          , Migrate.tests getConnStr
          , Windowing.tests getConnStr
          ]


setupTempPostgresDB :: TC.MonadDocker m => m ByteString
setupTempPostgresDB = do
    let user     = "postgres"
        password = "root"
        db       = "testdb"

    timescaleContainer <- TC.run $ TC.containerRequest (TC.fromTag "postgres:16.4")
        TC.& TC.setExpose [ 5432 ]
        TC.& TC.setEnv [ ("POSTGRES_USER", user)
                       , ("POSTGRES_PASSWORD", password)
                       , ("POSTGRES_DB", db)
                       ]
        TC.& TC.setWaitingFor (TC.waitForLogLine TC.Stderr ("database system is ready to accept connections" `TL.isInfixOf`))

    pure $ Postgres.postgreSQLConnectionString
                   ( defaultConnectInfo { connectHost     = "localhost"
                                        , connectUser     = unpack user
                                        , connectPassword = unpack password
                                        , connectDatabase = unpack db
                                        , connectPort     = fromIntegral $ TC.containerPort timescaleContainer 5432
                                        }
                   )