module Database.Beam.Postgres.Test.Select where

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple (autoMigrate)
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Migrate (migrationBackend)
import           Database.Beam.Postgres.Test
import qualified Database.PostgreSQL.Simple as Pg

import           Data.ByteString (ByteString)
import           Data.Functor.Classes
import           Data.Int
import           Data.Proxy (Proxy(..))
import           Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Typeable
import           Data.UUID (UUID, fromWords)
import           Data.Word

import qualified Hedgehog
import           Hedgehog ((===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Test.Tasty
import           Test.Tasty.HUnit

import           Unsafe.Coerce

tests :: IO ByteString -> TestTree
tests postgresConn =
    testGroup "Postgres Select Tests" []
