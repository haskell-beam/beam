{-# LANGUAGE AllowAmbiguousTypes #-}
module Database.Beam.Postgres.Test.Marshal where

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Migrate
import           Database.Beam.Migrate.Simple (autoMigrate)
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Migrate (migrationBackend)
import           Database.Beam.Postgres.Test

import           Data.ByteString (ByteString)
import           Data.Functor.Classes
import           Data.Int
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
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

textGen :: Hedgehog.Gen T.Text
textGen = Gen.text (Range.constant 0 1000) $ Gen.filter (/= '\NUL') Gen.unicode

uuidGen :: Hedgehog.Gen UUID
uuidGen = fromWords <$> Gen.integral Range.constantBounded
                    <*> Gen.integral Range.constantBounded
                    <*> Gen.integral Range.constantBounded
                    <*> Gen.integral Range.constantBounded

pointGen :: Hedgehog.Gen PgPoint
pointGen = PgPoint <$> Gen.double (Range.constant 0 1000)
                   <*> Gen.double (Range.constant 0 1000)

boxGen :: Hedgehog.Gen PgBox
boxGen = do PgPoint x1 y1 <- pointGen
            PgPoint x2 y2 <- pointGen
            pure (PgBox (PgPoint (min x1 x2) (min y1 y2))
                        (PgPoint (max x1 x2) (max y1 y2)))

boxCmp :: PgBox -> PgBox -> Bool
boxCmp (PgBox a1 b1) (PgBox a2 b2) =
    (a1 `ptCmp` a2 && b1 `ptCmp` b2) ||
    (a1 `ptCmp` b2 && b1 `ptCmp` a2)

ptCmp :: PgPoint -> PgPoint -> Bool
ptCmp (PgPoint x1 y1) (PgPoint x2 y2) =
    x1 `dblCmp` x2 && y1 `dblCmp` y2

dblCmp :: Double -> Double -> Bool
dblCmp x y =
    let ulp = abs ((unsafeCoerce x :: Int64) - (unsafeCoerce y :: Int64))
    in ulp < 50

tests :: IO ByteString -> TestTree
tests postgresConn =
    testGroup "Postgres Marshaling tests"
    [ marshalTest Gen.bool postgresConn
    , marshalTest (Gen.integral (Range.constantBounded @Int16))  postgresConn
    , marshalTest (Gen.integral (Range.constantBounded @Int32))  postgresConn
    , marshalTest (Gen.integral (Range.constantBounded @Int64))  postgresConn
    , marshalTest (Gen.integral (Range.constantBounded @Word16)) postgresConn
    , marshalTest (Gen.integral (Range.constantBounded @Word32)) postgresConn
    , marshalTest (Gen.integral (Range.constantBounded @Word64)) postgresConn
    , marshalTest textGen postgresConn
    , marshalTest uuidGen postgresConn

    , marshalTest' (\a b -> Hedgehog.assert (ptCmp a b))  pointGen postgresConn
    , marshalTest' (\a b -> Hedgehog.assert (boxCmp a b)) boxGen   postgresConn

    , marshalTest (Gen.maybe Gen.bool) postgresConn
    , marshalTest (Gen.maybe (Gen.integral (Range.constantBounded @Int16)))  postgresConn
    , marshalTest (Gen.maybe (Gen.integral (Range.constantBounded @Int32)))  postgresConn
    , marshalTest (Gen.maybe (Gen.integral (Range.constantBounded @Int64)))  postgresConn
    , marshalTest (Gen.maybe (Gen.integral (Range.constantBounded @Word16))) postgresConn
    , marshalTest (Gen.maybe (Gen.integral (Range.constantBounded @Word32))) postgresConn
    , marshalTest (Gen.maybe (Gen.integral (Range.constantBounded @Word64))) postgresConn
    , marshalTest (Gen.maybe textGen) postgresConn
    , marshalTest (Gen.maybe uuidGen) postgresConn

    , marshalTest' (\a b -> Hedgehog.assert (liftEq ptCmp a b))  (Gen.maybe pointGen) postgresConn
    , marshalTest' (\a b -> Hedgehog.assert (liftEq boxCmp a b)) (Gen.maybe boxGen) postgresConn

--    , marshalTest (Gen.double  (Range.exponentialFloat 0 1e40))  postgresConn
--    , marshalTest (Gen.integral (Range.constantBounded @Word))   postgresConn
--    , marshalTest (Gen.integral (Range.constantBounded @Int))    postgresConn

--    , marshalTest @Int8    postgresConn
--    , marshalTest @Integer postgresConn
--    , marshalTest @Word8   postgresConn
--    , marshalTest @TL.Text postgresConn
    -- TODO MORE!!!!
    ]

data MarshalTable a f
    = MarshalTable
    { _marshalTableId    :: C f (SqlSerial Int32)
    , _marshalTableEntry :: C f a
    } deriving (Generic)
instance Beamable (MarshalTable a)

instance Typeable a => Table (MarshalTable a) where
    data PrimaryKey (MarshalTable a) f = MarshalTableKey (C f (SqlSerial Int32))
      deriving (Generic, Beamable)
    primaryKey = MarshalTableKey . _marshalTableId

data MarshalDb a entity
    = MarshalDb
    { _marshalTbl :: entity (TableEntity (MarshalTable a))
    } deriving (Generic)
instance Typeable a => Database Postgres (MarshalDb a)

marshalTest :: forall a
             . ( Typeable a, Eq a, Show a
               , BeamSqlBackendSupportsDataType Postgres a
               , HasDefaultSqlDataType Postgres a
               , HasNullableConstraint (NullableStatus a) Postgres )
            => Hedgehog.Gen a -> IO ByteString -> TestTree
marshalTest = marshalTest' (===)

marshalTest' :: forall a
              . ( Typeable a, Show a
                , BeamSqlBackendSupportsDataType Postgres a
                , HasDefaultSqlDataType Postgres a
                , HasNullableConstraint (NullableStatus a) Postgres )
             => (forall m. (Hedgehog.MonadTest m, HasCallStack) => a -> a -> m ()) -> Hedgehog.Gen a -> IO ByteString -> TestTree
marshalTest' cmp gen postgresConn =
  testCase ("Can marshal " ++ show (typeRep (Proxy @a))) $
  withTestPostgres ("db_marshal_" <> show (typeRepFingerprint (typeRep (Proxy @a))))
                   postgresConn $ \conn -> do
    let marshalDbSettings = defaultMigratableDbSettings @Postgres @(MarshalDb a)
        marshalDb = unCheckDatabase marshalDbSettings

    runBeamPostgres conn $ do
      autoMigrate migrationBackend marshalDbSettings

    putStrLn "\n"

    passes <- Hedgehog.check . Hedgehog.property $ do
      a <- Hedgehog.forAll gen

      [MarshalTable rowId v] <-
        liftIO . runBeamPostgres conn $
        runInsertReturningList $ insert (_marshalTbl marshalDb) $ insertExpressions [ MarshalTable default_ (val_ a) ]
      v `cmp` a

      Just (MarshalTable _ v') <-
          liftIO . runBeamPostgres conn $
          runSelectReturningOne (lookup_ (_marshalTbl marshalDb) (MarshalTableKey rowId))
      v' `cmp` a

    assertBool "Hedgehog test failed" passes

