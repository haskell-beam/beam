{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Beam.DuckDB.Test.Query (tests) where

import Control.Monad (void)
import Data.Int (Int32)
import Data.List (nubBy, sort, sortOn)
import Data.Text (Text)
import Database.Beam
  ( Beamable,
    Columnar,
    Database,
    DatabaseSettings,
    Generic,
    Identity,
    SqlEq ((==.)),
    SqlOrd ((>.)),
    SqlValable (val_),
    Table (..),
    TableEntity,
    all_,
    dbModification,
    defaultDbSettings,
    guard_,
    insert,
    insertValues,
    leftJoin_,
    modifyTableFields,
    related_,
    runInsert,
    runSelectReturningList,
    select,
    tableModification,
    withDbModification,
    (>=.),
  )
import Database.Beam.DuckDB (DuckDB, runBeamDuckDB)
import Database.DuckDB.Simple (Connection, execute_, withConnection)
import Hedgehog (Gen, annotate, evalIO, forAll, property, (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Query"
    [ testGroup
        "Selection"
        [ testSelectAll,
          testSelectWithFilter,
          testSelectEquality
        ],
      testGroup
        "Projection"
        [ testProjectSingleColumn,
          testProjectMultipleColumns,
          testProjectWithExpression
        ],
      testGroup
        "Join"
        [ testInnerJoin,
          testMultiInnerJoin,
          testMixingJoinsWithFilters,
          testLeftJoin
        ]
    ]

testSelectAll :: TestTree
testSelectAll = testProperty "selecting all users should return the users initially inserted" $ property $ do
  users <- forAll genUsers
  results <- evalIO $
    withTestDb users [] [] $ \conn ->
      runBeamDuckDB conn $
        runSelectReturningList $
          select (all_ (_dbUsers testDb))
  sortOn _userId results === sortOn _userId users

testSelectWithFilter :: TestTree
testSelectWithFilter = testProperty "selecting users satisfying a condition works as expected" $ property $ do
  users <- forAll genUsers
  ageThreshold <- forAll genAge
  results <- evalIO $
    withTestDb users [] [] $ \conn ->
      runBeamDuckDB conn $
        runSelectReturningList $
          select $ do
            u <- all_ (_dbUsers testDb)
            guard_ (_userAge u >. val_ ageThreshold)
            pure u
  let expected = filter (\u -> _userAge u > ageThreshold) users
  sortOn _userId results === sortOn _userId expected

testSelectEquality :: TestTree
testSelectEquality = testProperty "" $ property $ do
  users <- forAll genUsers
  target <- forAll (Gen.element users)
  results <- evalIO $
    withTestDb users [] [] $ \conn ->
      runBeamDuckDB conn $
        runSelectReturningList $
          select $ do
            u <- all_ (_dbUsers testDb)
            guard_ (_userId u ==. val_ (_userId target))
            pure u
  results === [target]

testProjectSingleColumn :: TestTree
testProjectSingleColumn = testProperty "Single column projection works as expected" $ property $ do
  users <- forAll genUsers
  results <- evalIO $
    withTestDb users [] [] $ \conn ->
      runBeamDuckDB conn $
        runSelectReturningList $
          select $ do
            u <- all_ (_dbUsers testDb)
            pure (_userName u)
  sort results === sort (map _userName users)

testProjectMultipleColumns :: TestTree
testProjectMultipleColumns = testProperty "Multiple column projection works as expected" $ property $ do
  users <- forAll genUsers
  results <- evalIO $
    withTestDb users [] [] $ \conn ->
      runBeamDuckDB conn $
        runSelectReturningList $
          select $ do
            u <- all_ (_dbUsers testDb)
            pure (_userName u, _userAge u)
  let expected = map (\u -> (_userName u, _userAge u)) users
  sort results === sort expected

testProjectWithExpression :: TestTree
testProjectWithExpression = testProperty "Projection using an expression works as expected" $ property $ do
  products <- forAll genProducts
  results <- evalIO $
    withTestDb [] products [] $ \conn ->
      runBeamDuckDB conn $
        runSelectReturningList $
          select $ do
            p <- all_ (_dbProducts testDb)
            pure (_productId p, _productPrice p * val_ 2)
  let expected = map (\p -> (_productId p, _productPrice p * 2)) products
  sortOn fst results === sortOn fst expected

testInnerJoin :: TestTree
testInnerJoin = testProperty "Inner joins work as expected" $ property $ do
  users <- forAll genUsers
  products <- forAll genProducts
  orders <- forAll (genOrders users products)
  results <- evalIO $
    withTestDb users products orders $ \conn ->
      runBeamDuckDB conn $
        runSelectReturningList $
          select $ do
            o <- all_ (_dbOrders testDb)
            u <- related_ (_dbUsers testDb) (_orderUserId o)
            pure (_userName u, _orderQuantity o)

  let expected = do
        o <- orders
        let UserId uid = _orderUserId o
        u <- filter (\u -> _userId u == uid) users
        pure (_userName u, _orderQuantity o)
  sort results === sort expected

testMultiInnerJoin :: TestTree
testMultiInnerJoin = testProperty "Multi-inner joins work as expected" $ property $ do
  users <- forAll genUsers
  products <- forAll genProducts
  orders <- forAll (genOrders users products)
  results <- evalIO $
    withTestDb users products orders $ \conn ->
      runBeamDuckDB conn $
        runSelectReturningList $
          select $ do
            o <- all_ (_dbOrders testDb)
            u <- related_ (_dbUsers testDb) (_orderUserId o)
            p <- related_ (_dbProducts testDb) (_orderProductId o)
            pure (_userName u, _productName p, _orderQuantity o)
  let expected = do
        o <- orders
        let UserId uid = _orderUserId o
            ProductId pid = _orderProductId o
        u <- filter (\u -> _userId u == uid) users
        p <- filter (\p -> _productId p == pid) products
        pure (_userName u, _productName p, _orderQuantity o)
  sort results === sort expected

testMixingJoinsWithFilters :: TestTree
testMixingJoinsWithFilters = testProperty "Mixing joins and filters works as expected" $ property $ do
  users <- forAll genUsers
  products <- forAll genProducts
  orders <- forAll (genOrders users products)
  minQty <- forAll (Gen.int32 (Range.linear 1 50))
  results <- evalIO $
    withTestDb users products orders $ \conn ->
      runBeamDuckDB conn $
        runSelectReturningList $
          select $ do
            o <- all_ (_dbOrders testDb)
            guard_ (_orderQuantity o >=. val_ minQty)
            u <- related_ (_dbUsers testDb) (_orderUserId o)
            pure (_userName u, _orderQuantity o)
  let expected = do
        o <- filter (\o -> _orderQuantity o >= minQty) orders
        let UserId uid = _orderUserId o
        u <- filter (\u -> _userId u == uid) users
        pure (_userName u, _orderQuantity o)
  sort results === sort expected

testLeftJoin :: TestTree
testLeftJoin = testProperty "Left joins work as expected" $ property $ do
  users <- forAll genUsers
  products <- forAll genProducts
  -- Generate orders for only a subset of users
  let halfUsers = take (length users `div` 2) users
  orders <- forAll (genOrders halfUsers products)
  results <- evalIO $
    withTestDb users products orders $ \conn ->
      runBeamDuckDB conn $
        runSelectReturningList $
          select $ do
            u <- all_ (_dbUsers testDb)
            o <-
              leftJoin_
                (all_ (_dbOrders testDb))
                (\o -> _orderUserId o ==. primaryKey u)
            pure (_userId u, _orderQuantity o)

  let resultUserIds = nubBy (\a b -> fst a == fst b) (sortOn fst results)
  length resultUserIds === length users

  let usersWithoutOrders = filter (\u -> _userId u `notElem` map _userId halfUsers) users
      nothingRows = filter (\(uid, _) -> uid `elem` map _userId usersWithoutOrders) results
  annotate "Users without orders should have Nothing quantity"
  mapM_ (\(_, mq) -> mq === Nothing) nothingRows

data UserT f = User
  { _userId :: Columnar f Int32,
    _userName :: Columnar f Text,
    _userAge :: Columnar f Int32
  }
  deriving (Generic)

type User = UserT Identity

type UserId = PrimaryKey UserT Identity

deriving instance Show UserId

deriving instance Eq UserId

deriving instance Ord UserId

deriving instance Show User

deriving instance Eq User

deriving instance Ord User

instance Beamable UserT

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int32)
    deriving (Generic)
  primaryKey = UserId . _userId

instance Beamable (PrimaryKey UserT)

data ProductT f = Product
  { _productId :: Columnar f Int32,
    _productName :: Columnar f Text,
    _productPrice :: Columnar f Int32 -- cents, to avoid floating point
  }
  deriving (Generic)

type Product = ProductT Identity

type ProductId = PrimaryKey ProductT Identity

deriving instance Show ProductId

deriving instance Eq ProductId

deriving instance Ord ProductId

deriving instance Show Product

deriving instance Eq Product

deriving instance Ord Product

instance Beamable ProductT

instance Table ProductT where
  data PrimaryKey ProductT f = ProductId (Columnar f Int32)
    deriving (Generic)
  primaryKey = ProductId . _productId

instance Beamable (PrimaryKey ProductT)

data OrderT f = Order
  { _orderId :: Columnar f Int32,
    _orderUserId :: PrimaryKey UserT f,
    _orderProductId :: PrimaryKey ProductT f,
    _orderQuantity :: Columnar f Int32
  }
  deriving (Generic)

type Order = OrderT Identity

type OrderId = PrimaryKey OrderT Identity

deriving instance Show OrderId

deriving instance Eq OrderId

deriving instance Ord OrderId

deriving instance Show Order

deriving instance Eq Order

deriving instance Ord Order

instance Beamable OrderT

instance Table OrderT where
  data PrimaryKey OrderT f = OrderId (Columnar f Int32)
    deriving (Generic)
  primaryKey = OrderId . _orderId

instance Beamable (PrimaryKey OrderT)

data TestDB f = TestDB
  { _dbUsers :: f (TableEntity UserT),
    _dbProducts :: f (TableEntity ProductT),
    _dbOrders :: f (TableEntity OrderT)
  }
  deriving (Generic, Database be)

testDb :: DatabaseSettings DuckDB TestDB
testDb =
  defaultDbSettings
    `withDbModification` dbModification
      { _dbUsers =
          modifyTableFields
            tableModification
              { _userId = "id",
                _userName = "name",
                _userAge = "age"
              },
        _dbProducts =
          modifyTableFields
            tableModification
              { _productId = "id",
                _productName = "name",
                _productPrice = "price"
              },
        _dbOrders =
          modifyTableFields
            tableModification
              { _orderId = "id",
                _orderUserId = UserId "user_id",
                _orderProductId = ProductId "product_id",
                _orderQuantity = "quantity"
              }
      }

genName :: Gen Text
genName = Gen.text (Range.linear 1 50) Gen.alphaNum

genAge :: Gen Int32
genAge = Gen.int32 (Range.linear 1 120)

genPrice :: Gen Int32
genPrice = Gen.int32 (Range.linear 100 100000)

genQuantity :: Gen Int32
genQuantity = Gen.int32 (Range.linear 1 100)

genUsers :: Gen [User]
genUsers = do
  n <- Gen.int (Range.linear 3 20)
  traverse (\i -> User (fromIntegral i) <$> genName <*> genAge) [1 .. n]

genProducts :: Gen [Product]
genProducts = do
  n <- Gen.int (Range.linear 2 10)
  traverse (\i -> Product (fromIntegral i) <$> genName <*> genPrice) [1 .. n]

genOrders :: [User] -> [Product] -> Gen [Order]
genOrders users products = do
  n <- Gen.int (Range.linear 1 (length users * length products))
  traverse
    ( \i -> do
        uid <- Gen.element (map _userId users)
        pid <- Gen.element (map _productId products)
        Order (fromIntegral i) (UserId uid) (ProductId pid) <$> genQuantity
    )
    [1 .. n]

createTables :: Connection -> IO ()
createTables conn = do
  void $
    execute_
      conn
      "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT NOT NULL, age INTEGER NOT NULL)"
  void $
    execute_
      conn
      "CREATE TABLE products (id INTEGER PRIMARY KEY, name TEXT NOT NULL, price INTEGER NOT NULL)"
  void $
    execute_
      conn
      "CREATE TABLE orders (\
      \  id INTEGER PRIMARY KEY, \
      \  user_id INTEGER NOT NULL REFERENCES users(id), \
      \  product_id INTEGER NOT NULL REFERENCES products(id), \
      \  quantity INTEGER NOT NULL)"

seedData :: Connection -> [User] -> [Product] -> [Order] -> IO ()
seedData conn users products orders = runBeamDuckDB conn $ do
  runInsert $ insert (_dbUsers testDb) $ insertValues users
  runInsert $ insert (_dbProducts testDb) $ insertValues products
  runInsert $ insert (_dbOrders testDb) $ insertValues orders

-- Run a test with a fresh in-memory DB populated with the given data
withTestDb ::
  [User] ->
  [Product] ->
  [Order] ->
  (Connection -> IO a) ->
  IO a
withTestDb users products orders action =
  withConnection ":memory:" $ \conn -> do
    createTables conn
    seedData conn users products orders
    action conn
