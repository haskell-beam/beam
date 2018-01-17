{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Database.Beam
import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL
import Database.Beam.Backend.SQL92
import Database.Beam.Schema.SQL92

import Data.Data
import Data.Text (Text)
import qualified Data.Text as T
import Data.String

data Postgres = Postgres
  deriving Data

instance BeamBackend Postgres where
  data BackendLiteral Postgres
    = PgInt !Integer
    | PgText !Text
    | PgBool !Bool
    | PgNull
    deriving (Show, Eq, Data)

  backendNull = PgNull

-- * Beam schema
data Position = DepartmentLead
              | Analyst
                deriving (Show, Read, Eq, Ord, Enum)
instance BeamSql92Backend be => HasDefaultFieldSchema be Position where
    defFieldSchema = enumSchema
instance BeamSql92Backend be => FromBackendLiterals be Position
instance BeamSql92Backend be => FromBackendLiteral be Position where
    fromBackendLiteral = fromEnumValue
    toBackendLiteral = makeEnumValue

data EmployeeT f = Employee
                 { _employeeId         :: Columnar f Int
                 , _employeeFirstName  :: Columnar f Text
                 , _employeeLastName   :: Columnar f Text
                 , _employeeGroup      :: PrimaryKey GroupT f

                 , _employeePosition   :: Columnar f Position }
                 deriving Generic
instance Beamable EmployeeT

instance Table EmployeeT where
  data PrimaryKey EmployeeT f = EmployeeId (Columnar f Int)
    deriving Generic
  primaryKey = EmployeeId . _employeeId
instance Beamable (PrimaryKey EmployeeT)

data DepartmentT f = Department
                   { _deptId       :: Columnar f Text }
                   deriving Generic
instance Beamable DepartmentT

instance Table DepartmentT where
  data PrimaryKey DepartmentT f = DepartmentId (Columnar f Text)
    deriving Generic
  primaryKey = DepartmentId . _deptId
instance Beamable (PrimaryKey DepartmentT)

data GroupT f = Group
              { _groupId       :: Columnar f Text
              , _groupDeptId   :: PrimaryKey DepartmentT f
              , _groupLocation :: Columnar f Text }
                deriving Generic
instance Beamable GroupT

instance Table GroupT where
  data PrimaryKey GroupT f = GroupId (Columnar f Text) (Columnar f Text)
    deriving Generic
  primaryKey (Group groupId (DepartmentId deptId) _) = GroupId groupId deptId
instance Beamable (PrimaryKey GroupT)

data OrderT f = Order
              { _orderId  :: Columnar f Int
              , _orderTakenBy :: PrimaryKey EmployeeT f
              , _orderAmount :: Columnar f Int }
              deriving Generic
instance Beamable OrderT

instance Table OrderT where
  data PrimaryKey OrderT f = OrderId (Columnar f Int)
    deriving Generic
  primaryKey = OrderId . _orderId
instance Beamable (PrimaryKey OrderT)

data EmployeeDatabase q
  = EmployeeDatabase
  { _employees :: q EmployeeT
  , _departments :: q DepartmentT
  , _groups :: q GroupT
  , _orders :: q OrderT }
  deriving Generic

instance Database EmployeeDatabase

employeeDb :: DatabaseSettings Postgres EmployeeDatabase
employeeDb = autoDbSettings

instance BeamSqlBackend Postgres
instance BeamSql92Backend Postgres
instance HasDefaultFieldSchema Postgres Int where
  defFieldSchema = int
instance HasDefaultFieldSchema Postgres Text where
  defFieldSchema = varchar Nothing

instance FromBackendLiteral Postgres Text where
  fromBackendLiteral (PgText t) = Just t
  fromBackendLiteral _ = Nothing
  toBackendLiteral = PgText
instance FromBackendLiterals Postgres Text

instance FromBackendLiteral Postgres Bool where
  fromBackendLiteral (PgBool b) = Just b
  fromBackendLiteral _ = Nothing
  toBackendLiteral = PgBool
instance FromBackendLiterals Postgres Bool  

instance FromBackendLiteral Postgres Int where
  fromBackendLiteral (PgInt i) = Just (fromIntegral i)
  fromBackendLiteral _ = Nothing
  toBackendLiteral = PgInt . fromIntegral
instance FromBackendLiterals Postgres Int

instance FromBackendLiteral Postgres String where
  fromBackendLiteral (PgText t) = Just (T.unpack t)
  fromBackendLiteral _ = Nothing
  toBackendLiteral = PgText . fromString
instance FromBackendLiterals Postgres String  

main :: IO ()
main = putStrLn "Hello world"
