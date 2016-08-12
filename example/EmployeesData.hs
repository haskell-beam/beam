module EmployeesData where

import Database.Beam
import Data.Text

import Lens.Micro

-- * Beam schema
data Position = DepartmentLead
              | Analyst
                deriving (Show, Read, Eq, Ord, Enum)
instance HasDefaultFieldSchema Position where
    defFieldSchema = enumSchema
instance FromSqlValues Position

data EmployeeT f = Employee
                 { _employeeId         :: Columnar f AutoId
                 , _employeeFirstName  :: Columnar f Text
                 , _employeeLastName   :: Columnar f Text
                 , _employeeGroup      :: PrimaryKey GroupT f

                 , _employeePosition   :: Columnar f Position }
                 deriving (Generic)

data DepartmentT f = Department
                   { _deptId       :: Columnar f Text }
                   deriving (Generic)

data GroupT f = Group
              { _groupId       :: Columnar f Text
              , _groupDeptId   :: PrimaryKey DepartmentT f
              , _groupLocation :: Columnar f Text }
                deriving (Generic)

data OrderT f = Order
              { _orderId  :: Columnar f AutoId
              , _orderTakenBy :: PrimaryKey EmployeeT f
              , _orderAmount :: Columnar f Int }
              deriving Generic

Employee (LensFor employeeIdC)
         (LensFor employeeFirstNameC)
         (LensFor employeeLastNameC)
         (GroupId (LensFor employeeDeptIdC) (LensFor employeeGroupIdC))
         (LensFor employeePositionC) = tableConfigLenses

Department (LensFor deptIdC) = tableConfigLenses

Group (LensFor groupIdC)
      (DepartmentId (LensFor groupDeptIdC))
      (LensFor groupLocationC) = tableConfigLenses

Order (LensFor orderIdC)
      (EmployeeId (LensFor orderTakenByIdC))
      (LensFor orderAmountC) = tableConfigLenses

data EmployeeDatabase q = EmployeeDatabase
                        { _employees   :: q EmployeeT
                        , _departments :: q DepartmentT
                        , _groups      :: q GroupT
                        , _orders      :: q OrderT }
                        deriving Generic

instance Table EmployeeT where
    data PrimaryKey EmployeeT f = EmployeeId (Columnar f AutoId)
                                deriving Generic
    primaryKey = EmployeeId . _employeeId
instance Table DepartmentT where
    data PrimaryKey DepartmentT f = DepartmentId (Columnar f Text)
                                  deriving Generic
    primaryKey = DepartmentId . _deptId

    tblFieldSettings = defTblFieldSettings
                        & deptIdC . fieldSchema .~ textSchema (Varchar (Just 32))
instance Table GroupT where
    data PrimaryKey GroupT f = GroupId (Columnar f Text) (Columnar f Text)
                             deriving Generic
    primaryKey (Group groupId (DepartmentId deptId) _ ) = GroupId groupId deptId

    tblFieldSettings = defTblFieldSettings

instance Table OrderT where
    data PrimaryKey OrderT f = OrderId (Columnar f AutoId)
                             deriving Generic
    primaryKey = OrderId . _orderId

    tblFieldSettings = defTblFieldSettings

type Employee = EmployeeT Identity
type EmployeeId = PrimaryKey EmployeeT Identity
type Department = DepartmentT Identity
type DepartmentId = PrimaryKey DepartmentT Identity
type Group = GroupT Identity
type GroupId = PrimaryKey GroupT Identity
type Order = OrderT Identity
type OrderId = PrimaryKey OrderT Identity

deriving instance Show Group
deriving instance Show GroupId
deriving instance Show Employee
deriving instance Show EmployeeId
deriving instance Show Department
deriving instance Show DepartmentId
deriving instance Show Order
deriving instance Show OrderId

instance Database EmployeeDatabase
employeeDb :: DatabaseSettings EmployeeDatabase
employeeDb = autoDbSettings
