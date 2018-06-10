{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Query.DataTypes where

import Database.Beam.Backend.SQL
import Database.Beam.Query.Internal

import Data.Text (Text)
import Data.Time (LocalTime, Day, TimeOfDay)
import Data.Scientific (Scientific)
import Data.Typeable (Typeable)
import Data.Vector (Vector)

-- | A data type in a given 'IsSql92DataTypeSyntax' which describes a SQL type
-- mapping to the Haskell type @a@
newtype DataType be a = DataType (BeamSqlBackendCastTargetSyntax be)

instance Sql92DisplaySyntax (BeamSqlBackendCastTargetSyntax be) => Show (DataType be a) where
  show (DataType syntax) = "DataType (" ++ displaySyntax syntax ++ ")"
deriving instance Eq (BeamSqlBackendCastTargetSyntax be) => Eq (DataType be a)

-- | Cast a value to a specific data type, specified using 'DataType'.
--
-- Note: this may fail at run-time if the cast is invalid for a particular value
cast_ :: BeamSqlBackend be => QGenExpr ctxt be s a -> DataType be b -> QGenExpr ctxt be s b
cast_ (QExpr e) (DataType dt) = QExpr (castE <$> e <*> pure dt)

-- ** Data types

-- | SQL92 @INTEGER@ data type
int :: (BeamSqlBackend be, Integral a) => DataType be a
int = DataType intType

-- | SQL92 @SMALLINT@ data type
smallint :: (BeamSqlBackend be, Integral a) => DataType be a
smallint = DataType smallIntType

-- | SQL2008 Optional @BIGINT@ data type
bigint :: ( BeamSqlBackend be, BeamSqlT071Backend be, Integral a )
       => DataType be a
bigint = DataType bigIntType

-- TODO is Integer the right type to use here?
-- | SQL2003 Optional @BINARY@ data type
binary :: ( BeamSqlBackend be, BeamSqlT021Backend be )
       => Maybe Word -> DataType be Integer
binary prec = DataType (binaryType prec)

-- | SQL2003 Optional @VARBINARY@ data type
varbinary :: ( BeamSqlBackend be, BeamSqlT021Backend be )
          => Maybe Word -> DataType be Integer
varbinary prec = DataType (varBinaryType prec)

-- TODO should this be Day or something?
-- | SQL92 @DATE@ data type
date :: BeamSqlBackend be => DataType be Day
date = DataType dateType

-- | SQL92 @CHAR@ data type
char :: BeamSqlBackend be => Maybe Word -> DataType be Text
char prec = DataType (charType prec Nothing)

-- | SQL92 @VARCHAR@ data type
varchar :: BeamSqlBackend be => Maybe Word -> DataType be Text
varchar prec = DataType (varCharType prec Nothing)

-- | SQL92 @NATIONAL CHARACTER VARYING@ data type
nationalVarchar :: BeamSqlBackend be => Maybe Word -> DataType be Text
nationalVarchar prec = DataType (nationalVarCharType prec)

-- | SQL92 @NATIONAL CHARACTER@ data type
nationalChar :: BeamSqlBackend be => Maybe Word -> DataType be Text
nationalChar prec = DataType (nationalCharType prec)

-- | SQL92 @DOUBLE@ data type
double :: BeamSqlBackend be => DataType be Double
double = DataType doubleType

-- | SQL92 @NUMERIC@ data type
numeric :: BeamSqlBackend be => Maybe (Word, Maybe Word) -> DataType be Scientific
numeric x = DataType (numericType x)

-- | SQL92 @TIMESTAMP WITH TIME ZONE@ data type
timestamptz :: BeamSqlBackend be => DataType be LocalTime
timestamptz = DataType (timestampType Nothing True)

-- | SQL92 @TIMESTAMP WITHOUT TIME ZONE@ data type
timestamp :: BeamSqlBackend be => DataType be LocalTime
timestamp = DataType (timestampType Nothing False)

-- | SQL92 @TIME@ data type
time :: BeamSqlBackend be => Maybe Word -> DataType be TimeOfDay
time prec = DataType (timeType prec False)

-- | SQL99 @BOOLEAN@ data type
boolean :: BeamSql99DataTypeBackend be => DataType be Bool
boolean = DataType booleanType

-- | SQL99 @CLOB@ data type
characterLargeObject :: BeamSql99DataTypeBackend be => DataType be Text
characterLargeObject = DataType characterLargeObjectType

-- | SQL99 @BLOB@ data type
binaryLargeObject :: BeamSql99DataTypeBackend be => DataType be Text
binaryLargeObject = DataType binaryLargeObjectType

-- | SQL99 array data types
array :: (Typeable a, BeamSql99DataTypeBackend be)
      => DataType be a -> Int
      -> DataType be (Vector a)
array (DataType ty) sz = DataType (arrayType ty sz)

-- | Haskell requires 'DataType's to match exactly. Use this function to convert
-- a 'DataType' that expects a concrete value to one expecting a 'Maybe'
maybeType :: DataType be a -> DataType be (Maybe a)
maybeType (DataType sqlTy) = DataType sqlTy
