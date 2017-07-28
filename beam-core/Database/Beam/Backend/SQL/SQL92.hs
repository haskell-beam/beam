{-# LANGUAGE PolyKinds #-}

-- | Finally tagless encoding of SQL92 syntax
module Database.Beam.Backend.SQL.SQL92 where

import           Database.Beam.Backend.Types
import           Database.Beam.Backend.SQL.Types

import           Data.Int
import           Data.Text (Text)
import           Data.Time (LocalTime)

class ( BeamSqlBackend be ) =>
      BeamSql92Backend be where

-- * Finally tagless style

class HasSqlValueSyntax expr ty where
  sqlValueSyntax :: ty -> expr
class IsSqlExpressionSyntaxStringType expr ty

autoSqlValueSyntax :: (HasSqlValueSyntax expr String, Show a) => a -> expr
autoSqlValueSyntax = sqlValueSyntax . show

type Sql92SelectExpressionSyntax select = Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)
type Sql92SelectProjectionSyntax select = Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax select)
type Sql92SelectGroupingSyntax select = Sql92SelectTableGroupingSyntax (Sql92SelectSelectTableSyntax select)
type Sql92SelectFromSyntax select = Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select)

type Sql92ValueSyntax cmdSyntax = Sql92ExpressionValueSyntax (Sql92ExpressionSyntax cmdSyntax)
type Sql92ExpressionSyntax cmdSyntax = Sql92SelectExpressionSyntax (Sql92SelectSyntax cmdSyntax)
type Sql92HasValueSyntax cmdSyntax = HasSqlValueSyntax (Sql92ValueSyntax cmdSyntax)

-- Putting these in the head constraint can cause infinite recursion that would
-- need <UndecidableSuperclasses. If we define them here, we can easily use them
-- in functions that need them and avoid unnecessary extensions.
type Sql92SelectSanityCheck select =
  ( Sql92FromExpressionSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select)) ~
    Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)
  , Sql92TableSourceSelectSyntax (Sql92FromTableSourceSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select))) ~ select
  , Sql92ProjectionExpressionSyntax (Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax select)) ~
    Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)
  , Sql92OrderingExpressionSyntax (Sql92SelectOrderingSyntax select) ~
    Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select))
type Sql92SanityCheck cmd = ( Sql92SelectSanityCheck (Sql92SelectSyntax cmd)
                            , Sql92ExpressionValueSyntax (Sql92InsertValuesExpressionSyntax (Sql92InsertValuesSyntax (Sql92InsertSyntax cmd))) ~ Sql92ValueSyntax cmd
                            , Sql92ExpressionValueSyntax (Sql92UpdateExpressionSyntax (Sql92UpdateSyntax cmd)) ~ Sql92ValueSyntax cmd
                            , Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax cmd)) ~
                              Sql92InsertValuesExpressionSyntax (Sql92InsertValuesSyntax (Sql92InsertSyntax cmd)) )

type Sql92ReasonableMarshaller be =
   ( FromBackendRow be Int, FromBackendRow be SqlNull
   , FromBackendRow be Text, FromBackendRow be Bool
   , FromBackendRow be Char
   , FromBackendRow be Int16, FromBackendRow be Int32, FromBackendRow be Int64
   , FromBackendRow be LocalTime )

class ( IsSql92SelectSyntax (Sql92SelectSyntax cmd)
      , IsSql92InsertSyntax (Sql92InsertSyntax cmd)
      , IsSql92UpdateSyntax (Sql92UpdateSyntax cmd)
      , IsSql92DeleteSyntax (Sql92DeleteSyntax cmd) ) =>
  IsSql92Syntax cmd where
  type Sql92SelectSyntax cmd :: *
  type Sql92InsertSyntax cmd :: *
  type Sql92UpdateSyntax cmd :: *
  type Sql92DeleteSyntax cmd :: *

  selectCmd :: Sql92SelectSyntax cmd -> cmd
  insertCmd :: Sql92InsertSyntax cmd -> cmd
  updateCmd :: Sql92UpdateSyntax cmd -> cmd
  deleteCmd :: Sql92DeleteSyntax cmd -> cmd

class ( IsSql92SelectTableSyntax (Sql92SelectSelectTableSyntax select)
      , IsSql92OrderingSyntax (Sql92SelectOrderingSyntax select) ) =>
    IsSql92SelectSyntax select where
    type Sql92SelectSelectTableSyntax select :: *
    type Sql92SelectOrderingSyntax select :: *

    selectStmt :: Sql92SelectSelectTableSyntax select
               -> [Sql92SelectOrderingSyntax select]
               -> Maybe Integer {-^ LIMIT -}
               -> Maybe Integer {-^ OFFSET -}
               -> select

class ( IsSql92ExpressionSyntax (Sql92SelectTableExpressionSyntax select)
      , IsSql92AggregationExpressionSyntax (Sql92SelectTableExpressionSyntax select)
      , IsSql92ProjectionSyntax (Sql92SelectTableProjectionSyntax select)
      , IsSql92FromSyntax (Sql92SelectTableFromSyntax select)
      , IsSql92GroupingSyntax (Sql92SelectTableGroupingSyntax select)
      , IsSql92AggregationSetQuantifierSyntax (Sql92SelectTableSetQuantifierSyntax select)

      , Sql92GroupingExpressionSyntax (Sql92SelectTableGroupingSyntax select) ~ Sql92SelectTableExpressionSyntax select
      , Sql92FromExpressionSyntax (Sql92SelectTableFromSyntax select) ~ Sql92SelectTableExpressionSyntax select
      , Sql92SelectSelectTableSyntax (Sql92SelectTableSelectSyntax select) ~ select

      , Eq (Sql92SelectTableExpressionSyntax select) ) =>
    IsSql92SelectTableSyntax select where
  type Sql92SelectTableSelectSyntax select :: *
  type Sql92SelectTableExpressionSyntax select :: *
  type Sql92SelectTableProjectionSyntax select :: *
  type Sql92SelectTableFromSyntax select :: *
  type Sql92SelectTableGroupingSyntax select :: *
  type Sql92SelectTableSetQuantifierSyntax select :: *

  selectTableStmt :: Maybe (Sql92SelectTableSetQuantifierSyntax select)
                  -> Sql92SelectTableProjectionSyntax select
                  -> Maybe (Sql92SelectTableFromSyntax select)
                  -> Maybe (Sql92SelectTableExpressionSyntax select)   {-^ Where clause -}
                  -> Maybe (Sql92SelectTableGroupingSyntax select)
                  -> Maybe (Sql92SelectTableExpressionSyntax select) {-^ having clause -}
                  -> select

  unionTables, intersectTables, exceptTable ::
    Bool -> select -> select -> select

class IsSql92InsertValuesSyntax (Sql92InsertValuesSyntax insert) =>
  IsSql92InsertSyntax insert where

  type Sql92InsertValuesSyntax insert :: *
  insertStmt :: Text
             -> [ Text ]
             -> Sql92InsertValuesSyntax insert
             -> insert

class IsSql92ExpressionSyntax (Sql92InsertValuesExpressionSyntax insertValues) =>
  IsSql92InsertValuesSyntax insertValues where
  type Sql92InsertValuesExpressionSyntax insertValues :: *
  type Sql92InsertValuesSelectSyntax insertValues :: *

  insertSqlExpressions :: [ [ Sql92InsertValuesExpressionSyntax insertValues ] ]
                       -> insertValues
  insertFromSql :: Sql92InsertValuesSelectSyntax insertValues
                -> insertValues

class ( IsSql92ExpressionSyntax (Sql92UpdateExpressionSyntax update)
      , IsSql92FieldNameSyntax (Sql92UpdateFieldNameSyntax update)) =>
      IsSql92UpdateSyntax update where
  type Sql92UpdateFieldNameSyntax update :: *
  type Sql92UpdateExpressionSyntax update :: *

  updateStmt :: Text
             -> [(Sql92UpdateFieldNameSyntax update, Sql92UpdateExpressionSyntax update)]
             -> Maybe (Sql92UpdateExpressionSyntax update) {-^ WHERE -}
             -> update

class IsSql92ExpressionSyntax (Sql92DeleteExpressionSyntax delete) =>
  IsSql92DeleteSyntax delete where
  type Sql92DeleteExpressionSyntax delete :: *

  deleteStmt :: Text
             -> Maybe (Sql92DeleteExpressionSyntax delete)
             -> delete

class IsSql92FieldNameSyntax fn where
  qualifiedField :: Text -> Text -> fn
  unqualifiedField :: Text -> fn

class IsSql92QuantifierSyntax quantifier where
  quantifyOverAll, quantifyOverAny :: quantifier

class IsSql92DataTypeSyntax dataType where
  domainType :: Text -> dataType
  charType :: Maybe Word -> Maybe Text -> dataType
  varCharType :: Maybe Word -> Maybe Text -> dataType
  nationalCharType :: Maybe Word -> dataType
  nationalVarCharType :: Maybe Word -> dataType
  bitType :: Maybe Word -> dataType
  varBitType :: Maybe Word -> dataType
  numericType :: Maybe (Word, Maybe Word) -> dataType
  decimalType :: Maybe (Word, Maybe Word) -> dataType
  intType :: dataType
  smallIntType :: dataType
  floatType :: Maybe Word -> dataType
  doubleType :: dataType
  realType :: dataType

  dateType :: dataType
  timeType :: Maybe Word -> Bool {-^ With time zone -} -> dataType
  timestampType :: Maybe Word -> Bool {-^ With time zone -} -> dataType
  -- TODO interval type

class ( HasSqlValueSyntax (Sql92ExpressionValueSyntax expr) Int
      , IsSql92FieldNameSyntax (Sql92ExpressionFieldNameSyntax expr)
      , IsSql92QuantifierSyntax (Sql92ExpressionQuantifierSyntax expr) ) =>
    IsSql92ExpressionSyntax expr where
  type Sql92ExpressionQuantifierSyntax expr :: *
  type Sql92ExpressionValueSyntax expr :: *
  type Sql92ExpressionSelectSyntax expr :: *
  type Sql92ExpressionFieldNameSyntax expr :: *
  type Sql92ExpressionCastTargetSyntax expr :: *
  type Sql92ExpressionExtractFieldSyntax expr :: *

  valueE :: Sql92ExpressionValueSyntax expr -> expr
  rowE, coalesceE :: [ expr ] -> expr
  caseE :: [(expr, expr)]
        -> expr -> expr
  fieldE :: Sql92ExpressionFieldNameSyntax expr -> expr

  betweenE :: expr -> expr -> expr -> expr

  andE, orE, addE, subE, mulE, divE, likeE,
    modE, overlapsE, nullIfE, positionE
    :: expr
    -> expr
    -> expr

  eqE, neqE, ltE, gtE, leE, geE
    :: Maybe (Sql92ExpressionQuantifierSyntax expr)
    -> expr -> expr -> expr

  castE :: expr -> Sql92ExpressionCastTargetSyntax expr -> expr

  notE, negateE, isNullE, isNotNullE,
    isTrueE, isNotTrueE, isFalseE, isNotFalseE,
    isUnknownE, isNotUnknownE, charLengthE,
    octetLengthE, bitLengthE
    :: expr
    -> expr

  -- | Included so that we can easily write a Num instance, but not defined in SQL92.
  --   Implementations that do not support this, should use CASE .. WHEN ..
  absE :: expr -> expr

  extractE :: Sql92ExpressionExtractFieldSyntax expr -> expr -> expr

  existsE, uniqueE, subqueryE
    :: Sql92ExpressionSelectSyntax expr -> expr

  currentTimestampE :: expr

  defaultE :: expr

  inE :: expr -> [ expr ] -> expr

instance HasSqlValueSyntax syntax x => HasSqlValueSyntax syntax (SqlSerial x) where
  sqlValueSyntax (SqlSerial x) = sqlValueSyntax x

class IsSql92AggregationSetQuantifierSyntax (Sql92AggregationSetQuantifierSyntax expr) =>
  IsSql92AggregationExpressionSyntax expr where

  type Sql92AggregationSetQuantifierSyntax expr :: *

  countAllE :: expr
  countE, avgE, maxE, minE, sumE
    :: Maybe (Sql92AggregationSetQuantifierSyntax expr) -> expr -> expr

class IsSql92AggregationSetQuantifierSyntax q where
  setQuantifierDistinct, setQuantifierAll :: q

class IsSql92ExpressionSyntax (Sql92ProjectionExpressionSyntax proj) => IsSql92ProjectionSyntax proj where
  type Sql92ProjectionExpressionSyntax proj :: *

  projExprs :: [ (Sql92ProjectionExpressionSyntax proj, Maybe Text) ]
            -> proj

class IsSql92OrderingSyntax ord where
  type Sql92OrderingExpressionSyntax ord :: *
  ascOrdering, descOrdering
    :: Sql92OrderingExpressionSyntax ord -> ord

class IsSql92TableSourceSyntax tblSource where
  type Sql92TableSourceSelectSyntax tblSource :: *
  tableNamed :: Text -> tblSource
  tableFromSubSelect :: Sql92TableSourceSelectSyntax tblSource -> tblSource

class IsSql92GroupingSyntax grouping where
  type Sql92GroupingExpressionSyntax grouping :: *

  groupByExpressions :: [ Sql92GroupingExpressionSyntax grouping ] -> grouping

class ( IsSql92TableSourceSyntax (Sql92FromTableSourceSyntax from)
      , IsSql92ExpressionSyntax (Sql92FromExpressionSyntax from) ) =>
    IsSql92FromSyntax from where
  type Sql92FromTableSourceSyntax from :: *
  type Sql92FromExpressionSyntax from :: *

  fromTable :: Sql92FromTableSourceSyntax from
            -> Maybe Text
            -> from

  innerJoin, leftJoin, rightJoin
    :: from -> from
      -> Maybe (Sql92FromExpressionSyntax from)
      -> from

class IsSql92FromSyntax from =>
  IsSql92FromOuterJoinSyntax from where

  outerJoin :: from -> from -> Maybe (Sql92FromExpressionSyntax from) -> from
