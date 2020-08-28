{-# LANGUAGE PolyKinds #-}

-- | Finally tagless encoding of SQL92 syntax
module Database.Beam.Backend.SQL.SQL92 where

import Database.Beam.Backend.SQL.Types
import Database.Beam.Backend.SQL.Row

import Data.Int
import Data.Kind (Type)
import Data.Tagged
import Data.Text (Text)
import Data.Time (LocalTime)
import Data.Typeable

-- * Finally tagless style

class HasSqlValueSyntax expr ty where
  sqlValueSyntax :: ty -> expr

autoSqlValueSyntax :: (HasSqlValueSyntax expr String, Show a) => a -> expr
autoSqlValueSyntax = sqlValueSyntax . show

type Sql92SelectExpressionSyntax select = Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)
type Sql92SelectProjectionSyntax select = Sql92SelectTableProjectionSyntax (Sql92SelectSelectTableSyntax select)
type Sql92SelectGroupingSyntax select = Sql92SelectTableGroupingSyntax (Sql92SelectSelectTableSyntax select)
type Sql92SelectFromSyntax select = Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select)
type Sql92InsertExpressionSyntax select = Sql92InsertValuesExpressionSyntax (Sql92InsertValuesSyntax select)
type Sql92TableNameSyntax select = Sql92TableSourceTableNameSyntax (Sql92FromTableSourceSyntax (Sql92SelectFromSyntax select))

type Sql92ValueSyntax cmdSyntax = Sql92ExpressionValueSyntax (Sql92ExpressionSyntax cmdSyntax)
type Sql92ExpressionSyntax cmdSyntax = Sql92SelectExpressionSyntax (Sql92SelectSyntax cmdSyntax)
type Sql92ExtractFieldSyntax cmdSyntax = Sql92ExpressionExtractFieldSyntax (Sql92ExpressionSyntax cmdSyntax)
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
    Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select)
  , Sql92TableSourceExpressionSyntax (Sql92FromTableSourceSyntax (Sql92SelectTableFromSyntax (Sql92SelectSelectTableSyntax select))) ~
    Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax select))
type Sql92SanityCheck cmd =
  ( Sql92SelectSanityCheck (Sql92SelectSyntax cmd)
  , Sql92ExpressionValueSyntax (Sql92InsertValuesExpressionSyntax (Sql92InsertValuesSyntax (Sql92InsertSyntax cmd))) ~ Sql92ValueSyntax cmd
  , Sql92ExpressionValueSyntax (Sql92UpdateExpressionSyntax (Sql92UpdateSyntax cmd)) ~ Sql92ValueSyntax cmd
  , Sql92ExpressionValueSyntax (Sql92DeleteExpressionSyntax (Sql92DeleteSyntax cmd)) ~ Sql92ValueSyntax cmd

  , Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax cmd)) ~
    Sql92InsertValuesExpressionSyntax (Sql92InsertValuesSyntax (Sql92InsertSyntax cmd))

  , Sql92SelectTableExpressionSyntax (Sql92SelectSelectTableSyntax (Sql92SelectSyntax cmd)) ~
    Sql92UpdateExpressionSyntax (Sql92UpdateSyntax cmd)

  , Sql92DeleteExpressionSyntax (Sql92DeleteSyntax cmd) ~
    Sql92UpdateExpressionSyntax (Sql92UpdateSyntax cmd)

  , Sql92ExpressionSelectSyntax (Sql92InsertExpressionSyntax (Sql92InsertSyntax cmd)) ~
    Sql92SelectSyntax cmd

  , Sql92InsertValuesSelectSyntax (Sql92InsertValuesSyntax (Sql92InsertSyntax cmd)) ~
    Sql92SelectSyntax cmd

  , Sql92UpdateFieldNameSyntax (Sql92UpdateSyntax cmd) ~
    Sql92ExpressionFieldNameSyntax (Sql92InsertValuesExpressionSyntax (Sql92InsertValuesSyntax (Sql92InsertSyntax cmd)))
  )

type Sql92ReasonableMarshaller be =
   ( FromBackendRow be SqlNull
   , FromBackendRow be Text, FromBackendRow be Bool
   , FromBackendRow be Char
   , FromBackendRow be Int16, FromBackendRow be Int32, FromBackendRow be Int64
   , FromBackendRow be LocalTime )

-- | Type classes for syntaxes which can be displayed
class Sql92DisplaySyntax syntax where

  -- | Render the syntax as a 'String', representing the SQL expression it
  -- stands for
  displaySyntax :: syntax -> String

class ( IsSql92SelectSyntax (Sql92SelectSyntax cmd)
      , IsSql92InsertSyntax (Sql92InsertSyntax cmd)
      , IsSql92UpdateSyntax (Sql92UpdateSyntax cmd)
      , IsSql92DeleteSyntax (Sql92DeleteSyntax cmd) ) =>
  IsSql92Syntax cmd where
  type Sql92SelectSyntax cmd :: Type
  type Sql92InsertSyntax cmd :: Type
  type Sql92UpdateSyntax cmd :: Type
  type Sql92DeleteSyntax cmd :: Type

  selectCmd :: Sql92SelectSyntax cmd -> cmd
  insertCmd :: Sql92InsertSyntax cmd -> cmd
  updateCmd :: Sql92UpdateSyntax cmd -> cmd
  deleteCmd :: Sql92DeleteSyntax cmd -> cmd

class ( IsSql92SelectTableSyntax (Sql92SelectSelectTableSyntax select)
      , IsSql92OrderingSyntax (Sql92SelectOrderingSyntax select) ) =>
    IsSql92SelectSyntax select where
    type Sql92SelectSelectTableSyntax select :: Type
    type Sql92SelectOrderingSyntax select :: Type

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
  type Sql92SelectTableSelectSyntax select :: Type
  type Sql92SelectTableExpressionSyntax select :: Type
  type Sql92SelectTableProjectionSyntax select :: Type
  type Sql92SelectTableFromSyntax select :: Type
  type Sql92SelectTableGroupingSyntax select :: Type
  type Sql92SelectTableSetQuantifierSyntax select :: Type

  selectTableStmt :: Maybe (Sql92SelectTableSetQuantifierSyntax select)
                  -> Sql92SelectTableProjectionSyntax select
                  -> Maybe (Sql92SelectTableFromSyntax select)
                  -> Maybe (Sql92SelectTableExpressionSyntax select)   {-^ Where clause -}
                  -> Maybe (Sql92SelectTableGroupingSyntax select)
                  -> Maybe (Sql92SelectTableExpressionSyntax select) {-^ having clause -}
                  -> select

  unionTables, intersectTables, exceptTable ::
    Bool -> select -> select -> select

class ( IsSql92InsertValuesSyntax (Sql92InsertValuesSyntax insert)
      , IsSql92TableNameSyntax (Sql92InsertTableNameSyntax insert) ) =>
  IsSql92InsertSyntax insert where

  type Sql92InsertValuesSyntax insert :: Type
  type Sql92InsertTableNameSyntax insert :: Type

  insertStmt :: Sql92InsertTableNameSyntax insert
             -> [ Text ]
             -- ^ Fields
             -> Sql92InsertValuesSyntax insert
             -> insert

class IsSql92ExpressionSyntax (Sql92InsertValuesExpressionSyntax insertValues) =>
  IsSql92InsertValuesSyntax insertValues where
  type Sql92InsertValuesExpressionSyntax insertValues :: Type
  type Sql92InsertValuesSelectSyntax insertValues :: Type

  insertSqlExpressions :: [ [ Sql92InsertValuesExpressionSyntax insertValues ] ]
                       -> insertValues
  insertFromSql :: Sql92InsertValuesSelectSyntax insertValues
                -> insertValues

class ( IsSql92ExpressionSyntax (Sql92UpdateExpressionSyntax update)
      , IsSql92FieldNameSyntax (Sql92UpdateFieldNameSyntax update)
      , IsSql92TableNameSyntax (Sql92UpdateTableNameSyntax update) ) =>
      IsSql92UpdateSyntax update where

  type Sql92UpdateTableNameSyntax  update :: Type
  type Sql92UpdateFieldNameSyntax  update :: Type
  type Sql92UpdateExpressionSyntax update :: Type

  updateStmt :: Sql92UpdateTableNameSyntax update
             -> [(Sql92UpdateFieldNameSyntax update, Sql92UpdateExpressionSyntax update)]
             -> Maybe (Sql92UpdateExpressionSyntax update) {-^ WHERE -}
             -> update

class ( IsSql92TableNameSyntax (Sql92DeleteTableNameSyntax delete)
      , IsSql92ExpressionSyntax (Sql92DeleteExpressionSyntax delete) ) =>
  IsSql92DeleteSyntax delete where
  type Sql92DeleteTableNameSyntax  delete :: Type
  type Sql92DeleteExpressionSyntax delete :: Type

  deleteStmt :: Sql92DeleteTableNameSyntax delete -> Maybe Text
             -> Maybe (Sql92DeleteExpressionSyntax delete)
             -> delete

  -- | Whether or not the @DELETE@ command supports aliases
  deleteSupportsAlias :: Proxy delete -> Bool
  deleteSupportsAlias _ = False -- Delete aliases are a non-standard feature

class IsSql92FieldNameSyntax fn where
  qualifiedField :: Text -> Text -> fn
  unqualifiedField :: Text -> fn

class IsSql92QuantifierSyntax quantifier where
  quantifyOverAll, quantifyOverAny :: quantifier

class IsSql92ExtractFieldSyntax extractField where
  secondsField :: extractField
  minutesField :: extractField
  hourField :: extractField
  dayField :: extractField
  monthField :: extractField
  yearField :: extractField

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

class ( HasSqlValueSyntax (Sql92ExpressionValueSyntax expr) Int32
      , HasSqlValueSyntax (Sql92ExpressionValueSyntax expr) Bool
      , IsSql92FieldNameSyntax (Sql92ExpressionFieldNameSyntax expr)
      , IsSql92QuantifierSyntax (Sql92ExpressionQuantifierSyntax expr)
      , IsSql92DataTypeSyntax (Sql92ExpressionCastTargetSyntax expr)
      , IsSql92ExtractFieldSyntax (Sql92ExpressionExtractFieldSyntax expr)
      , Typeable expr ) =>
    IsSql92ExpressionSyntax expr where
  type Sql92ExpressionQuantifierSyntax expr :: Type
  type Sql92ExpressionValueSyntax      expr :: Type
  type Sql92ExpressionSelectSyntax     expr :: Type
  type Sql92ExpressionFieldNameSyntax  expr :: Type
  type Sql92ExpressionCastTargetSyntax expr :: Type
  type Sql92ExpressionExtractFieldSyntax expr :: Type

  valueE :: Sql92ExpressionValueSyntax expr -> expr

  rowE, quantifierListE, coalesceE :: [ expr ] -> expr
  quantifierListE = rowE

  caseE :: [(expr, expr)]
        -> expr -> expr
  fieldE :: Sql92ExpressionFieldNameSyntax expr -> expr

  betweenE :: expr -> expr -> expr -> expr
  betweenE a lower upper =
    (gtE Nothing a lower) `andE` (ltE Nothing a upper)

  andE, orE, addE, subE, mulE, divE, likeE,
    modE, overlapsE, nullIfE, positionE
    :: expr
    -> expr
    -> expr

  eqE, neqE, ltE, gtE, leE, geE
    :: Maybe (Sql92ExpressionQuantifierSyntax expr)
    -> expr -> expr -> expr

  -- | Compare the first and second argument for nullable equality, if
  -- they are both not null, return the result of the third expression
  --
  -- Some backends, like @beam-postgres@ totally ignore the third
  -- result, because all equality there is sensible.
  eqMaybeE, neqMaybeE :: expr -> expr -> expr -> expr

  eqMaybeE a b e =
    let aIsNull = isNullE a
        bIsNull = isNullE b
    in caseE [ ( aIsNull `andE` bIsNull, valueE (sqlValueSyntax True) )
             , ( aIsNull `orE` bIsNull, valueE (sqlValueSyntax False) ) ]
             e


  neqMaybeE a b e =
    let aIsNull = isNullE a
        bIsNull = isNullE b
    in caseE [ ( aIsNull `andE` bIsNull, valueE (sqlValueSyntax False) )
             , ( aIsNull `orE` bIsNull, valueE (sqlValueSyntax True) ) ]
             e

  castE :: expr -> Sql92ExpressionCastTargetSyntax expr -> expr

  notE, negateE, isNullE, isNotNullE,
    isTrueE, isNotTrueE, isFalseE, isNotFalseE,
    isUnknownE, isNotUnknownE, charLengthE,
    octetLengthE, bitLengthE,
    lowerE, upperE,
    trimE
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

  type Sql92AggregationSetQuantifierSyntax expr :: Type

  countAllE :: expr
  countE, avgE, maxE, minE, sumE
    :: Maybe (Sql92AggregationSetQuantifierSyntax expr) -> expr -> expr

class IsSql92AggregationSetQuantifierSyntax q where
  setQuantifierDistinct, setQuantifierAll :: q

class IsSql92ExpressionSyntax (Sql92ProjectionExpressionSyntax proj) => IsSql92ProjectionSyntax proj where
  type Sql92ProjectionExpressionSyntax proj :: Type

  projExprs :: [ (Sql92ProjectionExpressionSyntax proj, Maybe Text) ]
            -> proj

class IsSql92OrderingSyntax ord where
  type Sql92OrderingExpressionSyntax ord :: Type
  ascOrdering, descOrdering
    :: Sql92OrderingExpressionSyntax ord -> ord

class IsSql92TableNameSyntax tblName where
  tableName :: Maybe Text {-^ Schema -}
            -> Text {-^ Table name -}
            -> tblName

class IsSql92TableNameSyntax (Sql92TableSourceTableNameSyntax tblSource) =>
  IsSql92TableSourceSyntax tblSource where

  type Sql92TableSourceSelectSyntax tblSource :: Type
  type Sql92TableSourceExpressionSyntax tblSource :: Type
  type Sql92TableSourceTableNameSyntax tblSource :: Type

  tableNamed :: Sql92TableSourceTableNameSyntax tblSource
             -> tblSource
  tableFromSubSelect :: Sql92TableSourceSelectSyntax tblSource -> tblSource
  tableFromValues :: [ [ Sql92TableSourceExpressionSyntax tblSource ] ] -> tblSource

class IsSql92GroupingSyntax grouping where
  type Sql92GroupingExpressionSyntax grouping :: Type

  groupByExpressions :: [ Sql92GroupingExpressionSyntax grouping ] -> grouping

class ( IsSql92TableSourceSyntax (Sql92FromTableSourceSyntax from)
      , IsSql92ExpressionSyntax (Sql92FromExpressionSyntax from) ) =>
    IsSql92FromSyntax from where
  type Sql92FromTableSourceSyntax from :: Type
  type Sql92FromExpressionSyntax from :: Type

  fromTable :: Sql92FromTableSourceSyntax from
            -> Maybe (Text, Maybe [Text])
            -> from

  innerJoin, leftJoin, rightJoin
    :: from -> from
      -> Maybe (Sql92FromExpressionSyntax from)
      -> from

class IsSql92FromSyntax from =>
  IsSql92FromOuterJoinSyntax from where

  outerJoin :: from -> from -> Maybe (Sql92FromExpressionSyntax from) -> from

-- Tagged

instance HasSqlValueSyntax vs t => HasSqlValueSyntax vs (Tagged tag t) where
  sqlValueSyntax = sqlValueSyntax . untag

