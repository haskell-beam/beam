-- | This module implements an AST type for SQL92. It allows us to realize
--   the call structure of the builders defined in 'Database.Beam.Backend.SQL92'
module Database.Beam.Backend.SQL.AST where

import Prelude hiding (Ordering)

import Database.Beam.Backend.SQL.SQL92
import Database.Beam.Backend.SQL.SQL99

import Data.Text (Text)
import Data.Typeable

data Command
  = SelectCommand Select
  | InsertCommand Insert
  | UpdateCommand Update
  | DeleteCommand Delete
  deriving (Show, Eq)

instance IsSql92Syntax Command where
  type Sql92SelectSyntax Command = Select
  type Sql92UpdateSyntax Command = Update
  type Sql92InsertSyntax Command = Insert
  type Sql92DeleteSyntax Command = Delete

  selectCmd = SelectCommand
  insertCmd = InsertCommand
  updateCmd = UpdateCommand
  deleteCmd = DeleteCommand

data Select
    = Select
    { selectTable :: SelectTable
    , selectOrdering   :: [ Ordering ]
    , selectLimit, selectOffset :: Maybe Integer }
    deriving (Show, Eq)

instance IsSql92SelectSyntax Select where
  type Sql92SelectSelectTableSyntax Select = SelectTable
  type Sql92SelectOrderingSyntax Select = Ordering

  selectStmt = Select

data SelectTable
  = SelectTable
  { selectProjection :: Projection
  , selectFrom       :: Maybe From
  , selectWhere      :: Maybe Expression
  , selectGrouping   :: Maybe Grouping
  , selectHaving     :: Maybe Expression }
  | UnionTables Bool SelectTable SelectTable
  | IntersectTables Bool SelectTable SelectTable
  | ExceptTable Bool SelectTable SelectTable
  deriving (Show, Eq)

instance IsSql92SelectTableSyntax SelectTable where
  type Sql92SelectTableSelectSyntax SelectTable = Select
  type Sql92SelectTableExpressionSyntax SelectTable = Expression
  type Sql92SelectTableProjectionSyntax SelectTable = Projection
  type Sql92SelectTableFromSyntax SelectTable = From
  type Sql92SelectTableGroupingSyntax SelectTable = Grouping

  selectTableStmt = SelectTable
  unionTables = UnionTables
  intersectTables = IntersectTables
  exceptTable = ExceptTable

data Insert
  = Insert
  { insertTable :: Text
  , insertFields :: [ Text ]
  , insertValues :: InsertValues }
  deriving (Show, Eq)

instance IsSql92InsertSyntax Insert where
  type Sql92InsertValuesSyntax Insert = InsertValues

  insertStmt = Insert

data InsertValues
  = InsertValues
  { insertValuesExpressions :: [ [ Expression ] ] }
  | InsertSelect
  { insertSelectStmt :: Select }
  deriving (Show, Eq)

instance IsSql92InsertValuesSyntax InsertValues where
  type Sql92InsertValuesExpressionSyntax InsertValues = Expression
  type Sql92InsertValuesSelectSyntax InsertValues = Select

  insertSqlExpressions = InsertValues
  insertFromSql = InsertSelect

data Update
  = Update
  { updateTable :: Text
  , updateFields :: [ (FieldName, Expression) ]
  , updateWhere :: Maybe Expression }
  deriving (Show, Eq)

instance IsSql92UpdateSyntax Update where
  type Sql92UpdateFieldNameSyntax Update = FieldName
  type Sql92UpdateExpressionSyntax Update = Expression

  updateStmt = Update

data Delete
  = Delete
  { deleteTable :: Text
  , deleteWhere :: Maybe Expression }
  deriving (Show, Eq)

instance IsSql92DeleteSyntax Delete where
  type Sql92DeleteExpressionSyntax Delete = Expression

  deleteStmt = Delete

data FieldName
  = QualifiedField Text Text
  | UnqualifiedField Text
  deriving (Show, Eq)

instance IsSql92FieldNameSyntax FieldName where
  qualifiedField = QualifiedField
  unqualifiedField = UnqualifiedField

data ComparatorQuantifier
  = ComparatorQuantifierAny
  | ComparatorQuantifierAll
  deriving (Show, Eq)

instance IsSql92QuantifierSyntax ComparatorQuantifier where
  quantifyOverAll = ComparatorQuantifierAll
  quantifyOverAny = ComparatorQuantifierAny

data ExtractField
  = ExtractFieldTimeZoneHour
  | ExtractFieldTimeZoneMinute

  | ExtractFieldDateTimeYear
  | ExtractFieldDateTimeMonth
  | ExtractFieldDateTimeDay
  | ExtractFieldDateTimeHour
  | ExtractFieldDateTimeMinute
  | ExtractFieldDateTimeSecond
  deriving (Show, Eq)

data CastTarget
  = CastTargetDataType DataType
  | CastTargetDomainName Text
  deriving (Show, Eq)

data DataType
  = DataTypeChar Bool {- Varying -} (Maybe Int)
  | DataTypeNationalChar Bool (Maybe Int)
  | DataTypeBit Bool (Maybe Int)
  | DataTypeNumeric Int (Maybe Int)
  | DataTypeInteger
  | DataTypeSmallInt
  | DataTypeFloat (Maybe Int)
  | DataTypeReal
  | DataTypeDoublePrecision
  | DataTypeDate
  | DataTypeTime (Maybe Word) {- time fractional seconds precision -} Bool {- With time zone -}
  | DataTypeTimeStamp (Maybe Word) Bool
  | DataTypeInterval ExtractField
  | DataTypeIntervalFromTo ExtractField ExtractField
  deriving (Show, Eq)

data SetQuantifier
  = SetQuantifierAll | SetQuantifierDistinct
  deriving (Show, Eq)

instance IsSql92AggregationSetQuantifierSyntax SetQuantifier where
  setQuantifierDistinct = SetQuantifierDistinct
  setQuantifierAll = SetQuantifierAll

data Expression
  = ExpressionValue Value
  | ExpressionRow [ Expression ]

  | ExpressionIsNull Expression
  | ExpressionIsNotNull Expression
  | ExpressionIsTrue Expression
  | ExpressionIsNotTrue Expression
  | ExpressionIsFalse Expression
  | ExpressionIsNotFalse Expression
  | ExpressionIsUnknown Expression
  | ExpressionIsNotUnknown Expression

  | ExpressionCase [(Expression, Expression)] Expression
  | ExpressionCoalesce [Expression]
  | ExpressionNullIf Expression Expression

  | ExpressionFieldName FieldName

  | ExpressionBetween Expression Expression Expression
  | ExpressionBinOp Text Expression Expression
  | ExpressionCompOp Text (Maybe ComparatorQuantifier) Expression Expression
  | ExpressionUnOp Text Expression

  | ExpressionPosition Expression Expression
  | ExpressionCast Expression CastTarget
  | ExpressionExtract ExtractField Expression
  | ExpressionCharLength Expression
  | ExpressionOctetLength Expression
  | ExpressionBitLength Expression
  | ExpressionAbs Expression

  | ExpressionFunctionCall Expression [ Expression ]
  | ExpressionInstanceField Expression Text
  | ExpressionRefField Expression Text

  | ExpressionCountAll
  | ExpressionAgg Text (Maybe SetQuantifier) [ Expression ]

  | ExpressionSubquery Select
  | ExpressionUnique Select
  | ExpressionDistinct Select
  | ExpressionExists Select

  | ExpressionCurrentTimestamp
  deriving (Show, Eq)

instance IsSqlExpressionSyntaxStringType Expression Text

instance IsSql92ExpressionSyntax Expression where
  type Sql92ExpressionQuantifierSyntax Expression = ComparatorQuantifier
  type Sql92ExpressionValueSyntax Expression = Value
  type Sql92ExpressionSelectSyntax Expression = Select
  type Sql92ExpressionFieldNameSyntax Expression = FieldName
  type Sql92ExpressionCastTargetSyntax Expression = CastTarget
  type Sql92ExpressionExtractFieldSyntax Expression = ExtractField

  valueE = ExpressionValue
  rowE = ExpressionRow

  isNullE = ExpressionIsNull
  isNotNullE = ExpressionIsNotNull
  isTrueE = ExpressionIsTrue
  isNotTrueE = ExpressionIsNotTrue
  isFalseE = ExpressionIsFalse
  isNotFalseE = ExpressionIsNotFalse
  isUnknownE = ExpressionIsUnknown
  isNotUnknownE = ExpressionIsNotUnknown

  caseE = ExpressionCase
  coalesceE = ExpressionCoalesce
  nullIfE = ExpressionNullIf
  positionE = ExpressionPosition
  extractE = ExpressionExtract
  castE = ExpressionCast

  fieldE = ExpressionFieldName

  betweenE = ExpressionBetween
  andE = ExpressionBinOp "AND"
  orE = ExpressionBinOp "OR"

  eqE = ExpressionCompOp "=="
  neqE = ExpressionCompOp "<>"
  ltE = ExpressionCompOp "<"
  gtE = ExpressionCompOp ">"
  leE = ExpressionCompOp "<="
  geE = ExpressionCompOp ">="
  addE = ExpressionBinOp "+"
  subE = ExpressionBinOp "-"
  mulE = ExpressionBinOp "*"
  divE = ExpressionBinOp "/"
  modE = ExpressionBinOp "%"
  likeE = ExpressionBinOp "LIKE"
  overlapsE = ExpressionBinOp "OVERLAPS"

  notE = ExpressionUnOp "NOT"
  negateE = ExpressionUnOp "-"

  charLengthE = ExpressionCharLength
  octetLengthE = ExpressionOctetLength
  bitLengthE = ExpressionBitLength
  absE = ExpressionAbs

  subqueryE = ExpressionSubquery
  uniqueE = ExpressionUnique
  existsE = ExpressionExists

  currentTimestampE = ExpressionCurrentTimestamp

instance IsSql99ExpressionSyntax Expression where
  distinctE = ExpressionDistinct
  similarToE = ExpressionBinOp "SIMILAR TO"
  functionCallE = ExpressionFunctionCall
  instanceFieldE = ExpressionInstanceField
  refFieldE = ExpressionRefField

instance IsSql92AggregationExpressionSyntax Expression where
  type Sql92AggregationSetQuantifierSyntax Expression = SetQuantifier

  countAllE = ExpressionCountAll
  countE q = ExpressionAgg "COUNT" q . pure
  sumE q = ExpressionAgg "SUM" q . pure
  minE q = ExpressionAgg "MIN" q . pure
  maxE q = ExpressionAgg "MAX" q . pure
  avgE q = ExpressionAgg "AVG" q . pure

newtype Projection
  = ProjExprs [ (Expression, Maybe Text ) ]
  deriving (Show, Eq)

instance IsSql92ProjectionSyntax Projection where
  type Sql92ProjectionExpressionSyntax Projection = Expression

  projExprs = ProjExprs

data Ordering
  = OrderingAsc Expression
  | OrderingDesc Expression
  deriving (Show, Eq)

instance IsSql92OrderingSyntax Ordering where
  type Sql92OrderingExpressionSyntax Ordering = Expression

  ascOrdering = OrderingAsc
  descOrdering = OrderingDesc

newtype Grouping = Grouping [ Expression ] deriving (Show, Eq)

instance IsSql92GroupingSyntax Grouping where
  type Sql92GroupingExpressionSyntax Grouping = Expression

  groupByExpressions = Grouping

data TableSource
  = TableNamed Text
  | TableFromSubSelect Select
  deriving (Show, Eq)

instance IsSql92TableSourceSyntax TableSource where
  type Sql92TableSourceSelectSyntax TableSource = Select

  tableNamed = TableNamed
  tableFromSubSelect = TableFromSubSelect

data From
  = FromTable TableSource (Maybe Text)
  | InnerJoin From From (Maybe Expression)
  | LeftJoin From From (Maybe Expression)
  | RightJoin From From (Maybe Expression)
  | OuterJoin From From (Maybe Expression)
  deriving (Show, Eq)

instance IsSql92FromSyntax From where
  type Sql92FromTableSourceSyntax From = TableSource
  type Sql92FromExpressionSyntax From = Expression

  fromTable = FromTable
  innerJoin = InnerJoin
  leftJoin = LeftJoin
  rightJoin = RightJoin

data Value where
  Value :: (Show a, Eq a, Typeable a) => a -> Value

instance (Show a, Eq a, Typeable a) => HasSqlValueSyntax Value a where
  sqlValueSyntax = Value

instance Eq Value where
  Value a == Value b =
    case cast a of
      Just a' -> a' == b
      Nothing -> False
instance Show Value where
  showsPrec prec (Value a) =
    showParen (prec > app_prec) $
    ("Value " ++ ).
    showsPrec (app_prec + 1) a
    where app_prec = 10
