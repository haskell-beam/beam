{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.Beam.Haskell.Syntax where

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Migrate.SQL.SQL92
import           Database.Beam.Backend.SQL.Builder

import           Data.Char (toLower, toUpper)
import           Data.Hashable
import           Data.Monoid
import qualified Data.Set as S
import qualified Data.Map as M
import           Data.String
import qualified Data.Text as T

import qualified Language.Haskell.Exts as Hs

import           Text.PrettyPrint (render)

newtype HsBackendConstraint = HsBackendConstraint { buildHsBackendConstraint :: Hs.Type () -> Hs.Asst () }
newtype HsDbField = HsDbField { buildHsDbField :: Hs.Type () -> Hs.Type () }

newtype HsEntityName = HsEntityName { getHsEntityName :: String } deriving (Show, Eq, Ord, IsString)

data HsImport = HsImportAll | HsImportSome (S.Set (Hs.ImportSpec ()))
  deriving (Show, Eq, Generic)
instance Hashable HsImport
instance Monoid HsImport where
  mempty = HsImportSome mempty
  mappend HsImportAll _ = HsImportAll
  mappend _ HsImportAll = HsImportAll
  mappend (HsImportSome a) (HsImportSome b) =
    HsImportSome (a <> b)

importSome :: T.Text -> [ Hs.ImportSpec () ] -> HsImports
importSome modNm names = HsImports (M.singleton (Hs.ModuleName () (T.unpack modNm))
                                                (HsImportSome (S.fromList names)))

importTyNamed :: T.Text -> Hs.ImportSpec ()
importTyNamed nm = Hs.IAbs () (Hs.TypeNamespace ()) (Hs.Ident () (T.unpack nm))

importVarNamed :: T.Text -> Hs.ImportSpec ()
importVarNamed nm = Hs.IVar () (Hs.Ident () (T.unpack nm))

newtype HsImports = HsImports (M.Map (Hs.ModuleName ()) HsImport)
  deriving (Show, Eq)
instance Hashable HsImports where
  hashWithSalt s (HsImports a) = hashWithSalt s (M.assocs a)
instance Monoid HsImports where
  mempty = HsImports mempty
  mappend (HsImports a) (HsImports b) =
    HsImports (M.unionWith mappend a b)

data HsDataType
  = HsDataType
  { hsDataTypeMigration :: HsExpr
  , hsDataTypeType :: HsType
  } deriving (Eq, Show, Generic)
instance Hashable HsDataType

data HsType
  = HsType
  { hsTypeSyntax  :: Hs.Type ()
  , hsTypeImports :: HsImports
  } deriving (Show, Eq, Generic)
instance Hashable HsType

data HsExpr
  = HsExpr
  { hsExprSyntax  :: Hs.Exp ()
  , hsExprImports :: HsImports
  , hsExprConstraints :: [ Hs.Asst () ]
  , hsExprTypeVariables :: S.Set (Hs.Name ())
  } deriving (Show, Eq, Generic)
instance Hashable HsExpr

data HsColumnSchema
  = HsColumnSchema
  { mkHsColumnSchema :: T.Text -> HsExpr
  , hsColumnSchemaType :: HsType
  }
instance Show HsColumnSchema where
  show (HsColumnSchema mk _) = show (mk "fieldNm")
instance Eq HsColumnSchema where
  HsColumnSchema a aTy == HsColumnSchema b bTy = a "fieldNm" == b "fieldNm" && aTy == bTy
instance Hashable HsColumnSchema where
  hashWithSalt s (HsColumnSchema mk ty) = hashWithSalt s (mk "fieldNm", ty)

data HsDecl
  = HsDecl
  { hsDeclSyntax  :: Hs.Decl ()
  , hsDeclImports :: HsImports
  , hsDeclExports :: [ Hs.ExportSpec () ]
  }

data HsAction
  = HsAction
  { hsSyntaxMigration :: [ (Maybe (Hs.Pat ()), HsExpr) ]
  , hsSyntaxEntities  :: [ HsEntity ]
  }

instance Monoid HsAction where
  mempty = HsAction [] []
  mappend (HsAction ma ea) (HsAction mb eb) =
    HsAction (ma <> mb) (ea <> eb)

data HsBeamBackend
  = HsBeamBackendSingle HsType HsExpr
  | HsBeamBackendConstrained [ HsBackendConstraint ]
  | HsBeamBackendNone

instance Monoid HsBeamBackend where
  mempty = HsBeamBackendConstrained []
  mappend (HsBeamBackendSingle aTy aExp) (HsBeamBackendSingle bTy bExp)
    | aTy == bTy && aExp == bExp = HsBeamBackendSingle aTy aExp
    | otherwise = HsBeamBackendNone
  mappend a@HsBeamBackendSingle {} _ = a
  mappend _ b@HsBeamBackendSingle {} = b
  mappend HsBeamBackendNone _ = HsBeamBackendNone
  mappend _ HsBeamBackendNone = HsBeamBackendNone
  mappend (HsBeamBackendConstrained a) (HsBeamBackendConstrained b) =
    HsBeamBackendConstrained (a <> b)

data HsEntity
    = HsEntity
    { hsEntityBackend :: HsBeamBackend
    , hsEntityName    :: HsEntityName

    , hsEntityDecls   :: [ HsDecl ]
    , hsEntityDbDecl  :: HsDbField

    , hsEntityExp     :: HsExpr }

data HsModule
  = HsModule
  { hsModuleName :: String
  , hsModuleEntities :: [ HsEntity ]
  , hsModuleMigration :: [ (Maybe (Hs.Pat ()), HsExpr) ]
  }

hsActionsToModule :: String -> [ HsAction ] -> HsModule
hsActionsToModule modNm actions =
  let HsAction ms es = mconcat actions
  in HsModule modNm es ms

unqual :: String -> Hs.QName ()
unqual = Hs.UnQual () . Hs.Ident ()

entityDbFieldName :: HsEntity -> String
entityDbFieldName entity = "_" ++ getHsEntityName (hsEntityName entity)

databaseTypeDecl :: [ HsEntity ] -> Hs.Decl ()
databaseTypeDecl entities =
  Hs.DataDecl () (Hs.DataType ()) Nothing
              declHead [ conDecl ]
              (Just deriving_)
  where
    declHead = Hs.DHApp () (Hs.DHead () (Hs.Ident () "Db"))
                           (Hs.UnkindedVar () (Hs.Ident () "entity"))
    conDecl = Hs.QualConDecl () Nothing Nothing
                (Hs.RecDecl () (Hs.Ident () "Db") (mkField <$> entities))
    deriving_ = Hs.Deriving () [ Hs.IRule () Nothing Nothing $
                                 Hs.IHCon () $ Hs.UnQual () $
                                 Hs.Ident () "Generic" ]

    mkField entity = Hs.FieldDecl () [ Hs.Ident () (entityDbFieldName entity) ]
                                     (buildHsDbField (hsEntityDbDecl entity) $
                                      Hs.TyVar () (Hs.Ident () "entity"))

migrationTypeDecl :: HsBeamBackend -> Hs.Decl ()
migrationTypeDecl _ =
  Hs.TypeSig () [Hs.Ident () "migration"] migrationType
  where
    syntaxVar = tyVarNamed "syntax"

    migrationType = tyApp (tyConNamed "Migration")
                          [ syntaxVar
                          , tyConNamed "Db" ]

migrationDecl :: HsBeamBackend -> [ (Maybe (Hs.Pat ()), HsExpr) ] -> [HsEntity] -> Hs.Decl ()
migrationDecl _ migrations entities =
  Hs.FunBind () [ Hs.Match () (Hs.Ident () "migration") [] (Hs.UnGuardedRhs () body) Nothing ]
  where
    body = Hs.Do () (map (\(pat, expr) ->
                            let expr' = hsExprSyntax expr
                            in case pat of
                              Nothing -> Hs.Qualifier () expr'
                              Just pat' -> Hs.Generator () pat' expr') migrations ++
                     [Hs.Qualifier () (hsExprSyntax finalReturn)])

    finalReturn = hsApp (hsVar "pure")
                        [ hsRecCon "Db" (map (\e -> (fromString (entityDbFieldName e), hsEntityExp e)) entities) ]

renderHsSchema :: HsModule -> Either String String
renderHsSchema (HsModule modNm entities migrations) =
  let hsMod = Hs.Module () (Just modHead) modPragmas imports decls

      modHead = Hs.ModuleHead () (Hs.ModuleName () modNm) Nothing (Just modExports)
      modExports = Hs.ExportSpecList () (commonExports ++ foldMap (foldMap hsDeclExports . hsEntityDecls) entities)
      commonExports = [ Hs.EVar () (unqual "db")
                      , Hs.EVar () (unqual "migratableDb") ]

      modPragmas = [ Hs.LanguagePragma () [ Hs.Ident () "StandaloneDeriving"
                                          , Hs.Ident () "GADTs"
                                          , Hs.Ident () "ScopedTypeVariables"
                                          , Hs.Ident () "FlexibleContexts"
                                          , Hs.Ident () "FlexibleInstances"
                                          , Hs.Ident () "DeriveGeneric" ] ]

      HsImports importedModules = foldMap (\e -> foldMap hsDeclImports (hsEntityDecls e) <>
                                                 hsExprImports (hsEntityExp e)) entities <>
                                  foldMap (hsExprImports . snd) migrations
      imports = commonImports <>
                map (\(modName, spec) ->
                       case spec of
                         HsImportAll -> Hs.ImportDecl () modName True False False Nothing Nothing Nothing
                         HsImportSome nms ->
                           let importList = Hs.ImportSpecList () False (S.toList nms)
                           in Hs.ImportDecl () modName True False False Nothing Nothing (Just importList)
                    )
                    (M.assocs importedModules)

      commonImports = [ Hs.ImportDecl () (Hs.ModuleName () "Database.Beam") False False False Nothing Nothing Nothing
                      , Hs.ImportDecl () (Hs.ModuleName () "Control.Applicative") False False False Nothing Nothing Nothing ]

      backend = foldMap hsEntityBackend entities

      decls = foldMap (map hsDeclSyntax . hsEntityDecls) entities ++
              [ databaseTypeDecl entities
              , migrationTypeDecl backend
              , migrationDecl backend migrations entities ]

  in Right (render (Hs.prettyPrim hsMod))

-- * DDL Syntax definitions

data HsNone = HsNone deriving (Show, Eq, Ord, Generic)
instance Hashable HsNone

instance Monoid HsNone where
  mempty = HsNone
  mappend _ _ = HsNone

instance IsSql92DdlCommandSyntax HsAction where
  type Sql92DdlCommandCreateTableSyntax HsAction = HsAction
  type Sql92DdlCommandAlterTableSyntax HsAction = HsAction
  type Sql92DdlCommandDropTableSyntax HsAction = HsAction

  createTableCmd = id
  dropTableCmd = id
  alterTableCmd = id

instance IsSql92AlterTableSyntax HsAction where
  type Sql92AlterTableAlterTableActionSyntax HsAction = HsNone

  alterTableSyntax _ _ = error "alterTableSyntax"

instance IsSql92AlterTableActionSyntax HsNone where
  type Sql92AlterTableColumnSchemaSyntax HsNone = HsColumnSchema
  type Sql92AlterTableAlterColumnActionSyntax HsNone = HsNone

  alterColumnSyntax _ _ = HsNone
  addColumnSyntax _ _ = HsNone
  dropColumnSyntax _ = HsNone

instance IsSql92AlterColumnActionSyntax HsNone where
  setNullSyntax = HsNone
  setNotNullSyntax = HsNone

instance IsSql92DropTableSyntax HsAction where
  dropTableSyntax nm = HsAction [ (Nothing, dropTable) ] []
    where
      dropTable = hsApp (hsVar "dropTable") [ hsVar ("_" <> nm) ]

instance IsSql92CreateTableSyntax HsAction where
  type Sql92CreateTableOptionsSyntax HsAction = HsNone
  type Sql92CreateTableTableConstraintSyntax HsAction = HsNone
  type Sql92CreateTableColumnSchemaSyntax HsAction = HsColumnSchema

  createTableSyntax _ nm fields _ =
    HsAction [ ( Just (Hs.PVar () (Hs.Ident () varName))
               , migration ) ]
             [ entity ]
    where
      (varName, tyName, tyConName) =
        case T.unpack nm of
          [] -> error "No name for table"
          x:xs -> let tyName' = toUpper x:xs
                  in ( toLower x:xs, tyName' ++ "T", tyName')
      mkHsFieldName fieldNm = "_" ++ varName ++
                              case T.unpack fieldNm of
                                [] -> error "empty field name"
                                (x:xs) -> toUpper x:xs

      migration =
        hsApApp (hsApp (hsVar "createTable")
                       [hsStr nm])
                (map (\(fieldNm, ty) -> mkHsColumnSchema ty fieldNm) fields)
      entity = HsEntity
             { hsEntityBackend = HsBeamBackendConstrained []
             , hsEntityName    = HsEntityName varName
             , hsEntityDecls   = [ HsDecl tblDecl imports [] ]
             , hsEntityDbDecl  = HsDbField (\f -> tyApp f [ tyApp (tyConNamed "TableEntity") [tyConNamed tyName] ])
             , hsEntityExp     = hsVar (fromString varName)
             }

      imports = foldMap (\(_, ty) -> hsTypeImports (hsColumnSchemaType ty)) fields

      tblDecl = Hs.DataDecl () (Hs.DataType ()) Nothing
                  tblDeclHead [ tblConDecl ] (Just deriving_)
      tblDeclHead = Hs.DHApp () (Hs.DHead () (Hs.Ident () tyName))
                                (Hs.UnkindedVar () (Hs.Ident () "f"))
      tblConDecl = Hs.QualConDecl () Nothing Nothing (Hs.RecDecl () (Hs.Ident () tyConName) tyConFields)
      tyConFields = map (\(fieldNm, ty) ->
                           Hs.FieldDecl () [ Hs.Ident () (mkHsFieldName fieldNm) ]
                           (hsTypeSyntax (hsColumnSchemaType ty))) fields
      deriving_ = Hs.Deriving () [ inst "Generic" ]
      inst = Hs.IRule () Nothing Nothing . Hs.IHCon () . Hs.UnQual () . Hs.Ident ()

instance IsSql92ColumnSchemaSyntax HsColumnSchema where
  type Sql92ColumnSchemaColumnConstraintDefinitionSyntax HsColumnSchema = HsExpr
  type Sql92ColumnSchemaColumnTypeSyntax HsColumnSchema = HsDataType
  type Sql92ColumnSchemaExpressionSyntax HsColumnSchema = HsExpr

  columnSchemaSyntax dataType _ cs _ = HsColumnSchema (\nm -> fieldExpr nm)
                                                      (hsDataTypeType dataType)
    where
      fieldExpr nm = hsApp (hsVar "field")
                           ([ hsStr nm
                            , hsDataTypeMigration dataType ] ++
                            cs)

instance IsSql92TableConstraintSyntax HsNone where
  primaryKeyConstraintSyntax _ = HsNone
instance IsSql92ColumnConstraintDefinitionSyntax HsExpr where
  type Sql92ColumnConstraintDefinitionAttributesSyntax HsExpr = HsNone
  type Sql92ColumnConstraintDefinitionConstraintSyntax HsExpr = HsExpr

  constraintDefinitionSyntax Nothing expr Nothing = expr
  constraintDefinitionSyntax _ _ _ = error "constraintDefinitionSyntax{HsExpr}"

instance IsSql92MatchTypeSyntax HsNone where
  fullMatchSyntax = HsNone
  partialMatchSyntax = HsNone
instance IsSql92ReferentialActionSyntax HsNone where
  referentialActionCascadeSyntax = HsNone
  referentialActionNoActionSyntax = HsNone
  referentialActionSetDefaultSyntax = HsNone
  referentialActionSetNullSyntax = HsNone

instance IsSql92ExpressionSyntax HsExpr where
  type Sql92ExpressionFieldNameSyntax HsExpr = HsExpr
  type Sql92ExpressionSelectSyntax HsExpr = SqlSyntaxBuilder
  type Sql92ExpressionValueSyntax HsExpr = HsExpr
  type Sql92ExpressionQuantifierSyntax HsExpr = HsExpr
  type Sql92ExpressionExtractFieldSyntax HsExpr = HsExpr
  type Sql92ExpressionCastTargetSyntax HsExpr = HsDataType

  valueE = hsApp (hsVar "valueE") . pure
  rowE = error "rowE"

  currentTimestampE = hsVar "currentTimestampE"
  defaultE = hsVar "defaultE"

  coalesceE = hsApp (hsVar "coalesceE")
  fieldE = hsApp (hsVar "fieldE") . pure

  betweenE a b c = hsApp (hsVar "betweenE") [a, b, c]

  andE a b = hsApp (hsVar "andE") [a, b]
  orE a b = hsApp (hsVar "orE") [a, b]
  addE a b = hsApp (hsVar "addE") [a, b]
  subE a b = hsApp (hsVar "subE") [a, b]
  mulE a b = hsApp (hsVar "mulE") [a, b]
  divE a b = hsApp (hsVar "divE") [a, b]
  modE a b = hsApp (hsVar "modE") [a, b]
  likeE a b = hsApp (hsVar "likeE") [a, b]
  overlapsE a b = hsApp (hsVar "overlapsE") [a, b]
  positionE a b = hsApp (hsVar "positionE") [a, b]

  notE = hsApp (hsVar "notE") . pure
  negateE = hsApp (hsVar "negateE") . pure
  absE = hsApp (hsVar "absE") . pure
  charLengthE = hsApp (hsVar "charLengthE") . pure
  octetLengthE = hsApp (hsVar "octetLengthE") . pure
  bitLengthE = hsApp (hsVar "bitLengthE") . pure

  existsE = error "existsE"
  uniqueE = error "uniqueE"
  subqueryE = error "subqueryE"

  caseE = error "caseE"
  nullIfE a b = hsApp (hsVar "nullIfE") [a, b]

  castE = error "castE"
  extractE = error "extractE"

  isNullE = hsApp (hsVar "isNullE") . pure
  isNotNullE = hsApp (hsVar "isNotNullE") . pure
  isTrueE = hsApp (hsVar "isTrueE") . pure
  isFalseE = hsApp (hsVar "isFalseE") . pure
  isNotTrueE = hsApp (hsVar "isNotTrueE") . pure
  isNotFalseE = hsApp (hsVar "isNotFalseE") . pure
  isUnknownE = hsApp (hsVar "isUnknownE") . pure
  isNotUnknownE = hsApp (hsVar "isNotUnknownE") . pure

  eqE q a b = hsApp (hsVar "eqE")   [hsMaybe q, a, b]
  neqE q a b = hsApp (hsVar "neqE") [hsMaybe q, a, b]
  gtE q a b = hsApp (hsVar "gtE")   [hsMaybe q, a, b]
  ltE q a b = hsApp (hsVar "ltE")   [hsMaybe q, a, b]
  geE q a b = hsApp (hsVar "geE")   [hsMaybe q, a, b]
  leE q a b = hsApp (hsVar "leE")   [hsMaybe q, a, b]

instance IsSql92QuantifierSyntax HsExpr where
  quantifyOverAll = hsVar "quantifyOverAll"
  quantifyOverAny = hsVar "quantifyOverAny"

instance IsSql92ColumnConstraintSyntax HsExpr where
  type Sql92ColumnConstraintExpressionSyntax HsExpr = HsExpr
  type Sql92ColumnConstraintMatchTypeSyntax HsExpr = HsNone
  type Sql92ColumnConstraintReferentialActionSyntax HsExpr = HsNone

  notNullConstraintSyntax = hsVar "notNull"
  uniqueColumnConstraintSyntax = hsVar "unique"
  checkColumnConstraintSyntax = error "checkColumnConstraintSyntax"
  primaryKeyColumnConstraintSyntax = error "primaryKeyColumnConstraintSyntax"
  referencesConstraintSyntax = error "referencesConstraintSyntax"

instance IsSql92ConstraintAttributesSyntax HsNone where
  initiallyDeferredAttributeSyntax = HsNone
  initiallyImmediateAttributeSyntax = HsNone
  notDeferrableAttributeSyntax = HsNone
  deferrableAttributeSyntax = HsNone

instance HasSqlValueSyntax HsExpr Int where
  sqlValueSyntax = hsInt

instance IsSql92FieldNameSyntax HsExpr where
  qualifiedField tbl nm = hsApp (hsVar "qualifiedField") [ hsStr tbl, hsStr nm ]
  unqualifiedField nm = hsApp (hsVar "unqualifiedField") [ hsStr nm ]

instance IsSql92DataTypeSyntax HsDataType where
  intType = HsDataType (hsVarFrom "int" "Database.Beam.Migrate") (HsType (tyConNamed "Int") mempty)
  smallIntType = HsDataType (hsVarFrom "smallint" "Database.Beam.Migrate") (HsType (tyConNamed "Int16") (importSome "Data.Int" [ importTyNamed "Int16" ]))
  doubleType = HsDataType (hsVarFrom "double" "Database.Beam.Migrate") (HsType (tyConNamed "Double") mempty)

  floatType width = HsDataType (hsApp (hsVarFrom "float" "Database.Beam.Migrate")
                                      [ hsMaybe (hsInt <$> width) ])
                               (HsType (tyConNamed "Scientific") (importSome "Data.Scientific" [ importTyNamed "Scientific" ]))

  realType = HsDataType (hsVarFrom "real" "Database.Beam.Migrate") (HsType (tyConNamed "Double") mempty)

  charType _ Just {} = error "char collation"
  charType width Nothing = HsDataType (hsApp (hsVarFrom "char" "Database.Beam.Migrate")
                                             [ hsMaybe (hsInt <$> width) ])
                                      (HsType (tyConNamed "Text") (importSome "Data.Text" [ importTyNamed "Text" ]))

  varCharType _ Just {} = error "varchar collation"
  varCharType width Nothing = HsDataType (hsApp (hsVarFrom "varchar" "Database.Beam.Migrate")
                                                [ hsMaybe (hsInt <$> width) ])
                                         (HsType (tyConNamed "Text") (importSome "Data.Text" [ importTyNamed "Text" ]))

  nationalCharType width = HsDataType (hsApp (hsVarFrom "nationalChar" "Database.Beam.Migrate")
                                             [ hsMaybe (hsInt <$> width) ])
                                      (HsType (tyConNamed "Text") (importSome "Data.Text" [ importTyNamed "Text" ]))

  nationalVarCharType width = HsDataType (hsApp (hsVarFrom "nationalVarchar" "Database.Beam.Migrate")
                                                [ hsMaybe (hsInt <$> width) ])
                                         (HsType (tyConNamed "Text") (importSome "Data.Text" [ importTyNamed "Text" ]))

  bitType width = HsDataType (hsApp (hsVarFrom "bit" "Database.Beam.Migrate")
                                    [ hsMaybe (hsInt <$> width) ])
                             (HsType (tyConNamed "SqlBits") mempty)
  varBitType width = HsDataType (hsApp (hsVarFrom "varbit" "Database.Beam.Migrate")
                                       [ hsMaybe (hsInt <$> width) ])
                                (HsType (tyConNamed "SqlBits") mempty)

  dateType = HsDataType (hsVarFrom "date" "Database.Beam.Migrate") (HsType (tyConNamed "Day") (importSome "Data.Time" [ importTyNamed "Day" ]))

  timeType _ _ = error "timeType"
  domainType _ = error "domainType"
  timestampType _ = error "timestampType"

  numericType _ = error "numericType"
  decimalType _ = error "decimalType"

-- * HsSyntax utilities

tyApp :: Hs.Type () -> [ Hs.Type () ]
      -> Hs.Type ()
tyApp fn args = foldl (Hs.TyApp ()) fn args

tyConNamed :: String -> Hs.Type ()
tyConNamed nm = Hs.TyCon () (Hs.UnQual () (Hs.Ident () nm))

tyVarNamed :: String -> Hs.Type ()
tyVarNamed nm = Hs.TyVar () (Hs.Ident () nm)

combineHsExpr :: (Hs.Exp () -> Hs.Exp () -> Hs.Exp ())
              -> HsExpr -> HsExpr -> HsExpr
combineHsExpr f a b =
  HsExpr (f (hsExprSyntax a) (hsExprSyntax b))
         (hsExprImports a <> hsExprImports b)
         (hsExprConstraints a <> hsExprConstraints b)
         (hsExprTypeVariables a <> hsExprTypeVariables b)

hsApp :: HsExpr -> [HsExpr] -> HsExpr
hsApp fn args = foldl hsDoApp fn args
  where
    hsDoApp = combineHsExpr (Hs.App ())

hsApApp :: HsExpr -> [HsExpr] -> HsExpr
hsApApp fn [] = hsApp (hsVar "pure") [ fn ]
hsApApp fn (x:xs) = foldl mkAp (mkFmap fn x) xs
  where
    mkFmap = combineHsExpr (\a b -> Hs.InfixApp () a fmapOp b)
    mkAp = combineHsExpr (\a b -> Hs.InfixApp () a apOp b)

    fmapOp = hsOp "<$>"
    apOp = hsOp "<*>"

hsStr :: T.Text -> HsExpr
hsStr t = HsExpr (Hs.Lit () (Hs.String () s s)) mempty mempty mempty
  where s = T.unpack t

hsRecCon :: T.Text -> [ (T.Text, HsExpr) ] -> HsExpr
hsRecCon nm fs = foldl (combineHsExpr const) (HsExpr e mempty mempty mempty) (map snd fs)
  where
    e = Hs.RecConstr () (Hs.UnQual () (Hs.Ident () (T.unpack nm)))
                        (map (\(fieldNm, e') -> Hs.FieldUpdate () (Hs.UnQual () (Hs.Ident () (T.unpack fieldNm)))
                                                                  (hsExprSyntax e')) fs)

hsMaybe :: Maybe HsExpr -> HsExpr
hsMaybe Nothing = hsTyCon "Nothing"
hsMaybe (Just e) = hsApp (hsTyCon "Just") [e]

hsVar :: T.Text -> HsExpr
hsVar nm = HsExpr (Hs.Var () (Hs.UnQual () (Hs.Ident () (T.unpack nm)))) mempty mempty mempty

hsVarFrom :: T.Text -> T.Text -> HsExpr
hsVarFrom nm modNm = HsExpr (Hs.Var () (Hs.UnQual () (Hs.Ident () (T.unpack nm)))) (importSome modNm [ importVarNamed nm])
                            mempty mempty

hsTyCon :: T.Text -> HsExpr
hsTyCon nm = HsExpr (Hs.Con () (Hs.UnQual () (Hs.Ident () (T.unpack nm)))) mempty mempty mempty

hsInt :: (Integral a, Show a) => a -> HsExpr
hsInt i = HsExpr (Hs.Lit () (Hs.Int () (fromIntegral i) (show i))) mempty mempty mempty

hsOp :: T.Text -> Hs.QOp ()
hsOp nm = Hs.QVarOp () (Hs.UnQual () (Hs.Symbol () (T.unpack nm)))

-- * Orphans

instance Hashable (Hs.Exp ())
instance Hashable (Hs.QName ())
instance Hashable (Hs.ModuleName ())
instance Hashable (Hs.IPName ())
instance Hashable (Hs.Asst ())
instance Hashable (Hs.Literal ())
instance Hashable (Hs.Name ())
instance Hashable (Hs.Type ())
instance Hashable (Hs.QOp ())
instance Hashable (Hs.TyVarBind ())
instance Hashable (Hs.Kind ())
instance Hashable (Hs.Context ())
instance Hashable (Hs.SpecialCon ())
instance Hashable (Hs.Pat ())
instance Hashable (Hs.Sign ())
instance Hashable Hs.Boxed
instance Hashable (Hs.Promoted ())
instance Hashable (Hs.Binds ())
instance Hashable (Hs.Splice ())
instance Hashable (Hs.PatField ())
instance Hashable (Hs.Decl ())
instance Hashable (Hs.DeclHead ())
instance Hashable (Hs.IPBind ())
instance Hashable (Hs.RPat ())
instance Hashable (Hs.Stmt ())
instance Hashable (Hs.RPatOp ())
instance Hashable (Hs.XName ())
instance Hashable (Hs.ResultSig ())
instance Hashable (Hs.Alt ())
instance Hashable (Hs.Unpackedness ())
instance Hashable (Hs.InjectivityInfo ())
instance Hashable (Hs.PXAttr ())
instance Hashable (Hs.Rhs ())
instance Hashable (Hs.FieldUpdate ())
instance Hashable (Hs.TypeEqn ())
instance Hashable (Hs.QualStmt ())
instance Hashable (Hs.DataOrNew ())
instance Hashable (Hs.Bracket ())
instance Hashable (Hs.QualConDecl ())
instance Hashable (Hs.XAttr ())
instance Hashable (Hs.ConDecl ())
instance Hashable (Hs.Deriving ())
instance Hashable (Hs.InstRule ())
instance Hashable (Hs.FieldDecl ())
instance Hashable (Hs.GadtDecl ())
instance Hashable (Hs.InstHead ())
instance Hashable (Hs.FunDep ())
instance Hashable (Hs.ClassDecl ())
instance Hashable (Hs.Overlap ())
instance Hashable (Hs.InstDecl ())
instance Hashable (Hs.Assoc ())
instance Hashable (Hs.Op ())
instance Hashable (Hs.Match ())
instance Hashable (Hs.PatternSynDirection ())
instance Hashable (Hs.CallConv ())
instance Hashable (Hs.Safety ())
instance Hashable (Hs.Rule ())
instance Hashable (Hs.Activation ())
instance Hashable (Hs.RuleVar ())
instance Hashable (Hs.Annotation ())
instance Hashable (Hs.BooleanFormula ())
instance Hashable (Hs.Role ())
instance Hashable (Hs.GuardedRhs ())
instance Hashable (Hs.BangType ())
instance Hashable (Hs.ImportSpec ())
instance Hashable (Hs.Namespace ())
instance Hashable (Hs.CName ())
instance Hashable a => Hashable (S.Set a) where
  hashWithSalt s a = hashWithSalt s (S.toList a)
