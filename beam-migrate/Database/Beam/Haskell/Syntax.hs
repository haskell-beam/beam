{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

-- | Instances that allow us to use Haskell as a backend syntax. This allows us
-- to use migrations defined a la 'Database.Beam.Migrate.SQL' to generate a beam
-- schema.
--
-- Mainly of interest to backends.
--
-- Unfortunately, we define some orphan 'Hashable' instances that aren't defined
-- for us in @haskell-src-exts@.
module Database.Beam.Haskell.Syntax where

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Backend.SQL.AST
import           Database.Beam.Backend.SQL.Builder
import           Database.Beam.Migrate.Checks (HasDataTypeCreatedCheck(..))
import           Database.Beam.Migrate.SQL.SQL92
import           Database.Beam.Migrate.SQL.Types
import           Database.Beam.Migrate.Serialization

import           Data.Char (toLower, toUpper)
import           Data.Hashable
import           Data.Int
import           Data.List (find, nub)
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           Data.String
import qualified Data.Text as T
#if !MIN_VERSION_base(4, 11, 0)
import           Data.Semigroup
#endif

import qualified Language.Haskell.Exts as Hs

import           Text.PrettyPrint (render)

newtype HsDbField = HsDbField { buildHsDbField :: Hs.Type () -> Hs.Type () }

data HsConstraintDefinition
  = HsConstraintDefinition
  { hsConstraintDefinitionConstraint :: HsExpr }
  deriving (Show, Eq, Generic)
instance Hashable HsConstraintDefinition
instance Sql92DisplaySyntax HsConstraintDefinition where
  displaySyntax = show

newtype HsEntityName = HsEntityName { getHsEntityName :: String } deriving (Show, Eq, Ord, IsString)

data HsImport = HsImportAll | HsImportSome (S.Set (Hs.ImportSpec ()))
  deriving (Show, Eq, Generic)
instance Hashable HsImport
instance Semigroup HsImport where
  (<>) = mappend
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
importTyNamed = importVarNamed -- nm = Hs.IAbs () (Hs.TypeNamespace ()) (Hs.Ident () (T.unpack nm))

importVarNamed :: T.Text -> Hs.ImportSpec ()
importVarNamed nm = Hs.IVar () (Hs.Ident () (T.unpack nm))

newtype HsImports = HsImports (M.Map (Hs.ModuleName ()) HsImport)
  deriving (Show, Eq)
instance Hashable HsImports where
  hashWithSalt s (HsImports a) = hashWithSalt s (M.assocs a)
instance Semigroup HsImports where
  (<>) = mappend
instance Monoid HsImports where
  mempty = HsImports mempty
  mappend (HsImports a) (HsImports b) =
    HsImports (M.unionWith mappend a b)

data HsDataType
  = HsDataType
  { hsDataTypeMigration :: HsExpr
  , hsDataTypeType :: HsType
  , hsDataTypeSerialized :: BeamSerializedDataType
  } deriving (Eq, Show, Generic)
instance Hashable HsDataType where
  hashWithSalt salt (HsDataType mig ty _) = hashWithSalt salt (mig, ty)
instance Sql92DisplaySyntax HsDataType where
  displaySyntax = show
instance HasDataTypeCreatedCheck HsDataType where
  dataTypeHasBeenCreated _ _ = True -- TODO make this more robust

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
instance Sql92DisplaySyntax HsColumnSchema where
  displaySyntax = show

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

instance Semigroup HsAction where
  (<>) = mappend
instance Monoid HsAction where
  mempty = HsAction [] []
  mappend (HsAction ma ea) (HsAction mb eb) =
    HsAction (ma <> mb) (ea <> eb)

newtype HsBackendConstraint = HsBackendConstraint { buildHsBackendConstraint :: Hs.Type () -> Hs.Asst () }

data HsBeamBackend f
  = HsBeamBackendSingle HsType f
  | HsBeamBackendConstrained [ HsBackendConstraint ]
  | HsBeamBackendNone

instance Semigroup (HsBeamBackend f) where
  (<>) = mappend
instance Monoid (HsBeamBackend f) where
  mempty = HsBeamBackendConstrained []
  mappend (HsBeamBackendSingle aTy aExp) (HsBeamBackendSingle bTy _)
    | aTy == bTy = HsBeamBackendSingle aTy aExp
    | otherwise = HsBeamBackendNone
  mappend a@HsBeamBackendSingle {} _ = a
  mappend _ b@HsBeamBackendSingle {} = b
  mappend HsBeamBackendNone _ = HsBeamBackendNone
  mappend _ HsBeamBackendNone = HsBeamBackendNone
  mappend (HsBeamBackendConstrained a) (HsBeamBackendConstrained b) =
    HsBeamBackendConstrained (a <> b)

data HsEntity
    = HsEntity
    { hsEntityBackend :: HsBeamBackend HsExpr

    , hsEntityName    :: HsEntityName

    , hsEntityDecls   :: [ HsDecl ]
    , hsEntityDbDecl  :: HsDbField

    , hsEntityExp     :: HsExpr
    }

newtype HsFieldLookup = HsFieldLookup { hsFieldLookup :: T.Text -> Maybe (T.Text, Hs.Type ()) }
newtype HsTableConstraint = HsTableConstraint (T.Text -> HsFieldLookup -> HsTableConstraintDecls)

data HsTableConstraintDecls
    = HsTableConstraintDecls
    { hsTableConstraintInstance :: [ Hs.InstDecl () ]
    , hsTableConstraintDecls    :: [ HsDecl ]
    }

instance Semigroup HsTableConstraintDecls where
  (<>) = mappend

instance Monoid HsTableConstraintDecls where
  mempty = HsTableConstraintDecls [] []
  mappend (HsTableConstraintDecls ai ad) (HsTableConstraintDecls bi bd) =
    HsTableConstraintDecls (ai <> bi) (ad <> bd)

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

derivingDecl :: [Hs.InstRule ()] -> Hs.Deriving ()
derivingDecl =
#if MIN_VERSION_haskell_src_exts(1,20,0)
  Hs.Deriving () Nothing
#else
  Hs.Deriving ()
#endif

dataDecl :: Hs.DeclHead ()
         -> [Hs.QualConDecl ()]
         -> Maybe (Hs.Deriving ())
         -> Hs.Decl ()
dataDecl declHead cons deriving_ =
#if MIN_VERSION_haskell_src_exts(1,20,0)
  Hs.DataDecl () (Hs.DataType ()) Nothing declHead cons (maybeToList deriving_)
#else
  Hs.DataDecl () (Hs.DataType ()) Nothing declHead cons deriving_
#endif

insDataDecl :: Hs.Type ()
            -> [Hs.QualConDecl ()]
            -> Maybe (Hs.Deriving ())
            -> Hs.InstDecl ()
insDataDecl declHead cons deriving_ =
#if MIN_VERSION_haskell_src_exts(1,20,0)
   Hs.InsData () (Hs.DataType ()) declHead cons (maybeToList deriving_)
#else
   Hs.InsData () (Hs.DataType ()) declHead cons deriving_
#endif

databaseTypeDecl :: [ HsEntity ] -> Hs.Decl ()
databaseTypeDecl entities =
  dataDecl declHead [ conDecl ] (Just deriving_)
  where
    declHead = Hs.DHApp () (Hs.DHead () (Hs.Ident () "Db"))
                           (Hs.UnkindedVar () (Hs.Ident () "entity"))
    conDecl = Hs.QualConDecl () Nothing Nothing
                (Hs.RecDecl () (Hs.Ident () "Db") (mkField <$> entities))
    deriving_ = derivingDecl [ Hs.IRule () Nothing Nothing $
                               Hs.IHCon () $ Hs.UnQual () $
                               Hs.Ident () "Generic" ]

    mkField entity = Hs.FieldDecl () [ Hs.Ident () (entityDbFieldName entity) ]
                                     (buildHsDbField (hsEntityDbDecl entity) $
                                      Hs.TyVar () (Hs.Ident () "entity"))

migrationTypeDecl :: HsBeamBackend HsExpr -> [Hs.Type ()] -> Hs.Decl ()
migrationTypeDecl be inputs =
  Hs.TypeSig () [Hs.Ident () "migration"] migrationType
  where
    (beAssts, beVar) =
      case be of
        HsBeamBackendNone -> error "No backend matches"
        HsBeamBackendSingle ty _ -> ([], hsTypeSyntax ty)
        HsBeamBackendConstrained cs ->
          ( map (flip buildHsBackendConstraint beVar) cs
          , tyVarNamed "be" )

    resultType = tyApp (tyConNamed "Migration")
                       [ beVar
                       , tyApp (tyConNamed "CheckedDatabaseSettings")
                               [ beVar
                               , tyConNamed "Db" ] ]

    migrationUnconstrainedType
      | [] <- inputs = resultType
      | otherwise    = functionTy (tyTuple inputs) resultType

    constraints = nub beAssts
    migrationType
      | [] <- constraints  = migrationUnconstrainedType
      | [c] <- constraints = Hs.TyForall () Nothing (Just (Hs.CxSingle () c)) migrationUnconstrainedType
      | otherwise          = Hs.TyForall () Nothing (Just (Hs.CxTuple () constraints)) migrationUnconstrainedType

migrationDecl :: HsBeamBackend HsExpr -> [Hs.Exp ()] -> [ (Maybe (Hs.Pat ()), HsExpr) ] -> [HsEntity] -> Hs.Decl ()
migrationDecl _ _ migrations entities =
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

dbTypeDecl :: HsBeamBackend HsExpr -> Hs.Decl ()
dbTypeDecl be =
  Hs.TypeSig () [ Hs.Ident () "db" ] dbType
  where
    unconstrainedDbType = tyApp (tyConNamed "DatabaseSettings")
                                [ beVar, tyConNamed "Db" ]
    dbType
      | []  <- constraints, [] <- bindings = unconstrainedDbType
      | []  <- constraints  = Hs.TyForall () (Just bindings) Nothing unconstrainedDbType
      | [c] <- constraints = Hs.TyForall () (Just bindings) (Just (Hs.CxSingle () c)) unconstrainedDbType
      | otherwise          = Hs.TyForall () (Just bindings) (Just (Hs.CxTuple () constraints)) unconstrainedDbType

    constraints = nub beAssts
    (bindings, beAssts, beVar) =
      case be of
        HsBeamBackendNone -> error "No backend matches"
        HsBeamBackendSingle ty _ -> (standardBindings, [], hsTypeSyntax ty)
        HsBeamBackendConstrained cs ->
          ( tyVarBind "be":standardBindings
          , map (flip buildHsBackendConstraint beVar) cs
          , tyVarNamed "be" )

    standardBindings = []

    tyVarBind nm = Hs.UnkindedVar () (Hs.Ident () nm)

dbDecl :: HsBeamBackend HsExpr -> [HsExpr] -> Hs.Decl ()
dbDecl backend params =
  Hs.FunBind () [ Hs.Match () (Hs.Ident () "db") [] (Hs.UnGuardedRhs () body) Nothing ]
  where
    backendVar = case backend of
                   HsBeamBackendNone -> error "No syntax matches"
                   HsBeamBackendSingle ty _ -> hsTypeSyntax ty
                   HsBeamBackendConstrained _ -> tyVarNamed "be"

    body = hsExprSyntax $
           hsApp (hsVar "unCheckDatabase")
                 [ hsApp (hsVarFrom "runMigrationSilenced" "Database.Beam.Migrate")
                   [ hsApp (hsVisibleTyApp (hsVar "migration") backendVar) $
                     case params of
                       [] -> []
                       _  -> [ hsTuple params ]
                   ] ]

renderHsSchema :: HsModule -> Either String String
renderHsSchema (HsModule modNm entities migrations) =
  let hsMod = Hs.Module () (Just modHead) modPragmas imports decls

      modHead = Hs.ModuleHead () (Hs.ModuleName () modNm) Nothing (Just modExports)
      modExports = Hs.ExportSpecList () (commonExports ++ foldMap (foldMap hsDeclExports . hsEntityDecls) entities)
      commonExports = [ Hs.EVar () (unqual "db")
                      , Hs.EVar () (unqual "migration")
                      , Hs.EThingWith () (Hs.EWildcard () 0)
                                      (unqual "Db") [] ]

      modPragmas = [ Hs.LanguagePragma () [ Hs.Ident () "StandaloneDeriving"
                                          , Hs.Ident () "GADTs"
                                          , Hs.Ident () "ScopedTypeVariables"
                                          , Hs.Ident () "FlexibleContexts"
                                          , Hs.Ident () "FlexibleInstances"
                                          , Hs.Ident () "DeriveGeneric"
                                          , Hs.Ident () "TypeSynonymInstances"
                                          , Hs.Ident () "ExplicitNamespaces"
                                          , Hs.Ident () "TypeApplications"
                                          , Hs.Ident () "TypeFamilies"
                                          , Hs.Ident () "OverloadedStrings" ] ]

      HsImports importedModules = foldMap (\e -> foldMap hsDeclImports (hsEntityDecls e) <>
                                                 hsExprImports (hsEntityExp e)) entities <>
                                  foldMap (hsExprImports . snd) migrations <>
                                  importSome "Database.Beam.Migrate" [ importTyNamed "CheckedDatabaseSettings", importTyNamed "Migration"
                                                                     , importTyNamed "BeamMigrateSqlBackend"
                                                                     , importVarNamed "runMigrationSilenced"
                                                                     , importVarNamed "unCheckDatabase" ]
      imports = commonImports <>
                map (\(modName, spec) ->
                       case spec of
                         HsImportAll -> Hs.ImportDecl () modName False False False Nothing Nothing Nothing
                         HsImportSome nms ->
                           let importList = Hs.ImportSpecList () False (S.toList nms)
                           in Hs.ImportDecl () modName False False False Nothing Nothing (Just importList)
                    )
                    (M.assocs importedModules)

      commonImports = [ Hs.ImportDecl () (Hs.ModuleName () "Database.Beam") False False False Nothing Nothing Nothing
                      , Hs.ImportDecl () (Hs.ModuleName () "Control.Applicative") False False False Nothing Nothing Nothing ]

      backend = foldMap hsEntityBackend entities

      backendHs = case backend of
                    HsBeamBackendNone -> error "Can't instantiate Database instance: No backend matches"
                    HsBeamBackendSingle ty _ -> hsTypeSyntax ty
                    HsBeamBackendConstrained {} -> tyVarNamed "be" -- TODO constraints

      decls = foldMap (map hsDeclSyntax . hsEntityDecls) entities ++
              [ databaseTypeDecl entities

              , migrationTypeDecl backend []
              , migrationDecl backend [] migrations entities

              , hsInstance "Database" [ backendHs, tyConNamed "Db" ] []

              , dbTypeDecl backend
              , dbDecl backend [] ]

  in Right (render (Hs.prettyPrim hsMod))

-- * DDL Syntax definitions

data HsNone = HsNone deriving (Show, Eq, Ord, Generic)
instance Hashable HsNone

instance Semigroup HsNone where
  (<>) = mappend
instance Monoid HsNone where
  mempty = HsNone
  mappend _ _ = HsNone

data HsMigrateBackend = HsMigrateBackend

instance BeamMigrateOnlySqlBackend HsMigrateBackend
type instance BeamSqlBackendSyntax HsMigrateBackend = HsAction

hsMkTableName :: (Char -> Char) -> TableName -> String
hsMkTableName toNameCase (TableName sch nm) =
  case sch of
    Nothing ->
      case T.unpack nm of
        [] -> error "No name for table"
        x:xs -> toNameCase x:xs
    Just schNm ->
      case T.unpack schNm of
        [] -> error "Empty schema name"
        x:xs -> toNameCase x:xs ++ "_" ++ T.unpack nm

hsTableVarName, hsTableTypeName :: TableName -> String
hsTableVarName = hsMkTableName toLower
hsTableTypeName = hsMkTableName toUpper

instance IsSql92DdlCommandSyntax HsAction where
  type Sql92DdlCommandCreateTableSyntax HsAction = HsAction
  type Sql92DdlCommandAlterTableSyntax HsAction = HsAction
  type Sql92DdlCommandDropTableSyntax HsAction = HsAction

  createTableCmd = id
  dropTableCmd = id
  alterTableCmd = id

instance IsSql92AlterTableSyntax HsAction where
  type Sql92AlterTableTableNameSyntax HsAction = TableName
  type Sql92AlterTableAlterTableActionSyntax HsAction = HsNone

  alterTableSyntax _ _ = error "alterTableSyntax"

instance IsSql92AlterTableActionSyntax HsNone where
  type Sql92AlterTableColumnSchemaSyntax HsNone = HsColumnSchema
  type Sql92AlterTableAlterColumnActionSyntax HsNone = HsNone

  alterColumnSyntax _ _ = HsNone
  addColumnSyntax _ _ = HsNone
  dropColumnSyntax _ = HsNone
  renameTableToSyntax _ = HsNone
  renameColumnToSyntax _ _ = HsNone

instance IsSql92AlterColumnActionSyntax HsNone where
  setNullSyntax = HsNone
  setNotNullSyntax = HsNone

instance IsSql92DropTableSyntax HsAction where
  type Sql92DropTableTableNameSyntax HsAction = TableName

  dropTableSyntax nm = HsAction [ (Nothing, dropTable) ] []
    where
      dropTable = hsApp (hsVar "dropTable") [ hsVar (fromString (hsTableVarName nm)) ]

instance IsSql92CreateTableSyntax HsAction where
  type Sql92CreateTableTableNameSyntax HsAction = TableName
  type Sql92CreateTableOptionsSyntax HsAction = HsNone
  type Sql92CreateTableTableConstraintSyntax HsAction = HsTableConstraint
  type Sql92CreateTableColumnSchemaSyntax HsAction = HsColumnSchema

  createTableSyntax _ nm fields cs =
    HsAction [ ( Just (Hs.PVar () (Hs.Ident () varName))
               , migration ) ]
             [ entity ]
    where
      (varName, tyName, tyConName) =
        ( hsTableVarName nm, hsTableTypeName nm ++ "T", hsTableTypeName nm )

      mkHsFieldName fieldNm = "_" ++ varName ++
                              case T.unpack fieldNm of
                                [] -> error "empty field name"
                                (x:xs) -> toUpper x:xs

      HsTableConstraintDecls tableInstanceDecls constraintDecls = foldMap (\(HsTableConstraint mkConstraint) -> mkConstraint (fromString tyConName) fieldLookup) cs
      fieldLookup = HsFieldLookup $ \fieldNm ->
                    fmap (\(fieldNm', ty') -> (fromString (mkHsFieldName fieldNm'), ty')) $
                    find ( (== fieldNm) . fst ) tyConFields

      migration =
        hsApp (hsVarFrom "createTable" "Database.Beam.Migrate")
              [ hsStr (fromString (hsTableVarName nm))
              , hsApp (hsTyCon (fromString tyConName))
                      (map (\(fieldNm, ty) -> mkHsColumnSchema ty fieldNm) fields) ]
      entity = HsEntity
             { hsEntityBackend = HsBeamBackendConstrained [ beamMigrateSqlBackend ]

             , hsEntityName    = HsEntityName varName
             , hsEntityDecls   = [ HsDecl tblDecl imports
                                          [ Hs.EThingWith () (Hs.EWildcard () 0) (unqual tyName) [] ]
                                 , HsDecl tblBeamable imports []

                                 , HsDecl tblPun imports [ Hs.EVar () (unqual tyConName) ]

                                 , HsDecl tblShowInstance imports []
                                 , HsDecl tblEqInstance imports []

                                 , HsDecl tblInstanceDecl imports []
                                 ] ++
                                 constraintDecls
             , hsEntityDbDecl  = HsDbField (\f -> tyApp f [ tyApp (tyConNamed "TableEntity") [tyConNamed tyName] ])
             , hsEntityExp     = hsVar (fromString varName)
             }

      imports = foldMap (\(_, ty) -> hsTypeImports (hsColumnSchemaType ty)) fields

      tblDecl = dataDecl tblDeclHead [ tblConDecl ] (Just deriving_)
      tblDeclHead = Hs.DHApp () (Hs.DHead () (Hs.Ident () tyName))
                                (Hs.UnkindedVar () (Hs.Ident () "f"))
      tblConDecl = Hs.QualConDecl () Nothing Nothing (Hs.RecDecl () (Hs.Ident () tyConName) tyConFieldDecls)

      tyConFieldDecls = map (\(fieldNm, ty) ->
                                Hs.FieldDecl () [ Hs.Ident () (mkHsFieldName fieldNm) ] ty) tyConFields
      tyConFields = map (\(fieldNm, ty) -> ( fieldNm
                                           , tyApp (tyConNamed "Columnar")
                                                   [ tyVarNamed "f"
                                                   , hsTypeSyntax (hsColumnSchemaType ty) ])) fields

      deriving_ = derivingDecl [ inst "Generic" ]

      tblBeamable = hsInstance "Beamable" [ tyConNamed tyName ] []
      tblPun = Hs.TypeDecl () (Hs.DHead () (Hs.Ident () tyConName))
                              (tyApp (tyConNamed tyName) [ tyConNamed "Identity" ])

      tblEqInstance = hsDerivingInstance "Eq" [ tyConNamed tyConName ]
      tblShowInstance = hsDerivingInstance "Show" [ tyConNamed tyConName]

      tblInstanceDecl = hsInstance "Table" [ tyConNamed tyName ] tableInstanceDecls

instance IsSql92ColumnSchemaSyntax HsColumnSchema where
  type Sql92ColumnSchemaColumnConstraintDefinitionSyntax HsColumnSchema = HsConstraintDefinition
  type Sql92ColumnSchemaColumnTypeSyntax HsColumnSchema = HsDataType
  type Sql92ColumnSchemaExpressionSyntax HsColumnSchema = HsExpr

  columnSchemaSyntax dataType _ cs _ = HsColumnSchema (\nm -> fieldExpr nm)
                                                      (modTy $ hsDataTypeType dataType)
    where
      notNullable = any ((==notNullConstraintSyntax) . hsConstraintDefinitionConstraint) cs
      modTy t = if notNullable then t else t { hsTypeSyntax = tyApp (tyConNamed "Maybe") [ hsTypeSyntax t ] }
      modDataTy e = if notNullable then e else hsApp (hsVarFrom "maybeType" "Database.Beam.Migrate") [e]

      fieldExpr nm = hsApp (hsVarFrom "field" "Database.Beam.Migrate")
                           ([ hsStr nm
                            , modDataTy (hsDataTypeMigration dataType) ] ++
                            map hsConstraintDefinitionConstraint cs)

instance IsSql92TableConstraintSyntax HsTableConstraint where
  primaryKeyConstraintSyntax fields =
    HsTableConstraint $ \tblNm tblFields ->
    let primaryKeyDataDecl = insDataDecl primaryKeyType [ primaryKeyConDecl ] (Just primaryKeyDeriving)

        tableTypeNm = tblNm <> "T"
        tableTypeKeyNm = tblNm <> "Key"

        (fieldRecordNames, fieldTys) = unzip (fromMaybe (error "fieldTys") (mapM (hsFieldLookup tblFields) fields))

        primaryKeyType = tyApp (tyConNamed "PrimaryKey") [ tyConNamed (T.unpack tableTypeNm), tyVarNamed "f" ]
        primaryKeyConDecl  = Hs.QualConDecl () Nothing Nothing (Hs.ConDecl () (Hs.Ident () (T.unpack tableTypeKeyNm)) fieldTys)
        primaryKeyDeriving = derivingDecl [ inst "Generic" ]

        primaryKeyTypeDecl = Hs.TypeDecl () (Hs.DHead () (Hs.Ident () (T.unpack tableTypeKeyNm)))
                                            (tyApp (tyConNamed "PrimaryKey")
                                                   [ tyConNamed (T.unpack tableTypeNm)
                                                   , tyConNamed "Identity" ])

        primaryKeyFunDecl = Hs.InsDecl () (Hs.FunBind () [Hs.Match () (Hs.Ident () "primaryKey") [] (Hs.UnGuardedRhs () primaryKeyFunBody) Nothing])
        primaryKeyFunBody = hsExprSyntax $
                            hsApApp (hsVar tableTypeKeyNm)
                                    (map hsVar fieldRecordNames)

        decl d = HsDecl d mempty mempty

    in HsTableConstraintDecls [ primaryKeyDataDecl
                              , primaryKeyFunDecl ]
                              (HsDecl primaryKeyTypeDecl mempty [ Hs.EVar () (unqual (T.unpack tableTypeKeyNm)) ]:
                               map decl [ hsInstance "Beamable" [ tyParens (tyApp (tyConNamed "PrimaryKey") [ tyConNamed (T.unpack tableTypeNm)  ]) ] []
                                        , hsDerivingInstance "Eq" [ tyConNamed (T.unpack tableTypeKeyNm) ]
                                        , hsDerivingInstance "Show" [ tyConNamed (T.unpack tableTypeKeyNm) ]
                                        ])

instance IsSql92ColumnConstraintDefinitionSyntax HsConstraintDefinition where
  type Sql92ColumnConstraintDefinitionAttributesSyntax HsConstraintDefinition = HsNone
  type Sql92ColumnConstraintDefinitionConstraintSyntax HsConstraintDefinition = HsExpr

  constraintDefinitionSyntax Nothing expr Nothing = HsConstraintDefinition expr
  constraintDefinitionSyntax _ _ _ = error "constraintDefinitionSyntax{HsExpr}"

instance Sql92SerializableConstraintDefinitionSyntax HsConstraintDefinition where
  serializeConstraint _ = "unknown-constrainst"

instance IsSql92MatchTypeSyntax HsNone where
  fullMatchSyntax = HsNone
  partialMatchSyntax = HsNone
instance IsSql92ReferentialActionSyntax HsNone where
  referentialActionCascadeSyntax = HsNone
  referentialActionNoActionSyntax = HsNone
  referentialActionSetDefaultSyntax = HsNone
  referentialActionSetNullSyntax = HsNone

instance IsSql92ExtractFieldSyntax HsExpr where
  secondsField = hsVar "secondsField"
  minutesField = hsVar "minutesField"
  hourField    = hsVar "hourField"
  yearField    = hsVar "yearField"
  monthField   = hsVar "monthField"
  dayField     = hsVar "dayField"

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
  lowerE = hsApp (hsVar "lowerE") . pure
  upperE = hsApp (hsVar "upperE") . pure
  trimE = hsApp (hsVar "trimE") . pure

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

  inE a b = hsApp (hsVar "inE") [a, hsList b]

instance IsSql92QuantifierSyntax HsExpr where
  quantifyOverAll = hsVar "quantifyOverAll"
  quantifyOverAny = hsVar "quantifyOverAny"

instance IsSql92ColumnConstraintSyntax HsExpr where
  type Sql92ColumnConstraintExpressionSyntax HsExpr = HsExpr
  type Sql92ColumnConstraintMatchTypeSyntax HsExpr = HsNone
  type Sql92ColumnConstraintReferentialActionSyntax HsExpr = HsNone

  notNullConstraintSyntax = hsVarFrom "notNull" "Database.Beam.Migrate"
  uniqueColumnConstraintSyntax = hsVar "unique"
  checkColumnConstraintSyntax = error "checkColumnConstraintSyntax"
  primaryKeyColumnConstraintSyntax = error "primaryKeyColumnConstraintSyntax"
  referencesConstraintSyntax = error "referencesConstraintSyntax"

instance IsSql92ConstraintAttributesSyntax HsNone where
  initiallyDeferredAttributeSyntax = HsNone
  initiallyImmediateAttributeSyntax = HsNone
  notDeferrableAttributeSyntax = HsNone
  deferrableAttributeSyntax = HsNone

instance HasSqlValueSyntax HsExpr Int32 where
  sqlValueSyntax = hsInt
instance HasSqlValueSyntax HsExpr Bool where
  sqlValueSyntax True = hsVar "True"
  sqlValueSyntax False = hsVar "False"

instance IsSql92FieldNameSyntax HsExpr where
  qualifiedField tbl nm = hsApp (hsVar "qualifiedField") [ hsStr tbl, hsStr nm ]
  unqualifiedField nm = hsApp (hsVar "unqualifiedField") [ hsStr nm ]

hsErrorType :: String -> HsDataType
hsErrorType msg =
  HsDataType (hsApp (hsVar "error") [ hsStr ("Unknown type: " <> fromString msg) ]) (HsType (tyConNamed "Void") (importSome "Data.Void" [ importTyNamed "Void" ]))
             (BeamSerializedDataType "hsErrorType")

instance IsSql92DataTypeSyntax HsDataType where
  intType = HsDataType (hsVarFrom "int" "Database.Beam.Migrate") (HsType (tyConNamed "Int") mempty) intType
  smallIntType = HsDataType (hsVarFrom "smallint" "Database.Beam.Migrate") (HsType (tyConNamed "Int16") (importSome "Data.Int" [ importTyNamed "Int16" ])) intType
  doubleType = HsDataType (hsVarFrom "double" "Database.Beam.Migrate") (HsType (tyConNamed "Double") mempty) doubleType

  floatType width = HsDataType (hsApp (hsVarFrom "float" "Database.Beam.Migrate")
                                      [ hsMaybe (hsInt <$> width) ])
                               (HsType (tyConNamed "Scientific") (importSome "Data.Scientific" [ importTyNamed "Scientific" ]))
                               (floatType width)

  realType = HsDataType (hsVarFrom "real" "Database.Beam.Migrate") (HsType (tyConNamed "Double") mempty) realType

  charType _ Just {} = error "char collation"
  charType width Nothing = HsDataType (hsApp (hsVarFrom "char" "Database.Beam.Migrate")
                                             [ hsMaybe (hsInt <$> width) ])
                                      (HsType (tyConNamed "Text") (importSome "Data.Text" [ importTyNamed "Text" ]))
                                      (charType width Nothing)

  varCharType _ Just {} = error "varchar collation"
  varCharType width Nothing = HsDataType (hsApp (hsVarFrom "varchar" "Database.Beam.Migrate")
                                                [ hsMaybe (hsInt <$> width) ])
                                         (HsType (tyConNamed "Text") (importSome "Data.Text" [ importTyNamed "Text" ]))
                                         (varCharType width Nothing)

  nationalCharType width = HsDataType (hsApp (hsVarFrom "nationalChar" "Database.Beam.Migrate")
                                             [ hsMaybe (hsInt <$> width) ])
                                      (HsType (tyConNamed "Text") (importSome "Data.Text" [ importTyNamed "Text" ]))
                                      (nationalCharType width)

  nationalVarCharType width = HsDataType (hsApp (hsVarFrom "nationalVarchar" "Database.Beam.Migrate")
                                                [ hsMaybe (hsInt <$> width) ])
                                         (HsType (tyConNamed "Text") (importSome "Data.Text" [ importTyNamed "Text" ]))
                                         (nationalVarCharType width)

  bitType width = HsDataType (hsApp (hsVarFrom "bit" "Database.Beam.Migrate")
                                    [ hsMaybe (hsInt <$> width) ])
                             (HsType (tyConNamed "SqlBits") mempty)
                             (bitType width)

  varBitType width = HsDataType (hsApp (hsVarFrom "varbit" "Database.Beam.Migrate")
                                       [ hsMaybe (hsInt <$> width) ])
                                (HsType (tyConNamed "SqlBits") mempty)
                                (varBitType width)

  dateType = HsDataType (hsVarFrom "date" "Database.Beam.Migrate")
                        (HsType (tyConNamed "Day") (importSome "Data.Time" [ importTyNamed "Day" ])) dateType

  timeType p False = HsDataType (hsApp (hsVarFrom "time" "Database.Beam.Migrate") [ hsMaybe Nothing ] )
                                (HsType (tyConNamed "TimeOfDay") (importSome "Data.Time" [ importTyNamed "TimeOfDay" ]))
                                (timeType p False)
  timeType _ _ = error "timeType"
  domainType _ = error "domainType"
  timestampType Nothing True =
    HsDataType (hsVarFrom "timestamptz" "Database.Beam.Migrate")
               (HsType (tyConNamed "LocalTime") (importSome "Data.Time" [ importTyNamed "LocalTime" ]))
               (timestampType Nothing True)
  timestampType Nothing False =
    HsDataType (hsVarFrom "timestamp" "Database.Beam.Migrate")
               (HsType (tyConNamed "LocalTime") (importSome "Data.Time" [ importTyNamed "LocalTime" ]))
               (timestampType Nothing False)
  timestampType _ _ = error "timestampType with prec"

  numericType precDec =
    HsDataType (hsApp (hsVarFrom "numeric" "Database.Beam.Migrate")
                      [ hsMaybe (fmap (\(prec, dec) -> hsTuple [ hsInt prec, hsMaybe (fmap hsInt dec) ]) precDec) ])
               (HsType (tyConNamed "Scientific") (importSome "Data.Scientific" [ importTyNamed "Scientific" ]))
               (numericType precDec)

  decimalType = numericType

instance IsSql99DataTypeSyntax HsDataType where
  characterLargeObjectType =
    HsDataType (hsVarFrom "characterLargeObject" "Database.Beam.Migrate")
               (HsType (tyConNamed "Text") (importSome "Data.Text" [ importTyNamed "Text" ]))
               characterLargeObjectType
  binaryLargeObjectType =
    HsDataType (hsVarFrom "binaryLargeObject" "Database.Beam.Migrate")
               (HsType (tyConNamed "ByteString") (importSome "Data.ByteString" [ importTyNamed "ByteString" ]))
               binaryLargeObjectType
  booleanType =
    HsDataType (hsVarFrom "boolean" "Database.Beam.Migrate")
               (HsType (tyConNamed "Bool") mempty)
               booleanType
  arrayType (HsDataType migType (HsType typeExpr typeImports) serialized) len =
    HsDataType (hsApp (hsVarFrom "array" "Database.Beam.Migrate") [ migType, hsInt len ])
               (HsType (tyApp (tyConNamed "Vector") [typeExpr])
                       (typeImports <> importSome "Data.Vector" [ importTyNamed "Vector" ]))
               (arrayType serialized len)
  rowType _ = error "row types"

instance IsSql2003BinaryAndVarBinaryDataTypeSyntax HsDataType where
  binaryType prec =
    HsDataType (hsApp (hsVarFrom "binary" "Database.Beam.Migrate") [ hsMaybe (hsInt <$> prec) ])
               (HsType (tyConNamed "Integer") mempty)
               (binaryType prec)
  varBinaryType prec =
    HsDataType (hsApp (hsVarFrom "varbinary" "Database.Beam.Migrate") [ hsMaybe (hsInt <$> prec) ])
               (HsType (tyConNamed "Integer") mempty)
               (varBinaryType prec)

instance IsSql2008BigIntDataTypeSyntax HsDataType where
  bigIntType =
    HsDataType (hsVarFrom "bigint" "Database.Beam.Migrate")
               (HsType (tyConNamed "Int64") (importSome "Data.Int" [ importTyNamed "Int64" ]))
               bigIntType

instance Sql92SerializableDataTypeSyntax HsDataType where
  serializeDataType = fromBeamSerializedDataType . hsDataTypeSerialized

-- * HsSyntax utilities

tyParens :: Hs.Type () -> Hs.Type ()
tyParens = Hs.TyParen ()

functionTy :: Hs.Type () -> Hs.Type () -> Hs.Type ()
functionTy = Hs.TyFun ()

tyTuple :: [ Hs.Type () ] -> Hs.Type ()
tyTuple = Hs.TyTuple () Hs.Boxed

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

hsVisibleTyApp :: HsExpr -> Hs.Type () -> HsExpr
hsVisibleTyApp e t = e { hsExprSyntax = Hs.App () (hsExprSyntax e) (Hs.TypeApp () t) }

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

hsInstance :: T.Text -> [ Hs.Type () ] -> [ Hs.InstDecl () ] -> Hs.Decl ()
hsInstance classNm params decls =
  Hs.InstDecl () Nothing (Hs.IRule () Nothing Nothing instHead) $
  case decls of
    [] -> Nothing
    _  -> Just decls
  where
    instHead = foldl (Hs.IHApp ()) (Hs.IHCon () (Hs.UnQual () (Hs.Ident () (T.unpack classNm)))) params

hsDerivingInstance :: T.Text -> [ Hs.Type () ] -> Hs.Decl ()
hsDerivingInstance classNm params =
#if MIN_VERSION_haskell_src_exts(1,20,0)
  Hs.DerivDecl () Nothing Nothing (Hs.IRule () Nothing Nothing instHead)
#else
  Hs.DerivDecl () Nothing (Hs.IRule () Nothing Nothing instHead)
#endif
  where
    instHead = foldl (Hs.IHApp ()) (Hs.IHCon () (Hs.UnQual () (Hs.Ident () (T.unpack classNm)))) params

hsList, hsTuple :: [ HsExpr ] -> HsExpr
hsList = foldl (combineHsExpr addList) (HsExpr (Hs.List () []) mempty mempty mempty)
  where
    addList (Hs.List () ts) t = Hs.List () (ts ++ [t])
    addList _ _ = error "addList"
hsTuple = foldl (combineHsExpr addTuple) (HsExpr (Hs.Tuple () Hs.Boxed []) mempty mempty mempty)
  where
    addTuple (Hs.Tuple () boxed ts) t = Hs.Tuple () boxed (ts ++ [t])
    addTuple _ _ = error "addTuple"

inst :: String -> Hs.InstRule ()
inst = Hs.IRule () Nothing Nothing . Hs.IHCon () . Hs.UnQual () . Hs.Ident ()

beamMigrateSqlBackend :: HsBackendConstraint
beamMigrateSqlBackend =
  HsBackendConstraint $ \beTy ->
#if MIN_VERSION_haskell_src_exts(1, 22, 0)
  Hs.TypeA () (Hs.TyApp () (Hs.TyCon () (Hs.UnQual () (Hs.Ident () "BeamMigrateSqlBackend"))) beTy)
#else
  Hs.ClassA () (Hs.UnQual () (Hs.Ident () "BeamMigrateSqlBackend")) [ beTy ]
#endif



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
#if !MIN_VERSION_haskell_src_exts(1, 21, 0)
instance Hashable (Hs.Kind ())
#endif
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
#if MIN_VERSION_haskell_src_exts(1,20,0)
instance Hashable (Hs.DerivStrategy ())
instance Hashable (Hs.MaybePromotedName ())
#endif
instance Hashable a => Hashable (S.Set a) where
  hashWithSalt s a = hashWithSalt s (S.toList a)
