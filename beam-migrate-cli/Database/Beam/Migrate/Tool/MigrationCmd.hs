module Database.Beam.Migrate.Tool.MigrationCmd where

import           Database.Beam.Migrate.Tool.CmdLine
import           Database.Beam.Migrate.Tool.Registry
import           Database.Beam.Migrate.Tool.Diff

import qualified Data.HashSet as S
import           Data.Text (Text)
import           Data.UUID (UUID)

resolveFormats :: MigrationRegistry -> UUID -> UUID -> [MigrationFormat]
               -> IO [MigrationFormat]
resolveFormats reg fromId toId [] = do
  case (,) <$> lookupSchema fromId [] reg
           <*> lookupSchema toId [] reg of
    Nothing -> fail "Could not find schemas"
    Just (fromSchema, toSchema) ->
      let fromFormats = S.fromList $ registeredSchemaInfoFormats fromSchema
          toFormats = S.fromList $ registeredSchemaInfoFormats toSchema

          commonFormats = S.intersection fromFormats toFormats
      in if S.null commonFormats
         then fail "The schemas have no formats in common"
         else pure (S.toList commonFormats)           
           
resolveFormats _ _ _ fmts = pure fmts  
                         

newMigrationCmd :: MigrateCmdLine -> Text -> Text
                -> Bool -> Bool -> [MigrationFormat]
                -> IO ()
newMigrationCmd cmdLine from to autoGen leaveOpen fmts' =
    updatingRegistry cmdLine $ \reg -> do
      fromSrc <- parsePredicateFetchSourceSpec cmdLine reg from
      toSrc <- parsePredicateFetchSourceSpec cmdLine reg to

      -- TODO parallelize
      (fromIdMaybe, fromPreds) <- getPredicatesFromSpec cmdLine reg fromSrc
      (toIdMaybe,   toPreds)   <- getPredicatesFromSpec cmdLine reg toSrc

      case (,) <$> fromIdMaybe <*> toIdMaybe of
        Nothing -> fail "Could not find schemas"
        Just (fromId, toId) -> do
            fmts <- resolveFormats reg fromId toId fmts'

            case (,) <$> lookupSchema fromId fmts reg
                     <*> lookupSchema toId   fmts reg of
              Nothing -> fail "Could not find schemas with the given formats"
              Just (fromSchema, toSchema) ->
                  -- For each format, attempt to form a migration.
                  fail "Unimplemented"
