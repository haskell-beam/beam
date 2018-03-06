module Database.Beam.Migrate.Tool.MigrationCmd where

import Database.Beam.Migrate.Tool.CmdLine
import Database.Beam.Migrate.Tool.Registry

import Data.Text (Text)

newMigration :: MigrateCmdLine -> Text -> Text
             -> Bool -> Bool -> [MigrationFormat]
             -> IO ()
newMigration cmdLine from to autoGen leaveOpen fmts' =
    updatingRegistry cmdLine $ \reg -> do
      fromSrc <- parsePredicateFetchSourceSpec cmdLine reg from
      toSrc <- parsePredicateFetchSourceSpec cmdLine reg to

      -- TODO parallelize
      (fromIdMaybe, fromPreds) <- getPredicatesFromSpec cmdLine reg fromSrc
      (toIdMaybe,   toPreds)   <- getPredicatesFromSpec cmdLine reg toSrc

      case (,) <$> fromIdMaybe <*> toIdMaybe of
        Nothing -> fail "Could not find schemas"
        Just (fromId, toId) -> do
            fmts <- resolveFormats fromId toId fmts'

            case (,) <$> lookupSchema fromId fmts reg
                     <*> lookupSchema toId   fmts reg of
              Nothing -> fail "Could not find schemas with the given formats"
              Just (fromSchema, toSchema) ->
                  -- For each format, attempt to form a migration.
                  fail "Unimplemented"
