{-# LANGUAGE RecordWildCards #-}
module Main where

import Database.Beam
import Database.Beam.Migrate.Tool
import Database.Beam.Migrate.Types

import Control.Monad

import Data.Monoid
import Data.Proxy
import Data.Dynamic

import Options.Applicative
import Options.Applicative.Internal
import Options.Applicative.Types

import System.Directory
import System.IO
import System.Exit
import System.Environment

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe

showUsage :: Either (String, ExitCode) (ParserFailure ParserHelp) -> ParserInfo a -> IO b
showUsage failure opts =
  do progn <- getProgName
     let (helpMsg, exit) =
           case failure of
             Left (helpMsg, exit) -> (helpMsg, exit)
             Right failure -> renderFailure failure progn

     hPutStrLn stderr helpMsg
     exitWith exit

parsePartial :: ParserInfo a -> Args -> Either (ParserFailure ParserHelp) (a, Args)
parsePartial info a =
  let p = runParser (if infoIntersperse info then SkipOpts else AllowOpts) CmdStart (infoParser info) a
  in case runP p defaultPrefs of
    (Left err, ctxt) -> Left (parserFailure defaultPrefs info err ctxt)
    (Right (a, remaining), ctxt) -> Right (a, remaining)

main :: IO ()
main = do
  args <- getArgs

  let bareInfo = migrationCommandAndSubcommandOptions (pure ())
  case parsePartial bareInfo args of
    Left err ->
      do hPutStrLn stderr "Could not determine backend"
         showUsage (Right err) bareInfo
    Right ((BeamMigrationCommand {..}, _), _) ->
      do putStrLn $ "Loading backend '" ++ migrationCommandBackend ++ "'..."
         putStrLn $ "Loading dirs " ++ show migrationCommandPackagePath
         let ghciArgs = foldMap (\p -> [ "-package-db " <>  p]) migrationCommandPackagePath
             runInterpreter' :: InterpreterT IO a -> IO (Either InterpreterError a)
             runInterpreter' = unsafeRunInterpreterWithArgs ghciArgs
         res <- runInterpreter' $ do
           unsafeSetGhcOption "-v"

           setImports [ "Database.Beam.Migrate.Tool",  migrationCommandBackend ]
           interpret "SomeBeamMigrationBackend migrationBackend" (undefined :: SomeBeamMigrationBackend)
         case res of
           Left err -> hPutStrLn stderr ("Plugin load error: " ++ show err)
           Right (SomeBeamMigrationBackend be@(BeamMigrationBackend { ..} :: BeamMigrationBackend be cmdSyntax beOptions)) ->
             do let withBackendInfo = migrationCommandAndSubcommandOptions backendOptsParser
                (opts, subcommand) <- execParser withBackendInfo
                case (migrationCommandMigrationModule, subcommand) of
                  (Just migrationCommandMigrationModule, Just subcommand) ->
                    do putStrLn "Loading migration from migrations directory..."
                       res <- runInterpreter' $ do
                         reset
                         set [ languageExtensions := [ TypeFamilies, GADTs, RankNTypes, FlexibleInstances, FlexibleContexts, DeriveGeneric
                                                     , ScopedTypeVariables, MultiParamTypeClasses, OverloadedStrings ] ]
                         loadModules [ migrationCommandMigrationModule ]
                         setImports [ "Data.Dynamic"
                                    , "Database.Beam.Migrate.Types"
                                    , "Database.Beam.Migrate.Tool"
                                    , migrationCommandBackend ]
                         setTopLevelModules [ migrationCommandMigrationModule ]
                         migration <- interpret "toDyn (eraseMigrationType () migration)" (undefined :: Dynamic)
                         checkedDb <- interpret "SomeCheckedDatabase (evaluateDatabase migration)" (undefined :: SomeCheckedDatabase be)
                         pure (migration, checkedDb)
                       case res of
                         Left err -> do hPutStrLn stderr "Plugin error: could not load migrations: "
                                        case err of
                                          WontCompile errs -> mapM_ (hPutStrLn stderr . errMsg) errs
                                          _ -> hPutStrLn stderr (show err)
                         Right (migration, checkedDb) ->
                           case (,) <$> fromDynamic migration <*> pure checkedDb of
                             Nothing -> hPutStrLn stderr "Migration did not have correct type"
                             Just (migration :: MigrationSteps cmdSyntax () (), checkedDb :: SomeCheckedDatabase be) ->
                               invokeMigrationTool be opts subcommand migration checkedDb
                  (Nothing, _) -> showUsage (Left ("No migration module supplied", ExitFailure 1)) withBackendInfo
                  (_, Nothing) -> showUsage (Left ("Please specify a command", ExitFailure 1)) withBackendInfo
