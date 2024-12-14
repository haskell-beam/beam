module Main where

import Database.Beam.Migrate (stepNames)
import Database.Beam.Migrate.Simple (backendMigrationStepsScript)
import Database.Beam.Postgres.Syntax (pgRenderSyntaxScript, fromPgCommand, pgCommandType)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Backend.SQL ( BeamSqlBackendSyntax )
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import Data.Text (unpack)
import Pagila.Schema (migration)

main :: IO ()
main = do
  putStrLn "Migration steps:"
  mapM_ (putStrLn . unpack) (stepNames migration)
  putStrLn "-------------"
  putStrLn "For each migration step, the sequence of SQL scripts:"
  let
    renderer :: BeamSqlBackendSyntax Postgres -> String
    renderer syntax = "SQL command type: " <> commandType <> "\n"
      <> "SQL script: \n" <> sqlScript
      where
      commandType = show . pgCommandType $ syntax
      sqlScript = TL.unpack . TL.decodeUtf8 . pgRenderSyntaxScript . fromPgCommand $ syntax
  putStrLn $ backendMigrationStepsScript renderer migration
