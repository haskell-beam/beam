module RunMigration where

import Prelude
import Database.PostgreSQL.Simple (connect, close, ConnectInfo(..))
import Pagila.Schema (migrateDB)

-- https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#t:ConnectInfo
connInfo :: ConnectInfo
connInfo = ConnectInfo "localhost" (read "5432") "postgres" "foo" "postgres"

main :: IO ()
main = do
  putStrLn "Pagila migration"

  putStrLn "**This will overwrite data in your Postgres instance**"
  putStrLn "Type 'migrate' to proceed."
  x <- getLine

  if x == "migrate" then do
    putStrLn "Running migration..."
    c <- connect connInfo

    _ <- migrateDB c

    close c
  else do
    putStrLn "Input was not 'migrate'; quitting"
