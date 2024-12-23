module RunMigration where

import Prelude
import Database.PostgreSQL.Simple (connect, close, ConnectInfo(..))
import Pagila.Schema (migrateDB)

-- https://hackage.haskell.org/package/postgresql-simple-0.7.0.0/docs/Database-PostgreSQL-Simple.html#t:ConnectInfo
connInfo :: ConnectInfo
connInfo = ConnectInfo "localhost" (read "5432") "postgres" "foo" "postgres"

main :: IO ()
main = do
  putStrLn "Running migration..."
  c <- connect connInfo

  _ <- migrateDB c

  close c
