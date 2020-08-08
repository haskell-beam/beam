module Database.Beam.Sqlite.Test where

import Control.Exception
import Database.SQLite.Simple

withTestDb :: (Connection -> IO a) -> IO a
withTestDb = bracket (open ":memory:") close
