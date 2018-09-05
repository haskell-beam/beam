-- | Beam extensions are optional functionality that do not conform to any
-- standard and may have wildly different interpretations across backends.
--
-- In spite of these drawbacks, these are provided for the purposes of
-- pragmatism and convenience.
module Database.Beam.Migrate.SQL.BeamExtensions where

import Database.Beam.Backend.SQL
import Database.Beam.Migrate.SQL.Types
import Database.Beam.Migrate.SQL.Tables

import Data.Text (Text)

-- | Used to designate that a field should provide a default auto-incrementing value.
--
--   Usage:
--
-- @
-- field "Key" genericSerial
-- @
--
--   Then, when inserting into the table, you can use 'default_' to request the
--   database automatically assign a new value to the column. See
--   'runInsertReturning' for another Beam extension that may help if you want
--   to know which value was assigned.
--
--   Note that this is only provided for convenience. Backends often implement
--   auto-incrementing keys wildly differently. Many have restrictions on where
--   'genericSerial' may appear and may fail at run-time if these conditions
--   aren't met. Please refer to the backend of your choice for more
--   information.
class BeamMigrateSqlBackend be =>
  BeamSqlBackendHasSerial be where
  genericSerial :: FieldReturnType 'True 'False be (SqlSerial Int) a => Text -> a
