module Database.Beam.Migrate.SQL.BeamExtensions where

import Database.Beam.Backend.SQL
import Database.Beam.Migrate.SQL.SQL92
import Database.Beam.Migrate.SQL.Types

import Data.Text (Text)


class IsSql92ColumnSchemaSyntax syntax =>
  IsBeamSerialColumnSchemaSyntax syntax where
  genericSerial :: FieldReturnType 'False 'False syntax (SqlSerial Int) a => Text -> a
