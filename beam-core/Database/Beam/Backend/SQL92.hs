module Database.Beam.Backend.SQL92 where

import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL
import Database.Beam.SQL.Types
import Data.Int

class ( BeamSqlBackend be
      , FromBackendLiteral be Int, FromBackendLiterals be Int ) =>
      BeamSql92Backend be
