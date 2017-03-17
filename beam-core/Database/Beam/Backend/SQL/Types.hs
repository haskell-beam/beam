module Database.Beam.Backend.SQL.Types where

import Database.Beam.Backend.Types

class ( BeamBackend be
--      , FromBackendLiterals be SQLNull, FromBackendLiteral be SQLNull
      ) =>
      BeamSqlBackend be where

data SQLNull = SQLNull
               deriving (Show, Eq, Ord, Bounded, Enum)
