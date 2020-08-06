module Database.Beam.Postgres.Extensions.Internal where

import Data.Text (Text)

import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Postgres

type PgExpr ctxt s = QGenExpr ctxt Postgres s

type family LiftPg ctxt s fn where
  LiftPg ctxt s (Maybe a -> b) = Maybe (PgExpr ctxt s a) -> LiftPg ctxt s b
  LiftPg ctxt s (a -> b) = PgExpr ctxt s a -> LiftPg ctxt s b
  LiftPg ctxt s a = PgExpr ctxt s a

funcE :: IsSql99ExpressionSyntax expr => Text -> [expr] -> expr
funcE nm args = functionCallE (fieldE (unqualifiedField nm)) args
