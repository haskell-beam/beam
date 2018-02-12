module Database.Beam.Query.Types
  ( Q, QExpr, QGenExpr(..), QExprToIdentity, QWindow, QWindowFrame

  , Projectible, Aggregation

  , HasQBuilder(..) ) where

import Database.Beam.Query.Internal
import Database.Beam.Query.SQL92

import Database.Beam.Schema.Tables

import Database.Beam.Backend.SQL.Builder
import Database.Beam.Backend.SQL.AST
import Database.Beam.Backend.SQL.SQL92

import Control.Monad.Identity
import Data.Vector.Sized (Vector)

type family QExprToIdentity x
type instance QExprToIdentity (table (QGenExpr context syntax s)) = table Identity
type instance QExprToIdentity (table (Nullable c)) = Maybe (QExprToIdentity (table c))
type instance QExprToIdentity (QGenExpr context syntax s a) = a
type instance QExprToIdentity ()     = ()
type instance QExprToIdentity (a, b) = (QExprToIdentity a, QExprToIdentity b)
type instance QExprToIdentity (a, b, c) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c)
type instance QExprToIdentity (a, b, c, d) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d)
type instance QExprToIdentity (a, b, c, d, e) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e)
type instance QExprToIdentity (a, b, c, d, e, f) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f)
type instance QExprToIdentity (a, b, c, d, e, f, g) =
  ( QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f
  , QExprToIdentity g)
type instance QExprToIdentity (a, b, c, d, e, f, g, h) =
  ( QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f
  , QExprToIdentity g, QExprToIdentity h )
type instance QExprToIdentity (a, b, c, d, e, f, g, h, i) =
  ( QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f
  , QExprToIdentity g, QExprToIdentity h, QExprToIdentity i )
type instance QExprToIdentity (a, b, c, d, e, f, g, h, i, j) =
  ( QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f
  , QExprToIdentity g, QExprToIdentity h, QExprToIdentity i, QExprToIdentity j)
type instance QExprToIdentity (a, b, c, d, e, f, g, h, i, j, k) =
  ( QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f
  , QExprToIdentity g, QExprToIdentity h, QExprToIdentity i, QExprToIdentity j, QExprToIdentity k)
type instance QExprToIdentity (a, b, c, d, e, f, g, h, i, j, k, l) =
  ( QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f
  , QExprToIdentity g, QExprToIdentity h, QExprToIdentity i, QExprToIdentity j, QExprToIdentity k, QExprToIdentity l )
type instance QExprToIdentity (Vector n a) = Vector n (QExprToIdentity a)

class IsSql92SelectSyntax selectSyntax => HasQBuilder selectSyntax where
  buildSqlQuery :: Projectible (Sql92SelectExpressionSyntax selectSyntax) a =>
                   TablePrefix -> Q selectSyntax db s a -> selectSyntax
instance HasQBuilder SqlSyntaxBuilder where
  buildSqlQuery = buildSql92Query' True
instance HasQBuilder Select where
  buildSqlQuery = buildSql92Query' True
