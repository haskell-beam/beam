module Database.Beam.Query.Types
  ( Q, QExpr, QGenExpr(..), QExprToIdentity, QExprToField

  , Projectible, Aggregation

  , buildSqlQuery ) where

import Database.Beam.Syntax
import Database.Beam.Query.Internal
import Database.Beam.Query.SQL92

import Database.Beam.Schema.Tables

import Control.Monad.Identity
import Data.Vector.Sized (Vector)

type family QExprToIdentity x
type instance QExprToIdentity (table (QGenExpr context s)) = table Identity
type instance QExprToIdentity (table (Nullable c)) = Maybe (QExprToIdentity (table c))
type instance QExprToIdentity (QGenExpr context s a) = a
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
type instance QExprToIdentity (Vector n a) = Vector n (QExprToIdentity a)

-- TODO can this be unified with QExprToIdentity?
type family QExprToField x
type instance QExprToField (table (QGenExpr context s)) = table (QField s)
type instance QExprToField (table (Nullable (QGenExpr context s))) = table (Nullable (QField s))
type instance QExprToField (QGenExpr ctxt s a) = QField s a
type instance QExprToField () = ()
type instance QExprToField (a, b) = (QExprToField a, QExprToField b)
type instance QExprToField (a, b, c) = (QExprToField a, QExprToField b, QExprToField c)
type instance QExprToField (a, b, c, d) = (QExprToField a, QExprToField b, QExprToField c, QExprToField d)
type instance QExprToField (a, b, c, d, e) = (QExprToField a, QExprToField b, QExprToField c, QExprToField d, QExprToField e)
type instance QExprToField (a, b, c, d, e, f) =
  ( QExprToField a, QExprToField b, QExprToField c, QExprToField d
  , QExprToField e, QExprToField f )
type instance QExprToField (a, b, c, d, e, f, g) =
  ( QExprToField a, QExprToField b, QExprToField c, QExprToField d
  , QExprToField e, QExprToField f, QExprToField g )
type instance QExprToField (a, b, c, d, e, f, g, h) =
  ( QExprToField a, QExprToField b, QExprToField c, QExprToField d
  , QExprToField e, QExprToField f, QExprToField g, QExprToField h)
type instance QExprToField (Vector n a) = Vector n (QExprToField a)

-- TODO ocharles Move to beam-backend
buildSqlQuery :: Projectible ExpressionSyntax a => TablePrefix -> Q db s a -> SelectSyntax
buildSqlQuery = buildSql92Query' True
