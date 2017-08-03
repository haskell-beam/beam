module Database.Beam.Query.Unsafe where

import Database.Beam.Query.Internal

-- | Cast a 'QGenExpr' of one type into any other.
unsafeCast_ :: QGenExpr c syntax s a -> QGenExpr c syntax s b
unsafeCast_ (QExpr a) = QExpr a
