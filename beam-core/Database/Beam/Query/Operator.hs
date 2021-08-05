module Database.Beam.Query.Operator
  ( SqlBool

    -- ** General-purpose operators
  , (&&.), (||.), not_, div_, mod_
  , (&&?.), (||?.), sqlNot_

  , like_, similarTo_
  , like_', similarTo_'

  , concat_
  ) where

import           Database.Beam.Backend.SQL

import           Database.Beam.Query.Internal

import           Control.Applicative

import qualified Data.Text as T

-- | Phantom type representing a SQL /Tri-state/ boolean -- true, false, and unknown
--
-- This type has no values because it cannot be sent to or retrieved
-- from the database directly. Use 'isTrue_', 'isFalse_',
-- 'isNotTrue_', 'isNotFalse_', 'isUnknown_', 'isNotUnknown_', and
-- `unknownAs_` to retrieve the corresponding 'Bool' value.
data SqlBool

-- | SQL @AND@ operator
(&&.) :: BeamSqlBackend be
      => QGenExpr context be s Bool
      -> QGenExpr context be s Bool
      -> QGenExpr context be s Bool
(&&.) = qBinOpE andE

-- | SQL @OR@ operator
(||.) :: BeamSqlBackend be
      => QGenExpr context be s Bool
      -> QGenExpr context be s Bool
      -> QGenExpr context be
      s Bool
(||.) = qBinOpE orE

-- | SQL @AND@ operator for 'SqlBool'
(&&?.) :: BeamSqlBackend be
       => QGenExpr context be s SqlBool
       -> QGenExpr context be s SqlBool
       -> QGenExpr context be s SqlBool
(&&?.) = qBinOpE andE

-- | SQL @OR@ operator
(||?.) :: BeamSqlBackend be
       => QGenExpr context be s SqlBool
       -> QGenExpr context be s SqlBool
       -> QGenExpr context be s SqlBool
(||?.) = qBinOpE orE

infixr 3 &&., &&?.
infixr 2 ||., ||?.

-- | SQL @LIKE@ operator
like_
  :: (BeamSqlBackendIsString be text, BeamSqlBackend be)
  => QGenExpr ctxt be s text
  -> QGenExpr ctxt be s text
  -> QGenExpr ctxt be s Bool
like_ = like_'

-- | SQL @LIKE@ operator but heterogeneous over the text type
like_'
  :: ( BeamSqlBackendIsString be left
     , BeamSqlBackendIsString be right
     , BeamSqlBackend be
     )
  => QGenExpr ctxt be s left
  -> QGenExpr ctxt be s right
  -> QGenExpr ctxt be s Bool
like_' (QExpr scrutinee) (QExpr search) =
  QExpr (liftA2 likeE scrutinee search)

-- | SQL99 @SIMILAR TO@ operator
similarTo_
  :: (BeamSqlBackendIsString be text, BeamSql99ExpressionBackend be)
  => QGenExpr ctxt be s text
  -> QGenExpr ctxt be s text
  -> QGenExpr ctxt be s text
similarTo_ = similarTo_'

-- | SQL99 @SIMILAR TO@ operator but heterogeneous over the text type
similarTo_'
  :: ( BeamSqlBackendIsString be left
     , BeamSqlBackendIsString be right
     , BeamSql99ExpressionBackend be
     )
  => QGenExpr ctxt be s left
  -> QGenExpr ctxt be s right
  -> QGenExpr ctxt be s left
similarTo_' (QExpr scrutinee) (QExpr search) =
  QExpr (liftA2 similarToE scrutinee search)

infix 4 `like_`, `similarTo_`

-- | SQL @NOT@ operator
not_ :: forall be context s
      . BeamSqlBackend be
     => QGenExpr context be s Bool
     -> QGenExpr context be s Bool
not_ (QExpr a) = QExpr (fmap notE a)

-- | SQL @NOT@ operator, but operating on 'SqlBool' instead
sqlNot_ :: forall be context s
         . BeamSqlBackend be
        => QGenExpr context be s SqlBool
        -> QGenExpr context be s SqlBool
sqlNot_ (QExpr a) = QExpr (fmap notE a)

-- | SQL @/@ operator
div_ :: (Integral a, BeamSqlBackend be)
     => QGenExpr context be s a -> QGenExpr context be s a
     -> QGenExpr context be s a
div_ = qBinOpE divE

infixl 7 `div_`, `mod_`

-- | SQL @%@ operator
mod_ :: (Integral a, BeamSqlBackend be)
     => QGenExpr context be s a -> QGenExpr context be s a
     -> QGenExpr context be s a
mod_ = qBinOpE modE

-- | SQL @CONCAT@ function
concat_ :: BeamSql99ConcatExpressionBackend be
        => [ QGenExpr context be s T.Text ] -> QGenExpr context be s T.Text
concat_ es = QExpr (concatE <$> mapM (\(QExpr e) -> e) es)
