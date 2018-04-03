module Database.Beam.Query.Operator where
  -- ( SqlBool

  --   -- ** General-purpose operators
  -- , (&&.), (||.), not_, div_, mod_
  -- , (&&?.), (||?.), sqlNot_

  -- , like_, similarTo_

  -- , concat_
  -- ) where

import           Database.Beam.Backend
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
(&&.) :: QGenExpr context s Bool
      -> QGenExpr context s Bool
      -> QGenExpr context s Bool
(&&.) = qBinOpE andE

-- | SQL @OR@ operator
(||.) :: QGenExpr context s Bool
      -> QGenExpr context s Bool
      -> QGenExpr context s Bool
(||.) = qBinOpE orE

-- | SQL @AND@ operator for 'SqlBool'
(&&?.) :: QGenExpr context s SqlBool
       -> QGenExpr context s SqlBool
       -> QGenExpr context s SqlBool
(&&?.) = qBinOpE andE

-- | SQL @OR@ operator
(||?.) :: QGenExpr context s SqlBool
       -> QGenExpr context s SqlBool
       -> QGenExpr context s SqlBool
(||?.) = qBinOpE orE

-- infixr 3 &&., &&?.
-- infixr 2 ||., ||?.

-- -- | SQL @LIKE@ operator
-- like_ ::
--   ( IsSqlExpressionSyntaxStringType syntax text
--   , IsSql92ExpressionSyntax syntax ) =>
--   QExpr syntax s text -> QExpr syntax s text -> QExpr syntax s Bool
-- like_ (QExpr scrutinee) (QExpr search) =
--   QExpr (liftA2 likeE scrutinee search)

-- -- | SQL99 @SIMILAR TO@ operator
-- similarTo_ ::
--   ( IsSqlExpressionSyntaxStringType syntax text
--   , IsSql99ExpressionSyntax syntax ) =>
--   QExpr syntax s text -> QExpr syntax s text -> QExpr syntax s text
-- similarTo_ (QExpr scrutinee) (QExpr search) =
--   QExpr (liftA2 similarToE scrutinee search)

-- | SQL @NOT@ operator
not_ :: QGenExpr context s Bool
     -> QGenExpr context s Bool
not_ (QExpr a) = QExpr (fmap notE a)

-- | SQL @NOT@ operator, but operating on 'SqlBool' instead
sqlNot_ :: QGenExpr context s SqlBool
        -> QGenExpr context s SqlBool
sqlNot_ (QExpr a) = QExpr (fmap notE a)

-- -- | SQL @/@ operator
-- div_ :: (Integral a, IsSql92ExpressionSyntax syntax)
--      => QGenExpr context syntax s a -> QGenExpr context syntax s a
--      -> QGenExpr context syntax s a
-- div_ = qBinOpE divE

-- -- | SQL @%@ operator
-- mod_ :: (Integral a, IsSql92ExpressionSyntax syntax)
--      => QGenExpr context syntax s a -> QGenExpr context syntax s a
--      -> QGenExpr context syntax s a
-- mod_ = qBinOpE modE

-- -- | SQL @CONCAT@ function
-- concat_ :: IsSql99ConcatExpressionSyntax syntax
--         => [ QGenExpr context syntax s T.Text ] -> QGenExpr context syntax s T.Text
-- concat_ es = QExpr (concatE <$> mapM (\(QExpr e) -> e) es)
