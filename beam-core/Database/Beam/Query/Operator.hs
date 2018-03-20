module Database.Beam.Query.Operator
  ( -- ** General-purpose operators
    (&&.), (||.), not_, div_, mod_

  , like_, similarTo_

  , concat_
  ) where

import           Database.Beam.Backend.SQL

import           Database.Beam.Query.Internal

import           Control.Applicative

import qualified Data.Text as T

-- | SQL @AND@ operator
(&&.) :: IsSql92ExpressionSyntax syntax
      => QGenExpr context syntax s Bool
      -> QGenExpr context syntax s Bool
      -> QGenExpr context syntax s Bool
(&&.) = qBinOpE andE

-- | SQL @OR@ operator
(||.) :: IsSql92ExpressionSyntax syntax
      => QGenExpr context syntax s Bool
      -> QGenExpr context syntax s Bool
      -> QGenExpr context syntax s Bool
(||.) = qBinOpE orE

infixr 3 &&.
infixr 2 ||.

-- | SQL @LIKE@ operator
like_ ::
  ( IsSqlExpressionSyntaxStringType syntax text
  , IsSql92ExpressionSyntax syntax ) =>
  QExpr syntax s text -> QExpr syntax s text -> QExpr syntax s Bool
like_ (QExpr scrutinee) (QExpr search) =
  QExpr (liftA2 likeE scrutinee search)

-- | SQL99 @SIMILAR TO@ operator
similarTo_ ::
  ( IsSqlExpressionSyntaxStringType syntax text
  , IsSql99ExpressionSyntax syntax ) =>
  QExpr syntax s text -> QExpr syntax s text -> QExpr syntax s text
similarTo_ (QExpr scrutinee) (QExpr search) =
  QExpr (liftA2 similarToE scrutinee search)

-- | SQL @NOT@ operator
not_ :: forall syntax context s.
        IsSql92ExpressionSyntax syntax
     => QGenExpr context syntax s Bool
     -> QGenExpr context syntax s Bool
not_ (QExpr a) = QExpr (fmap notE a)

-- | SQL @/@ operator
div_ :: (Integral a, IsSql92ExpressionSyntax syntax)
     => QGenExpr context syntax s a -> QGenExpr context syntax s a
     -> QGenExpr context syntax s a
div_ = qBinOpE divE

-- | SQL @%@ operator
mod_ :: (Integral a, IsSql92ExpressionSyntax syntax)
     => QGenExpr context syntax s a -> QGenExpr context syntax s a
     -> QGenExpr context syntax s a
mod_ = qBinOpE modE

-- | SQL @CONCAT@ function
concat_ :: IsSql99ConcatExpressionSyntax syntax
        => [ QGenExpr context syntax s T.Text ] -> QGenExpr context syntax s T.Text
concat_ es = QExpr (concatE <$> mapM (\(QExpr e) -> e) es)
