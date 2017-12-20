module Database.Beam.Query.Operator
  ( -- ** General-purpose operators
    (&&.), (||.), not_, div_, mod_

  , like_, similarTo_

  , isTrue_, isNotTrue_
  , isFalse_, isNotFalse_
  , isUnknown_, isNotUnknown_

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

-- | SQL @IS TRUE@ operator
isTrue_ :: IsSql92ExpressionSyntax syntax
        => QGenExpr context syntax s a -> QGenExpr context syntax s Bool
isTrue_ (QExpr s) = QExpr (fmap isTrueE s)

-- | SQL @IS NOT TRUE@ operator
isNotTrue_ :: IsSql92ExpressionSyntax syntax
           => QGenExpr context syntax s a -> QGenExpr context syntax s Bool
isNotTrue_ (QExpr s) = QExpr (fmap isNotTrueE s)

-- | SQL @IS FALSE@ operator
isFalse_ :: IsSql92ExpressionSyntax syntax
         => QGenExpr context syntax s a -> QGenExpr context syntax s Bool
isFalse_ (QExpr s) = QExpr (fmap isFalseE s)

-- | SQL @IS NOT FALSE@ operator
isNotFalse_ :: IsSql92ExpressionSyntax syntax
            => QGenExpr context syntax s a -> QGenExpr context syntax s Bool
isNotFalse_ (QExpr s) = QExpr (fmap isNotFalseE s)

-- | SQL @IS UNKNOWN@ operator
isUnknown_ :: IsSql92ExpressionSyntax syntax
           => QGenExpr context syntax s a -> QGenExpr context syntax s Bool
isUnknown_ (QExpr s) = QExpr (fmap isUnknownE s)

-- | SQL @IS NOT UNKNOWN@ operator
isNotUnknown_ :: IsSql92ExpressionSyntax syntax
              => QGenExpr context syntax s a -> QGenExpr context syntax s Bool
isNotUnknown_ (QExpr s) = QExpr (fmap isNotUnknownE s)

-- | SQL @CONCAT@ function
concat_ :: IsSql99ConcatExpressionSyntax syntax
        => [ QGenExpr context syntax s T.Text ] -> QGenExpr context syntax s T.Text
concat_ es = QExpr (concatE <$> mapM (\(QExpr e) -> e) es)
