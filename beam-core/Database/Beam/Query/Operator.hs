module Database.Beam.Query.Operator
  ( -- ** General-purpose operators
    (&&.), (||.), not_, div_, mod_

  , like_, similarTo_

  , isTrue_, isNotTrue_
  , isFalse_, isNotFalse_
  , isUnknown_, isNotUnknown_
  ) where

import Database.Beam.Backend.SQL

import Database.Beam.Query.Internal

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
  QExpr (likeE scrutinee search)

-- | SQL99 @SIMILAR TO@ operator
similarTo_ ::
  ( IsSqlExpressionSyntaxStringType syntax text
  , IsSql99ExpressionSyntax syntax ) =>
  QExpr syntax s text -> QExpr syntax s text -> QExpr syntax s text
similarTo_ (QExpr scrutinee) (QExpr search) =
  QExpr (similarToE scrutinee search)

-- | SQL @NOT@ operator
not_ :: forall syntax context s.
        IsSql92ExpressionSyntax syntax
     => QGenExpr context syntax s Bool
     -> QGenExpr context syntax s Bool
not_ (QExpr a) = QExpr (notE a)

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
isTrue_ (QExpr s) = QExpr (isTrueE s)

-- | SQL @IS NOT TRUE@ operator
isNotTrue_ :: IsSql92ExpressionSyntax syntax
           => QGenExpr context syntax s a -> QGenExpr context syntax s Bool
isNotTrue_ (QExpr s) = QExpr (isNotTrueE s)

-- | SQL @IS FALSE@ operator
isFalse_ :: IsSql92ExpressionSyntax syntax
         => QGenExpr context syntax s a -> QGenExpr context syntax s Bool
isFalse_ (QExpr s) = QExpr (isFalseE s)

-- | SQL @IS NOT FALSE@ operator
isNotFalse_ :: IsSql92ExpressionSyntax syntax
            => QGenExpr context syntax s a -> QGenExpr context syntax s Bool
isNotFalse_ (QExpr s) = QExpr (isNotFalseE s)

-- | SQL @IS UNKNOWN@ operator
isUnknown_ :: IsSql92ExpressionSyntax syntax
           => QGenExpr context syntax s a -> QGenExpr context syntax s Bool
isUnknown_ (QExpr s) = QExpr (isUnknownE s)

-- | SQL @IS NOT UNKNOWN@ operator
isNotUnknown_ :: IsSql92ExpressionSyntax syntax
              => QGenExpr context syntax s a -> QGenExpr context syntax s Bool
isNotUnknown_ (QExpr s) = QExpr (isNotUnknownE s)

