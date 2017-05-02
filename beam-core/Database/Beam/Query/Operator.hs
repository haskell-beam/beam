module Database.Beam.Query.Operator
  ( (&&.), (||.), not_, div_, mod_

  , isTrue_, isNotTrue_
  , isFalse_, isNotFalse_
  , isUnknown_, isNotUnknown_
  ) where

import Database.Beam.Backend.SQL

import Database.Beam.Query.Internal


(&&.), (||.) :: IsSql92ExpressionSyntax syntax
             => QGenExpr context syntax s Bool
             -> QGenExpr context syntax s Bool
             -> QGenExpr context syntax s Bool
(&&.) = qBinOpE andE
(||.) = qBinOpE orE

infixr 3 &&.
infixr 2 ||.

not_ :: forall syntax context s.
        IsSql92ExpressionSyntax syntax
     => QGenExpr context syntax s Bool
     -> QGenExpr context syntax s Bool
not_ (QExpr a) = QExpr (notE a)

mod_, div_ ::
  (Integral a, IsSql92ExpressionSyntax syntax) =>
  QGenExpr context syntax s a -> QGenExpr context syntax s a -> QGenExpr context syntax s a
div_ = qBinOpE divE
mod_ = qBinOpE modE

isTrue_, isNotTrue_,
  isFalse_, isNotFalse_,
  isUnknown_, isNotUnknown_
    :: IsSql92ExpressionSyntax syntax =>
       QGenExpr context syntax s a -> QGenExpr context syntax s Bool
isTrue_ (QExpr s) = QExpr (isTrueE s)
isNotTrue_ (QExpr s) = QExpr (isNotTrueE s)
isFalse_ (QExpr s) = QExpr (isFalseE s)
isNotFalse_ (QExpr s) = QExpr (isNotFalseE s)
isUnknown_ (QExpr s) = QExpr (isUnknownE s)
isNotUnknown_ (QExpr s) = QExpr (isNotUnknownE s)

