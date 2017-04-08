{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Postgres-specific types, functions, and operators
module Database.Beam.Postgres.PgSpecific where

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Postgres.Syntax
import           Database.Beam.Query.Internal

import           Data.Attoparsec.ByteString
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder
import           Data.Int
import           Data.List (intersperse)
import           Data.Monoid
import           Data.String
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.ToField as Pg

-- * TsVector type

newtype TsVectorConfig = TsVectorConfig ByteString
  deriving (Show, Eq, Ord, IsString)
newtype TsVector = TsVector ByteString
  deriving (Show, Eq, Ord)

-- | Postgres TypeInfo for tsvector
-- TODO Is the Oid stable from postgres instance to postgres instance?
tsvectorType :: Pg.TypeInfo
tsvectorType = Pg.Basic (Pg.Oid 3614) 'U' ',' "tsvector"

instance Pg.FromField TsVector where
  fromField field d =
    if Pg.typeOid field /= Pg.typoid tsvectorType
    then Pg.returnError Pg.Incompatible field ""
    else case d of
           Just d -> pure (TsVector d)
           Nothing -> Pg.returnError Pg.UnexpectedNull field ""

instance Pg.ToField TsVector where
  toField (TsVector d) =
    Pg.Many [ Pg.Plain "($$"
            , Pg.Plain (byteString d)
            , Pg.Plain "$$::tsvector)" ]

english :: TsVectorConfig
english = TsVectorConfig "english"

toTsVector :: IsSqlExpressionSyntaxStringType PgExpressionSyntax str
           => Maybe TsVectorConfig -> QGenExpr context PgExpressionSyntax s str
           -> QGenExpr context PgExpressionSyntax s TsVector
toTsVector Nothing (QExpr (PgExpressionSyntax x)) =
  QExpr . PgExpressionSyntax $
  emit "to_tsquery(" <> x <> emit ")"
toTsVector (Just (TsVectorConfig configNm)) (QExpr (PgExpressionSyntax x)) =
  QExpr . PgExpressionSyntax $
  emit "to_tsquery('" <> escapeString configNm <> emit "', " <> x <> emit ")"

-- * TsQuery type

newtype TsQuery = TsQuery ByteString
  deriving (Show, Eq, Ord)

tsqueryType :: Pg.TypeInfo
tsqueryType = Pg.Basic (Pg.Oid 3615) 'U' ',' "tsquery"

instance Pg.FromField TsQuery where
  fromField field d =
    if Pg.typeOid field /= Pg.typoid tsqueryType
    then Pg.returnError Pg.Incompatible field ""
    else case d of
           Just d -> pure (TsQuery d)
           Nothing -> Pg.returnError Pg.UnexpectedNull field ""
