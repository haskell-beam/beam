{-# LANGUAGE ScopedTypeVariables #-}
module Database.Beam.Table where

import Database.Beam.Types

import Data.Text (Text)
import Data.Proxy

-- fieldNames :: Table t => Proxy t -> [Text]
-- fieldNames tableProxy = map tableFieldName (fieldsFor tableProxy)
--     where fieldsFor :: Table t => Proxy t -> [TableField t]
--           fieldsFor _ = fields

-- tableFieldName :: Table t => TableField t -> Text
-- tableFieldName x@(TableField field) = fieldName (tableProxyFor x) (proxyFor field)
--     where proxyFor :: a -> Proxy a
--           proxyFor _ = Proxy

--           tableProxyFor :: TableField t -> Proxy t
--           tableProxyFor _ = Proxy