{-# LANGUAGE TypeOperators, FlexibleInstances, UndecidableInstances, ScopedTypeVariables #-}
module Database.Beam.Schema.Database where

import Database.Beam.Schema

import Data.Proxy

instance (ToDatabaseSchema t1, ToDatabaseSchema t2) => ToDatabaseSchema (t1 :#: t2) where
    reifyDBSchema x = reifyDBSchema t1Proxy ++ reifyDBSchema t2Proxy
        where (t1Proxy, t2Proxy) = proxies x

              proxies :: Proxy (t1 :#: t2) -> (Proxy t1, Proxy t2)
              proxies _ = (Proxy, Proxy)
    tableNames x = tableNames t1Proxy ++ tableNames t2Proxy
        where (t1Proxy, t2Proxy) = proxies x

              proxies :: Proxy (t1 :#: t2) -> (Proxy t1, Proxy t2)
              proxies _ = (Proxy, Proxy)

instance Table t => ToDatabaseSchema (TableSchema t) where
    reifyDBSchema t = [(dbTableName tableProxy, reifyTableSchema tableProxy)]
        where tableProxy = tableProxy' t
              tableProxy' :: Proxy (TableSchema t) -> Proxy t
              tableProxy' _ = Proxy
    tableNames (t :: Proxy (TableSchema t)) = [GenTable (Proxy :: Proxy t)]