{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

-- ! BUILD_COMMAND: runhaskell --ghc-arg=-fglasgow-exts -XStandaloneDeriving -XTypeSynonymInstances -XDeriveGeneric -XGADTs -XOverloadedStrings -XFlexibleContexts -XFlexibleInstances -XTypeFamilies -XTypeApplications -XAllowAmbiguousTypes -XPartialTypeSignatures  -I../../docs/beam-templates
-- ! BUILD_DIR: beam-sqlite/examples/
-- ! EXTRA_DEPS: employee3common.hs employee3commonsql.hs
-- ! FORMAT: sql
module Main where

#include "employee3common.hs"

     [ jamesOrder1, bettyOrder1, jamesOrder2 ] <-
       runBeamSqlite conn $ do
         runInsertReturningList $
           insertReturning (shoppingCartDb ^. shoppingCartOrders) $
           insertExpressions $
           [ Order default_ currentTimestamp_ (val_ (pk james)) (val_ (pk jamesAddress1)) nothing_
           , Order default_ currentTimestamp_ (val_ (pk betty)) (val_ (pk bettyAddress1)) (just_ (val_ (pk bettyShippingInfo)))
           , Order default_ currentTimestamp_ (val_ (pk james)) (val_ (pk jamesAddress1)) nothing_ ]
#include "employee3commonsql.hs"

     (do let putStrLn :: String -> IO ()
             putStrLn _ = pure ()

             print _ = pure ()

         BEAM_PLACEHOLDER
       )
