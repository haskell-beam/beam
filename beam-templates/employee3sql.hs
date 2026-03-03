{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}

-- ! BUILD_COMMAND: runhaskell -package microlens-th -XStandaloneDeriving -XTypeSynonymInstances -XDeriveGeneric -XGADTs -XOverloadedStrings -XFlexibleContexts -XFlexibleInstances -XTypeFamilies -XTypeApplications -XAllowAmbiguousTypes -XPartialTypeSignatures  -I../../docs/beam-templates -fno-warn-partial-type-signatures
-- ! BUILD_DIR: beam-sqlite/examples/
-- ! EXTRA_DEPS: employee3common.hs employee3commonsql.hs
-- ! FORMAT: sql
module Main where

#include "employee3common.hs"
#include "employee3commonsql.hs"

     (do
        let putStrLn :: String -> IO ()
            putStrLn _ = pure ()

            print _ = pure ()

        BEAM_PLACEHOLDER
       )
