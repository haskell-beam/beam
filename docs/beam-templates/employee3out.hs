{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- ! BUILD_COMMAND: runhaskell --ghc-arg=-fglasgow-exts -XStandaloneDeriving -XTypeSynonymInstances -XDeriveGeneric -XGADTs -XOverloadedStrings -XFlexibleContexts -XFlexibleInstances -XTypeFamilies -XTypeApplications -XAllowAmbiguousTypes -XPartialTypeSignatures -I../../docs/beam-templates -fno-warn-partial-type-signatures
-- ! BUILD_DIR: beam-sqlite/examples/
-- ! EXTRA_DEPS: employee3common.hs employee3commonout.hs
-- ! FORMAT: console
module Main where

#include "employee3common.hs"
#include "employee3commonout.hs"

     BEAM_PLACEHOLDER
