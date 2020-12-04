{-# LANGUAGE ConstraintKinds #-}

module Database.Beam.Backend.Types
  ( BeamBackend(..)

  , Exposed, Nullable

  ) where

import GHC.Types

-- | Class for all Beam backends
class BeamBackend be where
  -- | Requirements to marshal a certain type from a database of a particular backend
  type BackendFromField be :: Type -> Constraint

-- | newtype mainly used to inspect the tag structure of a particular
--   'Beamable'. Prevents overlapping instances in some case. Usually not used
--   in end-user code.
data Exposed x

-- | Support for NULLable Foreign Key references.
--
-- > data MyTable f = MyTable
-- >                { nullableRef :: PrimaryKey AnotherTable (Nullable f)
-- >                , ... }
-- >                 deriving (Generic, Typeable)
--
-- See 'Columnar' for more information.
data Nullable (c :: Type -> Type) x
