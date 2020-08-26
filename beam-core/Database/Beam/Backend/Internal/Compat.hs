-- | This module contains utilities that backend writers can use to assist with
-- compatibility and breaking API changes.
--
-- Users should not need anything from this module.
module Database.Beam.Backend.Internal.Compat where

import GHC.TypeLits

-- | A type error directing the user to use an explicitly sized integers,
-- instead of 'Int' or 'Word'.
type PreferExplicitSize implicit explicit =
  'Text "The size of " ':<>:
  'ShowType implicit ':<>:
  'Text " is machine-dependent. Use an explicitly sized integer such as " ':<>:
  'ShowType explicit ':<>:
  'Text " instead."
