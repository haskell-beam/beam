-- | Defines type classes for 'Table's and 'Database's.
--
-- All important class methods of these classes can be derived automatically using 'Generic's and GHC's DefaultSignatures extension,
-- but you can override any method if necessary.
--
-- To get started, see 'Table', 'Columnar', and 'Nullable'.
module Database.Beam.Schema
    ( module X ) where

import Database.Beam.Schema.Tables as X
import Database.Beam.Schema.Fields as X
import Database.Beam.Schema.Lenses as X
