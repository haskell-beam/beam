-- | Defines type classes for 'Table's and 'Database's.
--
-- All important class methods of these classes can be derived automatically using 'Generic's and GHC's DefaultSignatures extension,
-- but you can override any method if necessary.
--
-- To get started, see 'Table', 'Columnar', and 'Nullable'.
module Database.Beam.Schema
    (
    -- * Database construction
    -- $db-construction
      Database

    , DatabaseSettings
    , DatabaseEntity

    -- ** #entities# Database entities
    -- $entities
    , TableEntity

    -- * Table construction
    , Table(..), Beamable
    , defTblFieldSettings, pk

    , Columnar, C, Columnar', Nullable
    , TableField, fieldName

    , TableSettings, HaskellTable

    -- * 'Generic'-deriving mechanisms
    , defaultDbSettings

    -- ** Modifying the derived schema
    , DatabaseModification, EntityModification, FieldModification
    , withDbModification, withTableModification
    , dbModification, tableModification
    , modifyTable, fieldNamed
    , setEntityName , modifyEntityName, modifyTableFields

    -- * Types for lens generation
    , Lenses, LensFor(..)

    , module Database.Beam.Schema.Lenses ) where

import Database.Beam.Schema.Tables
import Database.Beam.Schema.Lenses

-- $db-construction
-- Types and functions to express database types and auto-generate name mappings
-- for them. See the
-- [manual](https://haskell-beam.github.io/beam/user-guide/databases.md) for more
-- information.

-- $entities
-- Database entities represent things that can go into databases. Each entity in
-- your database that you want to access from Haskell must be given a field in
-- your database type. Each type of entity gets a particular entity tag, such as
-- 'TableEntity' or 'DomainTypeEntity'
