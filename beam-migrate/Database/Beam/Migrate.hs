-- | Top-level module import for @beam-migrate@.
--
-- This is most often the only module you want to import, unless you're
-- extending @beam-migrate@, implementing migrations support in a backend, or
-- writing tooling. If you are doing any of these things, please see the
-- Advanced features section below.
--
-- The key abstractions in @beam-migrate@ are the /checked database/ and the
-- /migration/.
--
-- == Checked databases
--
-- A checked database is similar to the 'DatabaseSettings' type from
-- @beam-core@ except it comes with a set of /predicates/.
-- Whereas a 'DatabaseSettings' object consists of a set of
-- database entities, a checked database (represented by the
-- 'CheckedDatabaseSettings' type) consists of a set of database entities along
-- with a set of /predicates/ (represented by types implementing
-- 'DatabasePredicate'). The /predicates/ are facts about a given
-- database schema. For example, a checked database with a table named
-- "Customers", would have a 'TableExistsPredicate' in its predicate set.
--
-- Predicates can be used to verify that a given beam schema is compatible with
-- a backend database or to generate migrations from a schema satisfying one set
-- of predicates to a schema satisfying another. Beam migrate provides a solver
-- for figuring out the exact sequence of steps needed to accomplish this. Beam
-- backends can provide additional predicates and solvers to implement
-- backend-specific migrations. For example, the @beam-postgres@ backend
-- provides predicates to assert that extensions have been loaded, and solvers
-- for emitting proper @CREATE EXTENSION@ statements where appropriate.
--
-- Predicates may also be serialized to disk in JSON format, providing an easy
-- means to detect significant changes in a beam schema.
--
-- As one final point, @beam-migrate@ can generate schemas in any beam-supported
-- SQL DDL syntax. The @beam-migrate@ module provides a DDL syntax for Haskell
-- in "Database.Beam.Haskell.Syntax". This allows @beam-migrate@ to translate
-- predicate sets into the corresponding Haskell schema and the corresponding
-- Haskell migration script. This reflection allows tools based off of
-- @beam-migrate@ to support schema generation from an existing database.
--
-- For more information on checked databases, see the
-- "Database.Beam.Migrate.Checks" module.
--
-- == Migrations
--
-- A migration is a value of type 'MigrationSteps a b', where @a@ and @b@ are
-- database types. Conceptually, a value of this type is a list of DDL commands
-- which can be used to bring a database of type @a@ to a database of type @b@.
-- For example, if @b@ is a database type containing all the tables as @a@, but
-- with a new table added, a migration with type 'MigrationSteps a b' may
-- represent a SQL @CREATE TABLE@ statement. Migrations can sometimes be
-- reversed to produce a value of type 'MigrationSteps b a'. In our example, the
-- corresponding reversed migration may be the appropriate @DROP TABLE@
-- statement.
--
-- Migration steps can used to modify a database schema, generate a migration
-- script in a given backend syntax, or generate an appropriate
-- 'DatabaseSettings' object for use with the rest of the beam ecosystem.
--
-- For more information in migrations see "Database.Beam.Migrate.Types"
--
-- == Syntax
--
-- For low-level access to the underlying SQL syntax builders, see
-- "Database.Beam.Migrate.SQL.SQL92"
--
-- == Advanced features
--
-- If you are writing a new beam backend, you will need to construct a value of
-- type 'BeamBackend'. See that module for more information.
--
-- If you are writing tooling, you will likely need to consume 'BeamBackend'.
-- You will likely also be interested in the migration generation. See the
-- documentation on 'heuristicSolver'.


module Database.Beam.Migrate
 ( module Database.Beam.Migrate.Actions
 , module Database.Beam.Migrate.Checks
 , module Database.Beam.Migrate.Generics
 , module Database.Beam.Migrate.SQL
 , module Database.Beam.Migrate.Types
 ) where

import Database.Beam.Migrate.Actions
import Database.Beam.Migrate.Checks
import Database.Beam.Migrate.Generics
import Database.Beam.Migrate.SQL
import Database.Beam.Migrate.Types
