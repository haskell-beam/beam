-- | Allows you to 'compile' complicated queries to a more compact
--   representation, for syntaxes that allow it.
module Database.Beam.Query.Compile where

class IsSplittableSyntax syntax where
  blh

class CompilableQuery  where
  compile :: CompilableQuery q => q -> CompileResult q
