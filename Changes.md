# Beam in a Backpack
## Changes Walkthrough
 
In the order presented in GitHub's PR view:

### beam-core/Database/Beam/Backend/SQL.hs

* SQL2003 is no longer re-exported. This in turn means that SQL99 and SQL92
  aren't exported. beam-core is SQL '92 only, and this syntax lives in
  Database.Beam.Syntax.

* `MonadBeam` is now a type class over just and `m` and `handle`. `syntax` and
  `be` are now monomorphic types that are implicit to the instantion of
  `beam-core`. The other changes to this type class simply choose concrete types
  for `syntax` (as `Command`, from the signature `Database.Beam.Syntax`).

### beam-core/Database/Beam/Backend/SQL/AST.hs

* Deleted - this AST is only used to dump SQL queries, which is currently a
  feature that is lost with this change.

### beam-core/Database/Beam/Backend/SQL/BeamExtensions.hs

* Like the changes to `MonadBeam`, the `be` and `syntax` type parameters have
  been removed from the type classes, and concrete types from
  `Database.Beam.Syntax` have been chosen.

### beam-core/Database/Beam/Backend/SQL/Builder.hs

* Removed.

### beam-core/Database/Beam/Backend/SQL/SQL2003.hs

* This finally tagless encoding SQL 2003 syntax is now in
  `beam-sql2003/Database/Beam/Syntax.hsig`. Backpack signatures can be defined
  multiple times for the same module, and Backpack will mix them together. Thus
  if someone depends on `beam-sql2003` and `beam-core`, they must provide a
  `Database.Beam.Syntax` that supports SQL92 and SQL2003.

### beam-core/Database/Beam/Backend/SQL/SQL92.hs

* Rather than using type classes, this has been moved to a signature in
  `Database.Beam.Syntax`.

### beam-core/Database/Beam/Backend/SQL/SQL99.hs

* Rather than using type classes, this has been moved to a signature in
  `Database.Beam.Syntax` in the `beam-sql99` package.
  
### beam-core/Database/Beam/Backend/SQL/Types.hs

* Remove `BeamSqlBackend`. One instantiation of this library is essentially an
  instance of this type class.

* Drop the `be` parameter to `FromBackendRow`.

### beam-core/Database/Beam/Backend/Types.hs

* Drop `BeamBackend` (same reason as dropping `BeamSqlBackend`).

* `FromBackendRowF` no longer mentions `be`. This would previously direct
  `BackendFromField`, but that has been moved to the signature now, allowing it
  to vary per-backend.

* Remaining changes are fall out from the above.

### beam-core/Database/Beam/Backend/URI.hs

* Changes now that `MonadBeam` has changed.

### beam-core/Database/Beam/Query.hs

* `SqlBool` is exported as SQL 99 (or 2003) depend on it. There may be other
  options here, as this might be meant to be internal.

* `QGenExprTable` is no longer parameterised on `syntax`.

* `QExprTable` is no longer parameterised on `syntax`.

* `SqlSelect` is no longer parameterised on `syntax`.

* `QueryableSqlSyntax` removed as it's no longer necessary. It is implied by the types.

* `dumpSqlSelect` has been removed. It may be possible to bring this back, but
  it is more work. I think it can be done by adding some kind of `render ::
  Command -> String` function to `Database.Beam.Syntax`.

* `SqlInsert` is no longer parameterised by syntax, and instead chooses
  `InsertSyntax`.

* `SqlInsertValues` is no longer parameterised by syntax, and instead chooses
  `InsertValuesSyntax`.

* `SqlUpdate` is no longer parameterised by syntax, and instead chooses
  `UpdateSyntax`.

* `SqlDelete` is no longer parameterised by syntax, and instead chooses
  `DeleteSyntax`.

* I think everything else is just fall out from previous changes, and removing
  uses of the `IsSqlXX` type classes.

### beam-core/Database/Beam/Query/Aggregate.hs

* `rank_`, `cumeDist_`, `percentRank_`, `denseRank_`, `every_`, `any_`, `some_`,
  `everyOver_`, `anyOver_`, `someOver_` `filterWhere_` and `filterWhere_'` are
  now in `beam-sql2003`.

* I think everything else is removing uses of the `IsSqlXX` type classes.

### beam-core/Database/Beam/Query/Combinators.hs

* I had to export `SqlOrderable` for `beam-sql2003`, but maybe this should be in
  an `.Internal` module.

* `distinct_` is moved to `beam-sql99`

* Window functions are moved to `beam-sql2003`.

* NULLS FIRST and NULLS LAST support has moved to `beam-sql2003`.

### beam-core/Database/Beam/Query/CustomSQL.hs

* `IsCustomSqlSyntax` has been removed, and added directly into
  `Database.Beam.Syntax`. This makes the assumption that all backends must
  support custom syntax, but I think this is a good assumption.

### beam-core/Database/Beam/Query/Extensions.hs

* Moved to `beam-sql2003`, as everything here seems to assume SQL 2003.

### beam-core/Database/Beam/Query/Internal.hs

* `QF` is no longer parameterised by `syntax`. I had to reformat this
  declaration at the same time. I believe it is the same otherwise, but
  `QForceSelect` appears to be entirely unused.

* `Q` monad is no longer parameterised by `syntax`.

* `QAssignment` is no longer parameterised by `fieldName` and `expr` syntax,
  instead choosing these to be `FieldNameSyntax` and `ExpressionSyntax`.

* `ContextSyntax` has been added to allow a context to choose the syntax used in
  `QGenExpr`. This allows `QGenExpr` to vary what it contains, without needing
  yet another type parameter. The syntax always seems to be a function of the
  context.

* `QGenExpr` no longer has a `syntax` parameter, but the syntax it carries does
  still vary according to `context`.

* `QWindow`, `QFrameBounds` and `QFrameBound` moved to `beam-sql2003`.

* `Aggregation` was removed and seems to be unused. 

* `ContextRewritable` now has a `ContextCompatible` associated constraint. This
  ensures that if you rewrite the context of something, the underlying syntax is
  the same. I'm not sure if this is right, I don't entirely understand what this
  class is doing - but everything does work with this assumption.

### beam-core/Database/Beam/Query/Operator.hs

* `concat_` has moved to `beam-sql99`.

### beam-core/Database/Beam/Query/Ord.hs

* `QQuantified` is no longer parameterised by syntax and chooses the syntax to
  be `ComparisonQuantifierSyntax` and `ExpressionSyntax`.

### beam-core/Database/Beam/Query/SQL92.hs

* A lot of changes, but just chasing types from earlier changes - I think. Worth
  reviewing this!

### beam-core/Database/Beam/Query/Types.hs

* `buildSqlQuery` is no longer in a type class, and has been moved to
  `Database.Beam.Syntax.Build` (as just a signature).

### beam-core/Database/Beam/Schema/Tables.hs

* `Database` is no longer parameterised by `be`.

### beam-core/Database/Beam/Syntax.hsig

* The Backpack signature of SQL92 syntax. This is basically the original SQL92
  module, but rather than using type classes and associated types we choose
  top-level types and functions, but as this is a signature we aren't committing
  to anything in particular.
  
  I've tried to order this file the same as `SQL92`.

* `HasSqlValueSyntax` and `IsStringType` probably don't belong here, but
  it does mean we can avoid orphan instances.

* `autoSqlValueSyntax` is not here as it seems to be unused.
