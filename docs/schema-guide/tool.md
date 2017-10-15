- A valid migration backend must export a value `migrationBackend :: BeamMigrationBackend be commandSyntax`
- Use it by specifying the `--backend` along with the module i.e. `--backend Database.Beam.Postgres.Migration`

- A schema module must export a value `migration :: MigrationSteps syntax () a'`
- Use it by specifying the `-M` flag i.e. `-M Path.To.Schema`
- You need to run this from your `src` directory, if you have one
- Since `libpq` is used under the hood, evironment variables such as `PGPASSWORD` can be accessed. See more here:
  https://www.postgresql.org/docs/current/static/libpq-envars.html
  
- `beam-migrate init` to set up the database for beam migrations
- `beam-migrate migrate` to do the migrations

