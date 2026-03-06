{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    testcontainers.url = "github:testcontainers/testcontainers-hs/e286bd2ba9747c2d8c3756a3a89910e579e318e3";
    testcontainers.flake = false;

    duckdb-ffi.url = "https://hackage.haskell.org/package/duckdb-ffi-1.4.1.5/duckdb-ffi-1.4.1.5.tar.gz";
    duckdb-ffi.flake = false;
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem =
        {
          self',
          pkgs,
          config,
          ...
        }:
        let
          pythonEnv = pkgs.python312.withPackages (ps: [
            ps.mkdocs
            ps.mkdocs-material
            ps.sqlparse
            ps.pyyaml
            ps.pygments
          ]);

          ghcWithBeam = config.haskellProjects.default.outputs.finalPackages.ghcWithPackages (hp: [
            hp.beam-core
            hp.beam-migrate
            hp.beam-postgres
            hp.beam-sqlite
            hp.beam-duckdb
            hp.microlens-th
          ]);

        in
        {

          haskellProjects.default = {
            basePackages = pkgs.haskell.packages.ghc910;

            packages = {
              testcontainers.source = inputs.testcontainers;
              duckdb-ffi.source = inputs.duckdb-ffi;
            };
            settings = {
              testcontainers.check = false;
              beam-postgres.check = false;
              beam-duckdb.check = false;
              duckdb-simple.check = false;
              duckdb-simple.jailbreak = true; # Version bounds are incompatible with GHC 9.10 it seems
              duckdb-ffi.broken = false;
              duckdb-ffi.check = false;
              duckdb-ffi.extraLibraries = [ pkgs.duckdb ];
            };

            devShell = {
              enable = true;

              hlsCheck.enable = false;

              tools = hp: {
                inherit (pkgs)
                  postgresql
                  sqlite-interactive
                  poetry
                  curl
                  pv
                  duckdb
                  ;
              };
            };
          };

          packages.all = pkgs.symlinkJoin {
            name = "all";
            paths = [
              self'.packages.beam-core
              self'.packages.beam-migrate
              self'.packages.beam-postgres
              self'.packages.pagila
              self'.packages.beam-sqlite
              self'.packages.beam-duckdb
            ];
          };

          packages.default = self'.packages.all;

          packages.docs = pkgs.stdenv.mkDerivation {
            pname = "beam-docs";
            version = "0";
            src = pkgs.nix-gitignore.gitignoreFilterSource (
              path: type:
              let
                prefix = "${builtins.toPath ./.}/";
                strippedPath = pkgs.lib.removePrefix prefix path;
              in
              pkgs.lib.or
                (builtins.elem strippedPath [
                  "beam-postgres"
                  "beam-sqlite"
                  "beam-duckdb"
                ])
                (
                  builtins.any (pkgs.lib.flip pkgs.lib.hasPrefix strippedPath) [
                    "build-docs.sh"
                    "mkdocs.yml"
                    "docs"
                    "docs/data"
                    "docs/data/schemas"
                    "beam-postgres/examples"
                    "beam-postgres/beam-docs.sh"
                    "beam-sqlite/examples"
                    "beam-sqlite/beam-docs.sh"
                    "beam-duckdb/beam-docs.sh"
                    "beam-duckdb/docs"
                  ]
                )
            ) [ ] ./.;

            nativeBuildInputs = [
              ghcWithBeam
              pythonEnv
              pkgs.postgresql
              pkgs.sqlite
              pkgs.curl
              pkgs.pv
              pkgs.duckdb
            ];

            buildPhase = ''
              # Required for DuckDB
              # We need the location of the source because DuckDB
              # has trouble finding data files in Nix (e.g. Parquet files)
              # unless the path is absolute
              export HOME=$(mktemp -d)
              export BEAM_SOURCE=$(pwd)

              mkdir -p docs/.beam-query-cache/data
              cp docs/data/exams.parquet docs/.beam-query-cache/data/exams.parquet

              mkdir -p docs/.beam-query-cache/chinook-data
              cp docs/data/schemas/Chinook_PostgreSql.sql docs/.beam-query-cache/chinook-data/Chinook_PostgreSql.sql
              cp docs/data/schemas/Chinook_Sqlite.sql docs/.beam-query-cache/chinook-data/Chinook_Sqlite.sql
              cp docs/data/schemas/Chinook_DuckDB.sql docs/.beam-query-cache/chinook-data/Chinook_DuckDB.sql

              mkdir postgres
              export PGHOST=$(mktemp -d /tmp/pg.XXXXXX)
              initdb -D postgres
              echo "unix_socket_directories = '$PGHOST'" >> postgres/postgresql.conf
              pg_ctl -D postgres -o "-c listen_addresses=localhost" start
              trap "pg_ctl -D postgres stop -m immediate" EXIT

              CI=true bash ./build-docs.sh builddocs
            '';

            installPhase = ''
              cp -r site $out
            '';
          };
        };
    };
}
