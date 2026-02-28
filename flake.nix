{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    testcontainers.url =
      "github:testcontainers/testcontainers-hs/e286bd2ba9747c2d8c3756a3a89910e579e318e3";
    testcontainers.flake = false;

    duckdb-ffi.url = "https://hackage.haskell.org/package/duckdb-ffi-1.4.1.5/duckdb-ffi-1.4.1.5.tar.gz";
    duckdb-ffi.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, config, ... }:
        let
          chinookPostgresRaw = pkgs.fetchurl {
            url = "https://raw.githubusercontent.com/lerocha/chinook-database/e7e6d5f008e35d3f89d8b8a4f8d38e3bfa7e34bd/ChinookDatabase/DataSources/Chinook_PostgreSql.sql";
            sha256 = "sha256-CVQAyq0WlAn7+0d72nsm9krVDLtMA1QcgHJhwdttNC4=";
          };

          chinookPostgres = pkgs.runCommand "chinook-postgres" {} ''
            ${pkgs.glibc.bin}/bin/iconv -f ISO-8859-2 -t UTF-8 ${chinookPostgresRaw} > $out
          '';

          chinookSqliteRaw = pkgs.fetchurl {
            url = "https://raw.githubusercontent.com/lerocha/chinook-database/e7e6d5f008e35d3f89d8b8a4f8d38e3bfa7e34bd/ChinookDatabase/DataSources/Chinook_Sqlite.sql";
            sha256 = "sha256-Zu+IP8fhmYwpgofjtMJLvL8jFRlKJ43mjLANivq6Q9s=";
          };

          chinookSqlite = pkgs.runCommand "chinook-sqlite" {} ''
            tail -c +4 ${chinookSqliteRaw} > $out
          '';

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
            hp.microlens-th
          ]);

        in {

          haskellProjects.default = {
            basePackages = pkgs.haskell.packages.ghc910;

            packages = {
              testcontainers.source = inputs.testcontainers;
              duckdb-ffi.source = inputs.duckdb-ffi;
            };
            settings = {
              testcontainers.check = false;
              beam-postgres.check = false;
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
            ];
          };

          packages.default = self'.packages.all;

          packages.docs = pkgs.stdenv.mkDerivation {
            pname = "beam-docs";
            version = "0";
            src = pkgs.nix-gitignore.gitignoreFilterSource
              (path: type: let
                prefix = "${builtins.toPath ./.}/";
                strippedPath = pkgs.lib.removePrefix prefix path;
              in pkgs.lib.or
                (builtins.elem strippedPath [
                  "beam-postgres"
                  "beam-sqlite"
                ])
                (builtins.any (pkgs.lib.flip pkgs.lib.hasPrefix strippedPath) [
                  "build-docs.sh"
                  "mkdocs.yml"
                  "docs"
                  "beam-postgres/examples"
                  "beam-postgres/beam-docs.sh"
                  "beam-sqlite/examples"
                  "beam-sqlite/beam-docs.sh"
                ])
              )
              []
              ./.;

            nativeBuildInputs = [
              ghcWithBeam
              pythonEnv
              pkgs.postgresql
              pkgs.sqlite
              pkgs.curl
              pkgs.pv
            ];

            buildPhase = ''
              mkdir postgres
              export PGHOST=$(mktemp -d /tmp/pg.XXXXXX)
              initdb -D postgres
              echo "unix_socket_directories = '$PGHOST'" >> postgres/postgresql.conf
              pg_ctl -D postgres -o "-c listen_addresses=localhost" start
              trap "pg_ctl -D postgres stop -m immediate" EXIT

              mkdir -p docs/.beam-query-cache/chinook-data
              cp ${chinookPostgres} docs/.beam-query-cache/chinook-data/Chinook_PostgreSql.sql
              cp ${chinookSqlite} docs/.beam-query-cache/chinook-data/Chinook_Sqlite.sql
              CI=true BEAM_DOC_BACKEND="beam-postgres beam-sqlite" bash ./build-docs.sh builddocs
            '';

            installPhase = ''
              cp -r site $out
            '';
          };
        };
    };
}
