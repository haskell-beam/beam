{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    testcontainers.url = "github:testcontainers/testcontainers-hs/e286bd2ba9747c2d8c3756a3a89910e579e318e3";
    testcontainers.flake = false;

    # duckdb-simple 1.0 and its dependencies are not in nixpkgs yet.
    duckdb-ffi.url = "https://hackage.haskell.org/package/duckdb-ffi-2.0.0.0/duckdb-ffi-2.0.0.0.tar.gz";
    duckdb-ffi.flake = false;

    duckdb-simple.url = "https://hackage.haskell.org/package/duckdb-simple-1.0.0.0/duckdb-simple-1.0.0.0.tar.gz";
    duckdb-simple.flake = false;

    geometry-simple.url = "https://hackage.haskell.org/package/geometry-simple-0.1.1.0/geometry-simple-0.1.1.0.tar.gz";
    geometry-simple.flake = false;

    hs-bindgen-runtime.url = "https://hackage.haskell.org/package/hs-bindgen-runtime-1.0.0.0/hs-bindgen-runtime-1.0.0.0.tar.gz";
    hs-bindgen-runtime.flake = false;
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
          # duckdb-simple 1.0 requires exactly DuckDB 2.0.0
          duckdb =
            let
              version = "2.0.0";
              platform =
                {
                  x86_64-linux = "linux-amd64";
                  aarch64-linux = "linux-arm64";
                  x86_64-darwin = "osx-universal";
                  aarch64-darwin = "osx-universal";
                }
                .${pkgs.stdenv.hostPlatform.system};
              sha256 =
                {
                  shared-libs = {
                    linux-amd64 = "993ad89e7dfca66cd6da994c2cad090c2fc160ddf9f550498a098ab4bb13cfb1";
                    linux-arm64 = "c60eecf91143ca6bbac55c4f4d40dc22520488b0ac257c86e0fe9a2b451cf2b7";
                    osx-universal = "14cd63180614cfd961f7959146ecd9087527ff20b67311efdc3fa1c29c18204d";
                  };
                  cli = {
                    linux-amd64 = "c6507206baf7793c6013dbd82368469a45f20dc0e17a6f92f204602f652ed2bc";
                    linux-arm64 = "859e1875d2a9515fbb4bf8871061437a14af82320da68c753f094c4b776348fe";
                    osx-universal = "8a8f67e7c7f7d1392fa63ba3971948391392fe2a25fca63e7008e33c50beb4c2";
                  };
                };
              fetchRelease =
                kind:
                pkgs.fetchurl {
                  url = "https://github.com/duckdb/duckdb/releases/download/v${version}/duckdb-${kind}-${platform}.tar.gz";
                  sha256 = sha256.${kind}.${platform};
                };
              libraryName = if pkgs.stdenv.hostPlatform.isDarwin then "libduckdb.dylib" else "libduckdb.so";
            in
            pkgs.stdenv.mkDerivation {
              pname = "duckdb";
              inherit version;
              dontUnpack = true;
              dontStrip = true;
              nativeBuildInputs =
                pkgs.lib.optional pkgs.stdenv.hostPlatform.isLinux pkgs.autoPatchelfHook
                # Rewriting the install name invalidates the code signature, which
                # macOS enforces on Apple Silicon (the process is killed with SIGKILL).
                ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
                  pkgs.fixDarwinDylibNames
                  pkgs.darwin.autoSignDarwinBinariesHook
                ];
              buildInputs = pkgs.lib.optional pkgs.stdenv.hostPlatform.isLinux pkgs.stdenv.cc.cc.lib;
              installPhase = ''
                mkdir -p $out/bin $out/lib $out/include
                tar -xzf ${fetchRelease "shared-libs"} -C $out/lib ${libraryName}
                tar -xzf ${fetchRelease "shared-libs"} -C $out/include --wildcards '*.h'
                tar -xzf ${fetchRelease "cli"} -C $out/bin duckdb
              '';
              meta.mainProgram = "duckdb";
            };

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
              duckdb-simple.source = inputs.duckdb-simple;
              geometry-simple.source = inputs.geometry-simple;
              hs-bindgen-runtime.source = inputs.hs-bindgen-runtime;
            };
            settings = {
              testcontainers.check = false;
              beam-postgres.check = false;
              beam-duckdb.check = false;
              duckdb-simple.check = false;
              duckdb-ffi.broken = false;
              duckdb-ffi.check = false;
              duckdb-ffi.extraLibraries = [ duckdb ];
              geometry-simple.check = false;
              hs-bindgen-runtime.check = false;
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
                  ;
                inherit duckdb;
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
              duckdb
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
