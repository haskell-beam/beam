{ nixpkgs ? import ../nix/nixpkgs.nix {}
, ghc ? nixpkgs.haskellPackages
}:
with nixpkgs;

let
  beamLib = import ../nix/lib.nix { inherit nixpkgs; };
  beamGhc = beamLib.makeBeamGhc ghc;
  docsEnv = poetry2nix.mkPoetryEnv {
    projectDir = ./.;
  };
  chinookPostgresRaw = fetchurl {
    url = "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_PostgreSql.sql";
    sha256 = "6945d59e3bca94591e2a96451b9bd69084b026f7fb7dbda3d15d06114ffb34c4";
  };
  chinookPostgres = runCommand "chinook-postgres" {} ''
    ${glibc.bin}/bin/iconv -f ISO-8859-2 -t UTF-8 ${chinookPostgresRaw} > $out
  '';
  chinookSqliteRaw = fetchurl {
    url = "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sql";
    sha256 = "b2e430ec8cb389509d25ec5bda2f958bbf6f0ca42e276fa5eb3de45eb816a460";
  };
  chinookSqlite = runCommand "chinook-sqlite" {} ''
    tail -c +4 ${chinookSqliteRaw} > $out
  '';

in

stdenv.mkDerivation {
  pname = "beam-docs";
  version = "0";
  src = nix-gitignore.gitignoreFilterSource
    (path: type: let
      prefix = "${builtins.toPath ./..}/";
      strippedPath = lib.removePrefix prefix path;
    in lib.or
      (builtins.elem strippedPath [
        "beam-postgres"
        "beam-sqlite"
      ])
      (builtins.any (lib.flip lib.hasPrefix strippedPath) [
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
    ./..;
  nativeBuildInputs = [
    (beamGhc.ghc.withPackages beamLib.beamPackageList)
    docsEnv
    poetry
    postgresql
    sqlite
    curl
    pv
  ];
  buildPhase = ''
    mkdir postgres tmp
    initdb -D postgres
    echo "unix_socket_directories = '$(pwd)/tmp'" >> postgres/postgresql.conf
    pg_ctl -D postgres start

    mkdir -p docs/.beam-query-cache/chinook-data
    cp ${chinookPostgres} docs/.beam-query-cache/chinook-data/Chinook_PostgreSql.sql
    cp ${chinookSqlite} docs/.beam-query-cache/chinook-data/Chinook_Sqlite.sql
    CI=true BEAM_DOC_BACKEND="beam-postgres beam-sqlite" bash ./build-docs.sh builddocs
  '';
  installPhase = ''
    cp -r site $out
  '';
}
