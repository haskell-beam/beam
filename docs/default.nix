{ nixpkgs ? import ../nix/nixpkgs.nix {}
, ghc ? nixpkgs.haskell.packages.ghc88 # TODO: Fixes for GHC 9+
}:
with nixpkgs;

let
  beamLib = import ../nix/lib.nix { inherit nixpkgs; };
  beamGhc = beamLib.makeBeamGhc ghc;
  docsEnv = poetry2nix.mkPoetryEnv {
    projectDir = ./.;
  };
  chinookPostgresRaw = fetchurl {
    url = "https://raw.githubusercontent.com/lerocha/chinook-database/e7e6d5f008e35d3f89d8b8a4f8d38e3bfa7e34bd/ChinookDatabase/DataSources/Chinook_PostgreSql.sql";
    sha256 = "sha256-CVQAyq0WlAn7+0d72nsm9krVDLtMA1QcgHJhwdttNC4=";
  };
  chinookPostgres = runCommand "chinook-postgres" {} ''
    ${glibc.bin}/bin/iconv -f ISO-8859-2 -t UTF-8 ${chinookPostgresRaw} > $out
  '';
  chinookSqliteRaw = fetchurl {
    url = "https://raw.githubusercontent.com/lerocha/chinook-database/e7e6d5f008e35d3f89d8b8a4f8d38e3bfa7e34bd/ChinookDatabase/DataSources/Chinook_Sqlite.sql";
    sha256 = "sha256-Zu+IP8fhmYwpgofjtMJLvL8jFRlKJ43mjLANivq6Q9s=";
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
