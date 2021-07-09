{ nixpkgs ? import <nixpkgs> {}
, nixpkgs-docs ? import ./pinned-docs.nix {}
, ghc ? nixpkgs.ghc
, docs ? false
}:

let
  python37Packages = nixpkgs-docs.python37Packages;
  fetchPypi = python37Packages.fetchPypi;

  pep562 = python37Packages.callPackage ./pep562 {};
  pymdown-extensions = python37Packages.callPackage ./pymdown-extensions { inherit pep562; };
  mkdocs = python37Packages.callPackage ./mkdocs {};
  mkdocs-minify-plugin = python37Packages.callPackage ./mkdocs-minify-plugin { inherit mkdocs; };
  mkdocs-material = python37Packages.callPackage ./mkdocs-material { inherit pymdown-extensions mkdocs-minify-plugin mkdocs; };

  beamPython = nixpkgs-docs.python37.withPackages (ps: [ mkdocs mkdocs-material ps.sqlparse ps.pyyaml ]);

  docsRequirements = [
    beamPython
    mkdocs
  ] ++ (with nixpkgs-docs; [
    bash
    pv
    sqlite
    ncurses
    libcxx
    icu
    gcc
    mysql
    zlib
    openssl
    stack
    gnupg
    dos2unix
    vim
    pcre
    git
  ]);
in
  nixpkgs.haskell.lib.buildStackProject {
    inherit ghc;
    name = "beam-env";
    buildInputs = [ nixpkgs.postgresql ] ++ nixpkgs.stdenv.lib.optionals docs docsRequirements;
    LANG = "en_us.UTF-8";
    python = beamPython;
  }
