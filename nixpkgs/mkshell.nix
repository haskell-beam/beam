{ nixpkgs ? import <nixpkgs> {}, ghc ? nixpkgs.haskell.packages.ghc881.ghc, docs ? false }:
with nixpkgs;

let
  fetchPypi = python37Packages.fetchPypi;

  pep562 = python37Packages.callPackage ./pep562 {};
  pymdown-extensions = python37Packages.callPackage ./pymdown-extensions { inherit pep562; };
  mkdocs-minify-plugin = python37Packages.callPackage ./mkdocs-minify-plugin {};
  mkdocs-material = python37Packages.callPackage ./mkdocs-material { inherit pymdown-extensions mkdocs-minify-plugin; };

  beamPython = python37.withPackages (ps: [ pkgs.mkdocs mkdocs-material ps.sqlparse ps.pyyaml ]);

  docsRequirements = [ bash beamPython mkdocs pv sqlite
                       ncurses libcxx icu gcc mysql zlib openssl stack
                       gnupg dos2unix vim pcre ];
in
  haskell.lib.buildStackProject {
    inherit ghc;
    name = "beam-env";
    buildInputs = [ postgresql ] ++ stdenv.lib.optionals docs docsRequirements;
    LANG = "en_us.UTF-8";
    python = beamPython;
  }
