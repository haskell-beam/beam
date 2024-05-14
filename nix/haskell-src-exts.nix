{ mkDerivation, array, base, containers, directory, filepath
, ghc-prim, happy, mtl, pretty, pretty-show, smallcheck, stdenv
, tasty, tasty-golden, tasty-smallcheck
}:
mkDerivation {
  pname = "haskell-src-exts";
  version = "1.21.1";
  sha256 = "0q1y8n3d82gid9bcx8wxsqqmj9mq11fg3gp5yzpfbw958dhi3j9f";
  libraryHaskellDepends = [ array base ghc-prim pretty ];
  libraryToolDepends = [ happy ];
  testHaskellDepends = [
    base containers directory filepath mtl pretty-show smallcheck tasty
    tasty-golden tasty-smallcheck
  ];
  doCheck = false;
  homepage = "https://github.com/haskell-suite/haskell-src-exts";
  description = "Manipulating Haskell source: abstract syntax, lexer, parser, and pretty-printer";
  license = stdenv.lib.licenses.bsd3;
}
