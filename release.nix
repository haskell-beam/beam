{ reflex-platform-fun ? import ./dep/reflex-platform
}:

let
  native-reflex-platform = reflex-platform-fun {};
  inherit (native-reflex-platform.nixpkgs) lib;
  systems = [ "x86_64-linux" "x86_64-darwin" ];

  perPlatform = lib.genAttrs systems (system: let
    reflex-platform = reflex-platform-fun { inherit system; };
    compilers = [
      "ghc"
      "ghcjs"
    ] ++ lib.optionals (reflex-platform.androidSupport) [
      "ghcAndroidAarch64"
      "ghcAndroidAarch32"
    ] ++ lib.optionals (reflex-platform.iosSupport) [
      "ghcIosAarch64"
    ];
    hsPkgs = lib.genAttrs compilers (ghc: let
      ghc' = reflex-platform.${ghc}.override {
        overrides = self: super: let
          haskellLib = native-reflex-platform.nixpkgs.haskell.lib;
        in {
          beam-core = self.callCabal2nix "beam-core" ./beam-core { };
          beam-migrate = self.callCabal2nix "beam-migrate" ./beam-migrate { };
          beam-migrate-cli = self.callCabal2nix "beam-migrate-cli" ./beam-migrate-cli { };
          beam-postgres = haskellLib.dontCheck (self.callCabal2nix "beam-postgres" ./beam-postgres { });
          beam-sqlite = self.callCabal2nix "beam-sqlite" ./beam-sqlite { };

          postgresql-simple = haskellLib.dontCheck
            (self.callCabal2nix "postgresql-simple" (reflex-platform.hackGet ./dep/postgresql-simple) {});
        };
      };
    in {
      inherit (ghc')
        beam-core
        ;
    } // lib.optionalAttrs (ghc == "ghc") {
      inherit (ghc')
        beam-migrate
        beam-migrate-cli
        beam-postgres
        beam-sqlite
        ;
    });
  in hsPkgs // {
    cache = reflex-platform.pinBuildInputs "reflex-${system}"
      (lib.concatLists (map builtins.attrValues (builtins.attrValues hsPkgs)));
  });

  metaCache = native-reflex-platform.pinBuildInputs "reflex-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform));

in perPlatform // { inherit metaCache; }
