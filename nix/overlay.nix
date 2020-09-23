self: super:
let
  beam-repo = super.eulerBuild.allowedPaths {
    root = ../.;
    paths = [
      ../cabal.project
      ../beam-core
      ../beam-migrate
      ../beam-migrate-cli
      ../beam-postgres
      ../beam-sqlite
    ];
  };

  beam-core-path = "${beam-repo}/beam-core";
  beam-migrate-path = "${beam-repo}/beam-migrate";
  beam-postgres-path = "${beam-repo}/beam-postgres";
  beam-sqlite-path = "${beam-repo}/beam-sqlite";
in
super.eulerBuild.mkEulerHaskellOverlay self super
  (hself: hsuper:
    let 
      # needed for ClassA error in beam-migrate
      haskell-src-exts_1_21_1 =
        self.eulerBuild.fastBuildExternal {
          drv = hself.callPackage ./haskell-src-exts.nix { };
        };
    in {
      beam-migrate =
        self.eulerBuild.fastBuildExternal {
          drv = hself.callCabal2nix "beam-migrate" beam-migrate-path {
            haskell-src-exts = haskell-src-exts_1_21_1;
          };
        };

      # TODO: use this override (will cause a lot of rebuilds)
      # haskell-src-exts = hself.haskell-src-exts_1_21_1;

      # TODO: remove this override after enabled HSE above
      # needed for hspec-wai-json used in euler-api-order
      # dontCheck
      haskell-src-meta =
        self.eulerBuild.fastBuildExternal {
          drv = hsuper.haskell-src-meta.override {
            haskell-src-exts = haskell-src-exts_1_21_1;
          };
        };

      interpolate = self.eulerBuild.fastBuildExternal {
        drv = hsuper.interpolate;
      };

      beam-core = self.eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "beam-core" beam-core-path { };
      };
      beam-sqlite = self.eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "beam-sqlite" beam-sqlite-path { };
      };

      # dontCheck
      beam-postgres = self.eulerBuild.fastBuildExternal {
        drv = hself.callCabal2nix "beam-postgres" beam-postgres-path { };
      };
  })
