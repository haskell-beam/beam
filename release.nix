{ nixpkgs ? import ./nixpkgs/pinned.nix {} }: with nixpkgs;

let
  beamPackages = ghc: [
    "beam-core"
    "beam-migrate"
    "beam-postgres"
    "beam-sqlite"
  ] ++ lib.optionals (ghc.ghc.version != "8.6.5") [
    # For unclear reasons, this fails to build on 8.6.5 with missing dynamic
    # libraries. It's probably somehow related to it being a binary GHC
    # distribution as opposed to built normally with nix.
    "beam-migrate-cli"
  ];
  ghcVersions = {
    ghc865 = haskell.packages.ghc865Binary.extend (_: super: {
      ghc = super.ghc.overrideAttrs (drv: {
        passthru = drv.passthru // {
          # TODO: Backport this to 21.05 since it's fixed on master.
          haskellCompilerName = "ghc-8.6.5";
        };
      });
      # Similar weird library issue as with beam-migrate-cli:
      constraints-extras = haskell.lib.disableCabalFlag super.constraints-extras "build-readme";
    });
    ghc884 = haskell.packages.ghc884;
    ghc8104 = haskell.packages.ghc8104;
    ghc901 = haskell.packages.ghc901.extend (composeExtensionList [
      (_: super: {
        blaze-textual = haskell.lib.overrideCabal super.blaze-textual (_: {
          # https://github.com/bos/blaze-textual/pull/14
          src = fetchFromGitHub {
            owner = "bos";
            repo = "blaze-textual";
            rev = "c93b53a4aaad5a6ee2ddf90010957981d75d3579";
            sha256 = "0z0ky132j5bcs4i5wvsrd09ndny7jwsaxvaigw5jiszyibj0syyg";
          };
        });
        cryptonite = haskell.lib.disableCabalFlag super.cryptonite "integer-gmp";
      })
      (applyToPackages haskell.lib.dontCheck [
        "mono-traversable"
      ])
      (applyToPackages haskell.lib.doJailbreak [
        "blaze-textual"
        "cryptohash-md5"
        "cryptohash-sha1"
        "generic-monoid"
        "pqueue"
      ])
      (pinHackageVersions {
        memory = "0.16.0";
      })
    ]);
  };

  composeExtensionList = lib.foldr lib.composeExtensions (_: _: {});
  applyToPackages = f: packages: _: super: lib.genAttrs packages
    (name: f super."${name}");

  pinHackageVersions = versions: self: _:
    lib.mapAttrs (n: v: self.callHackage n v {}) versions;

  mkPackageSet = ghc: ghc.extend (composeExtensionList [
    (self: _: lib.genAttrs (beamPackages ghc) (name:
      self.callCabal2nix name (./. + "/${name}") {}
    ))
    (applyToPackages haskell.lib.unmarkBroken [
      "tmp-postgres"
    ])
    (applyToPackages haskell.lib.dontCheck [
      "tmp-postgres"
    ])
    (_: super: {
      # Add postgresql binaries for tests:
      beam-postgres = haskell.lib.addBuildTool super.beam-postgres postgresql;
    })
  ]);
  mkPrefixedPackages = ghcVersion: ghc:
    { recurseForDerivations = true; } //
    (lib.genAttrs (beamPackages ghc) (name: (mkPackageSet ghc)."${name}"));

in lib.mapAttrs mkPrefixedPackages ghcVersions
