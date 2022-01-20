{ nixpkgs }: with nixpkgs;

rec {
  beamPackageNames = ghc: [
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
    ghc865 = haskell.packages.ghc865Binary.extend (composeExtensionList [
      (_: super: {
       # Similar weird library issue as with beam-migrate-cli:
        constraints-extras = haskell.lib.disableCabalFlag
          super.constraints-extras
          "build-readme";
      })
      (applyToPackages haskell.lib.doJailbreak [
        "mono-traversable"
      ])
    ]);
    inherit (haskell.packages) ghc884;
    inherit (haskell.packages) ghc8107;
    ghc901 = haskell.packages.ghc901.extend (composeExtensionList [
      (applyToPackages haskell.lib.doJailbreak [
        "pqueue"
      ])
    ]);
    ghc921 = haskell.packages.ghc921.extend (composeExtensionList [
      (applyToPackages haskell.lib.doJailbreak [
        "postgresql-libpq"
        "postgresql-simple"
        "pqueue"
      ])
      (pinHackageVersions {
        "some" = "1.0.3";
        # This is not needed, but it tests the version bounds:
        "vector-sized" = "1.5.0";
      })
      (pinHackageDirectVersions {
        constraints-extras = {
          pkg = "constraints-extras";
          ver = "0.3.2.1";
          sha256 = "03hsja50vzflqqmvvxgc9w32dqg51dlw8i0blpqb2ipv7njx4q2q";
        };
        hint = {
          pkg = "hint";
          ver = "0.9.0.5";
          sha256 = "0x3yyq4vdpz4rqymbrq70swjpi0k6bnja0vhwlpgbgpzdb3ij7vc";
        };
      })
      (self: _: {
        # This is not needed, but it tests the version bounds:
        aeson = self.aeson_2_0_1_0;
      })
    ]);
  };

  composeExtensionList = lib.foldr lib.composeExtensions (_: _: {});
  applyToPackages = f: packages: _: super: lib.genAttrs packages
    (name: f super."${name}");

  pinHackageVersions = versions: self: _:
    lib.mapAttrs (n: v: self.callHackage n v {}) versions;
  pinHackageDirectVersions = versions: self: _:
    lib.mapAttrs (n: v: self.callHackageDirect v {}) versions;

  # Extend a package set with Beam packages
  makeBeamGhc = ghc: ghc.extend (composeExtensionList [
    (self: _: lib.genAttrs (beamPackageNames ghc) (name:
      self.callCabal2nix name (./.. + "/${name}") {}
    ))
    (_: super: {
      # Add postgresql binaries for tests:
      beam-postgres = haskell.lib.addBuildTool super.beam-postgres postgresql;
    })
  ]);
  # Filter a package set to just Beam packages
  filterBeamPackages = ghc: lib.genAttrs
    (beamPackageNames ghc)
    (name: ghc."${name}");
  beamPackageList = ghc: lib.attrValues (filterBeamPackages ghc);
}
