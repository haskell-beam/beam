{ nixpkgs }: with nixpkgs;

rec {
  beamPackageNames = ghc: [
    "beam-core"
    "beam-migrate"
    "beam-sqlite"
  ] ++ lib.optionals (builtins.compareVersions ghc.ghc.version "9.6" < 0) [
    # postgres-options doesn't yet support 9.6+
    "beam-postgres"
  ];
  ghcVersions = {
    inherit (haskell.packages) ghc88;
    inherit (haskell.packages) ghc810;
    inherit (haskell.packages) ghc90;
    inherit (haskell.packages) ghc92;
    ghc94 = haskell.packages.ghc94.extend (composeExtensionList [
      (self: _: {
        postgresql-simple = self.postgresql-simple_0_6_5;

        # These are just to test upper bounds:
        free = self.free_5_2;
        # Currently doctests for vector 0.13.0.0 fail.
        vector = haskell.lib.dontCheck self.vector_0_13_0_0;
        vector-algorithms = self.vector-algorithms_0_9_0_1;
      })
    ]);
    ghc96 = haskell.packages.ghc96.extend (composeExtensionList [
      (applyToPackages haskell.lib.doJailbreak [
        "pqueue"
      ])
    ]);
  };

  # Currently unused as we don't need any overrides with current nixpkgs
  # and GHC versions.
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
