{ nixpkgs }: with nixpkgs;

rec {
  beamPackageNames = ghc: [
    "beam-core"
    "beam-migrate"
    "beam-postgres"
    "beam-sqlite"
    "beam-migrate-cli"
  ];
  ghcVersions = {
    inherit (haskell.packages) ghc88;
    inherit (haskell.packages) ghc810;
    inherit (haskell.packages) ghc90;
    inherit (haskell.packages) ghc92;
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
