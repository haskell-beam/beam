{ nixpkgs ? import ./nixpkgs/pinned.nix {} }: with nixpkgs;

let
  beamPackages = [
    "beam-core"
    "beam-migrate"
    "beam-migrate-cli"
    "beam-postgres"
    "beam-sqlite"
  ];
  ghcVersions = {
    ghc865 = haskell.packages.ghc865;
    ghc883 = haskell.packages.ghc883.extend (composeExtensionList [
      (pinHackageVersions {
        haskell-src-exts = "1.23.0";
      })
    ]);
  };
  baseHackageVersions = {
    hashable = "1.3.0.0";
    network = "2.6.3.1";
    postgresql-libpq = "0.9.4.2";
    tmp-postgres = "1.34.1.0";
    vector-sized = "1.4.0.0";
  };
  baseHackageDirectVersions = {
    generic-monoid = {
      version = "0.1.0.1";
      sha256 = "0n4ry5ag9m5hzvhph7zqrl2scqgy0x58mm88j6q4aqck9rzd8f3j";
    };
    sqlite-simple = {
      version = "0.4.18.0";
      sha256 = "1crp86argxqv5ryfiyj5v17a3wb8ngnb1zbhhx6d99i83skm5i86";
    };
  };

  composeExtensionList = lib.foldr lib.composeExtensions (_: _: {});
  mergeMaps = lib.foldr (a: b: a // b) {};
  applyToPackages = f: packages: _: super: lib.genAttrs packages
    (name: f super."${name}");

  pinHackageVersions = versions: self: _:
    lib.mapAttrs (n: v: self.callHackage n v {}) versions;
  pinHackageDirectVersions = versions: self: _:
    lib.mapAttrs (n: v: self.callHackageDirect {
      pkg = n;
      ver = v.version;
      sha256 = v.sha256;
    } {}) versions;

  mkPackageSet = ghc: ghc.extend (composeExtensionList [
    (pinHackageVersions baseHackageVersions)
    (pinHackageDirectVersions baseHackageDirectVersions)
    (self: _: lib.genAttrs beamPackages (name:
      self.callCabal2nix name (./. + "/${name}") {}
    ))
    (applyToPackages haskell.lib.dontCheck [
      "aeson"
      "network"
      "tmp-postgres"
    ])
    (_: super: {
      # Add postgresql binaries for tests:
      beam-postgres = haskell.lib.addBuildTool super.beam-postgres postgresql;
    })
  ]);
  mkPrefixedPackages = ghc: lib.mapAttrs'
    (name: value: lib.nameValuePair "${ghc.name}_${name}" value)
    (lib.genAttrs beamPackages (name: (mkPackageSet ghc.value)."${name}"));

in mergeMaps (map mkPrefixedPackages (lib.mapAttrsToList lib.nameValuePair ghcVersions))
