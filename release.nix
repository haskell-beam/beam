{ nixpkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "48723f48ab92381f0afd50143f38e45cf3080405";
    sha256 = "0h3b3l867j3ybdgimfn76lw7w6yjhszd5x02pq5827l659ihcf53";
  }) {}
}: with nixpkgs;

let
  beamPackages = [
    "beam-core"
    "beam-migrate"
    "beam-migrate-cli"
    "beam-postgres"
    "beam-sqlite"
  ];
  ghcVersions = [
    "ghc844"
    "ghc865"
    "ghc883"
  ];
  hackageVersions = {
    hashable = "1.3.0.0";
    network = "2.6.3.1";
    postgresql-libpq = "0.9.4.2";
    vector-sized = "1.4.0.0";
  };
  hackageDirectVersions = {
    sqlite-simple = {
      version = "0.4.18.0";
      sha256 = "1crp86argxqv5ryfiyj5v17a3wb8ngnb1zbhhx6d99i83skm5i86";
    };
  };

  composeExtensionList = lib.foldr lib.composeExtensions (_: _: {});
  mergeMaps = lib.foldr (a: b: a // b) {};
  applyToPackages = f: packages: _: super: lib.genAttrs packages
    (name: f super."${name}");

  mkPackageSet = ghc: ghc.extend (composeExtensionList [
    (self: _: lib.mapAttrs (n: v: self.callHackage n v {}) hackageVersions)
    (self: _: lib.mapAttrs (n: v: self.callHackageDirect {
      pkg = n;
      ver = v.version;
      sha256 = v.sha256;
    } {}) hackageDirectVersions)
    (self: _: lib.genAttrs beamPackages (name:
      self.callCabal2nix name (./. + "/${name}") {}
    ))
    (applyToPackages haskell.lib.dontCheck [
      "aeson"
      "network"
    ])
    (_: super: {
      # Add postgresql binaries for tests:
      beam-postgres = haskell.lib.addBuildTool super.beam-postgres postgresql;
    })
  ]);
  mkPrefixedPackages = version: lib.mapAttrs'
    (name: value: lib.nameValuePair "${version}_${name}" value)
    (lib.genAttrs beamPackages
      (name: (mkPackageSet haskell.packages."${version}")."${name}")
    );

in mergeMaps (map mkPrefixedPackages ghcVersions)
