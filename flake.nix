let nixhash = import ./nix/nixhash.nix;
in {
  inputs.nixpkgs.url = "github:nixos/nixpkgs/${nixhash}";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { nixpkgs, flake-utils, self, ... }:
    flake-utils.lib.forEachSystem (system:
      let pkgs = import nixpkgs { inherit system; };
          beamLib = import ./nix/lib.nix { nixpkgs = pkgs; };
          beamGhc = beamLib.makeBeamGhc ghc;
      in {
        devShells.default = beamGhc.shellFor {
          packages = beamLib.beamPackageList;
          nativeBuildInputs = [
            postgresql sqlite-interactive
            cabal-install
          ];
        };
      });
}
