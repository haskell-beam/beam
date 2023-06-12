{ nixpkgs ? import ./nix/nixpkgs.nix {}
, ghc ? nixpkgs.haskellPackages
}:
with nixpkgs;

let
  beamLib = import ./nix/lib.nix { inherit nixpkgs; };
  beamGhc = beamLib.makeBeamGhc ghc;

in beamGhc.shellFor {
  packages = beamLib.beamPackageList;
  nativeBuildInputs = [
    postgresql
    sqlite-interactive
  ];
}
