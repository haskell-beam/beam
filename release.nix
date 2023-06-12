{ nixpkgs ? import ./nix/nixpkgs.nix {} }: with nixpkgs;

let
  beamLib = import ./nix/lib.nix { inherit nixpkgs; };
  beamGhcs = lib.mapAttrs (_: beamLib.makeBeamGhc) beamLib.ghcVersions;
  makePrefixedPackages = ghcVersion: ghc:
    { recurseForDerivations = true; } //
    (beamLib.filterBeamPackages ghc);

in lib.mapAttrs makePrefixedPackages beamGhcs // {
  docs = import ./docs { inherit nixpkgs; };
  shell = import ./shell.nix { inherit nixpkgs; };
}
