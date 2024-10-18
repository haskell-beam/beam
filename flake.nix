{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/85bcb95aa83be667e562e781e9d186c57a07d757";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { nixpkgs, flake-utils, self, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
          beamLib = import ./nix/lib.nix { nixpkgs = pkgs; };
          beamGhc = beamLib.makeBeamGhc pkgs.haskellPackages;

          shellWithNativeInputs = nis: beamGhc.shellFor {
            packages = beamLib.beamPackageList;
            nativeBuildInputs = nis;
          };
      in rec {
        devShells.default = shellWithNativeInputs (with pkgs; [
            postgresql sqlite-interactive
            cabal-install haskell-language-server
        ]);

        devShells.build = shellWithNativeInputs []; # Shell only for building
        devShells.test = shellWithNativeInputs (with pkgs; [ postgresql sqlite-interactive ]);

        devShells.docs = shellWithNativeInputs (with pkgs; [ postgresql sqlite-interactive
                                                             (python3.withPackages (ps: with ps; [ mkdocs mkdocs-material sqlparse ])) ]);
      });
}
