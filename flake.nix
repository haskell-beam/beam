{
  description = "beam libraries";

  inputs = {
  };

  outputs = flakeInputs@{ self, euler-build, ... }:
    euler-build.mkEulerFlake {
      overlayPath = ./nix/overlay.nix;
      mkConfig = { nixpkgs }: {
        flakeName = "beam";
        defaultPackageName = "beam-core";
        exportPackages = [
          "beam-core"
          "beam-migrate"
          "beam-migrate-cli"
          "beam-postgres"
          "beam-sqlite"
        ];
        shellTools = with nixpkgs; [
          # haskellPackages.cabal-fmt
        ];
        # shellAttrs = {
        # };
      };
      inputs = flakeInputs;
    };
}
