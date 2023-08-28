{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } ({ withSystem, ... }: {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem = { self', pkgs, lib, config, ... }: {
        haskellProjects.default = {
          projectFlakeName = "beam";
          basePackages = pkgs.haskell.packages.ghc927;
          autoWire = ["packages" "checks" "devShells" "apps"];
          settings = {
            pretty-simple = {
              check = false;
            };
          };
        };
      };
    });
}  
