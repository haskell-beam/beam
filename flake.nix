{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";

    testcontainers.url =
      "github:testcontainers/testcontainers-hs/e286bd2ba9747c2d8c3756a3a89910e579e318e3";
    testcontainers.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {

        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc98;

          packages = {
            testcontainers.source = inputs.testcontainers;
          };
          settings = {
            testcontainers.check = false;
            beam-postgres.check = false;
          };

          devShell = {
           enable = true;

           hlsCheck.enable = false;

           tools = hp: {
             inherit (pkgs)
               postgresql
               sqlite-interactive
               poetry
               curl
               pv # http://www.ivarch.com/programs/pv.shtml
             ;
           };
          };
        };

        packages.all = pkgs.symlinkJoin {
          name = "all";
          paths = [
            self'.packages.beam-core
            self'.packages.beam-migrate
            self'.packages.beam-postgres
            self'.packages.pagila
            self'.packages.beam-sqlite
          ];
        };

        packages.default = self'.packages.all;

        # docs = import ./docs { inherit nixpkgs; };
      };
    };
}
