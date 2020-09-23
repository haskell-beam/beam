{ nixpkgs }:
{
  flakeName = "beam";
  defaultPackageName = "beam-core";
  exportPackages = [
    "beam-core"
    "beam-migrate"
    "beam-migrate-cli"
    "beam-postgres"
    "beam-sqlite"
  ];

  shellTools =
    with nixpkgs; [
      stack
    ];
  # shellAttrs = {
  #   withHoogle = false;
  # }
}
