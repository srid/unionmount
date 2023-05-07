{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
  };
  outputs = inputs@{ nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
      ];

      perSystem = { config, self', pkgs, ... }: {
        haskellProjects.default = {
          devShell.tools = hp: {
            treefmt = config.treefmt.build.wrapper;
          } // config.treefmt.build.programs;
        };

        packages.default = self'.packages.unionmount;

        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;
          flakeFormatter = false; # For https://github.com/numtide/treefmt-nix/issues/55

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          # programs.cabal-fmt.enable = true; (Problematic now)

          # We use fourmolu
          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ];
          };
        };
      };
    };
}
