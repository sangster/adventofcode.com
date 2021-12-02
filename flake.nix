{
  description = "AdventOfCode runner";
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
    haskellNix.url = github:input-output-hk/haskell.nix;
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.aoc.flake {};

      overlays = [ haskellNix.overlay
        (final: prev: {
          aoc =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8104";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                hlint = {};
                haskell-language-server = {};
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [
                hlint # Haskell Linter
                nixpkgs-fmt # Format nix files
              ];
            };
        })
      ];
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."aoc:exe:aoc";
    });
}
