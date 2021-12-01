# Inspiration: https://github.com/utdemir/nix-tree/blob/65dffe179b5d0fcf44d173ea2910f43ed187e136/flake.nix
{
  inputs = {
    flake-utils.url = github:numtide/flake-utils;
  };

  description = "AdventOfCode runner";

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = self: super: {
        aoc =
          super.symlinkJoin {
            name = "aoc";
            paths = [
              (super.haskell.lib.justStaticExecutables self.haskellPackages.aoc)
            ];
          };

        haskellPackages = super.haskellPackages.override {
          overrides = hself: hsuper: {
            aoc = hsuper.callCabal2nix "aoc" ./. {};
          };
        };
      };
    in
    { inherit overlay; } //
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
          config.allowUnfree = true; # TODO: Probably not the right spot for this.
          config.allowBroken = true;
        };
      in rec {
        packages = flake-utils.lib.flattenTree {
          inherit (pkgs) aoc;
        };
        defaultPackage = packages.aoc;

        apps = {
          aoc = flake-utils.lib.mkApp { drv = packages.aoc; };
        };
        defaultApp = apps.aoc;

        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p."aoc" ];

          # For `nix develop`
          # TODO: I'm not sure what all these do.
          buildinputs = with pkgs.haskellPackages; [
            cabal-install # TODO: What is this?
            haskell-language-server # TODO: What is this?
            ghcid # TODO: Haskell IDE integration?
            ormolu # Format haskell files
            hlint # Haskell Linter
            pkgs.nixpkgs-fmt # Format nix files
          ];
          withHoogle = false;
        };
      }
    );
}
