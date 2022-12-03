{
  description = "AdventOfCode runner";
  inputs.flake-utils.url = github:numtide/flake-utils;

  outputs = { self, nixpkgs, flake-utils }:
    let
      inherit (nixpkgs.lib) composeExtensions composeManyExtensions makeBinPath;

      name = "aoc";
      overlay = (_final: prev: rec {
        aoc = prev.haskell.lib.justStaticExecutables haskellPackages.aoc;

        haskellPackages = prev.haskellPackages.override (old: {
          overrides = composeExtensions
            (old.overrides or (_: _: {}))
            (_hfinal: hprev: {
              aoc = hprev.developPackage { inherit name; root = self; };
            });
        });
      });
    in
      { inherit overlay; } //
      flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };

          generate-sources = pkgs.writeShellScriptBin "generate-sources" ''
            export PATH="${with pkgs; lib.makeBinPath [ coreutils ]}:$PATH"
            ${pkgs.lib.readFile ./scripts/generate-sources.sh}
          '';
        in {
          packages.default = pkgs.aoc;
          apps.generate-sources = flake-utils.lib.mkApp {
            drv = generate-sources;
          };
          apps.aoc-wait = flake-utils.lib.mkApp {
            drv =
              let
                deps = with pkgs; [
                  coreutils
                  inotify-tools
                  generate-sources
                ];
              in pkgs.writeShellScriptBin "aoc-wait" ''
                export PATH="${pkgs.lib.makeBinPath deps}:$PATH"
                ${pkgs.lib.readFile ./scripts/aoc-wait.sh}
              '';
          };
        }
      );
}
