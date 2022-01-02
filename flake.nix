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

      apps = {
        aoc-wait = pkgs.writeShellScriptBin "aoc-wait" ''
          scriptName="$(basename "''${BASH_SOURCE[0]}")"

          usage() {
            cat <<EOF
          Usage: $scriptName [-hs] [-l FILE] [-t timeout] [arg...]

          Execute the application automatically upon file change.

          Available options:

          -h, --help      Print this help and exit.
          -l, --log       Copy STDOUT & STDERR to FILE.
          -s, --start     Run once when first starting.
          -t, --timeout   Kill app after N seconds.
          EOF
            exit
          }

          opts() {
            getopt -n "$scriptName" \
                   -o hl:st: \
                   -l help,log:,start,timeout: \
                   -- "$@"
          }

          parse_params() {
            eval set -- "$(opts "$@")"

            # default values of variables set from params
            at_start=0
            timeout=90
            logfile=""

            while :; do
              case "''${1-}" in
              -h | --help) usage ;;
              -s | --start) at_start=1 ;;
              -l | --log) logfile="$2"; shift ;;
              -t | --timeout) timeout="$2"; shift ;;
              --) shift; break ;;
              -?*) die "Unknown option: $1" ;;
              *) break ;;
              esac
              shift
            done

            args=("$@")

            return 0
          }

          parse_params "$@"

          function run() {
            timeout "$timeout" stack run "''${args[@]}"
            echo ---
          }

          function logRun() {
            if [ -n "$logfile" ]; then
              run 2>&1 | ${pkgs.coreutils}/bin/tee "$logfile"
            else
              run
            fi
          }

          [ $at_start -eq 1 ] && logRun

          ${pkgs.inotify-tools}/bin/inotifywait -m \
              -q \
              -e CLOSE_WRITE \
              -r app \
              -r inputs \
              -r src \
              --exclude '/[.#].*' \
            | while read file; do
               echo "inotify event: $file"
               logRun
            done
        '';
      };
    });
}
