# Dev shells. `default` has every in-scope tool plus the dev group; the
# per-language shells carry only their own manifest chain plus `dev`.
{ pkgs }:
let
  lib = pkgs.lib;
  manifest = import ./lib/manifest.nix { inherit (pkgs) lib; };
  depsDir = ../deps;
  packagesDir = ./packages;

  languages = [ "bash" "python" "c" "cpp" "haskell" "java" "kotlin" "javascript" "typescript" "csharp" ];

  toolsFor =
    names:
    let merged = manifest.merge depsDir (lib.concatMap (manifest.manifestChain depsDir) names);
    in {
      inherit merged;
      tools = manifest.resolveTools { inherit pkgs; inherit (pkgs) lib; inherit packagesDir; } merged.tools;
    };

  mkShell =
    { names, withDev ? true }:
    let
      r = toolsFor names;
      py = import ./python.nix {
        inherit pkgs withDev;
        manifestPythonPackages = r.merged.pythonPackages;
      };
      envVars = r.merged.env;
    in
    pkgs.mkShellNoCC {
      packages = r.tools ++ [ py.env ];
      shellHook = lib.concatStringsSep "\n" (
        [ "export HOME=$(mktemp -d)  # isolate tool config (shellcheck, pylint, dotnet) from the host" ]
        ++ lib.mapAttrsToList (k: v: "export ${k}=${lib.escapeShellArg (toString v)}") envVars
      );
    };
in
{
  default = mkShell { names = [ "core" "userland" "dev" ] ++ languages; };
}
// lib.genAttrs languages (l: mkShell { names = [ l "dev" ]; })
