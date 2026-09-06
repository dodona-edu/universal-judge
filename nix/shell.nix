# Dev shells. `default` has every in-scope tool plus the dev group; the
# per-language shells carry only their own manifest chain plus `dev`.
{ pkgs, pyproject-nix ? null }:
let
  lib = pkgs.lib;
  manifest = import ./lib/manifest.nix { inherit (pkgs) lib; };
  depsDir = ../deps;
  packagesDir = ./packages;

  toolsFor =
    names:
    let merged = manifest.merge depsDir (lib.concatMap (manifest.manifestChain depsDir) names);
    in {
      inherit merged;
      tools = manifest.resolveTools { inherit pkgs; inherit (pkgs) lib; inherit packagesDir; } merged.tools;
      pythonPkgs = manifest.resolvePythonPackages {
        python = pkgs.python312;
        inherit (pkgs) lib;
        inherit packagesDir;
      } merged.pythonPackages;
    };

  mkShell =
    { names, withDev ? true }:
    let
      r = toolsFor names;
      py = import ./python.nix {
        inherit pkgs pyproject-nix withDev;
        extraPythonPackages = r.pythonPkgs;
      };
      envVars = r.merged.env;
    in
    pkgs.mkShellNoCC {
      packages = r.tools ++ [ py.env ];
      shellHook = lib.concatStringsSep "\n" (
        lib.mapAttrsToList (k: v: "export ${k}=${lib.escapeShellArg (toString v)}") envVars
      );
    };
in
{
  default = mkShell { names = [ "core" "userland" "bash" "python" "dev" ]; };
  bash = mkShell { names = [ "bash" "dev" ]; };
  python = mkShell { names = [ "python" "dev" ]; };
}
