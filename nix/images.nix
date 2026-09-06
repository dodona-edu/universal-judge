# Image definitions. Built with dockerTools.streamLayeredImage; load with
# `nix build .#images.tested-core && ./result | docker load`.
{ pkgs, pyproject-nix ? null }:
let
  lib = pkgs.lib;
  manifest = import ./lib/manifest.nix { inherit (pkgs) lib; };
  depsDir = ../deps;
  packagesDir = ./packages;

  # Base files every image needs (pitfalls in section 6 of the plan).
  binShDash = pkgs.runCommand "bin-sh-dash" { } ''
    mkdir -p $out/bin
    ln -s ${pkgs.dash}/bin/dash $out/bin/sh
  '';

  baseContents = [
    binShDash
    pkgs.dockerTools.usrBinEnv
    pkgs.dockerTools.fakeNss
    pkgs.dockerTools.caCertificates
  ];

  fakeRootCommands = ''
    #!${pkgs.runtimeShell}
    mkdir -p tmp && chmod 1777 tmp
    mkdir -p mnt && chmod 711 mnt
    mkdir -p home/runner/workdir
    ${pkgs.shadow}/bin/groupadd -g 1000 runner || true
    ${pkgs.shadow}/bin/useradd -u 1000 -g 1000 -m -d /home/runner runner || true
    chown -R 1000:1000 home/runner
    cp ${../.devcontainer/main.sh} main.sh
    chmod 0755 main.sh
  '';

  mkImage =
    {
      name,
      manifestNames,
      withDev ? false,
      fromImage ? null,
    }:
    let
      chain = lib.concatMap (manifest.manifestChain depsDir) manifestNames;
      merged = manifest.merge depsDir chain;
      tools = manifest.resolveTools { inherit pkgs; inherit (pkgs) lib; inherit packagesDir; } merged.tools;
      pythonPkgs = manifest.resolvePythonPackages {
        python = pkgs.python312;
        inherit (pkgs) lib;
        inherit packagesDir;
      } merged.pythonPackages;
      py = import ./python.nix {
        inherit pkgs pyproject-nix withDev;
        extraPythonPackages = pythonPkgs;
      };
      runtimeEnv = pkgs.buildEnv {
        name = "${name}-env";
        paths = tools ++ [ py.env ];
        pathsToLink = [ "/bin" "/lib" "/share" ];
      };
      envList =
        [
          "PATH=${runtimeEnv}/bin:/bin:/usr/bin"
          "LANG=C.UTF-8"
          "LC_ALL=C.UTF-8"
          "NODE_PATH=/usr/lib/node_modules"
        ]
        ++ lib.mapAttrsToList (k: v: "${k}=${toString v}") merged.env;
    in
    pkgs.dockerTools.streamLayeredImage {
      inherit name fromImage;
      tag = "nix";
      contents = baseContents ++ [ runtimeEnv ];
      enableFakechroot = true;
      inherit fakeRootCommands;
      config = {
        User = "runner";
        WorkingDir = "/home/runner/workdir";
        Env = lib.unique envList;
      };
    };

  tested-core = mkImage {
    name = "tested-core";
    manifestNames = [ "core" ];
  };
in
{
  images = {
    inherit tested-core;
    tested-bash = mkImage {
      name = "tested-bash";
      manifestNames = [ "bash" ];
      fromImage = tested-core;
    };
    tested-python = mkImage {
      name = "tested-python";
      manifestNames = [ "python" ];
      fromImage = tested-core;
    };
    tested-all = mkImage {
      name = "tested-all";
      manifestNames = [ "core" "userland" "bash" "python" "dev" ];
      withDev = true;
    };
  };
}
