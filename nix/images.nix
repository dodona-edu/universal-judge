# Image definitions. Built with dockerTools.streamLayeredImage; load with
# `nix build .#images.tested-core && ./result | docker load`.
{ pkgs }:
let
  lib = pkgs.lib;
  manifest = import ./lib/manifest.nix { inherit (pkgs) lib; };
  depsDir = ../deps;
  packagesDir = ./packages;

  languages = [ "bash" "python" "c" "cpp" "haskell" "java" "kotlin" "javascript" "typescript" "csharp" ];

  # Base files every image needs (pitfalls in section 6 of the plan).
  binShDash = pkgs.runCommand "bin-sh-dash" { } ''
    mkdir -p $out/bin
    ln -s ${pkgs.dash}/bin/dash $out/bin/sh
  '';

  # /etc/passwd + /etc/group with the runner user. fakeNss points passwd at a
  # read-only store path, so useradd in fakeRootCommands cannot work; ship the
  # files directly instead.
  etcFiles = pkgs.runCommand "tested-etc" { } ''
    mkdir -p $out/etc
    cat > $out/etc/passwd <<'EOF'
    root:x:0:0:root:/root:/bin/sh
    runner:x:1000:1000:runner:/home/runner:/bin/sh
    nobody:x:65534:65534:nobody:/var/empty:/bin/sh
    EOF
    cat > $out/etc/group <<'EOF'
    root:x:0:
    runner:x:1000:
    nogroup:x:65534:
    EOF
    echo 'hosts: files dns' > $out/etc/nsswitch.conf
  '';

  baseContents = [
    binShDash
    etcFiles
    pkgs.dockerTools.usrBinEnv
    pkgs.dockerTools.caCertificates
  ];

  fakeRootCommands = ''
    #!${pkgs.runtimeShell}
    mkdir -p tmp && chmod 1777 tmp
    mkdir -p mnt && chmod 711 mnt
    mkdir -p home/runner/workdir
    chown -R 1000:1000 home/runner
    cp ${../.devcontainer/main.sh} main.sh
    chmod 0755 main.sh
  '';

  mkImage =
    {
      name,
      manifestNames,
      withDev ? false,
    }:
    let
      chain = lib.concatMap (manifest.manifestChain depsDir) manifestNames;
      merged = manifest.merge depsDir chain;
      tools = manifest.resolveTools { inherit pkgs; inherit (pkgs) lib; inherit packagesDir; } merged.tools;
      py = import ./python.nix {
        inherit pkgs withDev;
        manifestPythonPackages = merged.pythonPackages;
      };
      runtimeEnv = pkgs.buildEnv {
        name = "${name}-env";
        paths = tools ++ [ py.env ];
        pathsToLink = [ "/bin" "/lib" "/share" ];
        # tsx and node bins collide on a `.bin` dir; last wins is fine here.
        ignoreCollisions = true;
      };
      envList =
        [
          "PATH=${runtimeEnv}/bin:/bin:/usr/bin"
          "LANG=C.UTF-8"
          "LC_ALL=C.UTF-8"
          # matches the Dockerfile; ts/js tooling installs node_modules here
          "NODE_PATH=${runtimeEnv}/lib/node_modules"
          "HOME=/home/runner"
        ]
        ++ lib.mapAttrsToList (k: v: "${k}=${toString v}") merged.env;
    in
    pkgs.dockerTools.streamLayeredImage {
      inherit name;
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

  languageImage = l: mkImage { name = "tested-${l}"; manifestNames = [ l ]; };
in
{
  images = {
    tested-core = mkImage {
      name = "tested-core";
      manifestNames = [ "core" ];
    };
    tested-all = mkImage {
      name = "tested-all";
      manifestNames = [ "core" "userland" "dev" ] ++ languages;
      withDev = true;
    };
  }
  // lib.genAttrs (map (l: "tested-${l}") languages) (n: languageImage (lib.removePrefix "tested-" n));
}
