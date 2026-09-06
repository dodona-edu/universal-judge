{
  description = "Nix prototype for the TESTed judge images";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
      pkgsFor = system: import nixpkgs { inherit system; };

      buildFor =
        system:
        let
          pkgs = pkgsFor system;
          manifest = import ./nix/lib/manifest.nix { inherit (pkgs) lib; };
          depsDir = ./deps;
          packagesDir = ./nix/packages;
          # A cheap derivation per manifest: forces the resolver (and every
          # version check) to evaluate. `nix build .#manifests.python`.
          manifestCheck =
            name:
            let
              chain = manifest.manifestChain depsDir name;
              merged = manifest.merge depsDir chain;
              tools = manifest.resolveTools { inherit pkgs; inherit (pkgs) lib; inherit packagesDir; } merged.tools;
            in
            pkgs.runCommand "manifest-${name}" { } ''
              mkdir -p $out
              printf '%s\n' ${nixpkgs.lib.escapeShellArgs chain} > $out/chain.txt
              printf '%s\n' ${nixpkgs.lib.escapeShellArgs (map (t: "${t}") tools)} > $out/tools.txt
              cp ${pkgs.writeText "env.json" (builtins.toJSON merged.env)} $out/env.json
            '';
          allManifests = [ "core" "userland" "bash" "python" "c" "cpp" "haskell" "java" "kotlin" "javascript" "typescript" "csharp" "dev" ];
        in
        {
          manifests = nixpkgs.lib.genAttrs allManifests manifestCheck;
          images = (import ./nix/images.nix { inherit pkgs; }).images;
        };
    in
    {
      # Nested (manifests.*, images.*), so kept out of `packages` which
      # `nix flake check` requires to be flat derivations.
      legacyPackages = forAllSystems buildFor;

      devShells = forAllSystems (system: import ./nix/shell.nix { pkgs = pkgsFor system; });

      checks = forAllSystems (
        system:
        let b = buildFor system; in
        {
          manifests-core = b.manifests.core;
          manifests-python = b.manifests.python;
          manifests-bash = b.manifests.bash;
          image-tested-core = b.images.tested-core;
        }
      );
    };
}
