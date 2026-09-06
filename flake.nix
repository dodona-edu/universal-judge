{
  description = "Nix prototype for the TESTed judge images (core, bash, python)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    pyproject-nix = {
      url = "github:pyproject-nix/pyproject.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self, nixpkgs, pyproject-nix }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
      pkgsFor = system: import nixpkgs { inherit system; };
    in
    {
      packages = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
          images = import ./nix/images.nix { inherit pkgs pyproject-nix; };
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
              cat > $out/chain.txt <<'EOF'
              ${builtins.concatStringsSep "\n" chain}
              EOF
              cat > $out/tools.txt <<'EOF'
              ${builtins.concatStringsSep "\n" (map (t: "${t}") tools)}
              EOF
              cat > $out/env.json <<'EOF'
              ${builtins.toJSON merged.env}
              EOF
            '';
        in
        {
          manifests = nixpkgs.lib.genAttrs [ "core" "userland" "bash" "python" "dev" ] manifestCheck;
          images = images.images;
        }
      );

      devShells = forAllSystems (
        system: import ./nix/shell.nix { pkgs = pkgsFor system; inherit pyproject-nix; }
      );

      checks = forAllSystems (
        system:
        let pkgs = pkgsFor system; in
        {
          manifests-eval = self.packages.${system}.manifests.python;
        }
      );
    };
}
