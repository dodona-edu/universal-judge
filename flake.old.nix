{
  description = "TESTed";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devshell = {
      url = "github:numtide/devshell";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
  };

  outputs = { self, nixpkgs, devshell, flake-utils, mach-nix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ devshell.overlays.default ]; };
        python-env = pkgs.python311;
        ghc-aeson = pkgs.haskell.packages.ghc94.ghcWithPackages (p: [p.aeson]);
      in
      {
        devShells = rec {
          default = tested;
          tested = pkgs.devshell.mkShell {
            name = "TESTed";
            packages = with pkgs; [
              pkgs.nodejs
              pkgs.nodePackages.npm
              pkgs.nodePackages.eslint
              ghc-aeson
              python-env
              pkgs.pipenv
              pkgs.shellcheck
              pkgs.cppcheck
              pkgs.hlint
              pkgs.checkstyle
              pkgs.kotlin
              pkgs.ktlint
              pkgs.openjdk17
              pkgs.gcc
              pkgs.dotnetCorePackages.sdk_6_0
              pkgs.pylint
            ];
            devshell.startup.link.text = ''
              mkdir -p "$PRJ_DATA_DIR/current"
              ln -sfn "${python-env}/${python-env.sitePackages}" "$PRJ_DATA_DIR/current/python-packages"
              ln -sfn "${python-env}" "$PRJ_DATA_DIR/current/python"
            '';
            env = [
              {
                name = "DOTNET_ROOT";
                eval = "${pkgs.dotnetCorePackages.sdk_6_0}";
              }
             {
                name = "NODE_PATH";
                prefix = "$(npm get prefix)";
              }
            ];
            commands = [
              {
                name = "test:stable";
                category = "tests";
                help = "Run the non-flaky tests.";
                command = ''
                  python -m pytest tests/ -m "not flaky"
                '';
              }
              {
                name = "test:all";
                category = "tests";
                help = "Run all tests.";
                command = ''
                  python -m pytest tests/
                '';
              }
            ];
          };
        };
      }
    );
}

