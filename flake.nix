{
  description = "TESTed";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    poetry2nix = {
      url = "github:nix-community/poetry2nix?ref=refs/tags/2024.5.939250";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, devshell, flake-utils, poetry2nix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ devshell.overlays.default ];
        };
        poetry = poetry2nix.lib.mkPoetry2Nix { inherit pkgs; };

        python = pkgs.python312;

        # For some Python packages, we want customization, as Nix can't build it.
        overrides = final: prev: {
          marko = prev.marko.overridePythonAttrs (
            old: {
              format = "pyproject";
              nativeBuildInputs = [ prev.pdm-pep517 prev.pdm-backend ];
              doCheck = false;
            }
          );
          # We need PyYAML with C support, but poetry2nix does not do that apparently...
          pyyaml = prev.pyyaml.overridePythonAttrs (
            old: {
              nativeBuildInputs = old.nativeBuildInputs ++ [ prev.cython_0 prev.setuptools ];
              buildInputs = old.buildInputs ++ [ pkgs.libyaml ];
            }
          );
        };

        # This one isn't in Nix, so do it manually.
        ast = pkgs.buildNpmPackage rec {
          pname = "abstract-syntax-tree";
          version = "2.20.6";

          src = pkgs.fetchFromGitHub {
            owner = "buxlabs";
            repo = pname;
            rev = "94115dc1f1fd01731c7d43f3d7773dda12b9446f";
            hash = "sha256-jiXZQ0CbU2QSuDKaUBLin4LPSgiasEyBO/TlyNhNqrI=";
          };

          npmDepsHash = "sha256-ZSJvh3IP1VKJ0WR9axKEm4prUBVGyOKHyacIPSpImsU=";

          dontNpmBuild = true;

          meta = with pkgs.lib; {
            description = "A library for working with abstract syntax trees";
            license = licenses.mit;
            maintainers = [ ];
          };
        };

        # General dependencies for other languages
        haskell-deps = [
          (pkgs.haskell.packages.ghc94.ghcWithPackages (p: [ p.aeson ]))
          pkgs.hlint
        ];
        node-deps = [ pkgs.nodejs-18_x pkgs.nodePackages.eslint ast ];
        bash-deps = [ pkgs.shellcheck ];
        c-deps = [ pkgs.cppcheck pkgs.gcc ];
        java-deps = [ pkgs.openjdk17 pkgs.checkstyle ];
        kotlin-deps = [ pkgs.kotlin pkgs.ktlint ];
        csharp-deps = [ pkgs.dotnetCorePackages.sdk_6_0 ];

        all-other-dependencies = haskell-deps ++ node-deps ++ bash-deps
          ++ c-deps ++ java-deps ++ kotlin-deps ++ csharp-deps
          ++ [ pkgs.coreutils ];

        python-base-env = {
          projectDir = self;
          python = python;
          overrides = poetry.overrides.withDefaults overrides;
        };

        python-dev-env = poetry.mkPoetryEnv python-base-env;

        tested-env = {
          inherit (python-base-env) projectDir python overrides;
          propagatedBuildInputs = all-other-dependencies;
        };

        unit-test = pkgs.writeShellApplication {
          name = "unit-test";
          runtimeInputs = [ python-dev-env pkgs.poetry ] ++ all-other-dependencies;
          text = ''
            DOTNET_CLI_HOME="$(mktemp -d)"
            export DOTNET_CLI_HOME
            poetry run pytest -n auto --cov=tested --cov-report=xml tests/
          '';
        };

      in {
        checks = rec {
          default = simple-tests;
          simple-tests = pkgs.stdenvNoCC.mkDerivation {
            name = "simple-tests";
            src = self;
            doCheck = true;
            checkInputs = [ python-dev-env pkgs.poetry ] ++ all-other-dependencies;
            checkPhase = ''
              DOTNET_CLI_HOME="$(mktemp -d)"
              export DOTNET_CLI_HOME
              poetry run pytest -n auto --cov=tested --cov-report=xml tests/
            '';
            installPhase = ''
              touch $out # it worked!
            '';
          };
        };

        packages = rec {
          default = tested;
          tested = poetry.mkPoetryApplication {
            inherit (tested-env)
              projectDir python overrides propagatedBuildInputs;
            doCheck = false;
          };
        };

        devShells = rec {
          default = tested;
          tested = pkgs.devshell.mkShell {
            name = "TESTed";

            packages = [ python-dev-env pkgs.nodePackages.pyright pkgs.poetry ]
              ++ all-other-dependencies;

            devshell.startup.link.text = ''
              mkdir -p "$PRJ_DATA_DIR/current"
              ln -sfn "${python-dev-env}/${python-dev-env.sitePackages}" "$PRJ_DATA_DIR/current/python-packages"
              ln -sfn "${python-dev-env}" "$PRJ_DATA_DIR/current/python"
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
          };
          types = pkgs.mkShell {
            packages = [ python-dev-env pkgs.nodePackages.pyright ];
          };
          format = pkgs.mkShell { packages = [ python-dev-env pkgs.poetry ]; };
        };
      });
}

