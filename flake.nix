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
          version = "2.22.0";

          src = pkgs.fetchFromGitHub {
            owner = "buxlabs";
            repo = pname;
            rev = "36b343c80d94383d2c8cd8883dba52d9bf51be71";
            hash = "sha256-ACZf8BwWY476PZ+9mqsiOx6L4yGOvDPMZNFEHmqz4j4=";
          };

          npmDepsHash = "sha256-+I0Nu7KgZgjFQR12Z8iRNaFq269B7ythiq8sdi5or3Y=";

          dontNpmBuild = true;

          meta = with pkgs.lib; {
            description = "A library for working with abstract syntax trees";
            license = licenses.mit;
            maintainers = [ ];
          };
        };

        # General dependencies for other languages
        haskell-deps = [
          (pkgs.haskell.packages.ghc96.ghcWithPackages (p: [ p.aeson ]))
          pkgs.hlint
        ];
        node-deps = [ pkgs.nodejs_22 pkgs.nodePackages.eslint ast ];
        bash-deps = [ pkgs.shellcheck ];
        c-deps = [ pkgs.cppcheck pkgs.gcc13 ];
        java-deps = [ pkgs.openjdk21 pkgs.checkstyle ];
        kotlin-deps = [ pkgs.kotlin pkgs.ktlint ];
        csharp-deps = [ pkgs.dotnetCorePackages.sdk_8_0 ];

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

