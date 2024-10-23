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
      url = "github:nix-community/poetry2nix?ref=refs/tags/2024.10.1637698";
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

        nodejs_base = pkgs.nodejs_22;

        # This one isn't in Nix, so do it manually.
        ast = pkgs.buildNpmPackage rec {
          pname = "abstract-syntax-tree";
          version = "2.22.0";

          nodejs = nodejs_base;

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

        nextflow-linter = let
          codenarc = pkgs.fetchurl {
            url = "https://repo1.maven.org/maven2/org/codenarc/CodeNarc/3.5.0/CodeNarc-3.5.0-all.jar";
            hash = "sha256-wTGPxP5sjMPPake3WM6Fcz551WIe8C+c6AtCtjBa3jI=";
          };
          linter-rules = pkgs.fetchurl {
            url = "https://github.com/awslabs/linter-rules-for-nextflow/releases/download/v0.1.0/linter-rules-0.1.jar";
            hash = "sha256-IHl2jAtZyloZLXybLNBELnTyld+ZA9q5Srta7LeUeHs=";
          };
          slf4j-api = pkgs.fetchurl {
            url = "https://repo1.maven.org/maven2/org/slf4j/slf4j-api/1.7.36/slf4j-api-1.7.36.jar";
            hash = "sha256-0+9XXj5JeWeNwBvx3M5RAhSTtNEft/G+itmCh3wWocA=";
          };
          slf4j-simple = pkgs.fetchurl {
            url = "https://repo1.maven.org/maven2/org/slf4j/slf4j-simple/1.7.36/slf4j-simple-1.7.36.jar";
            hash = "sha256-Lzm+2UPWJN+o9BAtBXEoOhCHC2qjbxl6ilBvFHAQwQ8=";
          };
        in pkgs.writeScriptBin "nextflow-linter" ''
            ${pkgs.jre_minimal}/bin/java \
              -Dorg.slf4j.simpleLogger.defaultLogLevel=error \
              -classpath "${linter-rules}/linter-rules-0.1.jar:${codenarc}/CodeNarc-3.5.0-all.jar:${slf4j-api}/slf4j-api-1.7.36.jar:${slf4j-simple}/slf4j-simple-1.7.36.jar" \
              "org.codenarc.CodeNarc"
        '';

        # General dependencies for other languages
        haskell-deps = [
          (pkgs.haskell.packages.ghc96.ghcWithPackages (p: [ p.aeson ]))
          pkgs.hlint
        ];
        node-deps = [ nodejs_base pkgs.eslint ast ];
        bash-deps = [ pkgs.shellcheck ];
        c-deps = [ pkgs.cppcheck pkgs.gcc13 ];
        java-deps = [ pkgs.openjdk21 pkgs.checkstyle ];
        kotlin-deps = [ pkgs.kotlin pkgs.ktlint ];
        csharp-deps = [ pkgs.dotnetCorePackages.sdk_8_0 ];
        nextflow-deps = [ pkgs.nextflow pkgs.groovy nextflow-linter ];

        all-other-dependencies = haskell-deps ++ node-deps ++ bash-deps
          ++ c-deps ++ java-deps ++ kotlin-deps ++ csharp-deps ++ nextflow-deps
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
        poetry-package = (pkgs.poetry.override {python3 = python;}).overridePythonAttrs {
          doCheck = false;
        };
      in {
        packages = rec {
          default = tested;
          tested = poetry.mkPoetryApplication {
            inherit (tested-env)
              projectDir python overrides propagatedBuildInputs;
            doCheck = false;
            postInstall = ''
              wrapProgram "$out/bin/tested" \
                --prefix NODE_PATH : ${ast}/lib/node_modules
            '';
          };
          devShell = self.outputs.devShells.${system}.default;
          simple-tests = pkgs.stdenvNoCC.mkDerivation {
            name = "simple-tests";
            src = self;
            doCheck = true;
            checkInputs = [ python-dev-env poetry-package ] ++ all-other-dependencies;
            checkPhase = ''
              DOTNET_CLI_HOME="$(mktemp -d)"
              export DOTNET_CLI_HOME
              NODE_PATH=${ast}/lib/node_modules
              export NODE_PATH
              ESLINT_USE_FLAT_CONFIG=false
              export ESLINT_USE_FLAT_CONFIG
              poetry run pytest -n auto --cov=tested --cov-report=xml tests/
            '';
            installPhase = ''
              mkdir -p $out
              cp coverage.xml $out/coverage.xml
            '';
          };
        };

        devShells = rec {
          default = tested;
          tested = pkgs.devshell.mkShell {
            name = "TESTed";

            packages = [ python-dev-env pkgs.pyright poetry-package ]
              ++ all-other-dependencies;

            devshell.startup.link.text = ''
              mkdir -p "$PRJ_DATA_DIR/current"
              ln -sfn "${python-dev-env}/${python-dev-env.sitePackages}" "$PRJ_DATA_DIR/current/python-packages"
              ln -sfn "${python-dev-env}" "$PRJ_DATA_DIR/current/python"
            '';
            env = [
              {
                name = "DOTNET_ROOT";
                eval = "${pkgs.dotnetCorePackages.sdk_8_0}";
              }
              {
                name = "NODE_PATH";
                prefix = "${ast}/lib/node_modules";
              }

              {
                name = "ESLINT_USE_FLAT_CONFIG";
                eval = "false";
              }
              {
                name = "NXF_VER";
                eval = "${builtins.head (pkgs.lib.splitString "-" pkgs.nextflow.version)}";
              }
            ];
            commands = [
              {
                name = "run-intergation-tests";
                command = "poetry run pytest -x -n auto tests/test_integration_javascript.py";
                help = "Run JavaScript integration tests";
                category = "tests";
              }
              {
                name = "run-tests";
                command = "poetry run pytest -n auto tests/";
                help = "Run unit tests";
                category = "tests";
              }
            ];
          };
          types = pkgs.mkShell {
            packages = [ python-dev-env pkgs.pyright ];
          };
          format = pkgs.mkShell { packages = [ python-dev-env poetry-package ]; };
        };
      });
}

