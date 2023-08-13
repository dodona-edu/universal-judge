{
  description = "TESTed";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, devshell, flake-utils, mach-nix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ devshell.overlays.default ]; };
        python = pkgs.python311;
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
            maintainers = [];
          };
        };
        marko = python.pkgs.buildPythonPackage rec {
          pname = "marko";
          version = "2.0.0";
          format = "pyproject";
           
          src = pkgs.fetchPypi {
            inherit pname version;
            hash = "sha256-78JkYIkyUME3UQJa6SAuuxOJiHA2/A35AJxquHVGcDA=";
          };
            
          nativeBuildInputs = [
            python.pkgs.pdm-pep517 python.pkgs.pdm-backend
          ];
           
          doCheck = false;
           
          meta = with pkgs.lib; {
            homepage = "https://github.com/frostming/marko";
            license = licenses.mit;
            maintainers = [ ];
          };
        };
        core-packages = ps: with ps; [
            psutil
            pydantic
            attrs
            cattrs
            jsonschema
            typing-inspect
            pyyaml
            pygments
            python-i18n
        ];
        python-env = python.withPackages(ps: (core-packages ps) ++ [
            ps.pylint
            ps.pytest
            ps.pytest-mock
            ps.pytest-cov
            # For Pycharm
            ps.setuptools
            ps.isort
            ps.black
            ps.jinja2
            marko
        ]);
        core-deps = [
            (python.withPackages(ps: (core-packages ps) ++ [ps.pylint]))
        ];
        haskell-deps = [
            (pkgs.haskell.packages.ghc94.ghcWithPackages (p: [p.aeson]))
            pkgs.hlint
        ];
        node-deps = [
            pkgs.nodejs-18_x
            pkgs.nodePackages.eslint
            ast
        ];
        bash-deps = [
            pkgs.shellcheck
        ];
        c-deps = [
            pkgs.cppcheck
            pkgs.gcc
        ];
        java-deps = [
            pkgs.openjdk17
            pkgs.checkstyle
        ];
        kotlin-deps = [
            pkgs.kotlin
            pkgs.ktlint
        ];
        csharp-deps = [
            pkgs.dotnetCorePackages.sdk_6_0
        ];
      in
      {
        devShells = rec {
          default = tested;
          tested = pkgs.devshell.mkShell {
            name = "TESTed";
            packages = [python-env pkgs.nodePackages.pyright pkgs.pipenv] ++ haskell-deps ++ node-deps ++ bash-deps ++ c-deps ++ java-deps ++ kotlin-deps ++ csharp-deps;
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

