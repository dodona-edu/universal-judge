# Why this override exists:
# `nix develop -c black --check tested tests` must behave like CI, which pins
# black 24.4.2 (dev-dependencies.sh). nixpkgs ships black 26.x, which reformats
# 5 files the repo considers clean. Pin 24.4.2 from PyPI.
{ lib, black, fetchPypi }:
black.overridePythonAttrs (old: rec {
  version = "24.4.2";
  src = fetchPypi {
    pname = "black";
    inherit version;
    hash = "sha256-yHK1MFfwAAhdpmoZxV1o9vjdysJkI5KtOjVYeEBvvU0=";
  };
  # 24.x has no pytokens dependency and a smaller test surface.
  doCheck = false;
  doInstallCheck = false;
})
