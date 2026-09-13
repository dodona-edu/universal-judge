# Why this override exists:
# CI runs `pyright` at the version dev-dependencies.sh pins (1.1.365). nixpkgs
# ships a much newer pyright whose stricter inference reports 9 errors on the
# current tree. Pin 1.1.365 from npm; pyright bundles its own runtime.
#
# pyright downloads a matching node at first run unless one is on PATH — the
# wrapper forces the bundled resolution off by exporting PYRIGHT_PYTHON is not
# relevant here (that is the pip shim); the npm package just needs `node`.
#
# Pinned to nodejs_22, not the default `nodejs`: every other JS/TS manifest
# pins nodejs_22 (deps/javascript.toml, deps/typescript.toml, deps/dev.toml),
# but this file used the unversioned default, which nixpkgs currently points
# at nodejs_24 — a second ~90MB Node closure duplicated alongside the pinned
# one for no reason.
{ lib, buildNpmPackage, importNpmLock, nodejs_22 }:
buildNpmPackage {
  pname = "tested-pyright";
  version = "1.1.365";
  src = ./pyright;
  npmDeps = importNpmLock { npmRoot = ./pyright; };
  npmConfigHook = importNpmLock.npmConfigHook;
  dontNpmBuild = true;
  npmFlags = [ "--ignore-scripts" ];
  nodejs = nodejs_22;
  nativeBuildInputs = [ nodejs_22 ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib/node_modules $out/bin
    cp -r node_modules/. $out/lib/node_modules/
    for b in $out/lib/node_modules/.bin/*; do
      [ -e "$b" ] || continue
      ln -s "../lib/node_modules/.bin/$(basename "$b")" "$out/bin/$(basename "$b")"
    done
    runHook postInstall
  '';

  meta = {
    description = "pyright 1.1.365 for CI parity";
    mainProgram = "pyright";
  };
}
