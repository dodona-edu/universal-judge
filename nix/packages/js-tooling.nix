# Why this override exists:
# The JavaScript judge needs `eslint` (v8, old-style .eslintrc — v9 dropped it)
# and `abstract-syntax-tree` (required by tested/languages/javascript/parseAst.js).
# Neither is in nixpkgs at the versions the Dockerfile installs globally, so
# build a node_modules bundle from a pinned package-lock.json. Bins land in
# $out/bin; the tree is exposed at $out/lib/node_modules for NODE_PATH.
#
# Deps come from the lockfile via importNpmLock rather than a fixed-output
# npmDepsHash: the lockfile is the only thing to bump. Same in ts-tooling.nix
# and pyright.nix.
{ lib, buildNpmPackage, importNpmLock }:
buildNpmPackage {
  pname = "tested-js-tooling";
  version = "1.0.0";
  src = ./js-tooling;
  npmDeps = importNpmLock { npmRoot = ./js-tooling; };
  npmConfigHook = importNpmLock.npmConfigHook;
  dontNpmBuild = true;
  npmFlags = [ "--ignore-scripts" ];

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

  meta.description = "eslint 8 + abstract-syntax-tree for the JS judge";
}
