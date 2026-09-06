# Why this override exists:
# The TypeScript judge needs `tsx`, the `typescript` package (imported by
# tested/languages/typescript/parseAst.ts), eslint 8 and the matching
# @typescript-eslint/* plugins and @types/node. Built from a pinned lockfile;
# same layout as js-tooling.nix.
{ lib, buildNpmPackage }:
buildNpmPackage {
  pname = "tested-ts-tooling";
  version = "1.0.0";
  src = ./ts-tooling;
  npmDepsHash = "sha256-o+Df07kjr8dUJUBAob3iaMM9zq/opkcPkln7Nip27MM=";
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

  meta.description = "tsx + typescript + eslint 8 + typescript-eslint for the TS judge";
}
