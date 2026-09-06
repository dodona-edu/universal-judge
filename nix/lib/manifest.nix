# TOML manifest -> packages, with version checks.
#
# Rules (see section 4 of docs/nix-prototype-report.md / the plan):
#  1. The manifest version must be a prefix of the package version.
#  2. nix/packages/<name>.nix, if present, wins over nixpkgs.
#  3. Otherwise the `nixpkgs` field is an attribute path, else the tool name.
#  4. `includes` is transitive and resolved before sections are merged.
#  5. All [python.packages] plus the pyproject.toml judge deps become one env.
{ lib }:
rec {
  readManifest = depsDir: name: builtins.fromTOML (builtins.readFile (depsDir + "/${name}.toml"));

  # Transitive include chain, dependencies first, each manifest once.
  manifestChain =
    depsDir: name:
    let
      go =
        state: n:
        if lib.elem n state.seen then
          state
        else
          let
            m = readManifest depsDir n;
            incs = (m.image or { }).includes or [ ];
            afterIncludes = lib.foldl go (state // { seen = state.seen ++ [ n ]; }) incs;
          in
          afterIncludes // { acc = afterIncludes.acc ++ [ n ]; };
    in
    (go { seen = [ ]; acc = [ ]; } name).acc;

  # Merge the [tools], [python.packages] and [env] tables of a chain.
  # Later manifests win on conflicts; provenance (the file) is kept for errors.
  merge =
    depsDir: names:
    let
      manifests = map (n: {
        name = n;
        data = readManifest depsDir n;
      }) names;
      withProvenance =
        sel:
        lib.foldl (
          acc: m: acc // lib.mapAttrs (_: spec: { inherit spec; file = "${m.name}.toml"; }) (sel m.data)
        ) { } manifests;
    in
    {
      tools = withProvenance (d: d.tools or { });
      pythonPackages = withProvenance (d: (d.python or { }).packages or { });
      env = lib.foldl (acc: m: acc // (m.data.env or { })) { } manifests;
      image = lib.foldl (acc: m: if m.data ? image then m.data.image else acc) { } manifests;
      interpreter = lib.foldl (
        acc: m: if (m.data.python or { }) ? interpreter then m.data.python.interpreter else acc
      ) null manifests;
    };

  # Throw the exact error text from the plan on a version mismatch.
  checkVersion =
    { name, file, want, have }:
    if want == null || lib.hasPrefix (toString want) have then
      true
    else
      throw "deps: ${name} wants ${toString want}, nixpkgs has ${have}. Bump deps/${file} or add nix/packages/${name}.nix";

  # Resolve one [tools] entry to a derivation, checking its version.
  resolveTool =
    {
      pkgs,
      lib,
      packagesDir,
    }:
    name:
    { spec, file }:
    let
      s = if builtins.isString spec then { version = spec; } else spec;
      want = s.version or null;
      overridePath = packagesDir + "/${name}.nix";
      pkg =
        if builtins.pathExists overridePath then
          pkgs.callPackage overridePath { }
        else
          lib.getAttrFromPath (lib.splitString "." (s.nixpkgs or name)) pkgs;
      have = lib.getVersion pkg;
      checked = checkVersion { inherit name file want have; };
    in
    builtins.seq checked pkg;

  resolveTools =
    args: tools: lib.mapAttrsToList (resolveTool args) tools;

  # Resolve [python.packages] against a python package set. Overrides in
  # nix/packages/<name>.nix are called with python.pkgs.callPackage.
  resolvePythonPackages =
    { python, lib, packagesDir }:
    pythonPackages:
    lib.mapAttrsToList (
      name:
      { spec, file }:
      let
        want = if builtins.isString spec then spec else spec.version or null;
        overridePath = packagesDir + "/${name}.nix";
        pkg =
          if builtins.pathExists overridePath then
            python.pkgs.callPackage overridePath { }
          else
            python.pkgs.${name};
        have = lib.getVersion pkg;
        checked = checkVersion { inherit name file want have; };
      in
      builtins.seq checked pkg
    ) pythonPackages;
}
