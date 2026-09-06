# Builds the single Python environment for an image or dev shell.
#
# One withPackages environment only: the judge runs student code with the same
# interpreter, and pylint is imported as a library, so everything must share it.
#
# Dependency names AND version constraints come from pyproject.toml, parsed with
# builtins.fromTOML. A nixpkgs package that does not satisfy the constraint is a
# hard error pointing at nix/packages/<name>.nix (see pylint, black, isort).
{
  pkgs,
  # Manifest [python.packages]: { <name> = { spec; file; }; } from manifest.merge.
  manifestPythonPackages ? { },
  # Include the [dependency-groups] dev packages from pyproject.toml.
  withDev ? false,
  python ? pkgs.python312,
}:
let
  lib = pkgs.lib;
  pyproject = builtins.fromTOML (builtins.readFile ../pyproject.toml);

  # "attrs>=23.2,<24" -> { name = "attrs"; specs = [ { op = ">="; v = "23.2"; } { op = "<"; v = "24"; } ]; }
  parseReq =
    s:
    let
      # split into name and the constraint tail at the first operator char
      ops = [ ">=" "<=" "==" "!=" "~=" ">" "<" ];
      idx =
        lib.foldl (
          acc: i: if acc != null then acc else (if lib.elem (builtins.substring i 1 s) [ ">" "<" "=" "!" "~" ] then i else null)
        ) null (lib.range 0 (builtins.stringLength s - 1));
      name = if idx == null then s else builtins.substring 0 idx s;
      tail = if idx == null then "" else builtins.substring idx (builtins.stringLength s) s;
      clauses = lib.filter (c: c != "") (lib.splitString "," tail);
      parseClause =
        c:
        let
          op = lib.findFirst (o: lib.hasPrefix o c) "==" ops;
        in
        {
          inherit op;
          v = lib.removePrefix op (lib.strings.trim c);
        };
    in
    {
      name = lib.strings.trim name;
      specs = map parseClause clauses;
    };

  satisfies =
    have: spec:
    let
      inherit (spec) op v;
    in
    if op == ">=" then lib.versionAtLeast have v
    else if op == ">" then lib.versionOlder v have
    else if op == "<=" then !(lib.versionOlder v have)
    else if op == "<" then lib.versionOlder have v
    else if op == "==" then have == v || lib.hasPrefix "${v}." have
    else if op == "!=" then !(have == v)
    else if op == "~=" then lib.versionAtLeast have v
    else true;

  aliases = {
    "pyyaml" = "pyyaml";
    "pygments" = "pygments";
    "typing_inspect" = "typing-inspect";
  };
  toAttr = name: let n = lib.toLower (lib.replaceStrings [ "_" ] [ "-" ] name); in aliases.${n} or n;

  reqs =
    map parseReq (
      (pyproject.project.dependencies or [ ])
      ++ lib.optionals withDev (pyproject.dependency-groups.dev or [ ])
    );

  # Pinned packages (nix/packages/<name>.nix) are woven into the package set so
  # that a transitive user (e.g. pylint depending on isort) sees the same pin
  # and withPackages never gets two versions of one package.
  overridden = [ "pylint" "black" "isort" ];
  customPython = python.override {
    self = customPython;
    packageOverrides = final: prev: lib.genAttrs overridden (
      name:
      let
        f = import (./packages + "/${name}.nix");
        # override-style files (black, isort) take their own name as the base;
        # from-scratch files (pylint) do not.
        baseArg = lib.optionalAttrs (builtins.functionArgs f ? ${name}) { ${name} = prev.${name}; };
      in
      final.callPackage f baseArg
    );
  };
  pyPkgs = customPython.pkgs;

  resolve =
    req:
    let
      attr = toAttr req.name;
      overridePath = ./packages + "/${attr}.nix";
      pkg =
        if builtins.pathExists overridePath then
          pyPkgs.${attr} or (pyPkgs.callPackage overridePath { })
        else
          pyPkgs.${attr} or (throw "nix/python.nix: no python package '${attr}' (from '${req.name}') in nixpkgs; add nix/packages/${attr}.nix");
      have = lib.getVersion pkg;
      bad = lib.filter (spec: !(satisfies have spec)) req.specs;
    in
    if bad == [ ] then
      pkg
    else
      throw "deps: python ${req.name} wants ${
        lib.concatMapStringsSep "," (s: s.op + s.v) req.specs
      }, nixpkgs has ${have}. Add nix/packages/${attr}.nix to pin it.";

  # Manifest [python.packages]: resolved against the same scope, prefix-checked.
  resolveManifest =
    name: { spec, file }:
    let
      want = if builtins.isString spec then spec else spec.version or null;
      pkg = pyPkgs.${toAttr name} or (throw "deps: python ${name} (deps/${file}) not in nixpkgs; add nix/packages/${toAttr name}.nix");
      have = lib.getVersion pkg;
    in
    if want == null || lib.hasPrefix (toString want) have then
      pkg
    else
      throw "deps: ${name} wants ${toString want}, nixpkgs has ${have}. Bump deps/${file} or add nix/packages/${toAttr name}.nix";

  env = customPython.withPackages (
    _: (map resolve reqs) ++ lib.mapAttrsToList resolveManifest manifestPythonPackages
  );
in
{
  inherit env python;
}
