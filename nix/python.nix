# Builds the single Python environment for an image or dev shell.
#
# One withPackages environment only: the judge runs student code with the same
# interpreter, and pylint is imported as a library, so everything must share it.
{
  pkgs,
  pyproject-nix ? null,
  # Manifest-derived python packages (list of derivations from manifest.nix).
  extraPythonPackages ? [ ],
  # Include the [dependency-groups] dev packages from pyproject.toml.
  withDev ? false,
  python ? pkgs.python312,
}:
let
  lib = pkgs.lib;
  pyproject = builtins.fromTOML (builtins.readFile ../pyproject.toml);

  # "attrs>=23.2,<24" / "pytest-cov>=5.0,<6[extra]" -> "attrs" / "pytest-cov"
  depName =
    s:
    let
      stop = [ ">" "<" "=" "!" "~" " " ";" "[" "(" ];
      chars = lib.stringToCharacters s;
      taken = lib.take (
        lib.foldl (
          acc: c: if acc.done || lib.elem c stop then { inherit (acc) i; done = true; } else { i = acc.i + 1; done = false; }
        ) { i = 0; done = false; } chars
      ).i chars;
    in
    lib.concatStrings taken;

  # PyPI name -> nixpkgs python3Packages attribute.
  aliases = {
    "pyyaml" = "pyyaml";
    "pygments" = "pygments";
    "typing-inspect" = "typing-inspect";
    "typing_inspect" = "typing-inspect";
  };
  toAttr =
    name:
    let n = lib.toLower (lib.replaceStrings [ "_" ] [ "-" ] name);
    in aliases.${n} or n;

  wantNames =
    (map depName (pyproject.project.dependencies or [ ]))
    ++ lib.optionals withDev (map depName (pyproject.dependency-groups.dev or [ ]));

  wantPkgs = map (
    name:
    let
      attr = toAttr name;
      overridePath = ../nix/packages + "/${attr}.nix";
    in
    if builtins.pathExists overridePath then
      python.pkgs.callPackage overridePath { }
    else
      python.pkgs.${attr} or (throw "nix/python.nix: no python package '${attr}' (from '${name}') in nixpkgs; add nix/packages/${attr}.nix")
  ) wantNames;

  env = python.withPackages (_: wantPkgs ++ extraPythonPackages);
in
{
  inherit env python wantNames;
}
