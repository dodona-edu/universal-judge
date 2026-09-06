let
  p = import <nixpkgs> { };
  l = p.lib;
  probe = {
    gcc = p.gcc;
    cppcheck = p.cppcheck;
    hlint = p.hlint;
    jdk21 = p.jdk21;
    checkstyle = p.checkstyle;
    kotlin = p.kotlin;
    ktlint = p.ktlint;
    nodejs = p.nodejs;
    nodejs_22 = p.nodejs_22;
    "dotnet-sdk_8" = p.dotnet-sdk_8;
    binutils = p.binutils;
    ghc = p.haskellPackages.ghc;
    tsx = p.tsx;
  };
  text = l.concatStringsSep "\n" (l.mapAttrsToList (n: v: "${n} = ${l.getVersion v}") probe);
in
p.runCommand "versions" { inherit text; } "echo \"$text\" > $out"
