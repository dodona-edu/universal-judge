# Why this override exists — three gcc-version behaviour gaps vs the old image's
# Debian gcc 10, all in the "fix the environment, not the judge" spirit:
#
# 1. tested/languages/cpp/templates/values.tpp uses std::int8_t .. std::uint64_t
#    without #include <cstdint>. gcc 10 pulled <cstdint> in transitively; nixpkgs
#    gcc 13/14/15 do not -> "'int8_t' is not a member of 'std'". g++/c++
#    force-include <cstdint>.
#
# 2. gcc 14 promoted -Wimplicit-function-declaration / -Wimplicit-int /
#    -Wint-conversion / -Wincompatible-pointer-types from warnings to hard
#    errors. Two C quirk tests rely on such code still compiling and running.
#    gcc/cc get -Wno-error for exactly those four.
#
# 3. The judge renames main -> solution_main, so falling off the end of it is
#    UB. gcc 12+ emits a trap (SIGILL) there by default (-funreachable-traps);
#    gcc 10 just returned garbage. A submission that does this then "crashes"
#    instead of running, changing the judge's message count
#    (test_specific_oracle_exception_wrong[cpp]). -fno-unreachable-traps
#    restores the old behaviour.
#
# The clean fixes belong in the judge (one #include; decide whether those C
# constructs are errors; the template could return a value). Until then, wrap.
{ gcc, symlinkJoin, writeShellScriptBin, lib }:
let
  common = "-fno-unreachable-traps";
  cWarn = lib.concatStringsSep " " [
    "-Wno-error=implicit-function-declaration"
    "-Wno-error=implicit-int"
    "-Wno-error=int-conversion"
    "-Wno-error=incompatible-pointer-types"
  ];
  cWrap = name: writeShellScriptBin name ''exec ${gcc}/bin/${name} ${common} ${cWarn} "$@"'';
  cxxWrap = name: writeShellScriptBin name ''exec ${gcc}/bin/${name} ${common} -include cstdint "$@"'';
in
symlinkJoin {
  name = "gcc-${gcc.version}-tested";
  paths = [ (cxxWrap "g++") (cxxWrap "c++") (cWrap "gcc") (cWrap "cc") gcc ];
  passthru = { inherit (gcc) version; unwrapped = gcc; };
  meta = {
    description = "gcc wrapped to match Debian gcc 10 behaviour for the TESTed judge";
    inherit (gcc.meta) license platforms;
  };
}
