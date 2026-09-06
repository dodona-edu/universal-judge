# Why this override exists:
# tested/languages/cpp/templates/values.tpp uses std::int8_t .. std::uint64_t
# without #include <cstdint>. Debian bullseye's gcc 10 (the old image) pulled
# <cstdint> in transitively; every gcc in current nixpkgs (13/14/15) does not,
# so every C++ compile fails with "'int8_t' is not a member of 'std'".
#
# The clean fix is a one-line #include in the judge template. Until then, wrap
# g++/c++ to force-include <cstdint>. Plain gcc (C) is symlinked untouched.
{ gcc, symlinkJoin, writeShellScriptBin, lib }:
let
  cxxWrap = name: writeShellScriptBin name ''exec ${gcc}/bin/${name} -include cstdint "$@"'';
in
symlinkJoin {
  name = "gcc-${gcc.version}-tested";
  paths = [ (cxxWrap "g++") (cxxWrap "c++") gcc ];
  passthru = { inherit (gcc) version; unwrapped = gcc; };
  meta = {
    description = "gcc; g++/c++ force-include <cstdint>";
    inherit (gcc.meta) license platforms;
  };
}
