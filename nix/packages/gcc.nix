# Why this override exists:
#
# 1. tested/languages/cpp/templates/values.tpp uses std::int8_t .. std::uint64_t
#    without #include <cstdint>. Debian bullseye's gcc 10 pulled <cstdint> in
#    transitively; nixpkgs gcc 13/14/15 do not, so every C++ compile fails with
#    "'int8_t' is not a member of 'std'". -> g++/c++ force-include <cstdint>.
#
# 2. gcc 14 promoted -Wimplicit-function-declaration, -Wimplicit-int,
#    -Wint-conversion and -Wincompatible-pointer-types from warnings to errors
#    by default. tests/test_language_quirks.py::test_c_pointer_char_return and
#    test_io_function_file_input_exercise[c] rely on the old image's gcc 10
#    still *compiling* such code (with a warning) and running it. -> gcc/cc get
#    -Wno-error for exactly those four.
#
# The clean fixes belong in the judge (one #include, and deciding whether those
# C constructs should be compile errors). Until then, this wrapper.
{ gcc, symlinkJoin, writeShellScriptBin, lib }:
let
  cWarn = lib.concatStringsSep " " [
    "-Wno-error=implicit-function-declaration"
    "-Wno-error=implicit-int"
    "-Wno-error=int-conversion"
    "-Wno-error=incompatible-pointer-types"
  ];
  cWrap = name: writeShellScriptBin name ''exec ${gcc}/bin/${name} ${cWarn} "$@"'';
  cxxWrap = name: writeShellScriptBin name ''exec ${gcc}/bin/${name} -include cstdint "$@"'';
in
symlinkJoin {
  name = "gcc-${gcc.version}-tested";
  paths = [ (cxxWrap "g++") (cxxWrap "c++") (cWrap "gcc") (cWrap "cc") gcc ];
  passthru = { inherit (gcc) version; unwrapped = gcc; };
  meta = {
    description = "gcc; g++/c++ force-include <cstdint>, gcc/cc keep gcc-13 warning severity";
    inherit (gcc.meta) license platforms;
  };
}
