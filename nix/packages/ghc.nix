# Why this override exists:
#
# 1. The Haskell templates import Data.Aeson (tested/languages/haskell/templates/
#    Values.hs), so the judge's GHC needs aeson (+ transitive text/bytestring)
#    in its package database. The Dockerfile does `cabal v1-install --global
#    aeson`; here it is a ghcWithPackages wrapper.
#
# 2. GHC must be 9.6, not the nixpkgs default. The Dockerfile installs
#    `ghcup install ghc 9.6`. GHC 9.8+ added -Wx-partial (a stderr warning for
#    `head`/`tail`), which breaks tests/test_io_exercises.py::test_file_combinations
#    [runhaskell] (it expects clean stderr). ghc967 = 9.6.7, closest to the
#    old image's 9.6.x.
# Haddock is off for the whole package set: ghcWithPackages links every output
# (including "doc") of every listed package into the env by default, and GHC's
# own -doc output alone is ~690MB, plus ~90MB spread across aeson's transitive
# deps' own -doc outputs. None of it is reachable at runtime — only `doHaddock`
# controls whether the "doc" output exists at all, so disabling it drops these
# outputs from the closure instead of just hiding them.
#
# Tried and reverted: also overriding the compiler itself (enableProfiledLibs,
# enableDocs — both real common-hadrian.nix args) to shrink GHC's own ~1.8GB
# "out" and ~690MB "doc" outputs. That forks `haskell.packages.ghc967` away
# from the *unmodified* one nixpkgs' plain `hlint` (deps/haskell.toml, no
# override file) builds itself against — hlint no longer shares a GHC with us,
# so the closure ends up with pieces of two GHC toolchains instead of one
# smaller one. Net effect was a ~1.4GB *increase*. Redoing this properly would
# mean also building hlint from this same overridden package set — more
# moving parts pinned together than the size win is worth.
{ haskell }:
let
  pkgSet = haskell.packages.ghc967.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // { doHaddock = false; });
    };
  };
in
pkgSet.ghcWithPackages (p: [
  p.aeson
  p.text
  p.bytestring
])
