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
{ haskell }:
haskell.packages.ghc967.ghcWithPackages (p: [
  p.aeson
  p.text
  p.bytestring
])
