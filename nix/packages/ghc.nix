# Why this override exists:
# The Haskell templates import Data.Aeson (tested/languages/haskell/templates/
# Values.hs), so the judge's GHC must have aeson (and its transitive text /
# bytestring) in its package database. The Dockerfile does this with
# `cabal v1-install --global aeson`; here it is a ghcWithPackages wrapper.
#
# The wrapper's `settings` file points GHC at the exact cc/binutils it needs to
# link student programs at runtime; those stay in the closure, so no extra PATH
# entry is required for linking (the manifest still ships gcc/binutils for
# student code that shells out to a C compiler).
{ haskellPackages }:
haskellPackages.ghcWithPackages (p: [
  p.aeson
  p.text
  p.bytestring
])
