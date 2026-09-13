# Why this override exists:
# nixpkgs' cppcheck links against `python3.withPackages (ps: [ps.pygments])`
# for its HTML-report syntax highlighting — using the unpinned default `python3`,
# which in nixpkgs-unstable is 3.14, a second full interpreter (~140MB) next to
# the judge's own python312 (nix/python.nix, pinned because pylint/black/isort/
# pyright are all version-pinned against it). Point cppcheck at the same
# python312 instead so it shares that closure rather than duplicating it.
{ cppcheck, python312 }:
cppcheck.override { python3 = python312; }
