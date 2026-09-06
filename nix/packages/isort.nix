# Why this override exists:
# CI pins isort 5.13.2 (dev-dependencies.sh); nixpkgs ships isort 8.x, whose
# import grouping differs and fails `isort --check-only` on the repo. Pin 5.13.2.
{ isort, fetchPypi, poetry-core }:
isort.overridePythonAttrs (old: rec {
  version = "5.13.2";
  src = fetchPypi {
    pname = "isort";
    inherit version;
    hash = "sha256-SP38ufrOXVik9t3i5yofuNyvirJvlatJ+rhMLd77AQk=";
  };
  # 5.13.2 builds with poetry-core; nixpkgs 8.x uses hatchling.
  build-system = [ poetry-core ];
  doCheck = false;
  doInstallCheck = false;
})
