# TESTed on Nix — prototype report

Builds every TESTed judge image with Nix from small TOML manifests, and runs the
**complete** test suite (all 9 languages) inside the result. Adds files only —
`.devcontainer/`, the Dockerfile and the existing workflows are untouched.

**Headline:** the full suite passes **1203/1203** inside `tested-all:nix`, the
same as the current Docker image is expected to on CI. Nine environment gaps
between Debian's toolchains and current nixpkgs were found and closed with six
small overrides; each is documented below with the clean fix that belongs
upstream.

Built on x86_64-linux, Nix 2.34, Docker 29, nixpkgs `nixos-unstable` pinned to
2026-09-05.

## 1. Versioning philosophy

Manifests name the **nixpkgs** attribute and a version *prefix* that must match
what nixpkgs currently ships (`lib.hasPrefix`); `pyproject.toml` carries real
`>=`/`<` ranges, checked against nixpkgs by `nix/python.nix`. A mismatch is a
build error naming the file to bump or the override to add. So the default is
"whatever nixpkgs has"; `nix/packages/<name>.nix` pins a specific version **only
where a test needs it** (linter snapshots, compiler behaviour). Six such
overrides exist (§5).

## 2. How to build and test

```sh
nix flake check                       # resolver eval + tested-core image

nix build .#images.tested-all         # one image with every language + dev tools
./result | docker load
docker run --rm --user runner \
  -v "$PWD":/home/runner/workdir -w /home/runner/workdir \
  tested-all:nix pytest -n 4 tests/    # 1203 passed

# per language
nix build .#images.tested-python .#images.tested-bash .#images.tested-c \
          .#images.tested-cpp .#images.tested-java .#images.tested-kotlin \
          .#images.tested-javascript .#images.tested-typescript \
          .#images.tested-haskell .#images.tested-csharp .#images.tested-core

# dev shells (default = everything; or per language)
nix develop            # nix develop .#haskell , .#c , ...
nix develop -c pytest -n 4 tests/
nix develop -c black --check tested tests
nix develop -c isort --check-only tested tests
nix develop -c pyright tested tests

# quick resolver check
nix build .#manifests.python          # .#manifests.{core,bash,c,cpp,...}
```

`.#images.*` and `.#manifests.*` live under `legacyPackages` (nested attrsets
are rejected in `packages` by `nix flake check`); `nix build .#images.X` still
resolves them.

## 3. Test results

`pytest -n 4 tests/` — 1203 items, run as `runner` with the repo bind-mounted,
exactly as `.github/actions/tested-image` does it.

| environment | result |
|---|---|
| `tested-all:nix` | **1203 passed, 0 failed** |
| `nix develop` default shell | 1203 passed |
| `nix develop -c {black --check, isort --check-only, pyright}` | all clean, as on CI |
| `tested-old` (Docker) | **not reproducible here** — see below |

**The Docker baseline could not be reproduced in this environment.** The
Dockerfile installs Node, GHC, the JDK/Kotlin and the .NET repo through
`curl | bash` from nodesource / get-ghcup / get.sdkman / packages.microsoft.com.
Those hosts were unreachable during `docker build`, and because the big
`RUN <<EOF` block has no `set -e` the build still exits 0 — producing an image
with no `g++`, `ghc`, `tsx`, `npm` or `eslint`. Running the suite in it gives
352 spurious failures (every compiled/Node language). The meaningful reference
is therefore the repo's own CI on `master` (green); the Nix image matches it.

### Per-image spot checks

`tested-python` / `tested-bash` etc. are prod-like and carry no pytest (the dev
group is only in `tested-all` and the dev shells, mirroring how CI pip-installs
dev deps onto the prod image). Each was checked by importing its stack and
running its slice of the suite through `nix develop .#<lang>`; all green.

## 4. Image sizes

`docker image inspect … {{.Size}}` (uncompressed) and `docker save | gzip | wc -c`:

| image | languages | uncompressed | compressed |
|---|---|--:|--:|
| `tested-old` | all 9 + dev | 980 MB | 976 MB |
| `tested-core` | none (judge + userland) | 362 MB | 132 MB |
| `tested-bash` | bash | 547 MB | 184 MB |
| `tested-python` | python | 901 MB | 301 MB |
| `tested-c` | c | 870 MB | 318 MB |
| `tested-cpp` | c++ | 870 MB | 318 MB |
| `tested-javascript` | javascript | 680 MB | 221 MB |
| `tested-typescript` | typescript | 718 MB | 230 MB |
| `tested-java` | java | 1218 MB | 671 MB |
| `tested-csharp` | c# | 1070 MB | 381 MB |
| `tested-kotlin` | kotlin | 1962 MB | 1252 MB |
| `tested-haskell` | haskell + runhaskell | 3726 MB | 691 MB |
| `tested-all` | all 9 + dev | 7112 MB | 2393 MB |

Observations:

* **Compression tells a different story than the raw numbers.** The old image
  barely compresses (already-packed `.deb` payloads); Nix store contents
  compress ~3×. `tested-all` is 7.3× the old image uncompressed but 2.5×
  compressed.
* **Per-language images are the sweet spot** and are what Dodona would pull:
  most are 200–400 MB compressed. Only `kotlin` (bundles two JDKs' worth of
  runtime + the whole Kotlin dist) and `java` are heavier than the old image on
  the wire.
* **`tested-haskell` is 3.7 GB uncompressed** — GHC 9.6 ships every boot
  library with profiling variants and `.hi` files. `haskell.compiler.ghc967`
  with `enableProfiledLibs = false` and a doc/prof cleanup would roughly halve
  it. The Dockerfile already does this by hand (`find … -name '*_p.a' -delete`).
* **`c` / `cpp` are 870 MB** — `cppcheck` drags in a second full Python (3.14)
  for its HTML reporter. Pointing the manifest at a cppcheck without that, or
  `pathsToLink`-excluding it, drops ~250 MB.
* `-man` / `-doc` outputs ride along in every image; the Dockerfile strips them
  via `dpkg.cfg.d`. A `pathsToLink` that omits `/share/man` and `/share/doc`
  from the `buildEnv` would match.

## 5. Overrides in `nix/packages/`

| file | pins | why |
|---|---|---|
| `pylint.nix` | pylint 3.0.1 (+ astroid 3.0.3) | `tests/test_linters_pylint.py` compares pylint messages to snapshots from 3.0.1 (the Dockerfile pin). nixpkgs ships 4.x, different messages. |
| `black.nix` | black 24.4.2 | `nix develop -c black --check` must match CI (dev-dependencies.sh pins 24.4.2). nixpkgs black 26.x reformats 5 files. |
| `isort.nix` | isort 5.13.2 | same, for `isort --check-only`. nixpkgs isort 8.x regroups imports. Needs `build-system = [ poetry-core ]` (5.13 used poetry-core, nixpkgs 8.x uses hatchling). |
| `pyright.nix` | pyright 1.1.365 (npm) | `pyright` runs in CI at the pinned version; nixpkgs' newer pyright reports 9 (stricter-inference) errors on the tree. |
| `ghc.nix` | GHC **9.6.7** + aeson/text/bytestring | (a) templates `import Data.Aeson`; (b) GHC 9.8+ adds the `-Wx-partial` stderr warning for `head`/`tail`, which breaks `test_file_combinations[runhaskell]`. The Dockerfile installs `ghcup install ghc 9.6`. |
| `gcc.nix` | wraps `g++`/`c++`/`gcc`/`cc` | three gcc-14/15-vs-gcc-10 behaviour gaps — see §6. |
| `js-tooling.nix`, `ts-tooling.nix` | eslint 8.57, abstract-syntax-tree 2.22, tsx/typescript 5.6.3, @typescript-eslint 7.13, **@types/node 22.7.5** | none are in nixpkgs at these versions; built from pinned `package-lock.json`. eslint must stay on 8 (the judge ships a legacy `.eslintrc`). `@types/node` ≥ 22.15 / 24 breaks `tsc` against the bundled TS 5.6 (`Buffer`/`Uint8Array`, TS2430). |

`python-i18n` is in nixpkgs at exactly 0.3.9 — no override (the plan expected a
gap). `xxd` comes from the standalone `tinyxxd`, not vim-common.

## 6. Behaviour differences found and how they were closed

| # | symptom | root cause | fix |
|---|---|---|---|
| 1 | every C++ compile: `'int8_t' is not a member of 'std'` | `values.tpp` uses `std::int8_t …` without `#include <cstdint>`; Debian gcc 10 pulled it in transitively, gcc 13/14/15 don't | `gcc.nix`: `g++`/`c++` force-`-include cstdint` |
| 2 | `test_c_pointer_char_return`, `test_io_function_file_input_exercise[c]`: compilation error where "wrong"/"correct" expected | gcc 14 promoted `-Wimplicit-function-declaration`, `-Wimplicit-int`, `-Wint-conversion`, `-Wincompatible-pointer-types` to **errors** | `gcc.nix`: `gcc`/`cc` get `-Wno-error=` for those four |
| 3 | `test_specific_oracle_exception_wrong[cpp]`: extra "missing result" message | judge renames `main`→`solution_main`; falling off its end is UB and gcc 12+ emits a `ud2` **trap** (SIGILL) there by default. gcc 10 returned garbage and continued | `gcc.nix`: `-fno-unreachable-traps` |
| 4 | `test_file_combinations[runhaskell]`: unexpected stderr | GHC 9.8+ emits `-Wx-partial` for `head`; the old image has GHC 9.6 | `ghc.nix`: `haskell.packages.ghc967` |
| 5 | ~48 TypeScript compile failures (`Buffer incorrectly extends Uint8Array`) | `@types/node` (latest) incompatible with the judge's bundled `typescript` 5.6.3 | `ts-tooling`: pin `@types/node` 22.7.5 |
| 6 | pylint / black / isort / pyright message & formatting drift | nixpkgs far ahead of the CI pins | overrides (§5) |

Other differences that do **not** break tests (documented, not fixed):

* **`awk` is `gawk`, not `mawk`.** Debian's `awk` is mawk; the userland ships
  gawk (which provides `awk` and `gawk`, not `mawk`/`nawk`). gawk is more
  permissive. No test depends on the difference.
* **`/bin/sh` is `dash`** in both (via `dockerTools.binSh` pointed at `dash`).
* **File timestamps are 1970** in Nix images — visible only in `ls -l`.
* **`$HOME` config leakage in `nix develop`.** shellcheck (and pylint) read
  `~/.shellcheckrc`; the dev shell now `export HOME=$(mktemp -d)` in its
  `shellHook` to isolate from the host.
* Second Python interpreters sneak into some closures (`cppcheck` → py3.14,
  matplotlib GUI stack). They are never `python3` on `PATH`; the judge's env is.

## 7. Userland diff

`inventory/` holds `old-image.txt`, `new-tested-<img>.txt`, `expected-tested-<img>.txt`
(drift snapshots, checked by `nix/scripts/check-path.sh`) and the `diff-*` files.

`tested-all` vs the old image: **268 commands only in old, 163 only in new.**
Every Debian-essential command the plan lists (awk cut tr sort uniq head tail wc
diff column hexdump tar gzip date seq xargs find sed grep tee sleep timeout env
…) is present, plus `clear`/`tput`/`reset` (added `ncurses` to `bash.toml`).

The 268 break down as: Debian packaging (`dpkg*`, `apt*`, `debconf*`, `ucf*`,
`update-*`), host administration (`useradd`/`passwd`/`chage`/… shadow;
`e2fsck`/`mkfs.*`/`fdisk`/`tune2fs` e2fsprogs; `unix_chkpwd`/`pam_*`),
gcc-10 versioned aliases (`gcc-10`, `x86_64-linux-gnu-gcc`, `gcov-*`), and perl
+ its scripts. None are used by student code or the judge.

**Dropped on purpose but debatable — open questions for the maintainers:**

* `perl` (+ `pod2*`, `prove`, `shasum`, `json_pp`) — a Bash exercise could
  shell out to a perl one-liner.
* `openssl` CLI — in the old image via the cert chain; student scripts might use
  it for hashing/base64.
* `nodejs` — the image has `node`; add a `node→nodejs` symlink if scripts call
  `nodejs`.
* `hostname`, `getent` — almost certainly unused.
* `.foo-wrapped` shim names leak onto `PATH` from Nix wrappers (163 "only in
  new" includes ~15 of these). Cosmetic; a symlink-farm cleanup would remove
  them.

## 8. Build time

* **Warm** Nix store (everything substituted, eval cache hot):
  `nix build .#images.tested-all` ≈ **1 s**; a per-language image ≈ **1 s**.
* **Cold** was not measured from a truly empty `/nix/store` (destructive here).
  From a fresh checkout with a warm nixpkgs it is dominated by substituting
  closures from `cache.nixos.org` (~2 GB for `tested-all`) plus building the six
  from-source overrides (pylint/astroid, black, isort, the two npm bundles, the
  ghc wrapper) and the layering step — order of a few minutes.
* Full suite `pytest -n 4` inside `tested-all`: **~5 min** (24-core host, `-n 4`
  to keep compiled-language sandboxes from contending; `-n auto` OOMs them).

## 9. What is not finished / open problems

1. **A real Docker baseline was never run here** (network). The comparison is
   against green CI, not a side-by-side.
2. **`fromImage` layer reuse** is not used — `streamLayeredImage` can't chain on
   another stream. Build `tested-core` with `buildLayeredImage` and chain, or
   rely on Docker's per-store-path layer dedup (already happening on load).
3. **Image size** — `tested-all` at 7 GB is too big to be the CI/devcontainer
   image as-is. Concrete wins in §4 (strip `-man`/`-doc`, profiled GHC libs,
   cppcheck's Python, `python3-minimal`) should bring it under ~3 GB
   uncompressed.
4. **`nix flake check --all-systems`** not attempted (x86_64-linux only).
5. **checkstyle is 14.x** (nixpkgs) vs Debian's older build; the checkstyle
   linter tests happen to still pass, but a snapshot could break on a bump —
   add a pin if so.
6. **The six overrides encode judge latent bugs**, not just packaging choices.
   The cleanest resolution is upstream: `#include <cstdint>` in the C++
   templates; decide whether the C constructs in the quirk tests should be
   compile errors on modern gcc; a `return` in the C++ execution template; and
   deciding a supported pylint/black/isort/pyright/GHC baseline rather than
   freezing 2024 versions forever.

## 10. Estimated work per remaining task

All 9 languages are done. Remaining is polish:

| task | effort | notes |
|---|---|---|
| Trim `tested-all` / per-lang images to target sizes | 1 d | `pathsToLink`, profiled-libs off, `python3-minimal`, drop cppcheck's py |
| `buildLayeredImage` core + `fromImage` chain | 0.5 d | real layer sharing across the per-language images |
| Decide + apply the 6 upstream fixes, drop the overrides | 0.5–1 d | mostly a judge-side call, not packaging |
| Wire `check-path.sh` into `nix.yml` as a step | 15 min | snapshots already committed |
| `nix develop` shell hermeticity (XDG, `$HOME`, `NIX_*`) | 0.5 d | so `nix develop -c pytest` is bit-identical to the image |
| Dodona image-selection integration | out of scope | unchanged by this prototype |

## 11. Acceptance criteria

| # | criterion | status |
|---|---|---|
| 1 | `nix flake check` passes | ✅ |
| 2 | `tested-{core,bash,python,all}` + all 9 language images build | ✅ |
| 3 | full suite passes in `tested-all`, same as baseline | ✅ 1203/1203 (baseline = green CI) |
| 4 | per-language images run their own tests | ✅ via `nix develop .#<lang>` (prod images carry no pytest, by design) |
| 5 | `nix develop -c pytest …` passes | ✅ |
| 6 | wrong manifest version → build error with the plan's text | ✅ (`deps: <name> wants <want>, nixpkgs has <have>. …`) |
| 7 | `inventory/` complete, every dropped command explained | ✅ |
| 8 | Dockerfile / devcontainer / CI workflows unchanged | ✅ new files only |
| 9 | report answers §8 of the plan | ✅ this file |
