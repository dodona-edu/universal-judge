# TESTed Nix prototype — findings

Time-boxed prototype that builds the TESTed judge images with Nix from TOML
manifests, for the core, Bash and Python. It does **not** replace the Docker
build: every file here is new, and `.devcontainer/`, the Dockerfile and the
existing workflows are untouched.

Branch: `worktree-nix-prototype`. Built and tested on x86_64-linux with
Nix 2.34 and Docker 29, nixpkgs pinned to `nixos-unstable` 2026-09-05.

## 1. How to build and test

```sh
# Manifests only (fast resolver check)
nix build .#manifests.core .#manifests.bash .#manifests.python

# Images
nix build .#images.tested-core
nix build .#images.tested-bash
nix build .#images.tested-python
nix build .#images.tested-all
./result | docker load          # each result is a streamLayeredImage script

# Flake check (resolver eval + core image derivation)
nix flake check

# Dev shell
nix develop            # core+userland+bash+python+dev
nix develop .#bash     # bash + dev
nix develop .#python   # python + dev

# Test selection (same as the Docker baseline)
nix develop -c pytest -n auto tests/ -k "bash or python or pylint or shellcheck"

# In-container (mirrors .github/actions/tested-image)
docker run --rm --user runner \
  -v "$PWD":/home/runner/workdir -w /home/runner/workdir \
  tested-all:nix \
  pytest -n auto tests/ -k "bash or python or pylint or shellcheck"
```

The `.#images.*` attributes live under `legacyPackages` (nested attrsets are
not allowed in `packages` when `nix flake check` runs); `nix build .#images.X`
still resolves them.

## 2. Test results — baseline vs new

Selection (recorded in `inventory/baseline-notes.txt`):
`pytest -n auto tests/ -k "bash or python or pylint or shellcheck"` — 148 items.
Run as `runner`, repo mounted at the workdir, dev deps layered first, exactly
as `.github/actions/tested-image` does it.

| Environment | result |
|---|---|
| `tested-old` (Docker baseline) | **147 passed, 1 failed** — `test_linters_shellcheck.py::test_shellcheck_style` |
| `tested-all` image | **148 passed** |
| `tested-python` image | Python subset: not run in-container (see §7); `nix develop .#python` selection passes¹ |
| `tested-bash` image | Bash subset: `nix develop` default shell passes; `.#bash` shell: 1 host-config artefact (see §6) |
| `nix develop` (default shell) | 148 passed |

The one baseline failure is **pre-existing** and environment-driven: the old
image ships shellcheck 0.7.1, which does not emit `SC2034` ("foo appears
unused") for `foo=` `` `echo bar` ``, so the judge produces 2 annotations where
the snapshot expects 3. shellcheck 0.11.0 (nixpkgs) emits all 3 and the test
passes. So the new images are **greener** than the baseline, not identical.
No judge or test code was changed.

¹ `nix develop .#python`/default runs the selection green. The `.#bash` shell
shows `test_shellcheck_style` failing **only because the host's
`~/.shellcheckrc` disables `SC2034`** and shellcheck reads it from `$HOME`;
inside the images `$HOME=/home/runner` has no such file and the test passes.
The dev shell should scrub `HOME`/XDG for shellcheck — noted as an open problem.

## 3. Image sizes

`docker image inspect … {{.Size}}` (uncompressed) and `docker save | gzip | wc -c`:

| image | languages | uncompressed | compressed |
|---|---|---|---|
| `tested-old` | 9 + dev tools | 980 MB | 975 MB |
| `tested-core` | none (judge + userland only) | 361 MB | 132 MB |
| `tested-bash` | bash | 547 MB | 184 MB |
| `tested-python` | python | 901 MB | 301 MB |
| `tested-all` | core+userland+bash+python+dev | 1593 MB | 489 MB |

The old image barely compresses (already-packed apt payloads); the Nix images
compress ~3x. `tested-python` is close to the old image uncompressed but half
the size on the wire, while carrying only one language. `tested-all` is larger
uncompressed than the old image despite fewer languages: numpy pulls
`gfortran-lib` + `openblas`, matplotlib pulls the full `tk`/`tcl`/X stack,
pyright pulls `nodejs`, and every derivation keeps its own glibc/gcc-lib
closure instead of sharing Debian's. The `-man`/`-doc` outputs also ride along
(the Dockerfile strips them via `dpkg.cfg.d`). Trimming: use `python3-minimal`,
drop matplotlib's GUI backends, split `pyright` off into `dev` only, and add a
`pathsToLink`-limited `buildEnv` that excludes `/share/man` and `/share/doc`.

## 4. Build time

* Warm store (nixpkgs eval cache + all paths present): `nix build .#images.tested-all` ≈ **1.2 s**.
* Cold store was not measured from a truly empty `/nix/store` (destructive on
  this machine). From a fresh checkout with a warm nixpkgs the cost is
  dominated by substituting ~0.5–1.5 GB of closures from `cache.nixos.org`
  plus building the 3 from-source overrides (`pylint`, `astroid`, the
  `withPackages` wrapper) and the layering step — order of a few minutes.

## 5. Overrides in `nix/packages/`

| file | why |
|---|---|
| `pylint.nix` | Snapshot parity. `tests/test_linters_pylint.py` compares pylint messages against output from pylint 3.0.1 (the old Dockerfile pin). nixpkgs ships 4.x, whose messages differ. Pins 3.0.1 + the matching `astroid` 3.0.3 from PyPI, relaxes the stale `isort<6`/`dill` bounds, skips the pylint test suite. |

`python-i18n` is present in nixpkgs at exactly 0.3.9 — no override needed
(the plan flagged it as a likely gap). `xxd` comes from the standalone
`tinyxxd` package (nixpkgs `xxd`), not vim-common; version 1.3.16, so the
manifest pin is `"1"`.

## 6. Userland diff and decisions

`inventory/`:
* `old-image.txt` — `compgen -c` in `tested-old` as `runner` (746 commands)
* `new-tested-{core,bash,python,all}.txt` — same for each new image
* `expected-tested-*.txt` — snapshots for the drift check (§ step 6)
* `diff-bash.txt`, `diff-bash-only-in-old.txt` (319), `diff-bash-only-in-new.txt` (96)

**Every Debian-essential command the plan lists is present in `tested-bash`:**
awk cut tr sort uniq head tail wc diff column hexdump tar gzip date seq xargs
find sed grep tee sleep timeout env — all resolve. `clear`/`tput`/`reset` were
missing until `ncurses` was added to `deps/bash.toml`.

The 319 "only in old" commands break down as:

* **Out of scope** — other-language toolchains: `gcc`/`cpp`/`cc`/binutils
  `x86_64-linux-gnu-*`, `java*`/`jar`/`keytool`, `kotlin*`/`ktlint`,
  `node`/`nodejs`, `dotnet`, `cabal`/`ghc*`, `checkstyle`, `cppcheck`,
  `eslint`, `f2py`/`numpy-config`/`fonttools`/`ttx` (these last are in
  `tested-python`/`-all`, just not `-bash`). Not a regression — no manifest
  for those languages exists yet.
* **Dropped on purpose** — Debian machinery with no meaning under Nix:
  `dpkg*`, `apt*`, `debconf*`, `ucf*`, `update-alternatives`,
  `update-*`, `install-info`, `sensible-*`, `select-editor`, `run-parts`,
  `start-stop-daemon`, `invoke-rc.d`, `service`.
* **Dropped on purpose** — host administration a judge never does:
  `useradd`/`userdel`/`usermod`/`passwd`/`chage`/`gpasswd`/`groupadd`/… (shadow),
  `unix_chkpwd`/`pam_*` (PAM), `e2fsck`/`mkfs.*`/`fdisk`/`tune2fs`/`badblocks`/
  `debugfs`/`resize2fs`/`logsave` (e2fsprogs + disk), `getty`/`sulogin`.
* **Behaviour difference** — `mawk`/`nawk`: Debian's `awk` is mawk; the Nix
  userland uses `gawk` (provides `awk` and `gawk`, not `mawk`/`nawk`). See §7.
* **Open questions** (left out, listed for the maintainers):
  * `perl` (+ `pod2*`, `prove`, `shasum`, `json_pp`, `h2ph`) — judgement call;
    some Bash exercises or oracle scripts may shell out to perl one-liners.
  * `openssl` CLI — present in the old image (ca-certificates chain), absent
    here; student scripts could use `openssl` for hashing/base64.
  * `hostname`/`dnsdomainname` — from inetutils/nettools; probably unused.
  * `getent` — glibc tool from `glibc-bin`; occasionally used in scripts.

The 96 "only in new" are Nix packaging artefacts (`.foo-wrapped` shim names
leaking onto PATH from wrapper scripts — cosmetic, fixable with a symlink
cleanup layer), `xz`/`bzip2`/`lzma` helper names Debian splits differently,
and util-linux extras (`lsfd`, `uuidparse`, `rfkill`, `fadvise`, …) that the
newer util-linux ships.

### PATH drift check

Nix-level `compgen -c` in the image closure proved awkward (needs a container).
Fallback per the plan: `nix/scripts/check-path.sh <image> inventory/expected-<image>.txt`
diffs a live `compgen -c` against the checked-in snapshot and fails on drift
(`--update` to regenerate). Wired into `.github/workflows/nix.yml` would be one
extra step; left as a script for now.

## 7. Behaviour differences found

| area | old image | Nix images |
|---|---|---|
| `/bin/sh` | dash (`/bin/dash`) | dash (`dockerTools.binSh` pointed at `dash`) — matches |
| `awk` | **mawk** | **gawk** — gawk accepts more (gensub, `\|`, length(arr)); a script that relies on mawk's stricter parsing could behave differently. Not fixed in the prototype, per the plan. |
| `shellcheck` | 0.7.1 | 0.11.0 — fixes the stale `test_shellcheck_style` snapshot (§2) |
| `pylint` | 3.0.1 | 3.0.1 (override) — parity |
| locale | `C.UTF-8` via Debian | `LANG=LC_ALL=C.UTF-8`, no `glibcLocales` needed so far |
| file timestamps | build time | 1970-01-01 (Nix) — only visible in `ls -l`, no test depends on it |
| dev tools | pinned (black 24.4, isort 5.13, pyright 1.1.365) | nixpkgs latest (black 26.5, isort 8.0, pyright 1.1.412) — `black --check`, `isort --check-only`, `pyright` **do not pass**: newer black reformats 5 files, newer pyright reports 9 errors. Needs the same override treatment as pylint. |
| `/etc/passwd` | real, `useradd runner` | static file with `root`/`runner`/`nobody` (fakeNss + useradd-in-fakechroot can't write the store-backed passwd) |

`tested-python`/`tested-bash` in-container test runs: these images are
prod-like and carry **no pytest** (the dev group is only in `tested-all` and
the dev shells), mirroring how `.github/actions/tested-image` pip-installs dev
deps onto the prod image at CI time. `pip install` into a Nix store is
impossible, so the per-image Python/Bash runs were done through
`nix develop .#python` / `.#bash` instead, and `tested-python` was smoke-tested
in-container (`import pylint, numpy, pandas, … ` → ok, versions 3.0.1 / 2.5.1 /
3.0.4). A faithful in-container run for those two needs either a dev overlay
image or `withDev` on all images (rejected: pollutes the prod image).

## 8. Open problems / not finished

1. **Dev-tool version parity** — black/isort/pyright come from nixpkgs latest
   and fail `--check`. Add `nix/packages/{black,isort,pyright}.nix` pins, or
   pin via `pyproject-nix` honouring the `<25` upper bounds (currently
   `nix/python.nix` reads only the names, not the constraints).
2. **`nix/python.nix` ignores version constraints** from `pyproject.toml`
   (`pytest>=8.2,<9` resolves to 9.1.1). Only `[python.packages]` in the
   manifests get the prefix check.
3. **`pyproject-nix` is wired as an input but unused** — `nix/python.nix`
   parses `pyproject.toml` with `builtins.fromTOML` directly. Works, but the
   plan wanted the pyproject-nix renderers.
4. **Layer reuse via `fromImage`** was dropped: `streamLayeredImage` cannot
   chain onto another `streamLayeredImage` (it needs a real tarball). Options:
   build `tested-core` with `buildLayeredImage` and chain the others, or rely
   on Docker's content-addressed layer dedup (identical store-path layers are
   already shared across the four images on load).
5. **`.foo-wrapped` names on PATH** — add a cleanup so wrapper shims don't
   shadow real names in `compgen -c`.
6. **Dev shell isolation** — shellcheck (and likely pylint) read `$HOME`
   config; `nix develop` inherits the host `$HOME`. Set `HOME`/`XDG_*` in the
   shellHook.
7. **`nix flake check --all-systems`** not attempted (only x86_64-linux).
8. **Cold-store build time** not measured.

## 9. Estimated work per remaining language

Each language is a `deps/<lang>.toml` (a `[tools]` list of bare-name binaries)
plus a `tested-<lang>` image entry. Most are half a day:

| language | tools | effort | risk |
|---|---|---|---|
| JavaScript | `nodejs`, `eslint`, `abstract-syntax-tree` (npm) | 0.5 d | `abstract-syntax-tree` is not in nixpkgs → `buildNpmPackage` or `node2nix` a tiny lockfile |
| TypeScript | `nodejs`, `tsx`, `typescript`, `@typescript-eslint/*` | 0.5 d | same npm packaging; `tsx` **is** in nixpkgs |
| C | `gcc`, `cppcheck` | 0.25 d | trivial — both in nixpkgs |
| C++ | `gcc`/`g++`, `cppcheck` | 0.25 d | trivial |
| Java | `openjdk21`, `checkstyle` | 0.5 d | JDK writes `~/.java`; set `HOME` and a writable tmp |
| Kotlin | `kotlin`, `ktlint` | 0.5 d | `kotlin` in nixpkgs; ktlint pulls the JDK |
| Haskell | `ghc` (with `aeson`), `hlint` | **1.5–2 d** | **hardest.** GHC needs a working C toolchain **at runtime** to link user programs — the image must ship `gcc`, `binutils`, and the GHC-matched `libgmp`/`libffi`, and `settings`/`ghc --info` must point at them. `haskellPackages.ghcWithPackages [aeson]` handles the package DB, but the runtime cc wrapper and `NIX_*` env need to be reproduced without nix-shell. Expect iteration on "cannot execute cc" and missing `crt1.o`. |
| C# | `dotnet-sdk_8` | **1.5 d** | The .NET SDK writes to `$HOME/.dotnet`, `$HOME/.nuget` and `/tmp` on first run (telemetry opt-out, workload manifests, NuGet fallback folder). Under a read-only Nix store and a fresh container this fails until `DOTNET_CLI_TELEMETRY_OPTOUT`, `DOTNET_NOLOGO`, `DOTNET_SKIP_FIRST_TIME_EXPERIENCE`, `NUGET_PACKAGES` and a writable `HOME` are set in the image config, and a warm NuGet cache (for `aeson`-equivalent deps) may need to be baked in as a layer. |
