# This is the Dockerfile for the tested judge.
# It can be downloaded using docker pull ghcr.io/dodona-edu/dodona-tested.

# This docker image is run in our production environment.
# It should not contain any development tools or dependencies.
# Add those to dev-dependencies.sh instead.

FROM python:3.15.2-slim-trixie

ARG GCC_MAJOR=14
ARG CPPCHECK_VERSION=2.17

# Set up the environment

ARG SHELLCHECK_VERSION=0.11.0

# Kotlin
ENV SDKMAN_DIR=/usr/local/sdkman
ENV PATH=$SDKMAN_DIR/candidates/kotlin/current/bin:$PATH
ENV PATH=$SDKMAN_DIR/candidates/java/current/bin:$PATH
ARG TEMURIN_VERSION=25.0.5.0+8-tem
ARG KOTLIN_VERSION=2.4.21
ARG KTLINT_VERSION=1.8.0
ARG CHECKSTYLE_VERSION=14.4.0
# Haskell
ENV HASKELL_DIR=/usr/local/ghcupdir
ENV PATH=$HASKELL_DIR/ghc/bin:$PATH
ENV PATH=$HASKELL_DIR/cabal:$PATH
ARG GHC_VERSION=9.14.1
ARG CABAL_VERSION=3.18.2.0
ARG AESON_VERSION=2.3.2.0
ARG HLINT_VERSION=3.10
# Node
ENV NODE_PATH=/usr/lib/node_modules
ARG NODE_MAJOR=26

# .NET: every submission runs in a fresh container, so each dotnet call is a first run
ENV DOTNET_NOLOGO=1
ENV DOTNET_GENERATE_ASPNET_CERTIFICATE=false
ENV DOTNET_SKIP_WORKLOAD_INTEGRITY_CHECK=true
ENV DOTNET_CLI_TELEMETRY_OPTOUT=1
ARG DOTNET_SDK_CHANNEL=10.0

COPY dodona-tested/ /requirements/

# Install dependencies
# hadolint ignore=DL3013,DL3016
RUN <<EOF
    set -eux

    # Update apt-get
    apt-get update

    # Drop docs/man/locale from all apt-installed packages (keep copyright for licensing)
    cat > /etc/dpkg/dpkg.cfg.d/01_nodoc <<'CFG'
path-exclude /usr/share/doc/*
path-include /usr/share/doc/*/copyright
path-exclude /usr/share/man/*
path-exclude /usr/share/groff/*
path-exclude /usr/share/info/*
path-exclude /usr/share/locale/*
CFG

    # Install general dependencies
    apt-get install -y --no-install-recommends \
        procps \
        dos2unix \
        curl \
        wget \
        zip \
        unzip

    # Python dependencies
    apt-get install -y --no-install-recommends \
        gcc \
        libc6-dev \
        libyaml-dev
    # Without a wheel, fail the build instead of silently building PyYAML without libyaml
    PYYAML_FORCE_LIBYAML=1 pip install --no-cache-dir --upgrade -r /requirements/tested.txt

    # C/C++ dependencies
    apt-get install -y --no-install-recommends \
        gcc \
        g++ \
        cppcheck
    # Debian owns these versions: fail the build when a release changes them
    [ "$(gcc -dumpversion)" = "$GCC_MAJOR" ] || { echo "gcc is $(gcc -dumpversion), expected $GCC_MAJOR" >&2; exit 1; }
    [ "$(g++ -dumpversion)" = "$GCC_MAJOR" ] || { echo "g++ is $(g++ -dumpversion), expected $GCC_MAJOR" >&2; exit 1; }
    case "$(cppcheck --version)" in "Cppcheck $CPPCHECK_VERSION"*) ;; *) echo "$(cppcheck --version), expected Cppcheck $CPPCHECK_VERSION" >&2; exit 1 ;; esac

    # Bash dependencies
    apt-get install -y --no-install-recommends \
        bc \
        binutils \
        bsdmainutils \
        cowsay \
        ed \
        figlet \
        file \
        toilet \
        tree \
        vim \
        xxd \
        xz-utils
    bash -c 'set -o pipefail && curl -fsSL "https://github.com/koalaman/shellcheck/releases/download/v${SHELLCHECK_VERSION}/shellcheck-v${SHELLCHECK_VERSION}.linux.x86_64.tar.xz" | tar -xJ --strip-components=1 -C /usr/local/bin "shellcheck-v${SHELLCHECK_VERSION}/shellcheck"'

    # JavaScript dependencies
    bash -c 'set -o pipefail && curl -fsSL https://deb.nodesource.com/setup_${NODE_MAJOR}.x | bash -'
    apt-get install -y --no-install-recommends nodejs
    apt-get install -y --no-install-recommends jq
    # TypeScript 7 (the Go port) has no JavaScript API: TESTed parses
    # submissions with TypeScript 6's, installed alongside as tsc6.
    # package.json only declares the versions for Dependabot; global install, not npm ci, as TESTed resolves modules through NODE_PATH.
    # shellcheck disable=SC2046
    npm install -g $(jq -r '.dependencies | to_entries | map("\(.key)@\(.value)") | join(" ")' /requirements/package.json)

    # Haskell dependencies
    apt-get install -y --no-install-recommends \
        lld \
        autoconf \
        build-essential \
        zlib1g-dev \
        libgmp-dev
    mkdir -p /usr/local/lib/hlint
    bash -c 'set -o pipefail && curl -fsSL "https://github.com/ndmitchell/hlint/releases/download/v${HLINT_VERSION}/hlint-${HLINT_VERSION}-x86_64-linux.tar.gz" | tar -xz --strip-components=1 -C /usr/local/lib/hlint'
    ln -s /usr/local/lib/hlint/hlint /usr/local/bin/hlint
    bash -c "set -o pipefail && curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh"
    bash -c "source /root/.ghcup/env && ghcup install ghc ${GHC_VERSION} --isolate $HASKELL_DIR/ghc"
    bash -c "source /root/.ghcup/env && ghcup install cabal ${CABAL_VERSION} --isolate $HASKELL_DIR/cabal"
    cabal update
    cabal v1-install --global aeson-${AESON_VERSION}

    # GHC: drop profiling libs/interfaces, haddock/HTML docs, and debug symbols (unused at runtime)
    find "$HASKELL_DIR/ghc" -type f \( -name '*_p.a' -o -name '*.p_o' -o -name '*.p_hi' \) -delete
    rm -rf "$HASKELL_DIR/ghc/share/doc" "$HASKELL_DIR/ghc"/lib/*/doc
    find "$HASKELL_DIR/ghc" -type f -name '*.so*' -exec strip --strip-unneeded {} + 2>/dev/null || true
    find "$HASKELL_DIR/ghc/bin" -type f -exec strip --strip-unneeded {} + 2>/dev/null || true

    # C# dependencies
    curl https://packages.microsoft.com/config/debian/13/packages-microsoft-prod.deb --output packages-microsoft-prod.deb
    dpkg -i packages-microsoft-prod.deb
    rm packages-microsoft-prod.deb
    apt-get update
    apt-get install -y --no-install-recommends dotnet-sdk-${DOTNET_SDK_CHANNEL}

    # Java and Kotlin dependencies
    bash -c 'set -o pipefail && curl -s "https://get.sdkman.io?rcupdate=false" | bash'
    chmod a+x "$SDKMAN_DIR/bin/sdkman-init.sh"
    bash -c "source \"$SDKMAN_DIR/bin/sdkman-init.sh\" && sdk install java ${TEMURIN_VERSION} && sdk install kotlin ${KOTLIN_VERSION}"
    curl -sSLO https://github.com/pinterest/ktlint/releases/download/${KTLINT_VERSION}/ktlint
    chmod a+x ktlint
    mv ktlint /usr/local/bin

    # Java specific dependencies
    mkdir -p /usr/local/share/checkstyle
    curl -fsSL -o "/usr/local/share/checkstyle/checkstyle-${CHECKSTYLE_VERSION}-all.jar" \
        "https://github.com/checkstyle/checkstyle/releases/download/checkstyle-${CHECKSTYLE_VERSION}/checkstyle-${CHECKSTYLE_VERSION}-all.jar"
    printf '#!/bin/sh\nexec java -jar /usr/local/share/checkstyle/checkstyle-%s-all.jar "$@"\n' "$CHECKSTYLE_VERSION" > /usr/local/bin/checkstyle
    chmod 755 /usr/local/bin/checkstyle

    # Exercise dependencies
    pip install --no-cache-dir --upgrade -r /requirements/exercises.txt

    # Clean up caches
    apt-get clean
    rm -rf /var/lib/apt/lists/*
    rm -rf /root/.cache
    rm -rf /root/.npm
    rm -rf /usr/local/sdkman/tmp
    # Haskell: downloaded Hackage tarballs and ghcup download cache (aeson lives in GHC's global db)
    rm -rf /root/.cabal/packages /root/.cabal/logs /root/.ghcup/cache /root/.ghcup/tmp
    # .NET: NuGet/dotnet download and first-run caches (SDK itself untouched)
    rm -rf /root/.nuget /root/.dotnet /tmp/NuGet* /tmp/.dotnet "${HOME}/.local/share/NuGet"
    # sdkman: extracted JDK/Kotlin zips, plus JDK sources and man pages (not needed to compile/run)
    rm -rf "$SDKMAN_DIR/archives/"* "$SDKMAN_DIR/tmp/"*
    rm -f  "$SDKMAN_DIR"/candidates/java/*/src.zip
    rm -rf "$SDKMAN_DIR"/candidates/java/*/man "$SDKMAN_DIR"/candidates/kotlin/*/licenses

    # Setup permissions and user
    chmod 711 /mnt
    useradd -m runner
    mkdir /home/runner/workdir
    chown -R runner:runner /home/runner/workdir
EOF

# Precompile TESTed's Haskell helper modules with the flags of Haskell.compilation
# in tested/languages/haskell/config.py. TESTed only uses them if these sources match
# its templates. ADD keys the build cache on the downloaded content, so the next
# image build recompiles them after a template change (a RUN curl stays cached).
# The judge reads them as runner, but a remote ADD writes mode 600, and
# ADD --chmod=644 would also set the directories it creates to 644.
# Compile in place: -outputdir changes the flags GHC compares.
ARG TESTED_TEMPLATES=https://raw.githubusercontent.com/dodona-edu/universal-judge/master/tested/languages/haskell/templates
# DL3020: hadolint does not see that ${TESTED_TEMPLATES} is a URL.
# hadolint ignore=DL3020
ADD ${TESTED_TEMPLATES}/EvaluationUtils.hs ${TESTED_TEMPLATES}/Values.hs /usr/local/share/tested/haskell/static/
RUN <<EOF
    set -eux
    chmod 644 /usr/local/share/tested/haskell/static/*.hs
    bash -c "cd /usr/local/share/tested/haskell/static && ghc -no-link -fno-cse -fno-full-laziness -O0 EvaluationUtils.hs Values.hs"
EOF

USER runner
WORKDIR /home/runner/workdir

COPY main.sh /main.sh
# Ship the recipe the image was built from, so the judge's CI can check its
# own copy against it.
COPY dodona-tested.dockerfile /dodona-tested.dockerfile
