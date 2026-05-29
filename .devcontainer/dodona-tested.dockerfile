# This is the Dockerfile for the tested judge.
# It can be downloaded using docker pull dodona/dodona-tested.

# This docker image is run in our production environment.
# It should not contain any development tools or dependencies.
# Add those to dev-dependencies.sh instead.

FROM python:3.12.4-slim-bullseye

# Set up the environment

# Kotlin
ENV SDKMAN_DIR=/usr/local/sdkman
ENV PATH=$SDKMAN_DIR/candidates/kotlin/current/bin:$PATH
ENV PATH=$SDKMAN_DIR/candidates/java/current/bin:$PATH
# Haskell
ENV HASKELL_DIR=/usr/local/ghcupdir
ENV PATH=$HASKELL_DIR/ghc/bin:$PATH
ENV PATH=$HASKELL_DIR/cabal:$PATH
# Node
ENV NODE_PATH=/usr/lib/node_modules

# Install dependencies
# hadolint ignore=DL3013,DL3016
RUN <<EOF
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
        zip \
        unzip

    # Python dependencies
    pip install --no-cache-dir --upgrade \
        psutil==5.9.8 \
        attrs==23.2.0 \
        cattrs==23.2.3 \
        jsonschema==4.22.0 \
        typing_inspect==0.9.0 \
        pyyaml==6.0.1 \
        Pygments==2.18.0 \
        python-i18n==0.3.9 \
        pylint==3.0.1

    # C/C++ dependencies
    apt-get install -y --no-install-recommends \
        gcc \
        cppcheck

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
        shellcheck

    # JavaScript dependencies
    bash -c 'set -o pipefail && curl -fsSL https://deb.nodesource.com/setup_22.x | bash -'
    apt-get install -y --no-install-recommends nodejs
    npm install -g eslint@8.57 abstract-syntax-tree@2.22

    # TypeScript dependencies
    npm install -g typescript@5.6.3 tsx@4.19.2
    npm install -g @types/node @typescript-eslint/parser @typescript-eslint/eslint-plugin

    # Haskell dependencies
    apt-get install -y --no-install-recommends \
        hlint \
        autoconf \
        build-essential \
        zlib1g-dev \
        libgmp-dev
    bash -c "set -o pipefail && curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh"
    bash -c "source /root/.ghcup/env && ghcup install ghc 9.6 --isolate $HASKELL_DIR/ghc"
    bash -c "source /root/.ghcup/env && ghcup install cabal --isolate $HASKELL_DIR/cabal"
    cabal update
    cabal v1-install --global aeson

    # GHC: drop profiling libs/interfaces, haddock/HTML docs, and debug symbols (unused at runtime)
    find "$HASKELL_DIR/ghc" -type f \( -name '*_p.a' -o -name '*.p_o' -o -name '*.p_hi' \) -delete
    rm -rf "$HASKELL_DIR/ghc/share/doc" "$HASKELL_DIR/ghc"/lib/*/doc
    find "$HASKELL_DIR/ghc" -type f -name '*.so*' -exec strip --strip-unneeded {} + 2>/dev/null || true
    find "$HASKELL_DIR/ghc/bin" -type f -exec strip --strip-unneeded {} + 2>/dev/null || true

    # C# dependencies
    curl https://packages.microsoft.com/config/debian/11/packages-microsoft-prod.deb --output packages-microsoft-prod.deb
    dpkg -i packages-microsoft-prod.deb
    rm packages-microsoft-prod.deb
    apt-get update
    apt-get install -y --no-install-recommends dotnet-sdk-8.0

    # Java and Kotlin dependencies
    bash -c 'set -o pipefail && curl -s "https://get.sdkman.io?rcupdate=false" | bash'
    chmod a+x "$SDKMAN_DIR/bin/sdkman-init.sh"
    bash -c "source \"$SDKMAN_DIR/bin/sdkman-init.sh\" && sdk install java 21.0.3-tem && sdk install kotlin"
    curl -sSLO https://github.com/pinterest/ktlint/releases/download/1.2.1/ktlint
    chmod a+x ktlint
    mv ktlint /usr/local/bin

    # Java specific dependencies
    apt-get install -y --no-install-recommends checkstyle

    # Exercise dependencies
    pip install --no-cache-dir --upgrade \
      numpy==2.3.5 \
      pandas==3.0.1 \
      openpyxl==3.1.5

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

USER runner
WORKDIR /home/runner/workdir

COPY main.sh /main.sh
