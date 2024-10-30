FROM python:3.12.4-slim-bullseye

# Environment Kotlin
ENV SDKMAN_DIR /usr/local/sdkman
ENV HASKELL_DIR /usr/local/ghcupdir
ENV PATH $SDKMAN_DIR/candidates/kotlin/current/bin:$PATH
ENV PATH $SDKMAN_DIR/candidates/java/current/bin:$PATH
ENV PATH $HASKELL_DIR/ghc/bin:$PATH
ENV PATH $HASKELL_DIR/cabal:$PATH
ENV NODE_PATH /usr/lib/node_modules
# Add manual directory for default-jdk
RUN mkdir -p /usr/share/man/man1mkdir -p /usr/share/man/man1 \
 && apt-get update \
 # Install additional dependencies
 && apt-get install -y --no-install-recommends \
       procps \
       dos2unix \
       curl \
       zip \
       unzip \
       # Bash language dependencies
       bc binutils bsdmainutils cowsay ed figlet file toilet tree vim xxd \
       # Dependencies for GHCup
       autoconf build-essential zlib1g-dev libgmp-dev \
 && curl https://packages.microsoft.com/config/debian/11/packages-microsoft-prod.deb --output packages-microsoft-prod.deb \
 && dpkg -i packages-microsoft-prod.deb \
 && rm packages-microsoft-prod.deb \
 # JavaScript dependencies
 && bash -c 'set -o pipefail && curl -fsSL https://deb.nodesource.com/setup_22.x | bash -' \
 # Haskell dependencies
 && bash -c "set -o pipefail && curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1 sh" \
 && bash -c "source /root/.ghcup/env && ghcup install ghc 9.6 --isolate $HASKELL_DIR/ghc" \
 && bash -c "source /root/.ghcup/env && ghcup install cabal --isolate $HASKELL_DIR/cabal" \
  # Install programming languages \
 && apt-get install -y --no-install-recommends \
       # Checkstyle, other Java stuff uses SDKMAN! below
       checkstyle \
       hlint \
       # TESTed C judge dependency
       gcc \
       cppcheck \
       # TESTed Javascript judge dependency
       nodejs \
       # TESTed bash judge dependency
       shellcheck \
       # C# dependency
       dotnet-sdk-8.0 \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/* \
 # TESTed Judge depencencies
 && pip install --no-cache-dir --upgrade psutil==5.9.8 attrs==23.2.0 cattrs==23.2.3 jsonschema==4.22.0 typing_inspect==0.9.0 pyyaml==6.0.1 Pygments==2.18.0 python-i18n==0.3.9 pylint==3.0.1 \
 # TESTed Kotlin judge dependencies
 && bash -c 'set -o pipefail && curl -s "https://get.sdkman.io?rcupdate=false" | bash' \
 && chmod a+x "$SDKMAN_DIR/bin/sdkman-init.sh" \
 && bash -c "source \"$SDKMAN_DIR/bin/sdkman-init.sh\" && sdk install java 21.0.3-tem && sdk install kotlin" \
 && curl -sSLO https://github.com/pinterest/ktlint/releases/download/1.2.1/ktlint \
 && chmod a+x ktlint \
 && mv ktlint /usr/local/bin \
 # JavaScript dependencies
 && npm install -g eslint@8.57 abstract-syntax-tree@2.22 \
 # Haskell dependencies
 && cabal update \
 && cabal v1-install --global aeson \
 # Make sure the students can't find our secret path, which is mounted in
 # /mnt with a secure random name.
 && chmod 711 /mnt \
 # Add the user which will run the student's code and the judge.
 && useradd -m runner \
 && mkdir /home/runner/workdir \
 && chown -R runner:runner /home/runner/workdir

USER runner
WORKDIR /home/runner/workdir

COPY main.sh /main.sh
