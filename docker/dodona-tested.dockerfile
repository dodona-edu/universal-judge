FROM python:3.9.1-slim-buster

# First, install all necessary packages for running things.
RUN mkdir -p /usr/share/man/man1/
RUN apt-get update && apt-get install -y --no-install-recommends dos2unix curl zip unzip
RUN curl -fsSL https://deb.nodesource.com/setup_14.x | bash -
RUN apt-get update && apt-get install -y --no-install-recommends openjdk-11-jdk=11.0.9.1+1-1~deb10u2 haskell-platform gcc-8 nodejs hlint=2.1.10-2+b1 cppcheck=1.86-1 shellcheck
ENV SDKMAN_DIR /usr/local/sdkman
RUN curl -s "https://get.sdkman.io?rcupdate=false" | bash
RUN chmod a+x "$SDKMAN_DIR/bin/sdkman-init.sh"
RUN /bin/bash -c "source \"$SDKMAN_DIR/bin/sdkman-init.sh\" && sdk install kotlin 1.4.10"
ENV PATH $SDKMAN_DIR/candidates/kotlin/current/bin:$PATH

RUN pip install jsonschema psutil mako pydantic==1.7.3 toml typing_inspect pylint esprima==4.0.1 lark==0.10.1 pyyaml==5.3.1 python-i18n==0.3.9 Pygments==2.7.4
RUN cabal update && cabal install aeson --global --force-reinstalls
RUN npm install -g eslint@7.23.0

RUN chmod 711 /mnt

RUN useradd -m runner
RUN mkdir /home/runner/workdir && chown runner:runner /home/runner/workdir

USER runner
WORKDIR /home/runner/workdir

COPY main.sh /main.sh

