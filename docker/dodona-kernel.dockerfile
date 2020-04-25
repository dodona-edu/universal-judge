FROM python:3.8-buster

# First, install all necessary packages for running things.
RUN apt-get update && apt-get install -y default-jdk haskell-platform gcc-8

RUN pip install jsonschema psutil mako pydantic==1.4 toml typing_inspect pylint
RUN cabal update && cabal install aeson --global
ENV PATH $HOME/.cabal/bin:$PATH

RUN chmod 711 /mnt

RUN useradd -m runner
RUN mkdir /home/runner/workdir && chown runner:runner /home/runner/workdir

USER runner
WORKDIR /home/runner/workdir

COPY main.sh /main.sh

