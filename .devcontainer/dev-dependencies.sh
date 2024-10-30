#!/bin/bash

# Install dev dependencies

# install poetry
pip install poetry --user
# Install dependencies (include --dev for tests/development)
poetry install --dev
