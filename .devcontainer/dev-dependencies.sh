#!/bin/bash

# Install dev dependencies

# install poetry
pip install poetry --user
# Install dependencies
python -m poetry install --with dev
