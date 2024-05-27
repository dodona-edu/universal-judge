# Inherit from the Docker image for Dodona.
FROM dodona/dodona-tested

# Go back to being root.
USER root
WORKDIR /

# Install some additional dependencies needed for testing.
RUN pip install --no-cache-dir --upgrade pytest pytest-mock pytest-xdist

# The source of the judge is available in GITHUB_WORKSPACE.
# See https://docs.github.com/en/actions/creating-actions/dockerfile-support-for-github-actions
CMD pytest -n auto ${TESTED_SOURCE}/tests/
