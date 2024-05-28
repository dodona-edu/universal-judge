# Inherit from the Docker image for Dodona.
FROM dodona/dodona-tested

# Go back to being root.
USER root
WORKDIR /

# Install some additional dependencies needed for testing.
RUN pip install --no-cache-dir --upgrade pytest pytest-mock pytest-xdist jinja2 marko

# The source of the judge is available in TESTED_SOURCE.
CMD pytest -n auto ${TESTED_SOURCE}/tests/test_integration_javascript.py
