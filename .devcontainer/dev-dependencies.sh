#!/bin/bash

# Install dev dependencies
pip install --no-cache-dir --upgrade \
    pytest==8.2.1 \
    pytest-mock==3.14.0 \
    pytest-cov==5.0.0 \
    pytest-xdist==3.6.1 \
    syrupy==4.6.1 \
    black==24.4.2 \
    isort==5.13.2 \
    pyright==1.1.387

# add installed packages to path
cat <<EOF >> /home/runner/.bashrc
export PATH=$PATH:/home/runner/.local/bin
EOF
