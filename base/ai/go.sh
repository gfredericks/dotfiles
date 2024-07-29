#!/bin/bash

set -Eeou pipefail

VENV_DIR=venv

cd "$(dirname "$(realpath "$0")")"

if [ ! -d "$VENV_DIR" ]; then
    echo >&2 "Doing one-time venv setup..."
    python3 -m venv "$VENV_DIR"
    . "$VENV_DIR"/bin/activate
    pip install openai
else
    . "$VENV_DIR"/bin/activate
fi

exec python ai.py "$@"
