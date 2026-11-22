#!/usr/bin/env bash
# The name the convergence-setup action, the workflows' preflights, restore-country-corpus.sh and convergence_reuse.py
# run: convergence_setup.py beside it is the tool (its header says what each subcommand does).
exec python3 -B "$(dirname "${BASH_SOURCE[0]}")/convergence_setup.py" "$@"
