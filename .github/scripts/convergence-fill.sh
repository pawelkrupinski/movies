#!/usr/bin/env bash
# The name the convergence-fill action, convergence-setup's pair, restore-enrichment-tree.sh and convergence_reuse.py
# run: convergence_fill.py beside it is the tool (its header says what each subcommand does).
exec python3 -B "$(dirname "${BASH_SOURCE[0]}")/convergence_fill.py" "$@"
