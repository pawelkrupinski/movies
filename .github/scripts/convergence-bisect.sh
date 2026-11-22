#!/usr/bin/env bash
# The name convergence-bisect.yml copies out of the checkout and runs, and ConvergenceBisectSpec runs:
# convergence_bisect.py beside it is the tool (standalone; its header says what each subcommand does).
exec python3 -B "$(dirname "${BASH_SOURCE[0]}")/convergence_bisect.py" "$@"
