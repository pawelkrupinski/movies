#!/usr/bin/env bash
# The order seed for the nightly order-independence run (.github/workflows/
# order-independence.yml): the dispatch input when one was given, else the run id.
# Digits only: the seed reaches shells and the emulator runner's `script:`, and a
# free-text workflow_dispatch input is anyone-with-dispatch-rights' shell.
#
#   scripts/ci/order-seed.sh <requested-or-empty> <fallback>
set -uo pipefail
seed="${1:-${2:-}}"
if ! [[ "$seed" =~ ^[0-9]+$ ]]; then
    echo "order-seed: seed must be digits only, got '${seed}'" >&2
    exit 1
fi
printf '%s\n' "$seed"
