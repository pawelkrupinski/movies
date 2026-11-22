#!/bin/sh
# THE IDENTITY TAKES CHECK's name for the workflows (country-convergence-leg.yml) and the path lists that name it: the
# check itself is scripts/ci/identity_takes_check.py (its header says what it does; identity_takes_check_test.py).
exec python3 "$(dirname "$0")/identity_takes_check.py" "$@" # scripts/ci/identity_takes_check.py
