#!/bin/sh
# Purpose: The identity takes check, for one country inside its hermetic convergence row (a name for identity_takes_check.py)
# THE IDENTITY TAKES CHECK's name for the workflows (country-convergence-leg.yml) and the path lists that name it: the
# check itself is scripts/ci/identity_takes_check.py (its header says what it does; identity_takes_check_test.py).
exec python3 "$(dirname "$0")/identity_takes_check.py" "$@" # scripts/ci/identity_takes_check.py
