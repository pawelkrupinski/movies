#!/usr/bin/env bash
#
# seeded-order.sh's one promise: a seed names ONE order, whichever machine
# computes it. The golden order below is the contract -- it is asserted on the
# Linux runner (android.yml's script-helper step) and on a Mac alike, which is
# exactly the pair the nightly order-independence run crosses when a failure's
# seed is replayed locally.
#
#   scripts/seeded-order-test.sh
#
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$HERE/shell-spec.sh"
. "$HERE/seeded-order.sh"

printf '\033[36m▸\033[0m seeded-order.sh\n'

lines="$(printf '%s\n' a b c d e f)"
ordered() { printf '%s\n' "$lines" | seeded_order "$1" | tr '\n' ' '; }

check "a seed names a fixed order, on every platform" \
  "c d f a b e " "$(ordered 17234567890)"
check "...and a different seed another one" \
  "d c a f e b " "$(ordered 1)"
check "the same seed twice is the same order" \
  "$(ordered 99)" "$(ordered 99)"
check "every line comes back exactly once" \
  "a b c d e f " "$(printf '%s\n' "$lines" | seeded_order 7 | LC_ALL=C sort | tr '\n' ' ')"
check "blank lines are dropped, a last line without a newline is kept" \
  "2" "$(printf 'x\n\ny' | seeded_order 3 | wc -l | tr -d ' ')"
check "a name with regex and shell metacharacters passes through untouched" \
  'KinowoCoreTests.A/test$(x)*[y] ' "$(printf '%s\n' 'KinowoCoreTests.A/test$(x)*[y]' | seeded_order 5 | tr '\n' ' ')"

spec_summary
