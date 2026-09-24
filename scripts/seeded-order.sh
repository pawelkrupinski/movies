#!/usr/bin/env bash
# The seeded test order the nightly order-independence legs share
# (ios/scripts/swift-test-shuffled.sh, android/scripts/devtest-shuffled.sh).
#
# THE SAME SEED MUST GIVE THE SAME ORDER ON EVERY MACHINE, because the whole
# point of printing the seed is that a failure found on a CI runner can be
# replayed on a laptop. The first version keyed each line with awk's
# `srand(seed); rand()`, and awk's generator is the implementation's own: the
# Android leg ran under Ubuntu's mawk and was replayed under macOS's BSD awk,
# which put the same seed's classes in a different order (seed 17234567890 over
# a..f: mawk `e a c b d f`, BSD awk `e a b c d f`). The key here is the POSIX
# CRC of "<seed>\n<line>" from `cksum`, whose output the standard fixes, and
# the sort runs in the C locale so collation cannot differ either.
#
# Usage:
#   . "$REPO_ROOT/scripts/seeded-order.sh"
#   printf '%s\n' "$lines" | seeded_order "$seed"    # blank lines are dropped

seeded_order() {
  local seed="$1" line key
  while IFS= read -r line || [ -n "$line" ]; do
    [ -n "$line" ] || continue
    key="$(printf '%s\n%s' "$seed" "$line" | cksum)"
    printf '%s\t%s\n' "${key%% *}" "$line"
  done | LC_ALL=C sort -t "$(printf '\t')" -k1,1n -k2 | cut -f2-
}
