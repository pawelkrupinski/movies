#!/usr/bin/env bash
#
# Unit tests for the pure helpers in store-screenshots.sh — the mappings and the
# file-numbering that decide WHERE a capture lands and WHICH label it hunts for.
#
#   android/scripts/store-screenshots-test.sh
#
# Everything else in that script drives a live emulator and is verified by
# running it for real; these are the parts that can be checked without one, and
# they are the parts a typo silently breaks (a wrong locale dir publishes German
# shots to the UK listing, a wrong offset overwrites city 1 with city 2).
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=store-screenshots.sh
source "$HERE/store-screenshots.sh"          # sourcing must not start a capture

fails=0
check() { # $1 what, $2 expected, $3 actual
  if [ "$2" = "$3" ]; then printf '  \033[32m✓\033[0m %s\n' "$1"
  else printf '  \033[31m✗\033[0m %s\n     expected: %s\n     actual:   %s\n' "$1" "$2" "$3"; fails=$((fails + 1)); fi
}

printf '\033[36m▸\033[0m store-screenshots helpers\n'

# Every country --all-top walks must map to a locale and back, or a run writes
# into the wrong listing dir.
for c in $COUNTRIES; do
  check "$c → locale → country round-trips" "$c" "$(locale_country "$(country_locale "$c")")"
  check "$c has a listing locale"      "1" "$([ -n "$(country_locale "$c")" ] && echo 1 || echo 0)"
  check "$c has an area-picker label"  "1" "$([ -n "$(showlist_label "$c")" ] && echo 1 || echo 0)"
done
check "unknown country has no locale" "" "$(country_locale "fr")"

# The labels are what the split-city detection greps for; a wrong one means
# London silently captures the area picker instead of the listing.
check "uk area-picker label" "Show listings"     "$(showlist_label uk)"
check "pl area-picker label" "Pokaż repertuar"   "$(showlist_label pl)"
check "de area-picker label" "Programm anzeigen" "$(showlist_label de)"

# Blocks of four, end to end: city 1 → 1-4, city 2 → 5-8.
check "first city block"  "/d/1.png /d/2.png /d/3.png /d/4.png" "$(shot_paths /d 1 | tr '\n' ' ' | sed 's/ $//')"
check "second city block" "/d/5.png /d/6.png /d/7.png /d/8.png" "$(shot_paths /d 5 | tr '\n' ' ' | sed 's/ $//')"
check "blocks do not overlap" "" \
  "$(comm -12 <(shot_paths /d 1 | sort) <(shot_paths /d 5 | sort))"
# Past 9 the numbering must stay numeric, not lexical — this is why the Preview
# list is built from shot_paths rather than `ls`.
check "third city block" "/d/9.png /d/10.png /d/11.png /d/12.png" "$(shot_paths /d 9 | tr '\n' ' ' | sed 's/ $//')"

# --help must survive the header block growing: the new option near the top AND
# the env vars on the last line both have to survive, which a fixed line range
# stopped doing the moment the docs got longer.
check "usage documents --all-top" "1" "$(usage | grep -q -- '--all-top' && echo 1 || echo 0)"
check "usage reaches the last header line" "1" "$(usage | grep -q 'NO_OPEN' && echo 1 || echo 0)"
check "usage stops at the code" "" "$(usage | grep 'set -euo' || true)"

if [ "$fails" -eq 0 ]; then printf '\033[32m✓\033[0m all passed\n'; else printf '\033[31m✗\033[0m %s failed\n' "$fails"; fi
exit $((fails > 0))
