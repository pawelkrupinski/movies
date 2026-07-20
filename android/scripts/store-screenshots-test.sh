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

# Parallel pool: ports and serials. A wrong offset would boot two workers onto
# one instance (they'd fight over the AVD) or leave gaps adb never sees.
check "worker 0 port"   "5554" "$(pool_port 0)"
check "worker 1 port"   "5556" "$(pool_port 1)"
check "worker 2 port"   "5558" "$(pool_port 2)"
check "worker 0 serial" "emulator-5554" "$(pool_serial 0)"
check "worker 2 serial" "emulator-5558" "$(pool_serial 2)"

# Country→worker distribution. Every country must be captured exactly once no
# matter how K divides them; a bug here silently drops or double-shoots a country.
trim() { echo "$1" | tr -s ' ' | sed 's/^ //;s/ $//'; }
check "K=3: each worker one country" "pl" "$(trim "$(worker_slice "pl uk de" 3 0)")"
check "K=3: worker 1"                "uk" "$(trim "$(worker_slice "pl uk de" 3 1)")"
check "K=3: worker 2"                "de" "$(trim "$(worker_slice "pl uk de" 3 2)")"
check "K=2: worker 0 takes remainder" "pl de" "$(trim "$(worker_slice "pl uk de" 2 0)")"
check "K=2: worker 1"                 "uk"    "$(trim "$(worker_slice "pl uk de" 2 1)")"
check "K=1: one worker takes all"    "pl uk de" "$(trim "$(worker_slice "pl uk de" 1 0)")"
# Union of every worker's slice = the whole list, once each (the property that
# actually matters: full coverage, no duplicates).
for K in 1 2 3; do
  got=""; for W in $(seq 0 $((K - 1))); do got="$got $(worker_slice "pl uk de" "$K" "$W")"; done
  check "K=$K covers every country once" "de pl uk" "$(echo $got | tr ' ' '\n' | sort | tr '\n' ' ' | sed 's/ $//')"
done

# effective_k clamps to [1, #countries]: never more emulators than countries,
# never zero, and a non-numeric request falls back to 1 rather than crashing.
check "effective_k caps at country count" "3" "$(effective_k 9 3)"
check "effective_k passes a valid k"      "2" "$(effective_k 2 3)"
check "effective_k floors at 1"           "1" "$(effective_k 0 3)"
check "effective_k rejects non-numeric"   "1" "$(effective_k xx 3)"

# The single ASCII token the app's Polish-only, substring-matching fold accepts.
# `adb shell input text` can't type ł/ó/ü, nor spaces/&/commas cleanly, so
# search_term returns ONE longest [a-z0-9] word of the app-folded name.
# Polish diacritics ARE folded by the app, so a one-word name types whole:
check "search Kraków → krakow"   "krakow"   "$(search_term "Kraków")"
check "search Wrocław → wroclaw" "wroclaw"  "$(search_term "Wrocław")"
check "search Łódź → lodz"       "lodz"     "$(search_term "Łódź")"
# German umlauts are NOT folded by the app, so we take the longest run past them:
check "search München → nchen"   "nchen"    "$(search_term "München")"
check "search Köln → ln"         "ln"       "$(search_term "Köln")"
# Multi-word / punctuated names collapse to their longest bare word, dodging the
# space and & that input text can't send — the substring match still finds them:
check "search West Yorkshire → yorkshire"    "yorkshire" "$(search_term "West Yorkshire")"
check "search Edinburgh & Lothians → edinburgh" "edinburgh" "$(search_term "Edinburgh & Lothians")"
check "search plain ASCII" "manchester" "$(search_term "Manchester")"

# --help must survive the header block growing: the new option near the top AND
# the env vars on the last line both have to survive, which a fixed line range
# stopped doing the moment the docs got longer.
check "usage documents --all-top" "1" "$(usage | grep -q -- '--all-top' && echo 1 || echo 0)"
check "usage documents EMULATORS"  "1" "$(usage | grep -q 'EMULATORS' && echo 1 || echo 0)"
check "usage reaches the last header line" "1" "$(usage | grep -q 'NO_OPEN' && echo 1 || echo 0)"
check "usage stops at the code" "" "$(usage | grep 'set -euo' || true)"

# type_ must send spaces as %s — `adb shell input text` otherwise types only the
# first word. search_term now avoids spaces in city queries, but CLEAN_FILM titles
# ("Toy Story 5") still flow through type_ verbatim.
_adbcap="$(mktemp)"
adb() { printf '%s' "$*" > "$_adbcap"; }              # capture args; type_ redirects its stdout
NOISE="$(mktemp)"
type_ "Toy Story 5"
check "type_ encodes spaces as %s" "shell input text Toy%sStory%s5" "$(cat "$_adbcap")"

# Regression — the N=2 city drop. run_worker must shoot EVERY ranked city, not
# just the first. The bug: a `while read … < <(rank_cities …)` loop whose body
# (cmd_capture → adb) reads stdin, draining the process substitution so the first
# capture eats the rest of the list — one city shot at N≥2, silently. Here
# cmd_capture is stubbed to record its (name:firstfile) args AND consume stdin
# the way adb does; both cities must still fire, at file offsets 1 and 5.
captured=""
cmd_capture()    { captured="$captured ${2}:${4}"; cat >/dev/null 2>&1; }   # $2 name, $4 first
rank_cities()    { printf 'countline\n9\tone\tCityOne\n8\ttwo\tCityTwo\n'; } # count + 2 cities
country_locale() { echo "xx-XX"; }
COUNTRIES="zz"                                    # worker_slice "zz" 1 0 → "zz"
run_worker 0 1 2 </dev/null >/dev/null 2>&1 || true
check "run_worker shoots all N cities (not just #1)" "CityOne:1 CityTwo:5" \
  "$(echo "$captured" | tr -s ' ' | sed 's/^ *//;s/ *$//')"

# Cleanup closes ONLY the emulators this run booted, addressing each by its serial
# via `adb emu kill` — a reused instance never enters BOOTED_EMULATORS, so a
# developer's own emulator survives. Stub adb to record the SERIAL it targets; the
# stub writes to its own file, unaffected by stop_emulators' stdout→NOISE redirect.
_killcap="$(mktemp)"
adb() { printf '%s: %s\n' "${SERIAL:-<none>}" "$*" >> "$_killcap"; }
NOISE="$(mktemp)"
BOOTED_EMULATORS="emulator-5554 emulator-5556"
stop_emulators
check "stop_emulators kills each booted serial" \
  "emulator-5554: emu kill
emulator-5556: emu kill" "$(cat "$_killcap")"
: > "$_killcap"
BOOTED_EMULATORS=""                               # nothing to kill → no adb call
stop_emulators
check "stop_emulators leaves reused emulators alone" "" "$(cat "$_killcap")"
# Leave the list empty so the sourced script's EXIT trap kills nothing on test end.

if [ "$fails" -eq 0 ]; then printf '\033[32m✓\033[0m all passed\n'; else printf '\033[31m✗\033[0m %s failed\n' "$fails"; fi
exit $((fails > 0))
