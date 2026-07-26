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

# Blocks of four, end to end, zero-padded: city 1 → 001-004, city 2 → 005-008.
check "first city block"  "/d/001.png /d/002.png /d/003.png /d/004.png" "$(shot_paths /d 1 | tr '\n' ' ' | sed 's/ $//')"
check "second city block" "/d/005.png /d/006.png /d/007.png /d/008.png" "$(shot_paths /d 5 | tr '\n' ' ' | sed 's/ $//')"
check "blocks do not overlap" "" \
  "$(comm -12 <(shot_paths /d 1 | sort) <(shot_paths /d 5 | sort))"
# Zero-padding is what makes lexical order == numeric order past 9, so a plain
# glob or `ls` presents the blocks in capture order rather than 1, 10, 11, 2…
check "third city block" "/d/009.png /d/010.png /d/011.png /d/012.png" "$(shot_paths /d 9 | tr '\n' ' ' | sed 's/ $//')"
check "padding makes lexical order match numeric" "/d/009.png /d/010.png /d/011.png /d/012.png" \
  "$(shot_paths /d 9 | sort | tr '\n' ' ' | sed 's/ $//')"

# ── where shots land, and what Play actually publishes ───────────────────────
# gradle-play-publisher includes exactly `/listings/*/graphics/<dirName>/*`, and
# for phone shots ImageType.dirName is the KEBAB-CASE `phone-screenshots`. The
# camelCase `phoneScreenshots` is the Play API's field name and is never looked
# for on disk — a dir named that is silently skipped, which is precisely how the
# German listing came to ship with no screenshots while four PNGs sat in the repo.
check "captures live under the GPP dir" \
  "$LISTINGS/de-DE/graphics/phone-screenshots/candidates" "$(candidates_dir de-DE)"
# …and one segment deeper than the include pattern reaches, so raw captures can
# never be published by accident.
check "candidates is nested below the published dir" "candidates" \
  "$(basename "$(candidates_dir pl-PL)")"

# Every graphics dir in the repo must be a name GPP actually reads. This is the
# check that catches the whole class of bug: a typo, a camelCase slip, or a new
# asset type filed under an invented name all publish nothing, silently.
GPP_DIRNAMES="icon feature-graphic phone-screenshots tablet-screenshots large-tablet-screenshots tv-banner tv-screenshots wear-screenshots"
unknown=""
for d in "$LISTINGS"/*/graphics/*/; do
  [ -d "$d" ] || continue
  base="$(basename "$d")"
  case " $GPP_DIRNAMES " in *" $base "*) ;; *) unknown="$unknown $base";; esac
done
check "every graphics dir is a GPP ImageType.dirName" "" "$(echo $unknown)"

# The .gitignore blanket-ignores graphics/* and re-includes just the published
# screenshots dir, so the negation has to spell the SAME name — get it wrong and
# the shots stop being committed, and therefore stop publishing, with no error.
IGNORE="$HERE/../.gitignore"
check ".gitignore un-ignores the GPP dir" "1" \
  "$(grep -qxF '!app/src/main/play/listings/*/graphics/phone-screenshots/' "$IGNORE" && echo 1 || echo 0)"
check ".gitignore keeps candidates out of git" "1" \
  "$(grep -qxF 'app/src/main/play/listings/*/graphics/phone-screenshots/candidates/' "$IGNORE" && echo 1 || echo 0)"
check ".gitignore no longer names the API field" "" \
  "$(grep -vE '^[[:space:]]*#' "$IGNORE" | grep -n 'phoneScreenshots' || true)"

# next_shot_number is what makes runs APPEND rather than overwrite: it reports the
# number a fresh block starts at.
_shots="$(mktemp -d)"
check "empty dir starts at 1"   "1" "$(next_shot_number "$_shots")"
check "missing dir starts at 1" "1" "$(next_shot_number "$_shots/nope")"
touch "$_shots"/001.png "$_shots"/002.png "$_shots"/003.png "$_shots"/004.png
check "a four-shot block continues at 5" "5" "$(next_shot_number "$_shots")"
# 008 and 009 are the ones that bite: bash reads a leading-zero literal as OCTAL,
# and 8/9 are not octal digits, so an unguarded $((n)) aborts the whole script
# with "value too great for base". `10#` is what keeps this working.
touch "$_shots"/008.png "$_shots"/009.png
check "leading zeros are base 10, not octal" "10" "$(next_shot_number "$_shots")"
# The bug a lexical `ls | tail -1` would introduce: 10 sorts before 9, so the next
# run would start at 10 and overwrite an existing shot.
touch "$_shots"/010.png
check "10 counts above 9 (numeric, not lexical)" "11" "$(next_shot_number "$_shots")"
# Unpadded names from before the rename must still count, or the first run after
# upgrading would restart at 1 and overwrite them.
touch "$_shots"/12.png
check "legacy unpadded names still count" "13" "$(next_shot_number "$_shots")"
# Promoted/renamed strays must not derail the count.
touch "$_shots"/keep.png "$_shots"/2b.png
check "non-numeric names are ignored" "13" "$(next_shot_number "$_shots")"

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
check "usage documents the start rank" "1" "$(usage | grep -q -- '--all-top 2 4' && echo 1 || echo 0)"
check "usage documents --country-top" "1" "$(usage | grep -q -- '--country-top' && echo 1 || echo 0)"
check "usage documents skipping a bad city" "1" "$(usage | grep -qi 'SKIPPED, not fatal' && echo 1 || echo 0)"
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

# The optional START RANK must reach the ranking, or --top/--all-top silently
# re-shoot cities 1..N — the exact thing the option exists to avoid.
# Captured to a FILE, not a variable: cmd_top reads the ranking through `$( … )`
# and run_worker through `< <( … )`, both of which run the stub in a subshell
# where an assignment would die with it.
_rankcap="$(mktemp)"
rank_cities() { printf '%s' "$*" > "$_rankcap"; printf '99\n7\tone\tCityOne\n'; }
cmd_top uk 5 11 >/dev/null
check "--top forwards the start rank"      "uk 5 11" "$(cat "$_rankcap")"
cmd_top uk 5 >/dev/null
check "--top defaults the start rank to 1" "uk 5 1"  "$(cat "$_rankcap")"
# The table numbers rows by ABSOLUTE rank — with an offset in play a bare 1..N
# list would hide which slice you actually got.
check "--top numbers rows by absolute rank" "1" "$(cmd_top uk 1 11 | grep -c '^ *11\.')"

# --country-top is --all-top scoped to one country: it narrows the country list the
# pool machinery already walks rather than duplicating the capture loop, so one
# country means one emulator and identical append/skip/Preview behaviour.
_saved_countries="$COUNTRIES"
_alltopargs=""
cmd_all_top() { _alltopargs="$*"; }
cmd_country_top de 7 3
check "--country-top narrows the country list"     "de"  "$COUNTRIES"
check "--country-top forwards count + start rank"  "7 3" "$_alltopargs"
cmd_country_top pl 4
check "--country-top defaults the start rank to 1" "4 1" "$_alltopargs"
# A typo'd country must stop, not silently shoot nothing — country_locale returns
# empty for it, which would otherwise become a listing dir called "".
check "--country-top rejects an unknown country" "1" \
  "$( ( cmd_country_top fr 2 ) >/dev/null 2>&1; echo $? )"
check "--country-top rejects a missing country"  "1" \
  "$( ( cmd_country_top ) >/dev/null 2>&1; echo $? )"
COUNTRIES="$_saved_countries"

# Regression — the N=2 city drop. run_worker must shoot EVERY ranked city, not
# just the first. The bug: a `while read … < <(rank_cities …)` loop whose body
# (cmd_capture → adb) reads stdin, draining the process substitution so the first
# capture eats the rest of the list — one city shot at N≥2, silently.
#
# Recorded to FILES, not variables: run_worker now runs each cmd_capture in a
# subshell (so a city's die() can't kill the worker), and an assignment inside
# that subshell would vanish with it.
#
# LISTINGS is repointed at a temp dir first: run_worker mkdir -p's its
# destination, and with the repo path it used to leave a stray xx-XX/ listing
# behind on every test run.
LISTINGS="$(mktemp -d)"
_capcap="$(mktemp)"; _argcap="$(mktemp)"
cmd_capture() { # $2 city name, $4 first-file number
  printf '%s\n' "$2" >> "$_capcap"; printf '%s\n' "${4:-<none>}" >> "$_argcap"
  case "$2" in BadCity) exit 1;; esac      # `exit`, like the real die()
}
rank_cities()    { printf '%s' "$*" > "$_rankcap"
                   printf 'countline\n9\ta\tCityOne\n8\tb\tBadCity\n7\tc\tCityThree\n'; }
country_locale() { echo "xx-XX"; }
COUNTRIES="zz"                                    # worker_slice "zz" 1 0 → "zz"
_out="$(run_worker 0 1 3 4 </dev/null 2>&1 || true)"
check "run_worker shoots every ranked city (not just #1)" "CityOne BadCity CityThree" \
  "$(tr '\n' ' ' < "$_capcap" | sed 's/ $//')"
check "run_worker forwards the start rank" "zz 3 4" "$(cat "$_rankcap")"

# One bad city must not cost the rest of the country. cmd_capture ends in die()
# for a dozen ordinary reasons — blank list, mangled city search, stray tap — and
# run_worker used to stage the whole country and swap only at the end, so a single
# failure discarded every city already shot. At --all-top 10 that reliably wiped
# whole countries: 10/10 for Poland, 0 for the UK and Germany.
check "a failed city does not stop the ones after it" "1" \
  "$(grep -c '^CityThree$' "$_capcap" || true)"
check "the tally counts only what landed" "1" \
  "$(printf '%s' "$_out" | grep -c '2/3 cities')"
check "the failed city is named" "1" \
  "$(printf '%s' "$_out" | grep -c 'failed on BadCity')"
# Numbering is cmd_capture's job now (it appends from next_shot_number), so
# run_worker must NOT pin a first-file number — pinning one would reopen the
# overwrite bug whenever a city was skipped.
check "run_worker leaves numbering to cmd_capture" "<none> <none> <none>" \
  "$(tr '\n' ' ' < "$_argcap" | sed 's/ $//')"

# Every city failing is worth saying out loud rather than reporting a cheerful 0.
: > "$_capcap"
cmd_capture() { printf '%s\n' "$2" >> "$_capcap"; exit 1; }
_allbad="$(run_worker 0 1 3 1 </dev/null 2>&1 || true)"
check "all cities failing is reported, not glossed over" "1" \
  "$(printf '%s' "$_allbad" | grep -c 'NOTHING captured')"
check "it still tried every city" "3" "$(wc -l < "$_capcap" | tr -d ' ')"

# ── the closing Preview shows THIS run's shots, not the whole pile ────────────
# Runs append, so "open everything in candidates/" would reopen every shot ever
# taken — after three --all-top 2 runs that is ~72 images, mostly rejects. The
# baseline captured before the run is what narrows it back down.
mkdir -p "$(candidates_dir xx-XX)"
touch "$(candidates_dir xx-XX)"/001.png "$(candidates_dir xx-XX)"/002.png \
      "$(candidates_dir xx-XX)"/003.png "$(candidates_dir xx-XX)"/004.png
check "baselines record where each locale ends" "xx-XX:5" "$(baselines)"
check "baseline_for reads a locale out"      "5" "$(baseline_for "xx-XX:5 yy-YY:9" xx-XX)"
check "baseline_for reads the second"        "9" "$(baseline_for "xx-XX:5 yy-YY:9" yy-YY)"
# An untouched locale defaults to 1 — showing everything beats showing nothing.
check "baseline_for defaults to 1"           "1" "$(baseline_for "xx-XX:5" zz-ZZ)"

# `command -v` finds shell functions, so stubbing `open` intercepts the real one.
_opencap="$(mktemp)"
open() { printf '%s\n' "$@" >> "$_opencap"; }
touch "$(candidates_dir xx-XX)"/005.png "$(candidates_dir xx-XX)"/006.png
preview_all "xx-XX:5"
check "Preview opens only shots added since the baseline" \
  "$(candidates_dir xx-XX)/005.png
$(candidates_dir xx-XX)/006.png" "$(grep -Ev '^(-a|Preview)$' "$_opencap")"
: > "$_opencap"
preview_all "xx-XX:99"        # nothing new → nothing to open
check "Preview stays shut when the run added nothing" "" \
  "$(grep -Ev '^(-a|Preview)$' "$_opencap")"
unset -f open

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
