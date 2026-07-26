#!/usr/bin/env bash
#
# Unit tests for the pure helpers in ios/scripts/store-screenshots.sh — the parts
# that decide WHERE a capture lands, WHICH deep links it walks, and how a failed
# city is handled. Everything else drives a live simulator and is verified by
# running it for real.
#
#   ios/scripts/store-screenshots-test.sh
#
# The shared engine (countries, ranking, numbering, candidates) is covered by
# android/scripts/store-screenshots-test.sh; what is checked here is the iOS
# driver's own behaviour and that it inherits the shared layout correctly.
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=store-screenshots.sh
source "$HERE/store-screenshots.sh"          # sourcing must not start a capture

fails=0
check() { # $1 what, $2 expected, $3 actual
  if [ "$2" = "$3" ]; then printf '  \033[32m✓\033[0m %s\n' "$1"
  else printf '  \033[31m✗\033[0m %s\n     expected: %s\n     actual:   %s\n' "$1" "$2" "$3"; fails=$((fails + 1)); fi
}

printf '\033[36m▸\033[0m ios store-screenshots helpers\n'

# ── the layout mirrors Android ────────────────────────────────────────────────
# Same vocabulary as gradle-play-publisher's ImageType.dirName, so both stores are
# laid out identically and the promote-by-hand habit transfers.
check "shots land under ios/store/listings" \
  "$REPO_ROOT/ios/store/listings/de-DE/graphics/phone-screenshots/candidates" \
  "$(candidates_dir de-DE)"
check "candidates is nested below the published dir" "candidates" \
  "$(basename "$(candidates_dir pl-PL)")"
# The published dir is the parent — that is what gets uploaded, and it must NOT be
# the scratchpad.
check "the published dir is one level up" "phone-screenshots" \
  "$(basename "$(dirname "$(candidates_dir en-GB)")")"
# Tablets reuse the whole machinery through SHOT_CLASS rather than a second script.
_saved_class="$SHOT_CLASS"
SHOT_CLASS="tablet-screenshots"
check "SHOT_CLASS redirects to the tablet dir" "tablet-screenshots" \
  "$(basename "$(dirname "$(candidates_dir de-DE)")")"
check "the tablet device is picked for it" "$IOS_TABLET" "$(device_for_class tablet-screenshots)"
SHOT_CLASS="$_saved_class"
check "phones are the default class" "$IOS_PHONE" "$(device_for_class phone-screenshots)"
check "an unknown class falls back to the phone" "$IOS_PHONE" "$(device_for_class whatever)"

# ── the five deep links ───────────────────────────────────────────────────────
# Screens are reached by deep link, not taps, which is what lets one script serve
# every device size. Order is the store order: listing, rating, detail, next day,
# Filtry.
check "five screens, in order" \
  "kinowo://london kinowo://london?sort=rating kinowo://london/film?title=Dune kinowo://london?date=tomorrow kinowo://london" \
  "$(capture_urls london Dune | tr '\n' ' ' | sed 's/ $//')"
# The title arrives already url-encoded from first_film; it must be passed through
# untouched or a film with a space or & breaks the link.
check "an encoded title is passed through verbatim" \
  "kinowo://poznan/film?title=Toy%20Story%205" \
  "$(capture_urls poznan 'Toy%20Story%205' | sed -n 3p)"
# No film (an empty listing, or the API down) must still yield FIVE urls, or the
# capture loop would index past the end and leave a hole in the numbering.
check "a missing film still yields five screens" "5" "$(capture_urls london '' | wc -l | tr -d ' ')"
check "a missing film repeats the listing" "kinowo://london" "$(capture_urls london '' | sed -n 3p)"

# ── the Filtry screen ─────────────────────────────────────────────────────────
# It is the LAST screen, and it rides the plain listing url: the sheet opens over
# the listing, so a url carrying a sort or a date would show through behind it.
check "iOS shoots five screens a city" "5" "$SHOTS_PER_CITY"
check "Filtry is the last screen"      "4" "$(filters_screen_index)"
check "Filtry rides the plain listing" "kinowo://london" "$(capture_urls london Dune | sed -n 5p)"
# The numbering must follow, or a second city would overwrite the first's Filtry
# shot — the shared helper hands out blocks of SHOTS_PER_CITY, not a fixed four.
check "a city's block is five files" \
  "/d/001.png /d/002.png /d/003.png /d/004.png /d/005.png" \
  "$(shot_paths /d 1 | tr '\n' ' ' | sed 's/ $//')"
check "the next city starts past it" "/d/006.png" "$(shot_paths /d 6 | sed -n 1p)"
check "blocks never overlap" "" "$(comm -12 <(shot_paths /d 1 | sort) <(shot_paths /d 6 | sort))"

# ── --country-top ─────────────────────────────────────────────────────────────
# Before the stubs below: this needs the REAL country_locale to reject a bad code.
_alltopargs=""
cmd_all_top() { _alltopargs="$*"; }
cmd_country_top de 7 3
check "--country-top narrows the country list"     "de"  "$COUNTRIES"
check "--country-top forwards count + start rank"  "7 3" "$_alltopargs"
check "--country-top rejects an unknown country" "1" \
  "$( ( cmd_country_top fr 2 ) >/dev/null 2>&1; echo $? )"

# ── a failed city is skipped, not fatal ───────────────────────────────────────
# cmd_capture ends in die() (an exit) for ordinary reasons — a screen that never
# rendered, a city missing from the catalog. run_worker must lose that city only.
# Recorded to a FILE: each cmd_capture runs in a subshell, where an assignment
# would vanish with it.
LISTINGS="$(mktemp -d)"
_capcap="$(mktemp)"; _argcap="$(mktemp)"
cmd_capture() { # $2 city, $4 first-file number
  printf '%s\n' "$2" >> "$_capcap"; printf '%s\n' "${4:-<none>}" >> "$_argcap"
  case "$2" in BadCity) exit 1;; esac
}
rank_cities()    { printf 'countline\n9\ta\tCityOne\n8\tb\tBadCity\n7\tc\tCityThree\n'; }
country_locale() { echo "xx-XX"; }
COUNTRIES="zz"
_out="$(run_worker 0 1 3 4 </dev/null 2>&1 || true)"
check "every ranked city is attempted"          "CityOne BadCity CityThree" \
  "$(tr '\n' ' ' < "$_capcap" | sed 's/ $//')"
check "a failed city does not stop the next"    "1" "$(grep -c '^CityThree$' "$_capcap" || true)"
check "the tally counts only what landed"       "1" "$(printf '%s' "$_out" | grep -c '2/3 cities')"
check "the failed city is named"                "1" "$(printf '%s' "$_out" | grep -c 'failed on BadCity')"
# Numbering is cmd_capture's job (it appends from next_shot_number), so run_worker
# must not pin a first-file number — pinning one reopens the overwrite bug the
# moment a city is skipped.
check "run_worker leaves numbering to cmd_capture" "<none> <none> <none>" \
  "$(tr '\n' ' ' < "$_argcap" | sed 's/ $//')"

: > "$_capcap"
cmd_capture() { printf '%s\n' "$2" >> "$_capcap"; exit 1; }
_allbad="$(run_worker 0 1 3 1 </dev/null 2>&1 || true)"
check "all cities failing is said out loud" "1" "$(printf '%s' "$_allbad" | grep -c 'NOTHING captured')"
check "it still tried every city"           "3" "$(wc -l < "$_capcap" | tr -d ' ')"

# ── the docs stay honest ──────────────────────────────────────────────────────
check "usage documents --country-top" "1" "$(usage | grep -q -- '--country-top' && echo 1 || echo 0)"
check "usage documents the start rank" "1" "$(usage | grep -q -- '--all-top 2 4' && echo 1 || echo 0)"
check "usage documents the candidates pile" "1" "$(usage | grep -q 'candidates/' && echo 1 || echo 0)"
check "usage reaches the last header line" "1" "$(usage | grep -q 'NO_OPEN' && echo 1 || echo 0)"
check "usage stops at the code" "" "$(usage | grep 'set -euo' || true)"

if [ "$fails" -eq 0 ]; then printf '\033[32m✓\033[0m all passed\n'; else printf '\033[31m✗\033[0m %s failed\n' "$fails"; fi
exit $((fails > 0))
