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
SHOT_CLASS="tablet-11-screenshots"
check "the 11-inch class gets its own dir" "tablet-11-screenshots" \
  "$(basename "$(dirname "$(candidates_dir de-DE)")")"
SHOT_CLASS="$_saved_class"
# The 11" iPad is a DIFFERENT device, not the 13" renamed — that is the whole
# point of shooting it rather than letting Apple derive it.
check "the 11-inch iPad is its own device" "$IOS_TABLET_11" "$(device_for_class tablet-11-screenshots)"
check "the two iPads are different devices" "differ" \
  "$([ "$(device_for_class tablet-screenshots)" != "$(device_for_class tablet-11-screenshots)" ] && echo differ || echo same)"
check "phones are the default class" "$IOS_PHONE" "$(device_for_class phone-screenshots)"
check "an unknown class falls back to the phone" "$IOS_PHONE" "$(device_for_class whatever)"

# ── both classes, every run ───────────────────────────────────────────────────
# App Store Connect REQUIRES an iPad set while TARGETED_DEVICE_FAMILY includes
# iPad. Shooting only phones is what left three stale, empty iPad sets on the
# live listings, so the iPad is not opt-in.
check "the two required classes are the default" "phone-screenshots tablet-screenshots" "$SHOT_CLASSES"
check "the phone goes first" "phone-screenshots" "$(set -- $SHOT_CLASSES; echo "$1")"
# Both store-mandatory sizes must be in the default, or a submission blocks on a
# set nobody shot. Matched as whole words — "tablet-screenshots" is a substring of
# "tablet-11-screenshots", so a substring test would pass on the wrong one.
_shot_classes_list() { printf '%s\n' $SHOT_CLASSES; }
check "the required 6.9 phone is in the default" "1" \
  "$(_shot_classes_list | grep -cx 'phone-screenshots')"
check "the required 13-inch iPad is in the default" "1" \
  "$(_shot_classes_list | grep -cx 'tablet-screenshots')"
# Apple derives the 11" from the 13", so it is not worth a third of every run.
# Opt-in, but it must still WORK when asked for — hence the device mapping above.
check "the optional 11-inch iPad stays out of the default" "0" \
  "$(_shot_classes_list | grep -cx 'tablet-11-screenshots')"
check "asking for the 11-inch still works" "tablet-11-screenshots" \
  "$(SHOT_CLASSES=tablet-11-screenshots bash -c 'source "$1" >/dev/null 2>&1; echo "$SHOT_CLASSES"' _ "$HERE/store-screenshots.sh")"

# ── narrowing to one device ───────────────────────────────────────────────────
# Named by device, not by class: at the prompt you know which screen you want to
# reshoot, not which gradle-play-publisher directory it lands in.
check "--iphone narrows to the phone"   "phone-screenshots"     "$(class_for_flag --iphone)"
check "--ipad-13 narrows to the 13-inch" "tablet-screenshots"    "$(class_for_flag --ipad-13)"
check "--ipad-11 narrows to the 11-inch" "tablet-11-screenshots" "$(class_for_flag --ipad-11)"
# Empty is how the dispatch tells a device flag from a command — a command that
# mapped to a class would be swallowed as a flag and never run.
check "a command is not a device flag" "" "$(class_for_flag --all-top)"
check "a locale is not a device flag"  "" "$(class_for_flag pl-PL)"
check "no argument is not a device flag" "" "$(class_for_flag "")"
# Every class needs a flag. One reachable only by spelling out SHOT_CLASSES is
# the state the iPad was in before it went stale on all three listings.
for _c in phone-screenshots tablet-screenshots tablet-11-screenshots; do
  _found=""
  for _f in --iphone --ipad-13 --ipad-11; do
    [ "$(class_for_flag "$_f")" = "$_c" ] && _found="$_f"
  done
  check "$_c is reachable by a flag" "1" "$([ -n "$_found" ] && echo 1 || echo 0)"
done
# Back-compat: the documented SHOT_CLASS=tablet-screenshots invocation must still
# mean ONE pass, not "both, starting with the tablet".
check "an explicit SHOT_CLASS still pins a single pass" "tablet-screenshots" \
  "$(SHOT_CLASS=tablet-screenshots bash -c 'source "$1" >/dev/null 2>&1; echo "$SHOT_CLASSES"' _ "$HERE/store-screenshots.sh")"
check "SHOT_CLASSES overrides both" "phone-screenshots" \
  "$(SHOT_CLASSES=phone-screenshots bash -c 'source "$1" >/dev/null 2>&1; echo "$SHOT_CLASSES"' _ "$HERE/store-screenshots.sh")"

# for_each_class runs the WHOLE command once per class — a full ordinary pass
# each time — with SHOT_CLASS pointing at the one being shot, which is what makes
# candidates_dir land in the right place without any other function knowing.
_saved_class="$SHOT_CLASS"; _passes=""
_record_pass() { _passes="$_passes $SHOT_CLASS($(basename "$(dirname "$(candidates_dir pl-PL)")")):$*"; }
for_each_class _record_pass topcities 2 >/dev/null
check "each class gets its own pass, in order, with its own dir" \
  "phone-screenshots(phone-screenshots):topcities 2 tablet-screenshots(tablet-screenshots):topcities 2" \
  "${_passes# }"
# The opt-in class is not special-cased anywhere — asked for, it is just another
# pass with its own dir.
SHOT_CLASSES="tablet-11-screenshots"; _passes=""
for_each_class _record_pass solo >/dev/null
check "the opt-in class is an ordinary pass" "tablet-11-screenshots(tablet-11-screenshots):solo" "${_passes# }"
SHOT_CLASSES="phone-screenshots tablet-screenshots"; SHOT_CLASS="$_saved_class"

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

# ── the UI language is pinned at launch ───────────────────────────────────────
# The app forces the selected country's language and iOS fixes the bundle's
# localization at process start, so a country arriving with the deep link lands
# too late for `String(localized:)`. These arguments go in UserDefaults' ARGUMENT
# domain, which outranks anything persisted, so the process boots in the right
# language — without them shot 001 of a fresh install came out with Polish pills
# over a German listing.
check "German launches are pinned to German" \
  "-selectedCountryCode de -AppleLanguages (de) -AppleLocale de" \
  "$(language_args de | tr '\n' ' ' | sed 's/ $//')"
check "British launches are pinned to English, not en-GB" \
  "-selectedCountryCode uk -AppleLanguages (en) -AppleLocale en" \
  "$(language_args uk | tr '\n' ' ' | sed 's/ $//')"
check "Polish launches are pinned too, never left to the default" \
  "-selectedCountryCode pl -AppleLanguages (pl) -AppleLocale pl" \
  "$(language_args pl | tr '\n' ' ' | sed 's/ $//')"
# The UI language is the BUNDLE's (en), not the store locale's (en-GB) — passing
# the store locale would leave the bundle on its fallback.
check "the language is the bundle's, not the store locale's" "en" "$(country_language uk)"
check "an unknown country pins nothing rather than guessing" "" "$(language_args fr)"

# ── a stale build is never reused ─────────────────────────────────────────────
# The early return in build_app exists so a multi-city run builds once. It must
# NOT let a derived-data dir outlive the sources: one built before the iOS UI was
# translated kept shooting Polish screens for German cities for hours.
_stale_root="$(mktemp -d)"; mkdir -p "$_stale_root/ios/Kinowo" "$_stale_root/app"
_saved_root="$REPO_ROOT"; _saved_bin="$APP_BINARY"
REPO_ROOT="$_stale_root"; APP_BINARY="$_stale_root/app/Kinowo"
check "a missing app is stale" "0" "$( app_is_stale; echo $? )"
touch "$APP_BINARY"; chmod +x "$APP_BINARY"
touch -t 202601010000 "$_stale_root/ios/Kinowo/Old.swift"
check "an app newer than every source is reused" "1" "$( app_is_stale; echo $? )"
touch "$_stale_root/ios/Kinowo/Edited.swift"          # now newer than the app
check "a source edited since the build makes it stale" "0" "$( app_is_stale; echo $? )"
# The driver writes screenshots under ios/store DURING a run; counting those
# would rebuild after every city.
touch "$APP_BINARY"; mkdir -p "$_stale_root/ios/store/listings"
touch "$_stale_root/ios/store/listings/001.png"
check "screenshots the run just wrote don't count" "1" "$( app_is_stale; echo $? )"
# Build outputs are outputs, not sources.
mkdir -p "$_stale_root/ios/build"; touch "$_stale_root/ios/build/artifact.o"
check "build outputs don't count either" "1" "$( app_is_stale; echo $? )"
check "BUILD=1 still forces a rebuild" "0" "$( BUILD=1 app_is_stale; echo $? )"
REPO_ROOT="$_saved_root"; APP_BINARY="$_saved_bin"; rm -rf "$_stale_root"

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
check "usage documents SHOT_CLASSES" "1" "$(usage | grep -q 'SHOT_CLASSES' && echo 1 || echo 0)"
check "usage documents every device flag" "3" \
  "$(usage | grep -oE '\-\-(iphone|ipad-13|ipad-11)' | sort -u | wc -l | tr -d ' ')"
# The flag has to be consumed BEFORE the command case, or --iphone --all-top
# falls through to the positional branch and is read as a locale.
check "the dispatch narrows before it dispatches" "1" \
  "$(grep -c 'class_for_flag "\${1:-}"' "$HERE/store-screenshots.sh")"
check "usage says the iPad is shot too" "1" "$(usage | grep -qi 'IPAD' && echo 1 || echo 0)"
# The dispatch wiring itself: every CAPTURE path must go through for_each_class or
# the iPad pass silently stops happening. Dispatch only runs when the script is
# EXECUTED, so it can't be called from here — this reads the source instead, which
# is crude but guards the one line whose loss would be invisible.
check "all three capture paths run per class" "3" \
  "$(grep -c 'for_each_class cmd_' "$HERE/store-screenshots.sh")"
check "--top stays exempt (it only prints)" "1" \
  "$(grep -c 'shift; cmd_top' "$HERE/store-screenshots.sh")"
check "usage reaches the last header line" "1" "$(usage | grep -q 'NO_OPEN' && echo 1 || echo 0)"
check "usage stops at the code" "" "$(usage | grep 'set -euo' || true)"

if [ "$fails" -eq 0 ]; then printf '\033[32m✓\033[0m all passed\n'; else printf '\033[31m✗\033[0m %s failed\n' "$fails"; fi
exit $((fails > 0))
