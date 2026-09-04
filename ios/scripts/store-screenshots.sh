#!/usr/bin/env bash
#
# Generate App Store screenshots for the Kinowo / Showtimes iOS app. Just run it —
# it builds once, boots the simulators, drives the five store screens per city and
# writes them out per locale, for the PHONE and the IPAD.
#
#   ios/scripts/store-screenshots.sh en-GB "Birmingham"      # → candidates dir
#   ios/scripts/store-screenshots.sh pl-PL "Poznan" /tmp/x   # → a scratch dir (both passes append)
#   ios/scripts/store-screenshots.sh --top uk 10             # biggest cities by film count
#   ios/scripts/store-screenshots.sh --top uk 5 11           # ranks 11-15 instead of 1-5
#   ios/scripts/store-screenshots.sh --all-top 2             # top 2 cities of EVERY country
#   ios/scripts/store-screenshots.sh --all-top 2 4           # 2 cities from rank 4, every country
#   ios/scripts/store-screenshots.sh --country-top uk 10     # top 10 cities of ONE country
#   ios/scripts/store-screenshots.sh --country-top de 5 6    # de ranks 6-10 only
#
# A leading DEVICE FLAG narrows the run to one class; it goes in front of any of
# the commands above, and on its own means "just this device":
#
#   ios/scripts/store-screenshots.sh --iphone --all-top 2          # 6.9" phone only
#   ios/scripts/store-screenshots.sh --ipad-13 --all-top 2         # 13" iPad only
#   ios/scripts/store-screenshots.sh --ipad-11 --country-top uk 5  # 11" iPad only
#   ios/scripts/store-screenshots.sh --ipad-13 pl-PL "Poznan"      # one city, one device
#
# --top only PRINTS the ranking; --all-top and --country-top capture. Use
# --country-top to top up a single locale without re-shooting the other two.
#
# All three take an optional 1-based START RANK after the count, so
# "--all-top 2 4" means "two cities beginning at the 4th best" — that is how you
# shoot the runners-up without re-shooting the cities you already have.
#
# A city that fails is SKIPPED, not fatal: the rest of the country still gets shot
# and the summary says which ones were lost.
#
# Every run shoots the two classes the store REQUIRES, a full pass each: the
# 6.9" phone and the 13" iPad. App Store Connect requires an iPad set from any
# app whose TARGETED_DEVICE_FAMILY includes iPad, and ours does; shooting phones
# alone is how the first submission ended up with three stale, empty iPad sets
# nobody noticed.
#
# The 11" iPad is available but OPT-IN, since Apple derives it from the 13":
# ask for it with --ipad-11. Worth doing when the derived image would mislead —
# the listing lays out four columns at 11" and five at 13", so a squeezed 13"
# shows the wrong grid.
#
# Shots land in a candidates/ scratchpad INSIDE the published dir, one per class
# (ios/store/listings/<locale>/graphics/<class>/candidates/, gitignored) and every
# run APPENDS, zero-padded to three digits (city 1 → 001-005.png, city 2 →
# 006-010.png, …). Pick the keepers and move them up one level into
# phone-screenshots/, tablet-screenshots/ or tablet-11-screenshots/ — that is what
# goes to App Store Connect. The layout mirrors the Android side exactly, so one
# promote-by-hand habit works for both stores.
#
# Screens are reached by DEEP LINK (`kinowo://<slug>`, `…/film?title=…`), never by
# tapping coordinates: the same script then works on a 4" phone and a 13" iPad
# with no per-device tap map. The Filtry sheet is the one exception — it is app
# state rather than a URL, so it rides a launch hook instead, still no taps.
# That also makes SPLIT cities (London) a non-issue —
# rather than dismissing the area sheet the way the Android driver must, the
# launch pre-seeds `areaPickerSeenCities` so it never opens.
#
# One build covers all three locales: the city's country drives both the backend
# and the UI language, so pl-PL shoots Polish, en-GB English, de-DE German.
#
# Only ONE capture run at a time: a second is refused with the holder's pid rather
# than letting two runs fight over the same simulators. --top is exempt.
#
# Env: IOS_PHONE / IOS_TABLET / IOS_TABLET_11 pick the devices · SHOT_CLASSES
# names the passes ("phone-screenshots tablet-screenshots" by default; add
# tablet-11-screenshots for the 11" iPad; SHOT_CLASS=<one> still pins a single
# pass) · SETTLE=<s> per-screen wait (posters come off the network) · BUILD=1
# force a rebuild · NO_OPEN=1 skip the Preview.
#
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
LISTINGS="$REPO_ROOT/ios/store/listings"
# Same directory vocabulary as Android (gradle-play-publisher's ImageType.dirName)
# so both stores are laid out identically: phone-screenshots / tablet-screenshots.
#
# The two REQUIRED classes by default — 6.9" phone and 13" iPad. App Store
# Connect requires an iPad set from any app whose TARGETED_DEVICE_FAMILY includes
# iPad — ours does — so shooting only phones leaves a mandatory set to go stale
# silently, which is exactly what happened to the first submission's listings.
#
# `tablet-11-screenshots` is deliberately NOT in the default: Apple derives the
# 11" iPad from the 13", so it is optional, and shooting it costs a third of the
# run for a set the store will fill in anyway. Ask for it when you want the real
# thing rather than a squeezed 13" (the listing lays out four columns at 11",
# five at 13"):
#
#   SHOT_CLASSES=tablet-11-screenshots ios/scripts/store-screenshots.sh --all-top 2
#
# `SHOT_CLASSES` names the passes; `SHOT_CLASS` is whichever one is being shot
# right now, and is what `candidates_dir` reads. Setting SHOT_CLASS alone still
# pins a single-class run, so `SHOT_CLASS=tablet-screenshots …` keeps working.
SHOT_CLASSES="${SHOT_CLASSES:-${SHOT_CLASS:-phone-screenshots tablet-screenshots}}"
# Five screens a city, one more than Android: the same four plus the Filtry
# sheet. Set before sourcing so the shared numbering hands out blocks of five.
SHOTS_PER_CITY=5
# Countries, ranking, split-city lookup, candidates/ numbering and the --top table
# are shared with android/scripts/store-screenshots.sh.
# shellcheck source=../../scripts/store-screenshots-common.sh
source "$REPO_ROOT/scripts/store-screenshots-common.sh"

BUNDLE="dev.kinowo.Kinowo"
SETTLE="${SETTLE:-6}"
DERIVED="${TMPDIR:-/tmp}/kinowo-shots-dd"
APP="$DERIVED/Build/Products/Debug-iphonesimulator/Kinowo.app"
# The built executable, not the bundle dir: it is rewritten by every real build,
# so it is the honest "when was this app made" stamp for the staleness check.
APP_BINARY="$APP/Kinowo"
NOISE="$(mktemp)"                       # xcodebuild / simctl chatter lands here
BOOTED_DEVICES=""                       # udids THIS run booted; shut down on exit
MAIN_SHELL=1                            # cleared in worker subshells
CAPTURE_COUNTRY=""                      # the country the current city belongs to

# App Store Connect REQUIRES the largest phone (6.9") and, for an iPad-enabled
# app, the 13" iPad; it derives the smaller sizes from those. The 11" iPad is
# therefore optional — but a derived shot is the 13" layout squeezed, and the
# listing lays out a different number of columns at that width, so we shoot it
# for real rather than let Apple guess.
IOS_PHONE="${IOS_PHONE:-iPhone 17 Pro Max}"
IOS_TABLET="${IOS_TABLET:-iPad Pro 13-inch (M5)}"
IOS_TABLET_11="${IOS_TABLET_11:-iPad Pro 11-inch (M5)}"
device_for_class() {
  case "$1" in
    tablet-screenshots)    echo "$IOS_TABLET";;
    tablet-11-screenshots) echo "$IOS_TABLET_11";;
    *)                     echo "$IOS_PHONE";;
  esac
}

# The class a leading --iphone / --ipad-13 / --ipad-11 narrows the run to, or
# empty for anything else — which is how the dispatch tells a device flag from a
# command. Named by DEVICE rather than by class, because at the prompt you know
# which screen you are trying to reshoot, not which gradle-play-publisher
# directory it lands in. Every class has a flag: a class reachable only by
# spelling out SHOT_CLASSES is one that stops getting shot.
class_for_flag() { # $1 argument → class, or empty when it isn't a device flag
  case "$1" in
    --iphone)  echo phone-screenshots;;
    --ipad-13) echo tablet-screenshots;;
    --ipad-11) echo tablet-11-screenshots;;
  esac
}

# Run a capture command once per class — a full, ordinary single-class pass each
# time, rather than threading a second class through the city loop. Each pass is
# therefore exactly the run that has always worked: its own devices, its own
# candidates dir, its own numbering and Preview. The build is shared (build_app
# returns early once the app exists) and ensure_device reuses whatever is already
# booted, so the second pass costs one simulator boot, not a rebuild.
for_each_class() { # $1.. command + args to run per class
  local class
  for class in $SHOT_CLASSES; do
    SHOT_CLASS="$class"
    say "▪ $SHOT_CLASS on $(device_for_class "$SHOT_CLASS")"
    "$@"
  done
}

cleanup() { [ -n "${MAIN_SHELL:-}" ] && { shutdown_devices; release_lock; }; rm -f "$NOISE"; }
trap cleanup EXIT

# Shut down only what this run booted — a simulator the developer already had open
# is never in the list, so it survives.
shutdown_devices() {
  local udid
  for udid in ${BOOTED_DEVICES:-}; do
    xcrun simctl shutdown "$udid" >>"$NOISE" 2>&1 || true
  done
}

# ── the simulator ─────────────────────────────────────────────────────────────
# The udid of an available simulator NAMED $1, preferring one already booted so a
# developer's open simulator is reused instead of a second copy being started.
device_udid() { # $1 device name
  xcrun simctl list devices available -j 2>/dev/null | NAME="$1" python3 -c '
import json, os, sys
name = os.environ["NAME"]
hits = [d for r in json.load(sys.stdin)["devices"].values() for d in r if d.get("name") == name]
booted = [x for x in hits if x.get("state") == "Booted"]
print((booted or hits)[0]["udid"] if hits else "")
'
}

device_state() { # $1 udid
  xcrun simctl list devices -j 2>/dev/null | UDID="$1" python3 -c '
import json, os, sys
u = os.environ["UDID"]
print(next((x["state"] for r in json.load(sys.stdin)["devices"].values() for x in r if x["udid"] == u), ""))
'
}

ensure_device() { # $1 device name → echoes the udid
  local name="$1" udid
  udid="$(device_udid "$name")"
  [ -n "$udid" ] || die "no simulator named '$name' — install it in Xcode, or set IOS_PHONE / IOS_TABLET."
  if [ "$(device_state "$udid")" != "Booted" ]; then
    xcrun simctl boot "$udid" >>"$NOISE" 2>&1 || true
    BOOTED_DEVICES="$BOOTED_DEVICES $udid"
    xcrun simctl bootstatus "$udid" -b >>"$NOISE" 2>&1 || true
  fi
  echo "$udid"
}

# Whether the cached app has to be rebuilt: it's missing, BUILD=1 forces it, or
# something under ios/ has changed since it was built.
#
# The mtime check is the whole point. The early return exists so a multi-class,
# multi-city run builds ONCE — not so a derived-data dir from a previous day can
# keep shipping a binary that predates the change under test. That is not
# hypothetical: a cache built before the iOS UI was translated went on shooting
# POLISH screens for German and British cities for hours after the translations
# landed, and the screenshots looked plausible enough to be committed.
#
# `store` is pruned because the driver writes its own screenshots there mid-run,
# which would otherwise mark the app stale after the very first city; the build
# dirs are pruned because they're outputs, not sources.
app_is_stale() {
  [ -n "${BUILD:-}" ] && return 0
  [ -x "$APP_BINARY" ] || return 0
  find "$REPO_ROOT/ios" \
    \( -name build -o -name .build -o -name target -o -name store -o -name DerivedData \) -prune -o \
    -type f -newer "$APP_BINARY" -print -quit 2>/dev/null | grep -q .
}

# Debug, not Release: the KINOWO_UITEST_DEEPLINK hook that drives these screens is
# `#if DEBUG`. Same SwiftUI, same pixels — only the hooks differ.
build_app() {
  app_is_stale || return 0
  step "building Kinowo (Debug, simulator)"
    xcodebuild -project "$REPO_ROOT/ios/Kinowo.xcodeproj" -scheme Kinowo \
      -configuration Debug -sdk iphonesimulator -derivedDataPath "$DERIVED" \
      CODE_SIGNING_ALLOWED=NO build >>"$NOISE" 2>&1 || die "xcodebuild failed"
  done_
  [ -d "$APP" ] || die "no app at $APP after a successful build"
}

install_app() { # $1 udid
  xcrun simctl install "$1" "$APP" >>"$NOISE" 2>&1 || die "install failed on $1"
  # Pre-grant location so the permission alert never lands mid-screenshot, and pin
  # the status bar to Apple's canonical 9:41 / full-signal look.
  xcrun simctl privacy "$1" grant location "$BUNDLE" >>"$NOISE" 2>&1 || true
  xcrun simctl status_bar "$1" override --time "9:41" \
    --batteryState charged --batteryLevel 100 --cellularBars 4 --wifiBars 3 >>"$NOISE" 2>&1 || true
}

# ── one screen ────────────────────────────────────────────────────────────────
# NOT `simctl openurl`: a custom-scheme URL from outside the app makes SpringBoard
# put an "Open in Showtimes?" confirmation over everything and the screenshot
# catches that instead of the app. The launch-env hook feeds the same
# handleDeepLink path the UI tests use, with no dialog.
#
# `-areaPickerSeenCities` rides along as a launch ARGUMENT, which UserDefaults
# reads through NSArgumentDomain: a SPLIT city (London, 5 areas) would otherwise
# open its first-visit area sheet over the listing and we would capture the sheet.
# Suppressing it is the iOS counterpart of the Android driver tapping the picker's
# confirm button — and since every area is pre-selected either way, both keep the
# flat default rather than filtering cinemas out of the shot.
#
# The Filtry sheet is the one screen with no link route — it's app state, not a
# web URL — so a non-empty $5 asks the app to open it once the listing has
# loaded. Passed via `env` rather than an inline assignment because the app tests
# for the KEY's presence: an inline `VAR=` would set it to empty on every OTHER
# screen, which reads as present and would open the sheet over all five.
# The launch arguments that pin the UI language to $1's, or nothing for a country
# we don't know.
#
# The app forces the SELECTED COUNTRY's language rather than following the
# device, and iOS fixes the bundle's localization at PROCESS START. A country
# that arrives with the deep link therefore lands after the first frame: the
# root's `.environment(\.locale)` re-localizes SwiftUI `Text`, but anything
# resolved through `String(localized:)` — the date pills, the detail screen's
# meta captions — stays in the language the process booted in. Shot 001 of a
# fresh install came out with Polish pills over a German listing that way.
#
# These land in UserDefaults' ARGUMENT domain, which outranks anything persisted
# on the simulator, so the process boots in the right language and every screen
# is shot in one. Same three arguments KinowoUITests/LocalizationUITests sets,
# for exactly the same reason — see its `FixtureLaunch` note.
language_args() { # $1 country
  local language; language="$(country_language "$1")"
  [ -n "$language" ] || return 0
  printf '%s\n' -selectedCountryCode "$1" -AppleLanguages "($language)" -AppleLocale "$language"
}

shoot() { # $1 udid, $2 slug, $3 deep-link url, $4 output file, $5 non-empty → Filtry sheet
  local udid="$1" slug="$2" url="$3" out="$4" filters="${5:-}" line
  xcrun simctl terminate "$udid" "$BUNDLE" >>"$NOISE" 2>&1 || true
  local -a hooks=("SIMCTL_CHILD_KINOWO_UITEST_DEEPLINK=$url")
  [ -n "$filters" ] && hooks+=("SIMCTL_CHILD_KINOWO_UITEST_OPEN_FILTERS=1")
  local -a language=()
  while IFS= read -r line; do language+=("$line"); done < <(language_args "$CAPTURE_COUNTRY")
  env "${hooks[@]}" \
    xcrun simctl launch "$udid" "$BUNDLE" \
      -areaPickerSeenCities "(\"$slug\")" \
      ${language[@]+"${language[@]}"} >>"$NOISE" 2>&1 || return 1
  naps "$SETTLE"
  xcrun simctl io "$udid" screenshot "$out" >>"$NOISE" 2>&1 || return 1
  [ -s "$out" ] || return 1
}

# The five links one capture walks, in screen order: listing, listing sorted by
# rating, a film detail, another day, and the plain listing once more — that last
# one is the Filtry screen, which `shoot` opens the sheet over (see above), so it
# needs the listing behind it and no filters of its own. The detail's film is
# whatever the live listing puts first — the same film the user sees on top, so it
# is never a stale hardcoded title. With no film available the listing repeats
# rather than leaving a gap in the numbering.
capture_urls() { # $1 slug, $2 url-encoded film title (may be empty)
  local base="kinowo://$1"
  printf '%s\n' "$base" "$base?sort=rating"
  if [ -n "$2" ]; then printf '%s\n' "$base/film?title=$2"; else printf '%s\n' "$base"; fi
  printf '%s\n' "$base?date=tomorrow" "$base"
}

# Which screen (0-based) the Filtry sheet rides on: the last one capture_urls
# emits. Named rather than inlined so the loop and its tests agree on one answer.
filters_screen_index() { echo "$((SHOTS_PER_CITY - 1))"; }

first_film() { # $1 country, $2 slug — url-encoded title of the first listed film
  curl -fsS --max-time 20 "$(country_base "$1")/$2/api/repertoire" 2>/dev/null \
    | python3 -c 'import json,sys,urllib.parse; print(urllib.parse.quote(json.load(sys.stdin)[0]["title"]))' 2>/dev/null || true
}

# ── one city ──────────────────────────────────────────────────────────────────
cmd_capture() { # $1 locale, $2 city, $3 optional outdir, $4 optional first number
  local locale="$1" term="$2" outdir="${3:-}" first="${4:-}"
  local country; country="$(locale_country "$locale")"
  [ -n "$country" ] || die "unknown locale '$locale' (use en-GB | pl-PL | de-DE)"
  # Every shot of this city is launched in this country's language — a capture is
  # single-country by construction, so `shoot` reads it here rather than carrying
  # a seventh positional argument through the loop.
  CAPTURE_COUNTRY="$country"
  local dest="${outdir:-$(candidates_dir "$locale")}"
  mkdir -p "$dest"
  # Append: unless a caller pinned the block, start one past whatever is there.
  [ -n "$first" ] || first="$(next_shot_number "$dest")"

  local slug; slug="$(city_slug "$country" "$term")"
  [ -n "$slug" ] || die "'$term' isn't in $country's catalog — check the spelling."

  say "$locale · $term ($slug)"
  local udid; udid="$(ensure_device "$(device_for_class "$SHOT_CLASS")")"
  build_app
  install_app "$udid"

  local film; film="$(first_film "$country" "$slug")"
  [ -n "$film" ] || warn "$term: no film from the API — repeating the listing for shot 3"

  local -a urls=() shots=(); local line
  while IFS= read -r line; do urls+=("$line"); done < <(capture_urls "$slug" "$film")
  while IFS= read -r line; do shots+=("$line"); done < <(shot_paths "$dest" "$first")

  step "capturing $SHOTS_PER_CITY screens"
    local i=0 filters; local last; last="$(filters_screen_index)"
    for line in "${shots[@]}"; do
      filters=""; [ "$i" -eq "$last" ] && filters=1
      shoot "$udid" "$slug" "${urls[$i]}" "$line" "$filters" ||
        die "$term: screen $((i + 1)) never rendered — the simulator may be starved."
      i=$((i + 1))
    done
  done_
  xcrun simctl terminate "$udid" "$BUNDLE" >>"$NOISE" 2>&1 || true
  ok "$(printf 'wrote %s/{%03d..%03d}.png' "$dest" "$first" "$((first + SHOTS_PER_CITY - 1))")"

  if [ -z "${NO_OPEN:-}" ] && command -v open >/dev/null 2>&1; then
    open -a Preview "${shots[@]}" >>"$NOISE" 2>&1 || true
  fi
}

# ── many cities ───────────────────────────────────────────────────────────────
# One worker per country. Unlike Android's emulator pool these share a simulator —
# a device type can only be booted once — so countries run sequentially; the win
# here is the shared build and install, not parallelism.
run_worker() { # $1 W, $2 K, $3 N, $4 OFFSET
  local w="$1" k="$2" n="$3" off="${4:-1}" country locale dest name shot failed nfail
  for country in $(worker_slice "$COUNTRIES" "$k" "$w"); do
    locale="$(country_locale "$country")"
    dest="$(candidates_dir "$locale")"
    local names=()
    while IFS=$'\t' read -r _ _ name; do [ -n "$name" ] && names+=("$name"); done \
      < <(rank_cities "$country" "$n" "$off" | tail -n +2)
    mkdir -p "$dest"
    # Capture city by city, each landing the moment it works, and a failure costing
    # only that city. cmd_capture ends in die() (an exit) for ordinary reasons — a
    # screen that never rendered, a city missing from the catalog — so it runs in a
    # SUBSHELL: the exit stops the city, not the whole country.
    failed=""; nfail=0
    for name in "${names[@]}"; do
      if ( cmd_capture "$locale" "$name" ) </dev/null; then :
      else
        nfail=$((nfail + 1)); failed="$failed, $name"
        warn "$locale: $name failed — skipping it"
      fi
    done
    shot=$(( ${#names[@]} - nfail ))
    [ "$nfail" -eq 0 ] || warn "$locale: failed on${failed#,}"
    if [ "$shot" -eq 0 ]; then warn "$locale: NOTHING captured — all ${#names[@]} cities failed"
    else ok "$locale: $shot/${#names[@]} cities from rank $off → $dest"
    fi
  done
}

# Open THIS RUN's shots in one Preview: everything each locale gained since its
# baseline. Counting the range keeps the order numeric (010 after 009).

cmd_all_top() { # $1 N, $2 OFFSET — capture N cities of EVERY country
  local n="${1:-2}" off="${2:-1}"
  { [ "$n" -ge 1 ]; } 2>/dev/null || die "--all-top wants a city count, e.g. --all-top 2"
  { [ "$off" -ge 1 ]; } 2>/dev/null ||
    die "--all-top's start rank is 1-based, e.g. --all-top 2 4 (2 cities from the 4th)"

  local ncountries; ncountries=$(set -- $COUNTRIES; echo $#)
  # Snapshot where each candidates dir ends BEFORE anything is captured, so the
  # closing Preview shows only what this run added.
  local before; before="$(baselines)"
  local want_preview=1; [ -n "${NO_OPEN:-}" ] && want_preview=
  export NO_OPEN=1                       # suppress the per-city pop-up

  say "$n cities from rank $off × $ncountries countries"   # for_each_class named the device
  build_app
  run_worker 0 1 "$n" "$off"             # one worker: a device type boots only once

  if [ -n "$want_preview" ]; then preview_all "$before"; fi
}

# --all-top scoped to ONE country, by narrowing the list the loop already walks.

usage() { usage_of "${BASH_SOURCE[0]}"; }

# Dispatch only when executed — store-screenshots-test.sh sources this file to
# exercise the pure helpers, and must not trigger a capture by doing so.
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  # A leading device flag narrows the run to ONE class, in front of whatever
  # command follows. SHOT_CLASSES still works and is what a script should set;
  # this is the version you can type without remembering the class names.
  _narrowed="$(class_for_flag "${1:-}")"
  [ -n "$_narrowed" ] && { SHOT_CLASSES="$_narrowed"; shift; }

  case "${1:-}" in
    # Only the capture paths take the lock: --top just prints a ranking and is
    # safe to run beside anything.
    --top)         shift; cmd_top "$@";;
    --all-top)     shift; acquire_lock ios; for_each_class cmd_all_top "$@";;
    --country-top) shift; acquire_lock ios; for_each_class cmd_country_top "$@";;
    -h|--help|"")  usage;;
    *)             acquire_lock ios; for_each_class cmd_capture "$@";;
  esac
fi
