#!/usr/bin/env bash
#
# Generate App Store screenshots for the Kinowo / Showtimes iOS app. Just run it —
# it builds once, boots the simulator, drives the four store screens per city and
# writes them out per locale.
#
#   ios/scripts/store-screenshots.sh en-GB "Birmingham"      # → candidates dir
#   ios/scripts/store-screenshots.sh pl-PL "Poznan" /tmp/x   # → a scratch dir
#   ios/scripts/store-screenshots.sh --top uk 10             # biggest cities by film count
#   ios/scripts/store-screenshots.sh --top uk 5 11           # ranks 11-15 instead of 1-5
#   ios/scripts/store-screenshots.sh --all-top 2             # top 2 cities of EVERY country
#   ios/scripts/store-screenshots.sh --all-top 2 4           # 2 cities from rank 4, every country
#   ios/scripts/store-screenshots.sh --country-top uk 10     # top 10 cities of ONE country
#   ios/scripts/store-screenshots.sh --country-top de 5 6    # de ranks 6-10 only
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
# Shots land in a candidates/ scratchpad INSIDE the published dir
# (ios/store/listings/<locale>/graphics/phone-screenshots/candidates/, gitignored)
# and every run APPENDS, zero-padded to three digits (city 1 → 001-004.png, city 2
# → 005-008.png, …). Pick the keepers and move them up one level into
# phone-screenshots/ — that is what goes to App Store Connect. The layout mirrors
# the Android side exactly, so one promote-by-hand habit works for both stores.
#
# Screens are reached by DEEP LINK (`kinowo://<slug>`, `…/film?title=…`), never by
# tapping coordinates: the same script then works on a 4" phone and a 13" iPad
# with no per-device tap map. That also makes SPLIT cities (London) a non-issue —
# rather than dismissing the area sheet the way the Android driver must, the
# launch pre-seeds `areaPickerSeenCities` so it never opens.
#
# One build covers all three locales: the city's country drives both the backend
# and the UI language, so pl-PL shoots Polish, en-GB English, de-DE German.
#
# Env: IOS_PHONE / IOS_TABLET pick the devices · SHOT_CLASS=tablet-screenshots to
# shoot the iPad instead · SETTLE=<s> per-screen wait (posters come off the
# network) · BUILD=1 force a rebuild · NO_OPEN=1 skip the Preview.
#
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
LISTINGS="$REPO_ROOT/ios/store/listings"
# Same directory vocabulary as Android (gradle-play-publisher's ImageType.dirName)
# so both stores are laid out identically: phone-screenshots / tablet-screenshots.
SHOT_CLASS="${SHOT_CLASS:-phone-screenshots}"
# Countries, ranking, split-city lookup, candidates/ numbering and the --top table
# are shared with android/scripts/store-screenshots.sh.
# shellcheck source=../../scripts/store-screenshots-common.sh
source "$REPO_ROOT/scripts/store-screenshots-common.sh"

BUNDLE="dev.kinowo.Kinowo"
SETTLE="${SETTLE:-6}"
DERIVED="${TMPDIR:-/tmp}/kinowo-shots-dd"
APP="$DERIVED/Build/Products/Debug-iphonesimulator/Kinowo.app"
NOISE="$(mktemp)"                       # xcodebuild / simctl chatter lands here
BOOTED_DEVICES=""                       # udids THIS run booted; shut down on exit
MAIN_SHELL=1                            # cleared in worker subshells

# Apple only requires the largest phone and the largest tablet; every smaller size
# is derived from those, so one device per class is the whole matrix.
IOS_PHONE="${IOS_PHONE:-iPhone 17 Pro Max}"
IOS_TABLET="${IOS_TABLET:-iPad Pro 13-inch (M5)}"
device_for_class() { case "$1" in tablet-screenshots) echo "$IOS_TABLET";; *) echo "$IOS_PHONE";; esac; }

cleanup() { [ -n "${MAIN_SHELL:-}" ] && shutdown_devices; rm -f "$NOISE"; }
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

# Debug, not Release: the KINOWO_UITEST_DEEPLINK hook that drives these screens is
# `#if DEBUG`. Same SwiftUI, same pixels — only the hooks differ.
build_app() {
  [ -d "$APP" ] && [ -z "${BUILD:-}" ] && return 0
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
shoot() { # $1 udid, $2 slug, $3 deep-link url, $4 output file
  local udid="$1" slug="$2" url="$3" out="$4"
  xcrun simctl terminate "$udid" "$BUNDLE" >>"$NOISE" 2>&1 || true
  SIMCTL_CHILD_KINOWO_UITEST_DEEPLINK="$url" \
    xcrun simctl launch "$udid" "$BUNDLE" \
      -areaPickerSeenCities "(\"$slug\")" >>"$NOISE" 2>&1 || return 1
  naps "$SETTLE"
  xcrun simctl io "$udid" screenshot "$out" >>"$NOISE" 2>&1 || return 1
  [ -s "$out" ] || return 1
}

# The four deep links one capture walks, in screen order: listing, listing sorted
# by rating, a film detail, and another day. The detail's film is whatever the live
# listing puts first — the same film the user sees on top, so it is never a stale
# hardcoded title. With no film available the listing repeats rather than leaving a
# gap in the numbering.
capture_urls() { # $1 slug, $2 url-encoded film title (may be empty)
  local base="kinowo://$1"
  printf '%s\n' "$base" "$base?sort=rating"
  if [ -n "$2" ]; then printf '%s\n' "$base/film?title=$2"; else printf '%s\n' "$base"; fi
  printf '%s\n' "$base?date=tomorrow"
}

first_film() { # $1 country, $2 slug — url-encoded title of the first listed film
  curl -fsS --max-time 20 "$(country_base "$1")/$2/api/repertoire" 2>/dev/null \
    | python3 -c 'import json,sys,urllib.parse; print(urllib.parse.quote(json.load(sys.stdin)[0]["title"]))' 2>/dev/null || true
}

# ── one city ──────────────────────────────────────────────────────────────────
cmd_capture() { # $1 locale, $2 city, $3 optional outdir, $4 optional first number
  local locale="$1" term="$2" outdir="${3:-}" first="${4:-}"
  local country; country="$(locale_country "$locale")"
  [ -n "$country" ] || die "unknown locale '$locale' (use en-GB | pl-PL | de-DE)"
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

  step "capturing 4 screens"
    local i=0
    for line in "${shots[@]}"; do
      shoot "$udid" "$slug" "${urls[$i]}" "$line" ||
        die "$term: screen $((i + 1)) never rendered — the simulator may be starved."
      i=$((i + 1))
    done
  done_
  xcrun simctl terminate "$udid" "$BUNDLE" >>"$NOISE" 2>&1 || true
  ok "$(printf 'wrote %s/{%03d..%03d}.png' "$dest" "$first" "$((first + 3))")"

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
preview_all() { # $1 baselines captured before the run
  local country locale dest first last i f; local all=()
  for country in $COUNTRIES; do
    locale="$(country_locale "$country")"; dest="$(candidates_dir "$locale")"
    first="$(baseline_for "$1" "$locale")"
    last=$(( $(next_shot_number "$dest") - 1 ))
    for ((i = first; i <= last; i++)); do
      f="$(printf '%s/%03d.png' "$dest" "$i")"; [ -f "$f" ] && all+=("$f")
    done
  done
  [ ${#all[@]} -gt 0 ] && command -v open >/dev/null 2>&1 && open -a Preview "${all[@]}" >>"$NOISE" 2>&1 || true
}

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

  say "$n cities from rank $off × $ncountries countries on $(device_for_class "$SHOT_CLASS")"
  build_app
  run_worker 0 1 "$n" "$off"             # one worker: a device type boots only once

  if [ -n "$want_preview" ]; then preview_all "$before"; fi
}

# --all-top scoped to ONE country, by narrowing the list the loop already walks.
cmd_country_top() { # $1 country, $2 N, $3 optional start rank
  local country="${1:-}"
  [ -n "$country" ] && [ -n "$(country_locale "$country")" ] ||
    die "--country-top wants a country: --country-top uk 10 (or --country-top uk 10 4)"
  COUNTRIES="$country"
  cmd_all_top "${2:-2}" "${3:-1}"
}

usage() { usage_of "${BASH_SOURCE[0]}"; }

# Dispatch only when executed — store-screenshots-test.sh sources this file to
# exercise the pure helpers, and must not trigger a capture by doing so.
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  case "${1:-}" in
    --top)         shift; cmd_top "$@";;
    --all-top)     shift; cmd_all_top "$@";;
    --country-top) shift; cmd_country_top "$@";;
    -h|--help|"")  usage;;
    *)             cmd_capture "$@";;
  esac
fi
