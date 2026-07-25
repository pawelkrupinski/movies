#!/usr/bin/env bash
#
# Generate App Store screenshots for the Kinowo / Showtimes iOS app, one set per
# simulator display class. Just run it — it builds the app once, then boots each
# device, drives the store screens and writes them out grouped by the pixel size
# App Store Connect actually keys its slots on.
#
#   ios/scripts/store-screenshots.sh                  # every installed class, Poznań (pl)
#   ios/scripts/store-screenshots.sh london           # a UK city → English UI
#   ios/scripts/store-screenshots.sh berlin /tmp/shot # a DE city → German UI, scratch dir
#
# One build covers all three locales: the city's country drives both the backend
# and the UI language (`prefs.selectedCountry.languageCode`), so `poznan` shoots
# Polish, `london` English, `berlin` German — same binary, no rebuild.
#
# Screens are reached by DEEP LINK (`kinowo://<city>`, `…/film?title=…`), never
# by tapping coordinates: the same script then works on a 4" phone and a 13"
# iPad without a per-device tap map. The film for the detail shot is whatever
# the live API lists first for that city, so it's never a stale hardcoded title.
#
# Env: DEVICES="a,b" limit the device list · SETTLE=<s> per-screen wait (posters
# come off the network) · BUILD=1 force a rebuild · NO_OPEN=1 skip the Preview.
#
set -euo pipefail

CITY="${1:-poznan}"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
OUT_ROOT="${2:-$REPO_ROOT/ios/store/screenshots}"
BUNDLE="dev.kinowo.Kinowo"
SETTLE="${SETTLE:-6}"
DERIVED="${TMPDIR:-/tmp}/kinowo-shots-dd"

# Every installed iPhone + iPad. Duplicates by pixel size collapse on their own:
# output is keyed by the captured dimensions, so two devices that render the
# same size land in the same directory (last one wins, which is fine — they're
# pixel-identical).
DEFAULT_DEVICES="iPhone 17 Pro Max,iPhone Air,iPhone 17 Pro,iPhone 17,iPhone 17e,\
iPad Pro 13-inch (M5),iPad Air 13-inch (M4),iPad Pro 11-inch (M5),iPad Air 11-inch (M4),\
iPad (A16),iPad mini (A17 Pro)"

say()  { printf '\033[36m▸\033[0m %s\n' "$*"; }
ok()   { printf '\033[32m✓\033[0m %s\n' "$*"; }
warn() { printf '\033[33m!\033[0m %s\n' "$*" >&2; }
die()  { printf '\033[31m✗\033[0m %s\n' "$*" >&2; exit 1; }

# ── the app ───────────────────────────────────────────────────────────────────
APP="$DERIVED/Build/Products/Debug-iphonesimulator/Kinowo.app"
if [ ! -d "$APP" ] || [ -n "${BUILD:-}" ]; then
  # Debug, not Release: the KINOWO_UITEST_DEEPLINK hook that drives these
  # screens is `#if DEBUG`. Same SwiftUI, same pixels — only the hooks differ.
  say "building Kinowo (Debug, simulator)"
  xcodebuild -project "$REPO_ROOT/ios/Kinowo.xcodeproj" -scheme Kinowo \
    -configuration Debug -sdk iphonesimulator -derivedDataPath "$DERIVED" \
    CODE_SIGNING_ALLOWED=NO build >/dev/null 2>&1 || die "build failed"
fi
[ -d "$APP" ] || die "no app at $APP"

# ── the film for the detail shot ──────────────────────────────────────────────
# Whatever the live listing puts first — the same film the user sees on top.
# Each country is its own deployment, so find the host that actually serves this
# city rather than assuming the Polish one (the app does the same via `Country`).
FILM=""
for host in kinowo.fly.dev showtimes-uk.fly.dev showtimes-de.fly.dev; do
  FILM="$(curl -fsS --max-time 20 "https://$host/$CITY/api/repertoire" 2>/dev/null \
          | /usr/bin/python3 -c 'import json,sys; print(json.load(sys.stdin)[0]["title"])' 2>/dev/null || true)"
  [ -n "$FILM" ] && break
done
[ -n "$FILM" ] || warn "no film title from the API — skipping the detail shot"
FILM_ENC="$(/usr/bin/python3 -c 'import sys,urllib.parse; print(urllib.parse.quote(sys.argv[1]))' "$FILM" 2>/dev/null || true)"

# ── per-device capture ────────────────────────────────────────────────────────
shoot() {   # shoot <device> <index> <deeplink>
  local device="$1" index="$2" url="$3" tmp="${TMPDIR:-/tmp}/shot-$$.png"
  # NOT `simctl openurl`: a custom-scheme URL from outside the app makes
  # SpringBoard put an "Open in Showtimes?" confirmation over everything, and
  # the screenshot catches that instead of the app. The launch-env hook feeds
  # the same `handleDeepLink` path the UI tests use, with no dialog.
  xcrun simctl terminate "$device" "$BUNDLE" >/dev/null 2>&1 || true
  SIMCTL_CHILD_KINOWO_UITEST_DEEPLINK="$url" \
    xcrun simctl launch "$device" "$BUNDLE" >/dev/null 2>&1 || return 1
  sleep "$SETTLE"
  xcrun simctl io "$device" screenshot "$tmp" >/dev/null 2>&1 || return 1
  # Key the output on the real pixel size — that's what App Store Connect's
  # slots are defined by, and it makes same-size devices collapse together.
  local size
  size="$(sips -g pixelWidth -g pixelHeight "$tmp" | awk '/pixel/ {printf "%s", $2"x"} END {print ""}' | sed 's/x$//')"
  size="$(sips -g pixelWidth "$tmp" | awk '/pixelWidth/{w=$2} END{printf "%s", w}')x$(sips -g pixelHeight "$tmp" | awk '/pixelHeight/{h=$2} END{printf "%s", h}')"
  local dir="$OUT_ROOT/$CITY/$size"
  mkdir -p "$dir"
  mv "$tmp" "$dir/$index.png"
  echo "$size"
}

IFS=',' read -r -a DEVICES <<< "${DEVICES:-$DEFAULT_DEVICES}"
BOOTED=()
for device in "${DEVICES[@]}"; do
  device="$(echo "$device" | sed 's/^ *//;s/ *$//')"
  xcrun simctl list devices available | grep -qF "$device (" || { warn "no simulator: $device"; continue; }
  say "$device"

  was_booted="$(xcrun simctl list devices | grep -F "$device (" | grep -c Booted || true)"
  [ "$was_booted" = "0" ] && { xcrun simctl boot "$device" >/dev/null 2>&1 || true; BOOTED+=("$device"); }
  xcrun simctl bootstatus "$device" -b >/dev/null 2>&1 || true

  xcrun simctl install "$device" "$APP" >/dev/null 2>&1 || { warn "install failed"; continue; }
  # Pre-grant location so the permission alert never lands mid-screenshot, and
  # pin the status bar to Apple's canonical 9:41 / full-signal look.
  xcrun simctl privacy "$device" grant location "$BUNDLE" >/dev/null 2>&1 || true
  xcrun simctl status_bar "$device" override --time "9:41" \
    --batteryState charged --batteryLevel 100 --cellularBars 4 --wifiBars 3 >/dev/null 2>&1 || true

  size="$(shoot "$device" 1 "kinowo://$CITY" || true)"                      # the listing
  shoot "$device" 2 "kinowo://$CITY?sort=rating" >/dev/null || true          # sorted by rating
  [ -n "$FILM_ENC" ] && { shoot "$device" 3 "kinowo://$CITY/film?title=$FILM_ENC" >/dev/null || true; }
  shoot "$device" 4 "kinowo://$CITY?date=tomorrow" >/dev/null || true        # another day

  xcrun simctl terminate "$device" "$BUNDLE" >/dev/null 2>&1 || true
  ok "${size:-?}"
done

# Leave the machine as we found it — only shut down what this run booted.
for device in "${BOOTED[@]:-}"; do [ -n "$device" ] && xcrun simctl shutdown "$device" >/dev/null 2>&1 || true; done

say "written to $OUT_ROOT/$CITY"
find "$OUT_ROOT/$CITY" -name '*.png' | sort | sed 's|.*/screenshots/|  |'
[ -z "${NO_OPEN:-}" ] && [ "$(uname)" = "Darwin" ] && open "$OUT_ROOT/$CITY" 2>/dev/null || true
