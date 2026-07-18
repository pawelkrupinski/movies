#!/usr/bin/env bash
#
# Generate Google Play phone screenshots for the Kinowo / Showtimes app.
# Just run it — it boots the emulator, installs the app, drives the four store
# screens per city and pads them to a Play-safe ratio. One build makes pl/en/de
# shots: picking a country in the gate forces that locale + backend.
#
#   android/scripts/store-screenshots.sh en-GB "Birmingham"      # → repo listing dir
#   android/scripts/store-screenshots.sh pl-PL "Poznan" /tmp/x   # → a scratch dir
#   android/scripts/store-screenshots.sh --top uk 10             # biggest cities by film count
#
# When done it opens the four shots in Preview (macOS). Env: SPLIT=1 for split
# cities (London) · CLEAN_FILM="…" for a clean German detail (showtimes-de lags) ·
# BUILD=1 force rebuild+install · AVD=<name> · NO_OPEN=1 skip the Preview.
#
set -euo pipefail

SDK="${ANDROID_SDK_ROOT:-${ANDROID_HOME:-/opt/homebrew/share/android-commandlinetools}}"
export ANDROID_HOME="$SDK"
ADB="$SDK/platform-tools/adb"
EMU="$SDK/emulator/emulator"
PKG="net.pawel.kinowo"
ACT="pl.kinowo.MainActivity"
AVD="${AVD:-kinowo_xl}"                         # Pixel 9 Pro XL 1344×2992 — coords assume this
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
LISTINGS="$REPO_ROOT/android/app/src/main/play/listings"
NOISE="$(mktemp)"                               # adb / gradle / emulator chatter lands here

# ── clean output ──────────────────────────────────────────────────────────────
say()  { printf '\033[36m▸\033[0m %s\n' "$*"; }
step() { printf '  %s… ' "$*"; }
done_() { printf '\033[32m✓\033[0m\n'; }
ok()   { printf '\033[32m✓\033[0m %s\n' "$*"; }
warn() { printf '\033[33m!\033[0m %s\n' "$*" >&2; }
die()  { printf '\033[31m✗\033[0m %s\n' "$*" >&2; [ -s "$NOISE" ] && tail -5 "$NOISE" >&2; exit 1; }
cleanup() { rm -f "$NOISE"; }; trap cleanup EXIT

adb()  { "$ADB" "$@"; }
tap()  { adb shell input tap "$1" "$2" >>"$NOISE" 2>&1; }
type_() { adb shell input text "$1" >>"$NOISE" 2>&1; }
back() { adb shell input keyevent 4 >>"$NOISE" 2>&1; }
snap() { adb exec-out screencap -p > "$1"; }
naps() { command sleep "$1"; }

locale_country() { case "$1" in en-GB) echo uk;; pl-PL) echo pl;; de-DE) echo de;; *) echo "";; esac; }
country_name()   { case "$1" in pl) echo Polska;; uk) echo "United Kingdom";; de) echo Deutschland;; esac; }
country_base()   { case "$1" in pl) echo "https://kinowo.fly.dev";; uk) echo "https://showtimes-uk.fly.dev";; de) echo "https://showtimes-de.fly.dev";; *) echo "";; esac; }
country_pill_x() { case "$1" in pl) echo 205;; uk) echo 694;; de) echo 1096;; *) echo 0;; esac; }

# kinowo_xl tap coordinates (px)
X_SEARCH=694;  Y_SEARCH=680        # gate: search field
X_RESULT=694;  Y_RESULT=891        # gate: first result
X_ALL=1140;    Y_PILLS=164         # list: "All / Wszystkie / Alle" pill
X_FILTERS=1260                     # list: filters icon (same Y_PILLS)
X_FILM=250;    Y_FILM=1275         # list: first film → detail
X_CINEMA=180;  Y_CINEMA=682        # Filtry: cinema section header
X_SHOWLIST=895; Y_SHOWLIST=2047    # split-city "Show listings"
Y_SEARCH_FILM=2800                 # list: "Search films" field

# ── self-setup ────────────────────────────────────────────────────────────────
booted() { [ "$(adb get-state 2>/dev/null)" = "device" ] &&
           [ "$(adb shell getprop sys.boot_completed 2>/dev/null | tr -d '\r')" = "1" ]; }

ensure_emulator() {
  if ! booted; then
    # Clear any half-dead ("offline") device left by a previous shutdown so adb
    # doesn't target it, then boot a fresh one.
    [ -n "$(adb get-state 2>/dev/null || true)" ] && { adb kill-server >>"$NOISE" 2>&1 || true; naps 2; }
    "$EMU" -list-avds 2>/dev/null | grep -qx "$AVD" || die "AVD '$AVD' not found. Create it (Android Studio) or pass AVD=<name>."
    step "booting $AVD"
    nohup "$EMU" -avd "$AVD" -no-snapshot-load -no-boot-anim -netdelay none -netspeed full >>"$NOISE" 2>&1 &
    local t=0
    until booted; do naps 3; t=$((t+3)); [ $t -gt 240 ] && die "emulator didn't finish booting"; done
    done_
  fi
  adb shell wm size reset  >>"$NOISE" 2>&1 || true
  adb shell wm density reset >>"$NOISE" 2>&1 || true
  local size; size="$(adb shell wm size | tr -d '\r' | awk '{print $NF}')"
  [ "$size" = "1344x2992" ] || warn "screen is $size, not 1344×2992 ($AVD) — taps may miss."
}

ensure_app() {
  if [ -n "${BUILD:-}" ] || ! adb shell pm list packages 2>/dev/null | grep -q "$PKG"; then
    step "installing app"
    ( cd "$REPO_ROOT/android" && [ -f local.properties ] || echo "sdk.dir=$SDK" > local.properties
      cd "$REPO_ROOT/android" && ./gradlew :app:installDebug ) >>"$NOISE" 2>&1 || die "gradle installDebug failed"
    done_
  fi
}

# ── capture ───────────────────────────────────────────────────────────────────
pad_playsafe() { # $1 in, $2 out — pad width so long:short ≤ 1.98:1 (Play caps at 2:1)
  local h w; h="$(sips -g pixelHeight "$1" | awk '/pixelHeight/{print $2}')"
  w=$(python3 -c "import math;print(int(math.ceil($h/1.98)))")
  sips --padToHeightWidth "$h" "$w" --padColor 000000 "$1" --out "$2" >>"$NOISE" 2>&1
}

in_app() { adb shell dumpsys activity activities 2>/dev/null | grep -m1 topResumedActivity | grep -q "$PKG"; }

# Poll a screencap until it's bigger than $2 bytes (a blank/near-black frame
# compresses tiny; a rendered screen is 60 KB+, one with posters 250 KB+). This
# replaces fixed sleeps so a slow cold-boot first launch doesn't yield blanks.
wait_frame() { # $1 timeout-secs, $2 min-bytes
  local t=0 f="$NOISE.frame"
  while [ "$t" -lt "$1" ]; do
    snap "$f" 2>/dev/null || true
    [ -s "$f" ] && [ "$(wc -c < "$f")" -gt "$2" ] && { rm -f "$f"; return 0; }
    naps 2; t=$((t+2))
  done
  rm -f "$f"; return 1
}

cmd_capture() { # $1 locale, $2 search term, $3 optional outdir
  local locale="$1" term="$2" outdir="${3:-}"
  local country; country="$(locale_country "$locale")"
  [ -n "$country" ] || die "unknown locale '$locale' (use en-GB | pl-PL | de-DE)"
  local dest="${outdir:-$LISTINGS/$locale/graphics/phoneScreenshots}"
  local stage; stage="$(mktemp -d)"
  local wait=26; [ "$country" = de ] && wait=40      # de backend is slow

  say "$locale · $term"
  ensure_emulator
  ensure_app

  step "opening app"
    adb shell pm clear "$PKG" >>"$NOISE" 2>&1
    adb shell pm grant "$PKG" android.permission.ACCESS_COARSE_LOCATION >>"$NOISE" 2>&1 || true
    adb shell am start -n "$PKG/$ACT" >>"$NOISE" 2>&1
    wait_frame 120 60000 || warn "gate slow to render"   # cold first launch can take ~40s
    naps 2
  done_

  # Always tap the target country pill (even Polska) so a country left persisted by
  # a previous run — which pm clear on a fresh boot doesn't always reset — can't leak
  # in. Re-selecting the current country is a harmless no-op; a change recreates.
  step "country → $(country_name "$country")"
    tap "$(country_pill_x "$country")" 234; naps 4       # select → forces locale + backend
    wait_frame 120 60000 || warn "gate slow after country select"; naps 2
  done_

  step "loading $term"
    tap "$X_SEARCH" "$Y_SEARCH"; naps 1; type_ "$term"; naps 2; tap "$X_RESULT" "$Y_RESULT"
    wait_frame 50 60000 || true                          # list / area dialog chrome up
    [ -n "${SPLIT:-}" ] && { tap "$X_SHOWLIST" "$Y_SHOWLIST"; naps 3; }
    in_app || die "app fell out of foreground — a stray tap opened a browser (SPLIT=1 for split cities?)"
  done_

  step "capturing 4 screens"
    tap "$X_ALL" "$Y_PILLS"                               # "All" dates
    wait_frame "$((wait + 25))" 250000 || warn "$term list looks empty/blank — re-run"   # films + posters
    naps 3
    snap "$stage/1.png"                                   # repertoire
    if [ -n "${CLEAN_FILM:-}" ]; then
      tap "$X_SEARCH" "$Y_SEARCH_FILM"; naps 1; type_ "$CLEAN_FILM"; naps 3
    fi
    # Screen-to-screen transitions are quick animations between two non-blank
    # screens, so wait_frame (a size threshold) can't tell them apart — use fixed
    # settles here, and wait_frame only for blank→rendered (gate, list population).
    tap "$X_FILM" "$Y_FILM"; naps 5                       # open detail
    snap "$stage/2.png"                                   # detail
    back; naps 2; [ -n "${CLEAN_FILM:-}" ] && { back; naps 1; }
    tap "$X_FILTERS" "$Y_PILLS"; naps 4                   # Filtry sheet slides up
    snap "$stage/3.png"                                   # filters
    tap "$X_CINEMA" "$Y_CINEMA"; naps 3                   # expand cinema section
    snap "$stage/4.png"                                   # cinema filter
    back; naps 1
  done_

  mkdir -p "$dest"
  for n in 1 2 3 4; do pad_playsafe "$stage/$n.png" "$dest/$n.png"; done
  rm -rf "$stage"
  ok "wrote $dest/{1..4}.png"

  # Open the four shots in Preview to eyeball them (macOS). NO_OPEN=1 to skip.
  if [ -z "${NO_OPEN:-}" ] && command -v open >/dev/null 2>&1; then
    open -a Preview "$dest/1.png" "$dest/2.png" "$dest/3.png" "$dest/4.png" >>"$NOISE" 2>&1 || true
  fi
}

cmd_top() { # $1 country, $2 N — rank cities by live film count
  local country="$1" n="${2:-10}" base; base="$(country_base "$country")"
  [ -n "$base" ] || die "unknown country '$country' (use pl | uk | de)"
  BASE="$base" COUNTRY="$country" TOPN="$n" python3 - <<'PY'
import os, json, time, urllib.request, concurrent.futures as cf
base, country, n = os.environ["BASE"], os.environ["COUNTRY"], int(os.environ["TOPN"])
def get(url, t=30, tries=3):
    for i in range(tries):
        try:
            with urllib.request.urlopen(url, timeout=t) as r: return r.read()
        except Exception:
            if i == tries - 1: raise
            time.sleep(1.5)
cities = [c for c in json.loads(get(f"{base}/api/catalog"))["cities"] if c.get("country") == country]
def count(c):
    try:
        d = json.loads(get(f"{base}/{c['slug']}/api/repertoire"))
        return (len(d) if isinstance(d, list) else len(d.get("films", [])), c["slug"], c["name"])
    except Exception:
        return (-1, c["slug"], c["name"])
with cf.ThreadPoolExecutor(max_workers=8) as ex:
    rows = sorted(ex.map(count, cities), reverse=True)
print(f"top {n} {country} cities by live film count ({len(cities)} total):")
for films, slug, name in rows[:n]:
    print(f"  {films:4}  {slug:22} {name}")
PY
}

usage() { sed -n '3,14p' "$0" | sed 's/^#\{1,2\} \{0,1\}//'; }

case "${1:-}" in
  --top)        shift; cmd_top "$@";;
  -h|--help|"") usage;;
  *)            cmd_capture "$@";;
esac
