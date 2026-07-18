#!/usr/bin/env bash
#
# Generate Google Play phone screenshots for the Kinowo / Showtimes app by
# driving the app on a running emulator, then pad them to a Play-safe ratio.
#
# The app is a single deployment per country (pl → kinowo.fly.dev,
# uk → showtimes-uk.fly.dev, de → showtimes-de.fly.dev). Picking a country in
# the first-run gate forces that country's locale (pl / en / de) AND routes the
# repertoire requests at that country's backend, so the SAME build produces
# Polish, English and German screenshots — one city each.
#
# Four screens are captured per city, matching the committed Play set:
#   1.png  repertoire grid (hero, "All" dates)   2.png  film detail
#   3.png  filters sheet                          4.png  cinema filter (expanded)
#
# ── Requirements ──────────────────────────────────────────────────────────────
#   • A booted emulator. Coordinates are calibrated for the **kinowo_xl** AVD
#     (Pixel 9 Pro XL, 1344×2992) — the biggest phone AVD, which gives the
#     crispest store shots. Boot it with:
#         $ANDROID_HOME/emulator/emulator -avd kinowo_xl -no-snapshot-load &
#     A different resolution will mis-place taps; the script warns if wm size
#     isn't 1344×2992.
#   • The app installed:  (cd android && ./gradlew :app:installDebug)
#     It MUST include the in-session country-switch fix (commit 858f39d40) — the
#     older build fetched UK/DE cities from the PL backend and rendered empty.
#   • ImageMagick's `sips` (macOS built-in) for padding; python3 + curl for --top.
#
# ── Usage ─────────────────────────────────────────────────────────────────────
#   # Capture one city's four screens into the repo listing dir:
#   android/scripts/store-screenshots.sh en-GB "Birmingham"
#   android/scripts/store-screenshots.sh pl-PL "Poznan"
#   android/scripts/store-screenshots.sh de-DE "Frankfurt"           # slug frankfurt-am-main
#
#   # Capture to a scratch dir instead of the repo (review before committing):
#   android/scripts/store-screenshots.sh en-GB "Birmingham" /tmp/shots
#
#   # List the 10 biggest cities (by live film count) for a country, to choose from:
#   android/scripts/store-screenshots.sh --top uk 10
#   android/scripts/store-screenshots.sh --top pl
#   android/scripts/store-screenshots.sh --top de
#
# ── Notes / limitations ───────────────────────────────────────────────────────
#   • The detail shot opens the FIRST film. On de the showtimes-de enrichment is
#     still catching up, so some films carry Polish genres / raw-HTML synopses —
#     for a clean German detail, pass a specific clean film via CLEAN_FILM="…".
#   • Split cities (e.g. London) show a "Choose your areas" dialog on selection —
#     pass SPLIT=1 so the script dismisses it via "Show listings". On a flat city
#     that tap would hit a film/cinema link and open a browser, so it's opt-in.
#     Screen 4 is always the Filtry cinema section for consistency.
#       SPLIT=1 android/scripts/store-screenshots.sh en-GB "London"
#   • It's an on-device UI driver, so it's timing-sensitive; re-run if a screen
#     lands blank (the de backend is the usual culprit — it's slow).
#
set -euo pipefail

SDK="${ANDROID_SDK_ROOT:-${ANDROID_HOME:-/opt/homebrew/share/android-commandlinetools}}"
ADB="$SDK/platform-tools/adb"
PKG="net.pawel.kinowo"
ACT="pl.kinowo.MainActivity"
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
LISTINGS="$REPO_ROOT/android/app/src/main/play/listings"

# locale → (gate country code, gate pill x on kinowo_xl, backend base url, search term hint)
locale_country() { case "$1" in en-GB) echo uk;; pl-PL) echo pl;; de-DE) echo de;; *) echo "" ;; esac; }
country_base()   { case "$1" in pl) echo "https://kinowo.fly.dev";; uk) echo "https://showtimes-uk.fly.dev";; de) echo "https://showtimes-de.fly.dev";; *) echo "";; esac; }
# Gate pill x-centres (kinowo_xl, three pills: Polska / United Kingdom / Deutschland)
country_pill_x() { case "$1" in pl) echo 205;; uk) echo 694;; de) echo 1096;; *) echo 0;; esac; }

# ── kinowo_xl tap coordinates (px) ────────────────────────────────────────────
X_SEARCH=694;  Y_SEARCH=680        # gate: "search for a city" field
X_RESULT=694;  Y_RESULT=891        # gate: first search result row
X_ALL=1140;    Y_PILLS=164         # list: "All / Wszystkie / Alle" date pill
X_FILTERS=1260                     # list: filters (hamburger) icon  (same Y_PILLS)
X_FILM=250;    Y_FILM=1275         # list: first film card title → detail
X_CINEMA=180;  Y_CINEMA=682        # Filtry sheet: "Cinemas / Kina / Kinos" section header
X_SHOWLIST=895; Y_SHOWLIST=2047    # split-city "Choose your areas" → Show listings
Y_SEARCH_FILM=2800                 # list: "Search films" field (for CLEAN_FILM)

adb()  { "$ADB" "$@"; }
tap()  { adb shell input tap "$1" "$2"; }
snap() { adb exec-out screencap -p > "$1"; }               # $1 = out png
sleep_s() { command sleep "$1"; }

require_device() {
  adb wait-for-device
  [ "$(adb shell getprop sys.boot_completed | tr -d '\r')" = "1" ] || { echo "emulator not booted" >&2; exit 1; }
  local size; size="$(adb shell wm size | tr -d '\r' | awk '{print $NF}')"
  adb shell wm size reset  >/dev/null 2>&1 || true
  adb shell wm density reset >/dev/null 2>&1 || true
  [ "$size" = "1344x2992" ] || echo "WARN: screen is $size, not 1344x2992 (kinowo_xl) — taps may miss." >&2
}

# Pad to a Play-safe ratio: width so long:short ≤ 1.98:1, black bars (invisible on
# the dark UI). Google Play caps the side ratio at 2:1; native 1344×2992 is 2.23:1.
pad_playsafe() { # $1 in, $2 out
  local h w; h="$(sips -g pixelHeight "$1" | awk '/pixelHeight/{print $2}')"
  w=$(python3 -c "import math;print(int(math.ceil($h/1.98)))")
  sips --padToHeightWidth "$h" "$w" --padColor 000000 "$1" --out "$2" >/dev/null 2>&1
}

launch_fresh() {
  adb shell pm clear "$PKG" >/dev/null
  adb shell pm grant "$PKG" android.permission.ACCESS_COARSE_LOCATION 2>/dev/null || true
  adb shell am start -n "$PKG/$ACT" >/dev/null 2>&1
  sleep_s 34                                  # cold start renders the gate
}

select_country() { # $1 = pl|uk|de
  [ "$1" = pl ] && return 0                   # Polska is the default selection
  tap "$(country_pill_x "$1")" 234
  sleep_s 32                                  # setCountry → recreate + catalog reload
}

select_city() { # $1 = search term
  tap "$X_SEARCH" "$Y_SEARCH"; sleep_s 1
  adb shell input text "$1"; sleep_s 2
  tap "$X_RESULT" "$Y_RESULT"                 # first result
}

in_app() { [ "$(adb shell dumpsys activity activities 2>/dev/null | grep -m1 topResumedActivity | grep -c "$PKG")" != 0 ]; }

# Capture the four screens for the currently-selected city into $1 (dir), as 1..4.png
capture_set() { # $1 = outdir, $2 = data-wait secs
  local out="$1" wait="$2"
  mkdir -p "$out"
  sleep_s "$wait"                             # repertoire fetch (de backend is slow)
  # SPLIT cities (e.g. London) pop a "Choose your areas" dialog on selection — tap
  # "Show listings" to dismiss it. Do NOT tap blindly on a flat city: that y-coord
  # lands on a film card / cinema link and can open a browser, derailing the run.
  [ -n "${SPLIT:-}" ] && { tap "$X_SHOWLIST" "$Y_SHOWLIST"; sleep_s 3; }
  in_app || echo "WARN: app is not foreground — a stray tap likely opened a browser; re-run (SPLIT=1 for split cities?)." >&2
  tap "$X_ALL" "$Y_PILLS"; sleep_s 9          # "All" dates + let posters decode
  # If a specific clean film was requested (de detail), filter to it first for the hero-adjacent detail.
  snap "$out/1.png"                           # 1 · repertoire
  if [ -n "${CLEAN_FILM:-}" ]; then
    tap "$X_SEARCH" "$Y_SEARCH_FILM"; sleep_s 1
    adb shell input text "$CLEAN_FILM"; sleep_s 3
  fi
  tap "$X_FILM" "$Y_FILM"; sleep_s 6
  snap "$out/2.png"                           # 2 · detail
  adb shell input keyevent 4; sleep_s 2       # back to list
  [ -n "${CLEAN_FILM:-}" ] && { adb shell input keyevent 4; sleep_s 1; }  # clear film search
  tap "$X_FILTERS" "$Y_PILLS"; sleep_s 3
  snap "$out/3.png"                           # 3 · filters
  tap "$X_CINEMA" "$Y_CINEMA"; sleep_s 3
  snap "$out/4.png"                           # 4 · cinema filter (expanded)
  adb shell input keyevent 4; sleep_s 1
}

cmd_capture() { # $1 locale (en-GB/pl-PL/de-DE), $2 search term, $3 optional outdir
  local locale="$1" term="$2" outdir="${3:-}"
  local country; country="$(locale_country "$locale")"
  [ -n "$country" ] || { echo "unknown locale '$locale' (use en-GB|pl-PL|de-DE)" >&2; exit 2; }
  local dest="${outdir:-$LISTINGS/$locale/graphics/phoneScreenshots}"
  local stage; stage="$(mktemp -d)"
  # de is the slow backend — wait longer for its repertoire fetch.
  local wait=26; [ "$country" = de ] && wait=40

  require_device
  echo "▶ $locale  (country=$country, city='$term') → $dest"
  launch_fresh
  select_country "$country"
  select_city "$term"
  capture_set "$stage" "$wait"

  mkdir -p "$dest"
  for n in 1 2 3 4; do pad_playsafe "$stage/$n.png" "$dest/$n.png"; done
  rm -rf "$stage"
  echo "✓ wrote $dest/{1..4}.png (padded to Play-safe ratio)"
}

# Rank a country's cities by live film count and print the top-N slugs.
cmd_top() { # $1 country, $2 N
  local country="$1" n="${2:-10}" base; base="$(country_base "$country")"
  [ -n "$base" ] || { echo "unknown country '$country' (use pl|uk|de)" >&2; exit 2; }
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
cities = json.loads(get(f"{base}/api/catalog"))["cities"]
mine = [c for c in cities if c.get("country") == country]
def count(c):
    try:
        d = json.loads(get(f"{base}/{c['slug']}/api/repertoire"))
        return (len(d) if isinstance(d, list) else len(d.get("films", [])), c["slug"], c["name"])
    except Exception:
        return (-1, c["slug"], c["name"])
with cf.ThreadPoolExecutor(max_workers=8) as ex:      # capped per the repo's parallelism rule
    rows = sorted(ex.map(count, mine), reverse=True)
print(f"top {n} {country} cities by live film count ({len(mine)} total):")
for films, slug, name in rows[:n]:
    print(f"  {films:4}  {slug:22} {name}")
PY
}

case "${1:-}" in
  --top)   shift; cmd_top "$@";;
  -h|--help|"") sed -n '2,60p' "$0" | sed 's/^# \{0,1\}//';;
  *)       cmd_capture "$@";;
esac
