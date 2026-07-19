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
#   android/scripts/store-screenshots.sh --all-top 2             # top 2 cities of EVERY country
#
# --all-top fills each locale's listing dir with one four-shot block per city,
# numbered end to end (city 1 → 1-4.png, city 2 → 5-8.png, …). Play shows at
# most 8 phone shots, so N=2 is the practical ceiling. It drives one read-only
# emulator per country in parallel (~countries× faster); EMULATORS=<k> caps that
# (EMULATORS=1 = serial) for hosts that can't feed three emulators at once.
#
# When done it opens the shots in Preview (macOS). Env: EMULATORS=<k> parallel
# emulator count · CLEAN_FILM="…" for a clean German detail (showtimes-de lags) ·
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

# Every device-touching call funnels through here, so honouring $SERIAL in one
# place pins the WHOLE script to one emulator. Unset (the single-emulator paths)
# it targets adb's sole device exactly as before; --all-top exports a per-worker
# serial so each parallel country drives its own instance.
adb()  { "$ADB" ${SERIAL:+-s "$SERIAL"} "$@"; }
tap()  { adb shell input tap "$1" "$2" >>"$NOISE" 2>&1; }
# `adb shell input text` splits on spaces and types only the first word unless
# each space is sent as the literal escape `%s` — so multi-word cities ("West
# Yorkshire", "Isle of Wight") need encoding, not the raw string.
type_() { adb shell input text "${1// /%s}" >>"$NOISE" 2>&1; }
back() { adb shell input keyevent 4 >>"$NOISE" 2>&1; }
snap() { adb exec-out screencap -p > "$1"; }
naps() { command sleep "$1"; }

COUNTRIES="pl uk de"                            # every country --all-top walks
locale_country() { case "$1" in en-GB) echo uk;; pl-PL) echo pl;; de-DE) echo de;; *) echo "";; esac; }
country_locale() { case "$1" in pl) echo pl-PL;; uk) echo en-GB;; de) echo de-DE;; *) echo "";; esac; }
# The pill labels are ENDONYMS from the catalog — identical in every locale, so
# they double as the tap target (see tap_text) regardless of the app's language.
country_name()   { case "$1" in pl) echo Polska;; uk) echo "United Kingdom";; de) echo Deutschland;; esac; }
country_base()   { case "$1" in pl) echo "https://kinowo.fly.dev";; uk) echo "https://showtimes-uk.fly.dev";; de) echo "https://showtimes-de.fly.dev";; *) echo "";; esac; }
# The gate's "Country" header, which IS localized — picking a country forces that
# country's language, so seeing this label is proof the switch actually landed.
country_header() { case "$1" in pl) echo Kraj;; uk) echo Country;; de) echo Land;; esac; }
# The area picker's confirm button (`areapicker_confirm`), shown only by SPLIT
# cities — a city is split iff its catalog `areas` is non-empty, which today is
# London alone. We look for this label after picking a city instead of being told
# "this one is split" up front, so the run adapts on its own: a newly-split city
# needs no flag, and an un-split one just doesn't find it. This replaced a manual
# SPLIT=1 env var, which --all-top could not have set correctly per-city anyway.
showlist_label() { case "$1" in pl) echo "Pokaż repertuar";; uk) echo "Show listings";; de) echo "Programm anzeigen";; esac; }

# ── parallel pool: one emulator per worker ────────────────────────────────────
# --all-top can drive several read-only clones of the AVD at once, one per
# country, cutting wall-clock roughly by the emulator count. These pure helpers
# decide which instance and which countries each worker owns; the boot/install
# machinery is further down.
pool_port()   { echo "$((5554 + 2 * $1))"; }        # worker 0→5554, 1→5556, 2→5558
pool_serial() { echo "emulator-$(pool_port "$1")"; }
# Countries worker W handles when K workers share the list, round-robin: worker W
# takes indices W, W+K, W+2K… So K≥#countries gives each its own worker, and a
# smaller K packs the remainder onto earlier workers, run sequentially there.
worker_slice() { # $1 country list, $2 K, $3 W
  local list="$1" k="$2" w="$3" i=0 c
  for c in $list; do [ $((i % k)) -eq "$w" ] && printf '%s ' "$c"; i=$((i + 1)); done
}
# Clamp a requested worker count to [1, #countries] — booting more emulators than
# there are countries would leave the extras idle.
effective_k() { # $1 requested, $2 country count
  local req="$1" max="$2"
  { [ "$req" -ge 1 ]; } 2>/dev/null || req=1
  [ "$req" -le "$max" ] || req="$max"
  echo "$req"
}

# kinowo_xl tap coordinates (px)
X_ALL=1140;    Y_PILLS=164         # list: "All / Wszystkie / Alle" pill
X_FILTERS=1260                     # list: filters icon (same Y_PILLS)
X_FILM=250;    Y_FILM=1275         # list: first film → detail
X_CINEMA=180;  Y_CINEMA=682        # Filtry: cinema section header
X_SEARCH_FILM=694; Y_SEARCH_FILM=2800  # list: "Search films" field

# The four files one capture writes, in screen order. Both the capture (naming
# its output) and --all-top (listing what to open in Preview) need this mapping,
# so it lives in one place: city N of a run starts at 4N+1 and the blocks sit end
# to end in the listing dir.
shot_paths() { # $1 dir, $2 number of the first file
  local n; for n in 0 1 2 3; do printf '%s/%s.png\n' "$1" "$(($2 + n))"; done
}

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

# ── tapping by label, not by pixel ────────────────────────────────────────────
# The gate (country pills, city search, city rows) is driven through the
# accessibility tree instead of hardcoded coordinates. Coordinates alone can't
# tell "the screen I meant" from "whatever is on screen right now": a tap sent
# before the gate has rendered is swallowed, and every later tap then lands on
# the wrong screen — which is how `en-GB Liverpool` once produced GERMAN
# screenshots (the list's "All dates" tap at 1140,164 sits exactly on the
# Deutschland pill of the still-showing gate, and the first city row under it is
# Amberg). Waiting for a node with the expected TEXT makes each step assert the
# state it thinks it's in, so a miss fails loudly instead of silently capturing
# the wrong country.
ui_xml() { adb shell uiautomator dump /sdcard/kinowo-ui.xml >>"$NOISE" 2>&1 && adb shell cat /sdcard/kinowo-ui.xml 2>>"$NOISE"; }

# Tap-point of the first node reading $1; empty when absent. The text node
# itself isn't the clickable one (Compose wraps it), so we aim at the label's
# centre — the enclosing button always covers it.
# Text fields are skipped: typing "Liverpool" into the city search makes the
# EditText itself read "Liverpool", so matching on text alone finds the box we
# just typed into instead of the city row below it — and tapping the box is a
# no-op that leaves the run stranded on the gate.
#
# Comparison folds case and diacritics, matching how the app's own city search
# behaves: `store-screenshots.sh pl-PL Poznan` must find the row rendered
# "Poznań" (and `Wroclaw` → "Wrocław"), so callers can stay on ASCII.
node_center() { python3 -c '
import re, sys, unicodedata
def fold(s):
    s = s.replace("ł", "l").replace("Ł", "L")
    return "".join(c for c in unicodedata.normalize("NFD", s)
                   if not unicodedata.combining(c)).casefold()
text, xml = fold(sys.argv[1]), sys.stdin.read()
for node in re.finditer(r"<node[^>]*>", xml):
    n = node.group(0)
    if "EditText" in n: continue
    t = re.search(r" text=\"([^\"]*)\"", n)
    b = re.search(r" bounds=\"\[(\d+),(\d+)\]\[(\d+),(\d+)\]\"", n)
    if t and b and fold(t.group(1)) == text:
        x1, y1, x2, y2 = map(int, b.groups())
        print((x1 + x2) // 2, (y1 + y2) // 2); break
' "$1"; }

# Wait until a node reading $1 is on screen ($2 secs, default 120).
wait_text() {
  local t=0 limit="${2:-120}" hit
  while [ "$t" -lt "$limit" ]; do
    hit="$(ui_xml | node_center "$1")"
    [ -n "$hit" ] && { echo "$hit"; return 0; }
    naps 3; t=$((t+3))
  done
  return 1
}

# Wait for $1 then tap it. Dies with the label in the message, so a failure says
# WHICH element never appeared rather than leaving a wrong-looking screenshot.
tap_text() {
  local point; point="$(wait_text "$1" "${2:-120}")" || die "'$1' never appeared on screen — the app is on an unexpected screen."
  tap ${point}
}

# Tap the gate's city search box — the screen's only EditText, and unlabelled
# (its placeholder is drawn by Compose, not exposed as node text), so it's found
# by class rather than by text.
tap_field() {
  local point; point="$(ui_xml | python3 -c '
import re, sys
for node in re.finditer(r"<node[^>]*>", sys.stdin.read()):
    n = node.group(0)
    if "android.widget.EditText" in n:
        b = re.search(r" bounds=\"\[(\d+),(\d+)\]\[(\d+),(\d+)\]\"", n)
        if b:
            x1, y1, x2, y2 = map(int, b.groups())
            print((x1 + x2) // 2, (y1 + y2) // 2); break
')"
  [ -n "$point" ] || die "no search field on the gate — the app is on an unexpected screen."
  tap ${point}
}

# The current text of the gate's search box (its only EditText), read back from
# the accessibility tree.
field_text() {
  ui_xml | python3 -c '
import re, sys
for node in re.finditer(r"<node[^>]*>", sys.stdin.read()):
    n = node.group(0)
    if "android.widget.EditText" in n:
        t = re.search(r" text=\"([^\"]*)\"", n)
        print(t.group(1) if t else ""); break
'
}

# The ASCII text to type into the city search so the app filters to $1. Several
# constraints meet here: `adb shell input text` can't emit ł/ó/ü/… and mangles
# spaces/&/commas; the app's search folds ONLY the nine Polish diacritics
# (ą ć ę ł ń ó ś ż ź) and matches by SUBSTRING. So we apply that same Polish fold,
# then type the single longest [a-z0-9] TOKEN of the result — which the substring
# match still finds and which, being one alphanumeric word, dodges every character
# input text can't send. Examples: "Kraków"→"krakow" (ó folds), "München"→"nchen"
# (ü doesn't, so the tail after it), "West Yorkshire"→"yorkshire" (skips the
# space), "Edinburgh & Lothians"→"edinburgh" (skips " & "). tap_text then selects
# the row by its real name — it folds ü itself, via NFD — even when the token
# matches several rows (the five "…Yorkshire" counties), since it matches in full.
search_term() { # $1 city name
  python3 -c '
import sys, re
polish = {"ą":"a", "ć":"c", "ę":"e", "ł":"l", "ń":"n", "ó":"o", "ś":"s", "ź":"z", "ż":"z"}
folded = "".join(polish.get(c, c) for c in sys.argv[1].lower())
tokens = re.findall(r"[a-z0-9]+", folded)
print(max(tokens, key=len) if tokens else "")
' "$1"
}

# Type $1 into the focused search box and CONFIRM it landed, retyping if it
# didn't. `adb shell input text` silently drops and duplicates characters on a
# CPU-starved emulator — three parallel instances once turned "Warszawa" into
# "WWars", and the city then genuinely didn't exist, failing the row tap. Reading
# the field back and clearing+retyping until it matches makes the run resilient to
# that, at any emulator count. Returns nonzero if it never comes clean.
type_field_verified() { # $1 text
  local want="$1" got i j
  for i in 1 2 3 4; do
    got="$(field_text)"
    [ "$got" = "$want" ] && return 0
    if [ -n "$got" ]; then                               # wrong content — clear it first
      adb shell input keyevent KEYCODE_MOVE_END >>"$NOISE" 2>&1
      for ((j = 0; j < ${#got} + 2; j++)); do adb shell input keyevent 67 >>"$NOISE" 2>&1; done  # 67 = DEL
    fi
    naps 1; type_ "$want"; naps 2
  done
  [ "$(field_text)" = "$want" ]
}

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

cmd_capture() { # $1 locale, $2 search term, $3 optional outdir, $4 optional first file number
  local locale="$1" term="$2" outdir="${3:-}" first="${4:-1}"
  local country; country="$(locale_country "$locale")"
  [ -n "$country" ] || die "unknown locale '$locale' (use en-GB | pl-PL | de-DE)"
  local dest="${outdir:-$LISTINGS/$locale/graphics/phoneScreenshots}"
  local stage; stage="$(mktemp -d)"
  local wait=26; [ "$country" = de ] && wait=40      # de backend is slow

  say "$locale · $term"
  # In pool mode the orchestrator has already booted this worker's emulator and
  # installed the app on it, so skip both — and crucially skip ensure_emulator's
  # kill-server/boot fallback, which under parallel workers would tear the adb
  # server out from under the others.
  [ -n "${POOL:-}" ] || { ensure_emulator; ensure_app; }

  step "opening app"
    adb shell pm clear "$PKG" >>"$NOISE" 2>&1
    adb shell pm grant "$PKG" android.permission.ACCESS_COARSE_LOCATION >>"$NOISE" 2>&1 || true
    adb shell am start -n "$PKG/$ACT" >>"$NOISE" 2>&1
    # Wait for a country PILL, not just a big frame: right after `am start` the
    # launcher is still on screen and compresses well past any size threshold,
    # so a size-only wait returns before the app has drawn anything.
    wait_text "$(country_name "$country")" 150 >/dev/null ||
      die "the city gate never appeared — cold launch may have failed (see BUILD=1)."
  done_

  # We always tap the target pill (even Polska): a country left persisted by a
  # previous run — which pm clear on a fresh boot doesn't always reset — would
  # otherwise leak in. Re-selecting the current country is a harmless no-op.
  step "country → $(country_name "$country")"
    tap_text "$(country_name "$country")" 30             # forces locale + backend
    # Picking a country forces its language, so the localized "Country" header
    # coming back is proof the switch took effect — the app recreates itself
    # (~30s of blank screen) between the tap and that label appearing.
    wait_text "$(country_header "$country")" 150 >/dev/null ||
      die "country never switched to $(country_name "$country") — the pill tap didn't land."
    naps 2
  done_

  step "loading $term"
    tap_field                                            # gate: the city search box
    naps 1
    local typed; typed="$(search_term "$term")"          # ASCII the app's fold will match
    type_field_verified "$typed" ||
      die "the city search kept mangling '$typed' — the emulator is starved (try a lower EMULATORS)."
    # Tap the row NAMED $term rather than "whatever is first": on the wrong
    # country (or before the list loads) there is no such row, so this dies
    # instead of opening some other city — the Amberg failure above.
    tap_text "$term" 60
    wait_frame 50 60000 || true                          # list / area dialog chrome up
    # Split cities (London) land on the area picker rather than the list. Give its
    # confirm button a short look-in: present → tap through it, absent → this city
    # isn't split and we're already on the list. Short timeout because every
    # un-split city pays it.
    local showlist; showlist="$(wait_text "$(showlist_label "$country")" 12 || true)"
    [ -n "$showlist" ] && { tap ${showlist}; naps 3; }
    in_app || die "app fell out of foreground — a stray tap opened a browser."
    # Everything below taps by coordinate, which is only safe once we are off the
    # gate — the list's "All dates" spot sits on the gate's Deutschland pill, so a
    # run still stranded here would silently capture GERMAN screens.
    if ui_xml | node_center "$(country_header "$country")" | grep -q .; then
      die "still on the city gate after picking $term — nothing was captured."
    fi
  done_

  step "capturing 4 screens"
    tap "$X_ALL" "$Y_PILLS"                               # "All" dates
    wait_frame "$((wait + 25))" 250000 || warn "$term list looks empty/blank — re-run"   # films + posters
    naps 3
    snap "$stage/1.png"                                   # repertoire
    if [ -n "${CLEAN_FILM:-}" ]; then
      tap "$X_SEARCH_FILM" "$Y_SEARCH_FILM"; naps 1; type_ "$CLEAN_FILM"; naps 3
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
  local n=1 out; local -a shots=()
  while IFS= read -r out; do shots+=("$out"); done < <(shot_paths "$dest" "$first")
  for out in "${shots[@]}"; do pad_playsafe "$stage/$n.png" "$out"; n=$((n + 1)); done
  rm -rf "$stage"
  ok "wrote $dest/{$first..$((first + 3))}.png"

  # Open the four shots in Preview to eyeball them (macOS). NO_OPEN=1 to skip —
  # which is how --all-top suppresses the per-city pop-up so it can open the whole
  # set once at the end.
  if [ -z "${NO_OPEN:-}" ] && command -v open >/dev/null 2>&1; then
    open -a Preview "${shots[@]}" >>"$NOISE" 2>&1 || true
  fi
}

# Rank a country's cities by live film count. Prints the city total on the first
# line, then N × "films<TAB>slug<TAB>name", best first — a shape both the human
# --top table and --all-top's capture loop read, so the ranking exists once.
rank_cities() { # $1 country, $2 N
  local country="$1" n="$2" base; base="$(country_base "$country")"
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
print(len(cities))
for films, slug, name in rows[:n]:
    print(f"{films}\t{slug}\t{name}")
PY
}

cmd_top() { # $1 country, $2 N — the ranking as a human-readable table
  local country="$1" n="${2:-10}" ranked
  ranked="$(rank_cities "$country" "$n")"
  printf 'top %s %s cities by live film count (%s total):\n' "$n" "$country" "$(printf '%s\n' "$ranked" | head -1)"
  printf '%s\n' "$ranked" | tail -n +2 |
    while IFS=$'\t' read -r films slug name; do printf '  %4s  %-22s %s\n' "$films" "$slug" "$name"; done
}

# Boot K read-only clones of the AVD on ports 5554, 5556, … Several instances can
# only share one AVD image if EVERY one is -read-only AND the AVD's stale *.lock
# files are gone first — a read-write instance (or a leftover lock from a crashed
# one) makes the rest error out with "Another emulator instance is running". We
# also stagger the launches: two started in the same instant race on acquiring
# the multi-instance lock and both bail.
boot_pool() { # $1 K
  local k="$1" w port serial t
  "$EMU" -list-avds 2>/dev/null | grep -qx "$AVD" || die "AVD '$AVD' not found. Create it (Android Studio) or pass AVD=<name>."
  step "clearing emulators + locks"
    adb kill-server >>"$NOISE" 2>&1 || true
    pkill -f "qemu-system.*$AVD" 2>/dev/null || true
    naps 3
    rm -f "$HOME/.android/avd/$AVD.avd/"*.lock 2>/dev/null || true
    "$ADB" start-server >>"$NOISE" 2>&1 || true
  done_
  for ((w = 0; w < k; w++)); do
    port="$(pool_port "$w")"
    step "booting $AVD :$port (read-only)"
    nohup "$EMU" -avd "$AVD" -read-only -port "$port" -no-snapshot-load -no-boot-anim -netdelay none -netspeed full >>"$NOISE" 2>&1 &
    naps 25                              # stagger so instances don't race the lock
    done_
  done
  for ((w = 0; w < k; w++)); do
    serial="$(pool_serial "$w")"
    step "waiting $serial"
    t=0
    until [ "$(SERIAL="$serial" adb shell getprop sys.boot_completed 2>/dev/null | tr -d '\r')" = "1" ]; do
      naps 5; t=$((t + 5)); [ $t -gt 420 ] && die "$serial didn't finish booting"
    done
    SERIAL="$serial" adb shell wm size reset >>"$NOISE" 2>&1 || true
    done_
  done
}

# Build the debug APK ONCE (progress on stderr, path on stdout) — the pool then
# installs that one artifact on each emulator. Building per worker would run
# several Gradle daemons at once, which the project bans; assembleDebug + N cheap
# `adb install` is the correct shape.
build_apk() {
  step "building APK" >&2
  ( cd "$REPO_ROOT/android" && { [ -f local.properties ] || echo "sdk.dir=$SDK" > local.properties; }
    cd "$REPO_ROOT/android" && ./gradlew :app:assembleDebug ) >>"$NOISE" 2>&1 || die "gradle assembleDebug failed"
  local apk="$REPO_ROOT/android/app/build/outputs/apk/debug/app-debug.apk"
  [ -f "$apk" ] || die "APK not found at $apk"
  done_ >&2
  echo "$apk"
}

# One worker: capture its slice of countries on its own emulator, sequentially.
# Runs inside a subshell with SERIAL + POOL exported, so every cmd_capture here
# drives this worker's instance and skips the boot/install the orchestrator did.
run_worker() { # $1 W, $2 K, $3 N
  local w="$1" k="$2" n="$3" country locale dest stage first name
  for country in $(worker_slice "$COUNTRIES" "$k" "$w"); do
    locale="$(country_locale "$country")"
    dest="$LISTINGS/$locale/graphics/phoneScreenshots"
    # Read the ranked city names into an array BEFORE capturing any of them.
    # cmd_capture's adb calls read stdin, so running it inside a
    # `while read … < <(rank_cities …)` loop lets the first capture drain the
    # process substitution — starving the loop and silently shooting only city 1
    # at N≥2. Buffering the list first decouples it from cmd_capture's stdin.
    local names=()
    while IFS=$'\t' read -r _ _ name; do [ -n "$name" ] && names+=("$name"); done \
      < <(rank_cities "$country" "$n" | tail -n +2)
    # Stage the shots and swap them in only once every city of this country
    # succeeds — a failed city aborts the worker, leaving the old listing whole
    # rather than half-overwritten.
    stage="$(mktemp -d)"; first=1
    for name in "${names[@]}"; do
      cmd_capture "$locale" "$name" "$stage" "$first"
      first=$((first + 4))
    done
    mkdir -p "$dest"
    rm -f "$dest"/*.png                  # a smaller N would otherwise strand the old tail
    mv "$stage"/*.png "$dest"/; rmdir "$stage"
    ok "$locale: top $n → $dest"
  done
}

# Open every captured shot in one Preview, walking cities in ranked order via
# shot_paths (which keeps 10.png after 9.png, not after 1.png).
preview_all() { # $1 per_locale
  local per="$1" country locale dest i f; local all=()
  for country in $COUNTRIES; do
    locale="$(country_locale "$country")"; dest="$LISTINGS/$locale/graphics/phoneScreenshots"
    for ((i = 1; i <= per; i += 4)); do
      while IFS= read -r f; do [ -f "$f" ] && all+=("$f"); done < <(shot_paths "$dest" "$i")
    done
  done
  [ ${#all[@]} -gt 0 ] && command -v open >/dev/null 2>&1 && open -a Preview "${all[@]}" >>"$NOISE" 2>&1 || true
}

cmd_all_top() { # $1 N — capture the top N cities of EVERY country, one emulator per country
  local n="${1:-2}"
  { [ "$n" -ge 1 ]; } 2>/dev/null || die "--all-top wants a city count, e.g. --all-top 2"
  local per_locale=$((n * 4))
  [ "$per_locale" -le 8 ] ||
    warn "$n cities × 4 screens = $per_locale shots per locale, but Play publishes at most 8."

  local ncountries; ncountries=$(set -- $COUNTRIES; echo $#)
  local k; k="$(effective_k "${EMULATORS:-$ncountries}" "$ncountries")"

  # Suppress the per-city Preview pop-up; we open the whole set once at the end,
  # but only if a Preview was wanted at all.
  local want_preview=1; [ -n "${NO_OPEN:-}" ] && want_preview=
  export NO_OPEN=1

  say "top $n × $ncountries countries on $k emulator(s)"
  boot_pool "$k"
  local apk; apk="$(build_apk)"
  local w serial
  for ((w = 0; w < k; w++)); do
    serial="$(pool_serial "$w")"
    step "installing app on $serial"
    SERIAL="$serial" adb install -r "$apk" >>"$NOISE" 2>&1 || die "install failed on $serial"
    done_
  done
  export POOL=1                          # workers: emulator + app already provisioned

  # Launch one worker per emulator, each to its own log so parallel progress
  # can't interleave into garbage; flush each log the moment its worker exits.
  local pids=() logs=()
  for ((w = 0; w < k; w++)); do
    local log; log="$(mktemp)"; logs[$w]="$log"
    ( SERIAL="$(pool_serial "$w")" NOISE="$(mktemp)" run_worker "$w" "$k" "$n" ) >"$log" 2>&1 &
    pids[$w]="$!"
    say "worker $w → $(pool_serial "$w") → $(worker_slice "$COUNTRIES" "$k" "$w")"
  done

  local remaining="$k" rc; local flushed=()
  while [ "$remaining" -gt 0 ]; do
    naps 5
    for ((w = 0; w < k; w++)); do
      [ -n "${flushed[$w]:-}" ] && continue
      kill -0 "${pids[$w]}" 2>/dev/null && continue
      rc=0; wait "${pids[$w]}" || rc=$?
      printf '\n\033[36m▸ worker %s (%s)\033[0m\n' "$w" "$(pool_serial "$w")"
      cat "${logs[$w]}"
      [ "$rc" -ne 0 ] && warn "worker $w exited $rc"
      flushed[$w]=1; remaining=$((remaining - 1))
    done
  done
  rm -f "${logs[@]}"

  # An `if`, not `A && B`: under `set -e` a bare `[ -z "$want_preview" ] && …`
  # makes the whole function (and script) exit nonzero when NO_OPEN skips the
  # Preview, reporting failure on a clean run.
  if [ -n "$want_preview" ]; then preview_all "$per_locale"; fi
}

# Print the header block — every comment line after the shebang, up to the first
# line that isn't one. Reading the block rather than a fixed line range means
# growing the docs can't silently truncate --help. Keyed to BASH_SOURCE, not $0,
# so it still reads THIS file when the test sources it.
usage() { awk 'NR > 2 && /^#/ { sub(/^#+ ?/, ""); print; next } NR > 2 { exit }' "${BASH_SOURCE[0]}"; }

# Dispatch only when executed — store-screenshots-test.sh sources this file to
# exercise the pure helpers, and must not trigger a capture by doing so.
if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  case "${1:-}" in
    --top)        shift; cmd_top "$@";;
    --all-top)    shift; cmd_all_top "$@";;
    -h|--help|"") usage;;
    *)            cmd_capture "$@";;
  esac
fi
