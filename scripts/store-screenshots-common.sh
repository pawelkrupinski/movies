#!/usr/bin/env bash
#
# Shared engine for the store-screenshot drivers — android/scripts and
# ios/scripts both source this. Everything here is platform-agnostic: which
# countries exist, how cities are ranked, whether a city is split, where shots
# land and what they're numbered. The platform scripts keep only what genuinely
# differs — an emulator pool driven by taps, or simulators driven by deep links.
#
# Sourced, never executed. Callers must set before sourcing (or immediately
# after): LISTINGS (the per-locale listing root) and, if they shoot anything
# other than phones, SHOT_CLASS.

# ── output ────────────────────────────────────────────────────────────────────
say()   { printf '\033[36m▸\033[0m %s\n' "$*"; }
step()  { printf '  %s… ' "$*"; }
done_() { printf '\033[32m✓\033[0m\n'; }
ok()    { printf '\033[32m✓\033[0m %s\n' "$*"; }
warn()  { printf '\033[33m!\033[0m %s\n' "$*" >&2; }
# $NOISE is optional: the Android driver funnels adb/gradle chatter into one, the
# iOS driver has nothing that noisy. Tail it only when the caller kept one.
die()   { printf '\033[31m✗\033[0m %s\n' "$*" >&2
          [ -n "${NOISE:-}" ] && [ -s "$NOISE" ] && tail -5 "$NOISE" >&2
          exit 1; }
naps()  { command sleep "$1"; }

# ── one run at a time ─────────────────────────────────────────────────────────
# These drivers are actively hostile to a second concurrent run of themselves, and
# used to say nothing about it. The Android pool opens by doing `adb kill-server`,
# `pkill -f qemu-system.*$AVD` and deleting the AVD's lock files, and both drivers
# allocate device slots from the same end — so run two at once and the second
# kills the first's devices, pulls the adb server out from under it, and then both
# drive the same instance, sending each other's taps and `pm clear`s into one app.
# Observed live: 3 emulators, one showing "System isn't responding" every 5s, one
# country's shots landing while another's silently stopped.
#
# A directory is the lock: mkdir is atomic on POSIX, unlike test-then-touch, and
# macOS has no flock(1). The holder's pid goes inside so a lock left by a killed
# run can be told from a live one and cleared instead of blocking forever.
LOCK_ROOT="${LOCK_ROOT:-${TMPDIR:-/tmp}}"
LOCK_DIR=""

acquire_lock() { # $1 resource name (the device set this driver owns)
  local dir="$LOCK_ROOT/kinowo-screenshots-$1.lock" holder
  if mkdir "$dir" 2>/dev/null; then
    LOCK_DIR="$dir"; echo "$$" > "$dir/pid"; return 0
  fi
  holder="$(cat "$dir/pid" 2>/dev/null || true)"
  if [ -n "$holder" ] && kill -0 "$holder" 2>/dev/null; then
    die "another store-screenshots run (pid $holder) is already driving the $1 devices.
   Two runs sabotage each other — wait for it, or stop it first:  kill $holder"
  fi
  # No live holder: a previous run was killed before it could release.
  warn "clearing a stale $1 lock left by pid ${holder:-unknown}"
  rm -rf "$dir"
  mkdir "$dir" 2>/dev/null || die "could not take the $1 lock at $dir"
  LOCK_DIR="$dir"; echo "$$" > "$dir/pid"
}

# Released from the EXIT trap, and only by the shell that took it — worker
# subshells clear MAIN_SHELL precisely so they don't tidy up the parent's things.
release_lock() { [ -n "$LOCK_DIR" ] && rm -rf "$LOCK_DIR"; LOCK_DIR=""; }

# ── countries ─────────────────────────────────────────────────────────────────
# Every country --all-top walks. A country is one deployment + one locale + one
# UI language, so this list is the single place a new country is added.
COUNTRIES="${COUNTRIES:-pl uk de}"
locale_country() { case "$1" in en-GB) echo uk;; pl-PL) echo pl;; de-DE) echo de;; *) echo "";; esac; }
country_locale() { case "$1" in pl) echo pl-PL;; uk) echo en-GB;; de) echo de-DE;; *) echo "";; esac; }
# The UI language a country forces, mirroring `Country.languageCode` in the apps.
# NOT derived from the locale: the store locale is the listing's (en-GB), the UI
# language is the bundle's (en), and only the second is what a launch pins.
country_language() { case "$1" in pl) echo pl;; uk) echo en;; de) echo de;; *) echo "";; esac; }
# ENDONYMS from the catalog — identical in every locale, so they double as a tap
# target regardless of the app's language.
country_name()   { case "$1" in pl) echo Polska;; uk) echo "United Kingdom";; de) echo Deutschland;; esac; }
country_base()   { case "$1" in pl) echo "https://kinowo.net";; uk) echo "https://uk.showtimes.cc";; de) echo "https://de.showtimes.cc";; *) echo "";; esac; }
# The gate's "Country" header, which IS localized — seeing it is proof the
# country switch landed.
country_header() { case "$1" in pl) echo Kraj;; uk) echo Country;; de) echo Land;; esac; }
# The area picker's confirm button, shown only by SPLIT cities.
showlist_label() { case "$1" in pl) echo "Pokaż repertuar";; uk) echo "Show listings";; de) echo "Programm anzeigen";; esac; }

# ── where shots land ──────────────────────────────────────────────────────────
# The published directory name is dictated by gradle-play-publisher's
# ImageType.dirName (`phone-screenshots`, `tablet-screenshots`, …) — the iOS side
# mirrors it so both stores read the same shape.
SHOT_CLASS="${SHOT_CLASS:-phone-screenshots}"

# A candidates/ scratchpad INSIDE the published dir. Captures are raw material —
# a blank list, a detail that hadn't enriched, a city that turned out dull — so
# they never land straight on what the store serves; promoting is a deliberate
# `mv` up one level. Safe by construction on Android: GPP includes
# `/listings/*/graphics/<dirName>/*`, a SINGLE path segment, so nothing nested
# here can be published by accident.
candidates_dir() { # $1 locale
  echo "$LISTINGS/$1/graphics/$SHOT_CLASS/candidates"
}

# How many files one city's capture writes. The two drivers differ — Android
# shoots four screens, iOS five (it adds the Filtry sheet) — so a driver that
# isn't on four sets this before sourcing, exactly like SHOT_CLASS.
SHOTS_PER_CITY="${SHOTS_PER_CITY:-4}"

# The files one capture writes, in screen order. Zero-padded to three digits
# so LEXICAL order matches NUMERIC order: 010 sorts after 009, where 10 sorted
# after 1. That matters beyond tidiness — anything globbing the directory (a file
# browser, `ls`, an uploader's ordering) would otherwise present them wrongly.
shot_paths() { # $1 dir, $2 number of the first file
  local n; for ((n = 0; n < SHOTS_PER_CITY; n++)); do printf '%s/%03d.png\n' "$1" "$(($2 + n))"; done
}

# The number a fresh block starts at: one past the highest N.png already in $1,
# or 1 when the dir is empty or missing. This is what makes runs APPEND instead
# of overwrite. Compared numerically, not lexically — a dir holding 9 and 10 must
# continue at 11, and `ls | tail -1` would say 10.
next_shot_number() { # $1 dir
  local f n last=0
  for f in "$1"/*.png; do
    [ -e "$f" ] || continue                    # no match → the glob itself
    n="${f##*/}"; n="${n%.png}"
    case "$n" in ''|*[!0-9]*) continue;; esac  # ignore promoted/renamed strays
    # `10#` forces base 10: bash reads a leading-zero literal as OCTAL, so a
    # zero-padded 008/009 would abort the script with "value too great for base".
    n=$((10#$n))
    [ "$n" -gt "$last" ] && last="$n"
  done
  echo "$((last + 1))"
}

# Where each locale's candidates dir ENDS before a run — "pl-PL:5 en-GB:1".
# Captured up front so the Preview at the end shows just this run's shots: now
# that runs append, "everything in the dir" is the wrong set.
baselines() {
  local country locale out=""
  for country in $COUNTRIES; do
    locale="$(country_locale "$country")"
    out="$out $locale:$(next_shot_number "$(candidates_dir "$locale")")"
  done
  echo "${out# }"
}

# A locale's baseline out of that string, defaulting to 1 for a locale the run
# didn't touch — a missing entry shows everything rather than nothing.
baseline_for() { # $1 baselines, $2 locale
  local pair
  for pair in $1; do
    case "$pair" in "$2":*) echo "${pair#*:}"; return;; esac
  done
  echo 1
}

# ── work splitting ────────────────────────────────────────────────────────────
# Countries worker W handles when K workers share the list, round-robin: worker W
# takes indices W, W+K, W+2K… So K≥#countries gives each its own worker, and a
# smaller K packs the remainder onto earlier workers, run sequentially there.
worker_slice() { # $1 country list, $2 K, $3 W
  local list="$1" k="$2" w="$3" i=0 c
  for c in $list; do [ $((i % k)) -eq "$w" ] && printf '%s ' "$c"; i=$((i + 1)); done
}

# Clamp a requested worker count to [1, #countries] — more workers than countries
# would leave the extras idle.
effective_k() { # $1 requested, $2 country count
  local req="$1" max="$2"
  { [ "$req" -ge 1 ]; } 2>/dev/null || req=1
  [ "$req" -le "$max" ] || req="$max"
  echo "$req"
}

# ── the backend ───────────────────────────────────────────────────────────────
# Rank a country's cities by live film count. Prints the city total on the first
# line, then N × "films<TAB>slug<TAB>name", best first — a shape both the human
# --top table and the capture loops read, so the ranking exists once. OFFSET is a
# 1-based rank to start the slice at, so (N=2, OFFSET=4) returns the 4th and 5th
# best. Every city is counted either way — the ranking has to be complete before
# it can be sliced — but the fetch is concurrent, so the whole thing costs about
# one request's wall-clock rather than one per city.
rank_cities() { # $1 country, $2 N, $3 optional 1-based start rank
  local country="$1" n="$2" off="${3:-1}" base; base="$(country_base "$country")"
  [ -n "$base" ] || die "unknown country '$country' (use pl | uk | de)"
  BASE="$base" COUNTRY="$country" TOPN="$n" OFFSET="$off" python3 - <<'PY'
import os, json, time, urllib.request, concurrent.futures as cf
base, country, n = os.environ["BASE"], os.environ["COUNTRY"], int(os.environ["TOPN"])
off = int(os.environ.get("OFFSET", "1"))
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
for films, slug, name in rows[off - 1: off - 1 + n]:
    print(f"{films}\t{slug}\t{name}")
PY
}

# How many AREAS the app will offer for this city. Non-zero means it is a SPLIT
# city and opens a first-visit area picker over the listing. `/api/catalog`
# resolves whatever the caller typed (name or slug, diacritics or not) to a slug;
# the split itself lives in `/<slug>/api/cinemas`, the endpoint both apps read.
# London is 5 today (Central/North/East/South/West); everything else is 0.
#
# Asking the backend beats hardcoding "London": a city that becomes split needs
# no change here, one that stops being split stops being special-cased, and every
# country is covered without a per-country list. Prints -1 if the lookup fails,
# which callers treat as "might be split" rather than guessing either way.
city_area_count() { # $1 country, $2 city name or slug
  BASE="$(country_base "$1")" CITY="$2" python3 - <<'PY'
import os, json, urllib.request, unicodedata
def fold(s):
    s = s.replace("ł", "l").replace("Ł", "L")
    return "".join(c for c in unicodedata.normalize("NFD", s)
                   if not unicodedata.combining(c)).casefold()
def get(url):
    with urllib.request.urlopen(url, timeout=30) as r: return json.loads(r.read())
base, want = os.environ["BASE"], fold(os.environ["CITY"])
try:
    slug = next((c["slug"] for c in get(f"{base}/api/catalog")["cities"]
                 if want in (fold(c["name"]), fold(c["slug"]))), None)
    print(len(get(f"{base}/{slug}/api/cinemas").get("areas") or []) if slug else -1)
except Exception:
    print(-1)
PY
}

# The city SLUG for whatever the caller typed — deep links and API paths need it,
# and a human types "Poznan" or "Wrocław", not "poznan"/"wroclaw".
city_slug() { # $1 country, $2 city name or slug
  BASE="$(country_base "$1")" CITY="$2" python3 - <<'PY'
import os, json, urllib.request, unicodedata
def fold(s):
    s = s.replace("ł", "l").replace("Ł", "L")
    return "".join(c for c in unicodedata.normalize("NFD", s)
                   if not unicodedata.combining(c)).casefold()
base, want = os.environ["BASE"], fold(os.environ["CITY"])
try:
    with urllib.request.urlopen(f"{base}/api/catalog", timeout=30) as r:
        cities = json.loads(r.read())["cities"]
    print(next((c["slug"] for c in cities
                if want in (fold(c["name"]), fold(c["slug"]))), ""))
except Exception:
    print("")
PY
}

# ── the human-readable ranking ────────────────────────────────────────────────
cmd_top() { # $1 country, $2 N, $3 optional 1-based start rank — a readable table
  local country="$1" n="${2:-10}" off="${3:-1}" ranked
  { [ "$off" -ge 1 ]; } 2>/dev/null || die "--top's start rank is 1-based, e.g. --top uk 5 11"
  ranked="$(rank_cities "$country" "$n" "$off")"
  printf '%s %s cities from rank %s by live film count (%s total):\n' \
    "$n" "$country" "$off" "$(printf '%s\n' "$ranked" | head -1)"
  # Number each row with its ABSOLUTE rank, not 1..N — with an offset in play,
  # "4." is the whole point and a bare list would hide which slice you got.
  printf '%s\n' "$ranked" | tail -n +2 | { rank="$off"
    while IFS=$'\t' read -r films slug name; do
      printf '  %3s. %4s  %-22s %s\n' "$rank" "$films" "$slug" "$name"; rank=$((rank + 1))
    done; }
}

# Print a driver's header block — every comment line after the shebang, up to the
# first line that isn't one. Reading the block rather than a fixed line range
# means growing the docs can't silently truncate --help. The caller passes its own
# path, since BASH_SOURCE here is THIS file.
usage_of() { # $1 script path
  awk 'NR > 2 && /^#/ { sub(/^#+ ?/, ""); print; next } NR > 2 { exit }' "$1"
}
