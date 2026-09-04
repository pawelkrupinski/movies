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
# EVERY lookup a capture depends on, for EVERY country — not a subset. A missing
# entry returns the empty string rather than failing, so the run reads
# "country never switched to " with a blank name and all ten cities are skipped;
# that is exactly how Spain's first capture attempt died. Adding a country means
# adding a row to each of these, and this loop is what says so.
for c in $COUNTRIES; do
  check "$c → locale → country round-trips" "$c" "$(locale_country "$(country_locale "$c")")"
  check "$c has a listing locale"      "1" "$([ -n "$(country_locale "$c")" ] && echo 1 || echo 0)"
  check "$c has an area-picker label"  "1" "$([ -n "$(showlist_label "$c")" ] && echo 1 || echo 0)"
  check "$c has a country pill name"   "1" "$([ -n "$(country_name "$c")" ] && echo 1 || echo 0)"
  check "$c has a gate header"         "1" "$([ -n "$(country_header "$c")" ] && echo 1 || echo 0)"
  check "$c has a backend base URL"    "1" "$([ -n "$(country_base "$c")" ] && echo 1 || echo 0)"
  check "$c has a listing marker"      "1" "$([ -n "$(listing_marker "$c")" ] && echo 1 || echo 0)"
done
check "unknown country has no locale" "" "$(country_locale "fr")"
check "unknown country has no pill name" "" "$(country_name "fr")"

# The labels are what the split-city detection greps for; a wrong one means
# London silently captures the area picker instead of the listing.
check "uk area-picker label" "Show listings"     "$(showlist_label uk)"
check "pl area-picker label" "Pokaż repertuar"   "$(showlist_label pl)"
check "de area-picker label" "Programm anzeigen" "$(showlist_label de)"
check "es area-picker label" "Ver la cartelera"  "$(showlist_label es)"

# These must match pl.kinowo.model.Country.displayName character for character —
# the capture taps the pill BY ITS TEXT.
check "pl country pill" "Polska"         "$(country_name pl)"
check "uk country pill" "United Kingdom" "$(country_name uk)"
check "de country pill" "Deutschland"    "$(country_name de)"
check "us country pill" "United States"  "$(country_name us)"
check "es country pill" "España"         "$(country_name es)"

# Must match R.string.all exactly — it is what proves the listing (not a film's
# detail) is on screen before the FILTERS icon is tapped. Those two icons share a
# position, so tapping too early opens the share sheet instead.
check "pl listing marker" "Wszystkie" "$(listing_marker pl)"
check "es listing marker" "Todo"      "$(listing_marker es)"
check "de listing marker" "Alle"      "$(listing_marker de)"

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

# ── a closed emulator is brought back ────────────────────────────────────────
# Closing an emulator window mid-run used to cost every REMAINING city of that
# country: adb had nothing to talk to, so each capture died in turn. The worker now
# re-boots its own instance on the SAME port, so $SERIAL keeps addressing it.
_bootcap="$(mktemp)"; _adbcap2="$(mktemp)"
boot_readonly() { printf 'boot %s\n' "$1" >> "$_bootcap"; }
adb()  { printf '%s\n' "$*" >> "$_adbcap2"; }
naps() { :; }
NOISE="$(mktemp)"
SERIAL="emulator-5556"
BOOTED_EMULATORS=""
POOL_APK="/tmp/app-debug.apk"

# Alive: nothing to do — a healthy emulator must not be rebooted between cities.
booted() { return 0; }
: > "$_bootcap"
revive_pool_device
check "a live emulator is left alone" "" "$(cat "$_bootcap")"

# Dead, then alive on the next poll: re-boot on the port taken from the SERIAL.
_polls=0
booted() { _polls=$((_polls + 1)); [ "$_polls" -gt 1 ]; }
: > "$_bootcap"; : > "$_adbcap2"
revive_pool_device >/dev/null 2>&1
check "a closed emulator is re-booted"            "boot 5556" "$(cat "$_bootcap")"
check "on the port its SERIAL already names"      "1" "$(grep -c '^boot 5556$' "$_bootcap")"
# A read-only instance boots from the pristine AVD image, so the app is gone.
check "the app is re-installed after the reboot"  "1" \
  "$(grep -c 'install -r /tmp/app-debug.apk' "$_adbcap2")"
# Shutting the revived instance down is boot_pool's business, not this function's:
# the revive runs inside the per-city subshell, where an assignment to
# BOOTED_EMULATORS would be thrown away. What must hold is that boot_pool records
# every pool serial in the MAIN shell UP FRONT, so cleanup covers a revived
# instance too (same serial). Asserting it here rather than in the revive is the
# difference between a test that describes production and one that only describes
# how the test called it.
check "boot_pool records each serial before booting it" "1" \
  "$(grep -c 'BOOTED_EMULATORS="\$BOOTED_EMULATORS \$serial"' "$HERE/store-screenshots.sh")"
check "the revive does not try to record it itself" "0" \
  "$(sed -n '/^revive_pool_device/,/^}/p' "$HERE/store-screenshots.sh" | grep -c 'BOOTED_EMULATORS=')"
# …and the capture path must actually call it. Without this the checks above pass
# while nothing invokes the revive, which is exactly the gap that let the original
# bug through.
check "the capture path revives its pool device" "1" \
  "$(grep -c 'then revive_pool_device; else ensure_emulator' "$HERE/store-screenshots.sh")"
unset -f booted boot_readonly adb naps
POOL_APK=""

# ── one run at a time ─────────────────────────────────────────────────────────
# Two concurrent runs sabotage each other: the pool opens with `adb kill-server`,
# `pkill -f qemu-system.*$AVD` and lock-file deletion, and both allocate ports from
# 5554 up, so the second kills the first's emulators and then both drive the same
# instance. Observed live as a "System isn't responding" dialog cycling every 5s.
# The lock is a DIRECTORY because mkdir is atomic (test-then-touch races) and macOS
# has no flock(1).
LOCK_ROOT="$(mktemp -d)"
acquire_lock testres
check "the first run takes the lock" "1" "$([ -d "$LOCK_ROOT/kinowo-screenshots-testres.lock" ] && echo 1 || echo 0)"
check "it records its own pid"       "$$" "$(cat "$LOCK_ROOT/kinowo-screenshots-testres.lock/pid")"

# A second run, while the holder is ALIVE, must refuse — and name the pid so the
# operator can act instead of guessing.
_second="$( ( acquire_lock testres ) 2>&1 || true )"
check "a second run is refused"        "1" "$(printf '%s' "$_second" | grep -c "already driving")"
check "the refusal names the holder"   "1" "$(printf '%s' "$_second" | grep -c "pid $$")"
check "a refused run exits nonzero"    "1" "$( ( acquire_lock testres ) >/dev/null 2>&1; echo $? )"

# Releasing lets the next run in.
release_lock
check "release frees the lock" "0" "$([ -d "$LOCK_ROOT/kinowo-screenshots-testres.lock" ] && echo 1 || echo 0)"
acquire_lock testres
check "the next run can take it" "1" "$([ -d "$LOCK_ROOT/kinowo-screenshots-testres.lock" ] && echo 1 || echo 0)"
release_lock

# A lock left behind by a KILLED run must not block forever — a dead pid is
# cleared and taken over, or every crash would need a manual rm.
mkdir -p "$LOCK_ROOT/kinowo-screenshots-testres.lock"
echo 999999 > "$LOCK_ROOT/kinowo-screenshots-testres.lock/pid"   # no such process
_stale="$( acquire_lock testres 2>&1 || true )"
check "a stale lock is taken over"     "$$" "$(cat "$LOCK_ROOT/kinowo-screenshots-testres.lock/pid")"
check "and taking it over is reported" "1"  "$(printf '%s' "$_stale" | grep -c 'stale')"
release_lock

# Two separate PROCESSES — the case that actually bites. The in-process checks
# above share a shell, so they say nothing about cross-process behaviour. Note what
# this does NOT prove: the two starts are a second apart, so a non-atomic
# test-then-create would pass it too — a genuinely simultaneous race is not
# reproducible from a shell test. mkdir is used because it IS atomic; this check
# covers the exclusion, not the atomicity. The holder is killed rather than
# released, which also leaves the stale lock the next check clears.
COMMON="$HERE/../../scripts/store-screenshots-common.sh"
LOCK_ROOT="$LOCK_ROOT" bash -c "source '$COMMON'; acquire_lock proc; sleep 5" >/dev/null 2>&1 &
_holder=$!
sleep 1                                            # let it take the lock
check "a second PROCESS is refused while one holds it" "1" \
  "$(LOCK_ROOT="$LOCK_ROOT" bash -c "source '$COMMON'; acquire_lock proc" >/dev/null 2>&1; echo $?)"
check "the holder is still alive (it was not clobbered)" "0" \
  "$(kill -0 $_holder 2>/dev/null; echo $?)"
kill $_holder 2>/dev/null || true; wait $_holder 2>/dev/null || true
# The killed holder left its lock behind; a fresh process must clear and take it.
check "the next process clears the killed holder's lock" "0" \
  "$(LOCK_ROOT="$LOCK_ROOT" bash -c "source '$COMMON'; acquire_lock proc" >/dev/null 2>&1; echo $?)"
rm -rf "$LOCK_ROOT/kinowo-screenshots-proc.lock"

# --top is a read-only ranking and must NOT be gated, or checking the list while a
# capture runs would fail for no reason.
check "--top does not take the lock" "0" \
  "$(grep -c 'acquire_lock.*cmd_top' "$HERE/store-screenshots.sh")"
check "every capture path does"      "3" \
  "$(grep -c 'acquire_lock android' "$HERE/store-screenshots.sh")"

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
check "usage documents the one-run-at-a-time rule" "1" "$(usage | grep -qi 'ONE capture run at a time' && echo 1 || echo 0)"
check "usage documents the emulator revive" "1" "$(usage | grep -qi 're-boots it on the same port' && echo 1 || echo 0)"
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

# ── the first-visit area picker (London and any future split city) ────────────
# A split city opens a dialog OVER the listing with all areas pre-ticked. Until it
# is dismissed every coordinate tap lands on the dialog, so this used to capture
# the picker as screenshot 1. Whether to expect one comes from the backend
# (/<slug>/api/cinemas → areas), not a hardcoded city name.
_tapcap="$(mktemp)"; _waitcap="$(mktemp)"
tap()  { printf 'tap %s\n' "$*" >> "$_tapcap"; }
naps() { :; }
wait_text() { printf '%s\n' "$1" >> "$_waitcap"; echo "500 900"; }
ui_xml()    { :; }                               # dialog already gone

# A flat city must not wait for a dialog that is never coming — the old blind
# 12s probe was paid by every single city.
city_area_count() { echo 0; }
: > "$_tapcap"; : > "$_waitcap"
dismiss_area_picker uk Manchester 30
check "a flat city never waits for a picker" "" "$(cat "$_waitcap")"
check "a flat city taps nothing"             "" "$(cat "$_tapcap")"

# A split city taps its confirm button, found by the LOCALIZED label.
city_area_count() { echo 5; }
: > "$_tapcap"; : > "$_waitcap"
dismiss_area_picker uk London 30
check "a split city waits for the localized confirm" "Show listings" "$(cat "$_waitcap")"
check "a split city taps confirm"                   "tap 500 900"   "$(cat "$_tapcap")"
: > "$_tapcap"; : > "$_waitcap"
dismiss_area_picker de Berlin 30
check "the German label is used for de" "Programm anzeigen" "$(cat "$_waitcap")"

# Split per the backend but no picker on screen = we are on the wrong screen, and
# capturing would silently produce the wrong shots. Must fail the city loudly.
wait_text() { return 1; }
check "a split city with no picker fails loudly" "1" \
  "$( ( dismiss_area_picker uk London 1 ) >/dev/null 2>&1; echo $? )"
# …but an unreachable backend (-1) must NOT fail the city: unknown is not "split".
city_area_count() { echo -1; }
check "an unreachable catalog does not fail the city" "0" \
  "$( ( dismiss_area_picker uk London 1 ) >/dev/null 2>&1; echo $? )"

# A confirm tap that misses leaves the dialog up; retry once, then fail rather
# than shoot the dialog. node_center is REAL here — ui_xml feeds it a node that
# still reads the confirm label, so the "still up" detection is genuine.
city_area_count() { echo 5; }
wait_text() { echo "500 900"; }
ui_xml() { echo '<node text="Show listings" bounds="[400,880][600,920]" />'; }
: > "$_tapcap"
check "a picker that will not dismiss fails loudly" "1" \
  "$( ( dismiss_area_picker uk London 1 ) >/dev/null 2>&1; echo $? )"
check "it retried the tap before giving up" "2" "$(grep -c '^tap ' "$_tapcap")"
unset -f tap naps wait_text ui_xml city_area_count

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

# Android 14's opportunistic Private DNS (DNS-over-TLS) hangs behind a VPN on the
# host, so every lookup fails and the capture silently writes screenshots of the
# app's "could not load" state. The emulators boot -read-only, so the setting
# cannot persist in the AVD and MUST be reapplied on each boot — if this call
# goes missing, the run still exits 0 and the damage only shows up in the images.
# uiautomator sometimes returns the window frame with an EMPTY Compose semantics
# tree (~2KB vs ~20KB). Every caller reads that as "element absent", so one bad
# sample marks a city unreachable. ui_xml must retry rather than trust it.
printf '\033[36m▸\033[0m an empty semantics dump is retried\n'
# An earlier case unset ui_xml; re-source to get the real one back.
source "$HERE/store-screenshots.sh"
_uicap="$(mktemp)"
NOISE="$(mktemp)"
naps() { :; }
# First read is the frame-only dump; the second carries real text.
adb() {
  case "$*" in
    *"cat /sdcard/kinowo-ui.xml")
      printf '%s\n' "$*" >> "$_uicap"
      if [ "$(grep -c cat "$_uicap")" -ge 2 ]; then
        printf '<hierarchy><node text="Polska" bounds="[1,2][3,4]"/></hierarchy>'
      else
        printf '<hierarchy><node text="" bounds="[0,0][1,1]"/></hierarchy>'
      fi ;;
    *) printf '%s\n' "$*" >> "$_uicap" ;;
  esac
}
: > "$_uicap"
_x="$(ui_xml)"
check "it retries past the empty dump" "1" \
  "$(printf '%s' "$_x" | grep -c 'text="Polska"')"
check "it dumped more than once" "1" \
  "$([ "$(grep -c 'uiautomator dump' "$_uicap")" -ge 2 ] && echo 1 || echo 0)"

# `--country-top es` narrows COUNTRIES to "es". The gate check must still accept
# ANY country's pill — reading COUNTRIES there means waiting for España, which is
# off-screen, so a perfectly drawn gate times out as "never appeared".
printf '\033[36m▸\033[0m the gate is recognised under --country-top\n'
source "$HERE/store-screenshots.sh"
NOISE="$(mktemp)"
naps() { :; }
COUNTRIES="es"
ui_xml() { printf '<hierarchy><node text="Polska" bounds="[144,204][276,264]"/></hierarchy>'; }
if wait_gate 6; then _g=0; else _g=1; fi
check "a Polish pill still proves the gate drew" "0" "$_g"

# The country pill row scrolls, and uiautomator reports only what is on screen.
# Spain is the FIFTH country, so on a fresh gate it is off-screen and looks
# missing — the capture used to blame the gate and skip every city.
printf '\033[36m▸\033[0m the country row is scrolled to find a pill\n'
_scrollcap="$(mktemp)"
NOISE="$(mktemp)"
# Narrowed the way `--country-top es` leaves it. The anchor lookup must NOT read
# COUNTRIES: under --country-top that is just "es", so it would hunt the very pill
# it is trying to scroll into view, find nothing, and return without swiping.
COUNTRIES="es"
ui_xml() { printf 'x'; }
# Spain becomes visible only once a swipe has been recorded — the capture file is
# the state, so it survives the subshells `$( ui_xml | node_center )` runs in.
node_center() {
  cat >/dev/null
  case "$1" in
    Polska) echo "200 234" ;;                      # an anchor pill, always visible
    España) [ -s "$_scrollcap" ] && echo "900 234" || echo "" ;;
    *) echo "" ;;
  esac
}
# `wm size` reports WIDTHxHEIGHT; the swipe must stay inside the WIDTH. Reading
# the last field gave the height (2992 on a 1344-wide portrait screen), so every
# swipe began off-screen and scrolled nothing while the run looked busy.
adb() {
  printf '%s\n' "$*" >> "$_scrollcap"
  case "$*" in "shell wm size") printf 'Physical size: 1344x2992\n';; esac
}
naps() { :; }
: > "$_scrollcap"
_point="$(scroll_to_country "España" || true)"
check "it finds the pill once scrolled into view" "900 234" "$_point"
check "it swiped the row at the anchor pill's Y" "1" \
  "$(grep -c 'shell input swipe .* 234 .* 234 300' "$_scrollcap" || true)"
# Both X coordinates must land inside the 1344-wide screen — not the 2992 height.
check "the swipe stays within the screen WIDTH" "" \
  "$(awk '/shell input swipe/ { if ($4 >= 1344 || $6 >= 1344) print "off-screen: "$4" -> "$6 }' "$_scrollcap")"

# The row is filled from /api/catalog, which lands AFTER the first pill renders.
# A pill that only appears on the ninth pass must still be found — a fixed handful
# of swipes used to give up against the bundled three and call Spain missing.
: > "$_scrollcap"
_late=0
node_center() {
  cat >/dev/null
  case "$1" in
    Polska) echo "200 234" ;;
    España) _late=$((_late + 1)); [ "$(grep -c swipe "$_scrollcap")" -ge 9 ] && echo "900 234" || echo "" ;;
    *) echo "" ;;
  esac
}
_point="$(scroll_to_country "España" || true)"
check "it keeps scrolling while the catalog is still loading" "900 234" "$_point"

# A country the app does not carry must FAIL, not quietly return an empty point
# that `tap` would then aim at nothing.
node_center() { cat >/dev/null; case "$1" in Polska) echo "200 234";; *) echo "";; esac; }
: > "$_scrollcap"
if scroll_to_country "Nowhere" >/dev/null 2>&1; then _rc=0; else _rc=1; fi
check "an absent country fails instead of returning empty" "1" "$_rc"

printf '\033[36m▸\033[0m private DNS is disabled on boot\n'
_dnscap="$(mktemp)"
adb() { printf '%s\n' "$*" >> "$_dnscap"; }
NOISE="$(mktemp)"
: > "$_dnscap"
disable_private_dns
check "disable_private_dns turns it off" \
  "shell settings put global private_dns_mode off" "$(cat "$_dnscap")"
: > "$_dnscap"
disable_private_dns "emulator-5558"
check "it targets the given serial" \
  "-s emulator-5558 shell settings put global private_dns_mode off" "$(cat "$_dnscap")"

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
