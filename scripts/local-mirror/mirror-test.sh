#!/usr/bin/env bash
# mirror.sh's paths mirror-resilience-test.sh does not drive: how the prod source URI is tuned, which databases are
# mirrored (configured or discovered), what the staleness gate's exit codes mean, a tailer whose resume token expired,
# the backoff a stream ending over and over earns, the file set the code-change watch digests, the no-server checks
# (fence, collection digest) running under node and never mongosh, and the startup body with no database to mirror. No Mongo, no ssh, no vault: mirror.sh is sourced for its functions, or run with fake
# `nc`/`mongosh` on PATH.
#
#   scripts/local-mirror/mirror-test.sh
set -uo pipefail
# the URIs these checks assert name the pinned port; the free-port pick has its own spec (prod-tunnel-test.sh)
export PROD_TUNNEL_PORT=27017
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/shell-spec.sh
. "$HERE/../shell-spec.sh"

printf '\033[36m▸\033[0m local-mirror/mirror.sh\n'
work="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-mirror-test.XXXXXX")"
trap 'rm -rf "$work"' EXIT
# No case reads the machine's own fences: a what-if running on the laptop fences its mirror under
# $HOME/.cache/kinowo/mirror-fence, and the staleness gate below read that one ("held by the what-if fence
# what-if-9177": "staleness.js exiting 3 re-seeds" got tail). Each case names its fences, or gets none.
export KINOWO_MIRROR_FENCE_DIR="$work/no-fences"

# in_mirror <command...> -> the command run in a subshell that sourced mirror.sh (its functions, no startup)
in_mirror() {
  (
    # shellcheck source=scripts/local-mirror/mirror.sh
    . "$HERE/mirror.sh"
    SRCZ=mongodb://source DST=mongodb://mirror
    "$@"
  )
}

# ── The source URI: compression and a selection timeout, each added once ────────────────────────────
srcz_of() { MONGODB_URI="$1" load_endpoints; echo "$SRCZ"; }
check "the tunnel URI gains zlib compression and a 10s selection timeout" \
  "mongodb://u:p@127.0.0.1:27017/kinowo?authSource=admin&directConnection=true&compressors=zlib&serverSelectionTimeoutMS=10000" \
  "$(in_mirror srcz_of "mongodb://u:p@10.20.0.13:27017/kinowo?authSource=admin")"
check "...neither is doubled when the URI already sets it" \
  "mongodb://127.0.0.1:27017/kinowo?compressors=snappy&serverSelectionTimeoutMS=500&directConnection=true" \
  "$(in_mirror srcz_of "mongodb://h/kinowo?compressors=snappy&serverSelectionTimeoutMS=500")"
no_uri() { unset MONGODB_URI; load_endpoints; }
check "no MONGODB_URI is refused (exit 1)" "1" "$(in_mirror no_uri >/dev/null 2>&1; echo $?)"

# ── Which databases ──────────────────────────────────────────────────────────────────────────────
discovered() {
  # shellcheck disable=SC2329 # invoked by discover_dbs
  mongosh_at() { printf 'Current Mongosh Log ID: x\nkinowo kinowo_uk kinowo_de\n'; }
  discover_dbs
}
check "KINOWO_MIRROR_DBS, comma-separated, names the databases" "kinowo kinowo_uk" \
  "$(KINOWO_MIRROR_DBS="kinowo,kinowo_uk" in_mirror discover_dbs)"
check "...unset, the tunnel's kinowo* databases are discovered (the listing's last line)" "kinowo kinowo_uk kinowo_de" \
  "$(KINOWO_MIRROR_DBS="" in_mirror discovered)"

# ── The staleness gate: 3 alone means re-seed ──────────────────────────────────────────────────────
gate() {
  GATE_EXIT=$1   # a global: needs_reseed's own `local code` would shadow a local of ours inside the stub
  # shellcheck disable=SC2329 # invoked by needs_reseed
  mongosh_at() { return "$GATE_EXIT"; }
  set -e
  if needs_reseed kinowo; then echo reseed; else echo tail; fi
}
# ...under a HOME whose machine holds a live what-if fence on that database, as the laptop's does while a what-if runs
machine="$work/machine-home"; mkdir -p "$machine/.cache/kinowo/mirror-fence/kinowo"
printf '%s run\n' "$(($(date +%s) + 600))" > "$machine/.cache/kinowo/mirror-fence/kinowo/what-if-machine"
check "staleness.js exiting 3 re-seeds, whatever fences the machine holds" "reseed" "$(HOME="$machine" in_mirror gate 3)"
check "...exiting 0 tails" "tail" "$(in_mirror gate 0)"
check "...a check that itself failed (exit 1) tails, never blocks the sync" "tail" "$(in_mirror gate 1)"

# ── supervise_db: a tailer exiting 2 re-seeds; one ending over and over backs off ───────────────────
# The fake tailer exits with $TAIL_EXIT; every case ends the subshell itself (reseed's exit, or the sleep log
# filling), and is bounded by the clock besides, so a regression can only FAIL, never hang.
cat > "$work/tailer" <<'FAKE'
#!/usr/bin/env bash
exit "${TAIL_EXIT:-1}"
FAKE
chmod +x "$work/tailer"
supervised() { # <case> -> runs supervise_db with the collaborators stubbed; prints what the case records
  local which=$1
  (
    # shellcheck source=scripts/local-mirror/mirror.sh
    . "$HERE/mirror.sh"
    # shellcheck disable=SC2329 # the stubs are invoked by supervise_db
    {
      ensure_local_mongo() { return 0; }
      tunnel_ready()  { return 0; }
      needs_reseed() { return 1; }
    }
    # shellcheck disable=SC2034 # read by the sourced supervise_db
    SRCZ=mongodb://source DST=mongodb://mirror MONGOSH_AT="$work/tailer" TARGETS_POLL=999 AUDIT_POLLS=1000
    case "$which" in
      expired)
        # shellcheck disable=SC2329
        reseed() { echo "reseeded $1" > "$work/expired"; exit 0; }
        # shellcheck disable=SC2329
        sleep() { [ "$1" = 999 ] && { command sleep 0.05; return 0; }; echo "backed off $1" > "$work/expired"; exit 0; }
        TAIL_EXIT=2 supervise_db kinowo_de 0 ;;
      backoff)
        # shellcheck disable=SC2329
        sleep() {
          [ "$1" = 999 ] && { command sleep 0.05; return 0; }
          echo "$1" >> "$work/backoffs"
          [ "$(wc -l < "$work/backoffs")" -ge 6 ] && exit 0
          return 0
        }
        TAIL_EXIT=1 supervise_db kinowo 0 > "$work/backoff.log" 2>&1 ;;
    esac
  ) > "$work/$which.out" 2>&1 &
  local child=$!
  wait_until "the supervised child ended on its own" '! kill -0 "$child" 2>/dev/null'
  kill "$child" 2>/dev/null; wait "$child" 2>/dev/null
}
export TAIL_EXIT
supervised expired
check "a supervisor with no readable published digest still tails (\"don't know\", not the end of it)" "1" \
  "$(grep -c 'resume token expired' "$work/expired.out" 2>/dev/null)"
check "a tailer whose resume token expired (exit 2) re-seeds at once, no backoff" "reseeded kinowo_de" "$(cat "$work/expired" 2>/dev/null)"
supervised backoff
check "a stream ending over and over backs off 2s, then 15s after four" "2 2 2 2 15 15" "$(tr '\n' ' ' < "$work/backoffs" 2>/dev/null | sed 's/ $//')"
check "...and says so only for the first three in a row" "3" "$(grep -c 'stream ended' "$work/backoff.log" 2>/dev/null)"

# ── The code-change watch digests every file mirror.sh runs, and only those ─────────────────────────
files="$(in_mirror runtime_files)"
missing=""
while IFS= read -r f; do [ -f "$f" ] || missing="$missing $f"; done <<< "$files"
check "every file the code digest names exists (no stale \$HERE reference)" "" "$missing"
check "...mirror.sh itself, its tailer and seed among them" "3" \
  "$(printf '%s\n' "$files" | grep -cE '/(mirror\.sh|tail\.js|seed\.js)$')"
check "...a spec is not among them, so editing one restarts nothing" "0" "$(printf '%s\n' "$files" | grep -c -- '-test\.sh$\|-spec\.js$')"

# ── The checks that need no server run under node, never mongosh ───────────────────────────────────
# Each mongosh start checks in with macOS's window server as a GUI app (no-app-registration.js), and the daemon ran
# these every minute per database: one every ~18 s, moving the owner's desktop focus (2026-11-21). A `mongosh` on PATH
# here records each start, so any no-server check that still reaches it shows up.
mkdir -p "$work/nodb-bin"
cat > "$work/nodb-bin/mongosh" <<FAKE
#!/bin/sh
echo "\$*" >> "$work/raw-mongosh"
exit 0
FAKE
chmod +x "$work/nodb-bin/mongosh"
: > "$work/raw-mongosh"
fences="$work/fences"
fence() { mkdir -p "$fences/$1"; printf '%s %s\n' "$3" "$4" > "$fences/$1/$2"; }   # fence <db> <holder> <expiry s> <kind>
holder_of() { PATH="$work/nodb-bin:$PATH" KINOWO_MIRROR_FENCE_DIR="$fences" in_mirror fence_holder "$1"; }
now_s="$(date +%s)"
if command -v node >/dev/null; then
  check "no fence directory: no holder" "" "$(holder_of kinowo_pl)"
  fence kinowo_pl what-if-1 $((now_s + 600)) run
  check "a run fence in force names its holder" "what-if-1" "$(holder_of kinowo_pl)"
  check "...for its own database only" "" "$(holder_of kinowo_de)"
  fence kinowo_de what-if-2 $((now_s - 5)) run
  check "an expired fence holds nothing" "" "$(holder_of kinowo_de)"
  fence kinowo_uk what-if-3 $((now_s + 600)) linger
  check "a linger fence in force names its holder" "what-if-3" "$(holder_of kinowo_uk)"
  fence kinowo_uk what-if-4 $((now_s + 600)) drain
  check "...but a drain lifts every linger (fence-rule.js, as tail.js reads it)" "" "$(holder_of kinowo_uk)"
  fence kinowo_fr what-if-5 "not-a-number" run
  check "a half-written fence file holds nothing" "" "$(holder_of kinowo_fr)"

  digest="$(PATH="$work/nodb-bin:$PATH" in_mirror targets_fingerprint)"
  expected_count="$(python3 -I -B -c 'import json,sys; print(len({c["name"] for c in json.load(open(sys.argv[1]))["collections"]}))' \
    "$HERE/../../common/src/main/resources/debug-mirror-collections.json")"
  check "the collection-list digest is <distinct names>-<8 hex>" "$expected_count" \
    "$(printf '%s\n' "$digest" | sed -nE 's/^([0-9]+)-[0-9a-f]{8}$/\1/p')"
  check "...the same value the rule gives under nodb.js for that list" \
    "$(node "$HERE/nodb.js" --file "$HERE/mirror-targets.js" --file "$HERE/targets-fingerprint.js")" "$digest"
  # A list that cannot be read is "don't know" (empty), never a digest: the copies' list file is malformed.
  mkdir -p "$work/broken/scripts/local-mirror" "$work/broken/common/src/main/resources"
  cp "$HERE/nodb.js" "$HERE/mirror-targets.js" "$HERE/targets-fingerprint.js" "$work/broken/scripts/local-mirror/"
  printf '{"collections": [' > "$work/broken/common/src/main/resources/debug-mirror-collections.json"
  broken_digest() { HERE="$work/broken/scripts/local-mirror"; targets_fingerprint; }
  check "...an unreadable list reads as nothing, never as a change" "" "$(PATH="$work/nodb-bin:$PATH" in_mirror broken_digest)"
else
  echo "  (skipped the fence and digest values: no node)"
fi
check "...and none of those reads started a mongosh" "" "$(cat "$work/raw-mongosh")"

# The supervisor's steady state: a tailer streaming, the audit due every poll (AUDIT_POLLS=1). Each audit asks the
# fence (node) and then the staleness check, which needs the server and goes through mongosh_at (stubbed here, as
# every database check is). Bounded: it is TERMed once it has audited enough, or fails at SPEC_WAIT_BOUND.
cat > "$work/streaming-tailer" <<'FAKE'
#!/usr/bin/env bash
exec sleep 30
FAKE
chmod +x "$work/streaming-tailer"
: > "$work/raw-mongosh"; : > "$work/db-checks"; rm -rf "$fences"
(
  # shellcheck source=scripts/local-mirror/mirror.sh
  . "$HERE/mirror.sh"
  # shellcheck disable=SC2329 # the stubs are invoked by supervise_db
  {
    ensure_local_mongo() { return 0; }
    tunnel_ready()  { return 0; }
    mongosh_at() { echo "db $*" >> "$work/db-checks"; return 0; }
  }
  # shellcheck disable=SC2034 # read by the sourced supervise_db
  SRCZ=mongodb://source DST=mongodb://mirror MONGOSH_AT="$work/streaming-tailer" TARGETS_POLL=0.05 AUDIT_POLLS=1 \
    FENCE_DIR="$fences"
  PATH="$work/nodb-bin:$PATH" supervise_db kinowo_pl 0
) > "$work/steady.log" 2>&1 &
steady=$!
wait_until "the supervisor audited a tailing mirror three times" '[ "$(grep -c staleness "$work/db-checks")" -ge 3 ]'
fence kinowo_pl what-if-9 $(($(date +%s) + 600)) run
if command -v node >/dev/null; then
  wait_until "the supervisor saw the what-if fence" 'grep -q "held by the what-if fence what-if-9" "$work/steady.log"'
fi
kill "$steady" 2>/dev/null; wait "$steady" 2>/dev/null
check "a tailing supervisor's audits start no mongosh for the fence (only the staleness check reaches the server)" "" \
  "$(cat "$work/raw-mongosh")"
check "...and the staleness check goes through mongosh_at, never bare mongosh" "0" \
  "$(grep -vc 'staleness.js' "$work/db-checks")"

# ── The startup body, with nothing to mirror ───────────────────────────────────────────────────────
# nc answers (the local Mongo and the tunnel are "up"); mongosh pings fine and lists no kinowo* database.
mkdir -p "$work/bin" "$work/home" "$work/tmp"
printf '#!/bin/sh\nexit 0\n' > "$work/bin/nc"
cat > "$work/bin/mongosh" <<'FAKE'
#!/usr/bin/env bash
case "$*" in *listDatabases*) echo "" ;; *) exit 0 ;; esac
FAKE
chmod +x "$work/bin/nc" "$work/bin/mongosh"
out="$(HOME="$work/home" TMPDIR="$work/tmp" PATH="$work/bin:$PATH" KINOWO_MIRROR_DBS="" \
  MONGODB_URI="mongodb://u:p@10.20.0.13:27017/kinowo" bash "$HERE/mirror.sh" 2>&1)"
code=$?
check "no kinowo* database through the tunnel ends the run (exit 1)" "1" "$code"
check "...naming the likely cause" "1" "$(printf '%s\n' "$out" | grep -c 'is MONGODB_URI right')"
check "...and leaves no temp file behind" "" "$(ls "$work/tmp")"

# ── A seed waits for a governor ticket (a starved laptop), then runs holding it ─────────────────────
spec_fake_governor "$work/agentctl"
export GOV_CALLS="$work/gov-calls"
# seeded <db> -> reseed with the spec's governor governing and mongosh standing in (it records whether it holds a ticket)
# shellcheck disable=SC2329 # invoked by in_mirror
seeded() {
  unset KINOWO_GOV_CPU_HELD KINOWO_GOV_CLASS KINOWO_JVM_SLOT_HELD
  export KINOWO_GOVERNOR=1 KINOWO_GOVERNOR_BIN="$work/agentctl" KINOWO_JVM_GATE_OS=Darwin AGENTCTL_GOV_DIR="$work/gov"
  # shellcheck disable=SC2329 # invoked by reseed
  mongosh_at() { echo "seed $*$([ -n "${KINOWO_JVM_SLOT_HELD:-}" ] && { : >&7; } 2>/dev/null && echo ' ticket')" >> "$work/seeds"; }
  reseed "$1"
}
: > "$GOV_CALLS"; : > "$work/seeds"
GOV_HOLD="$work/admit" in_mirror seeded kinowo_pl > /dev/null 2>&1 &
seeding=$!
wait_until "the seed asked the governor" '[ -e "$work/admit.waiting" ]'
check "a seed not admitted has not started" "0" "$(grep -c . "$work/seeds")"
touch "$work/admit"; wait "$seeding"
check "...admitted, it runs by itself, holding its ticket" "1" "$(grep -c "^seed mongodb://source .*SRC_DB='kinowo_pl'.*seed.js ticket$" "$work/seeds")"
check "...charged one core and 1 GB as mirror-seed" "1" \
  "$(grep -c -- "^gov admit .*--mem 1 --label mirror seed kinowo_pl .*--job mirror-seed --cores 1$" "$GOV_CALLS")"

spec_summary
