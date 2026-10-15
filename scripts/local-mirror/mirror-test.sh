#!/usr/bin/env bash
# mirror.sh's paths mirror-resilience-test.sh does not drive: how the prod source URI is tuned, which databases are
# mirrored (configured or discovered), what the staleness gate's exit codes mean, a tailer whose resume token expired,
# the backoff a stream ending over and over earns, the file set the code-change watch digests, and the startup body
# with no database to mirror. No Mongo, no ssh, no vault: mirror.sh is sourced for its functions, or run with fake
# `nc`/`mongosh` on PATH.
#
#   scripts/local-mirror/mirror-test.sh
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=scripts/shell-spec.sh
. "$HERE/../shell-spec.sh"

printf '\033[36m▸\033[0m local-mirror/mirror.sh\n'
work="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-mirror-test.XXXXXX")"
trap 'rm -rf "$work"' EXIT

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
check "staleness.js exiting 3 re-seeds" "reseed" "$(in_mirror gate 3)"
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
      ensure_tunnel() { return 0; }
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
  local child=$! deadline=$((SECONDS + 20))
  while kill -0 "$child" 2>/dev/null && [ "$SECONDS" -lt "$deadline" ]; do command sleep 0.1; done
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

spec_summary
