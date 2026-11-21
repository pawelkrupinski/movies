#!/usr/bin/env bash
# Assertions for prod-tunnel.sh's mongosh_at: it must stay a drop-in for `mongosh URI …` — the same
# process to kill when backgrounded, the same stdout. No vault, no prod.
#
#   scripts/local-mirror/mongosh-at-test.sh
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$HERE/../shell-spec.sh"

SCRATCH="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-mongosh-at.XXXXXX")"
trap 'rm -rf "$SCRATCH"' EXIT
FAKE="$SCRATCH/bin"; mkdir -p "$FAKE"
export PIDFILE="$SCRATCH/mongosh.pid"
# A long-running mongosh, like mirror.sh's tailer: records its own pid, then streams forever.
printf '#!/bin/sh\necho $$ > "$PIDFILE"\nexec sleep 30\n' > "$FAKE/mongosh"
chmod +x "$FAKE/mongosh"

echo "[spec] mongosh_at"
# mirror.sh backgrounds the tailer and later kills "$!". macOS's /bin/bash (3.2, what the launchd
# agent runs) forks a subshell for a backgrounded function, so "$!" must still be mongosh itself.
for shell in /bin/bash "$BASH"; do
  [ -x "$shell" ] || continue
  rm -f "$PIDFILE"
  # The shell under test backgrounds mongosh, tells us (the fifo) when to kill "$!", and waits for it to be gone.
  rm -f "$SCRATCH/go"; mkfifo "$SCRATCH/go"
  PATH="$FAKE:$PATH" "$shell" -c '
    . "'"$HERE"'/prod-tunnel.sh"
    "$MONGOSH_AT" "mongodb://127.0.0.1:1/x" --quiet --file tail.js &
    p=$!
    read -r _ < "'"$SCRATCH"'/go"
    kill "$p"; wait "$p" 2>/dev/null' &
  shell_pid=$!
  wait_until "mongosh recorded its pid" '[ -s "$PIDFILE" ]'
  echo go > "$SCRATCH/go"; wait "$shell_pid"
  child="$(cat "$PIDFILE" 2>/dev/null)"
  alive="$(kill -0 "$child" 2>/dev/null && echo alive || echo gone)"
  [ "$alive" = alive ] && kill "$child" 2>/dev/null
  check "killing \$! stops the backgrounded mongosh itself ($("$shell" -c 'echo $BASH_VERSION'))" "gone" "$alive"
done

# With a --file and no later --eval, mongosh prints the last --eval's value: the connect must print
# nothing, or identity-calibrate.sh's prod-<db>.jsonl gains a stray first line. Needs a real mongosh
# and the local Mongo (no credentials); skipped where they aren't.
if command -v mongosh >/dev/null && nc -z -w1 127.0.0.1 28017 2>/dev/null; then
  printf 'print("{\\"row\\":1}")\n' > "$SCRATCH/x.js"
  out="$(bash -c '. "'"$HERE"'/prod-tunnel.sh"; mongosh_at "mongodb://127.0.0.1:28017/kinowo_local?directConnection=true" --quiet --file "'"$SCRATCH"'/x.js"')"
  check "stdout is the script's own output, nothing from the connect" '{"row":1}' "$out"
else
  echo "  (skipped the stdout case: no mongosh or no local Mongo on :28017)"
fi

# Every mongosh it starts preloads no-app-registration.js, so a launch never checks in with macOS's window server as
# a GUI app (mongosh's process.title does that through LaunchServices; one launch every ~18 s moved the desktop focus).
printf '#!/bin/sh\nprintf "%%s" "$NODE_OPTIONS" > "$PIDFILE"\n' > "$FAKE/mongosh"
rm -f "$PIDFILE"
NODE_OPTIONS="--max-old-space-size=512" PATH="$FAKE:$PATH" bash -c '. "'"$HERE"'/prod-tunnel.sh"; mongosh_at "mongodb://127.0.0.1:1/x" --quiet --eval 1'
check "mongosh starts with the title guard preloaded, the caller's NODE_OPTIONS kept" \
  "--require $HERE/no-app-registration.js --max-old-space-size=512" "$(cat "$PIDFILE" 2>/dev/null)"
if command -v node >/dev/null; then
  check "...under which a title still reads back what was written" "mongosh x" \
    "$(node --require "$HERE/no-app-registration.js" -e 'process.title = "mongosh x"; console.log(process.title)')"
fi
# The check-in itself, where macOS can show it: a real mongosh against the local Mongo must leave no lsappinfo entry.
if command -v lsappinfo >/dev/null && command -v mongosh >/dev/null && nc -z -w1 127.0.0.1 28017 2>/dev/null; then
  bash -c '. "'"$HERE"'/prod-tunnel.sh"; exec "$MONGOSH_AT" "mongodb://127.0.0.1:28017/kinowo_local?directConnection=true" --quiet --eval "sleep(4000)"' >/dev/null 2>&1 &
  real=$!
  wait_until "the real mongosh is past its startup" 'ps -p "$real" -o args= | grep -q mongosh && sleep 2.5'
  check "a real mongosh started by mongosh_at registers no GUI app (lsappinfo)" "0" "$(lsappinfo list 2>/dev/null | grep -c "pid = $real ")"
  kill "$real" 2>/dev/null; wait "$real" 2>/dev/null
else
  echo "  (skipped the lsappinfo case: not macOS, no mongosh or no local Mongo on :28017)"
fi

spec_summary
