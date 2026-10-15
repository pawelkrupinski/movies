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
  PATH="$FAKE:$PATH" "$shell" -c '
    . "'"$HERE"'/prod-tunnel.sh"
    "$MONGOSH_AT" "mongodb://127.0.0.1:1/x" --quiet --file tail.js &
    p=$!
    for _ in 1 2 3 4 5 6 7 8 9 10; do [ -s "$PIDFILE" ] && break; sleep 0.1; done
    kill "$p"; wait "$p" 2>/dev/null; sleep 0.2'
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

spec_summary
