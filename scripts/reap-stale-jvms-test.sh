#!/usr/bin/env bash
# scripts/reap-stale-jvms.sh rule C against a fake process table (PS_COMMAND / CWD_COMMAND), --dry-run: no real process
# is looked at, signalled or killed.
# Run: bash scripts/reap-stale-jvms-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

scratch="$(mktemp -d)"; trap 'rm -rf "$scratch"' EXIT
mkdir -p "$scratch/live" "$scratch/final" "$scratch/shared"
export LOG_FILE="$scratch/log"

# pid ppid tty etime cputime args
SERVER='/usr/bin/java -jar /x/sbt-launch.jar --detach-stdio'
ONESHOT='/usr/bin/java -jar /x/sbt-launch.jar -batch test'
cat > "$scratch/ps" <<PS
 101 1 ?? 05:00:00 00:00:10 $SERVER
 102 1 ?? 02:00:00 00:00:10 $SERVER
 103 1 ?? 02:00:00 00:00:10 $ONESHOT
 104 1 ?? 03:00:00 00:00:10 $ONESHOT
 105 1 ?? 03:00:00 00:00:10 $SERVER
 106 1 ?? 03:00:00 00:00:10 $SERVER
PS
# 101 older + 102 newer server share live/; 103 (older) and 104 are one-shot runs sharing shared/ with no server named;
# 105 sits in a renamed .part dir (final/ exists); 106 in a removed dir
cat > "$scratch/cwd" <<CWD
101 $scratch/live
102 $scratch/live
103 $scratch/shared
104 $scratch/shared
105 $scratch/final.part
106 $scratch/removed
CWD
reaped() { PS_COMMAND="cat $scratch/ps" CWD_COMMAND="cat $scratch/cwd" PORT_LISTENER_PIDS="" \
  bash "$REPO_ROOT/scripts/reap-stale-jvms.sh" --dry-run | sed -n 's/^WOULD REAP pid=\([0-9]*\) .*rule=\([a-z-]*\).*/\1:\2/p' | sort | tr '\n' ' '; }

check "only the older duplicate server and the server of a removed dir are reaped" "101:dup-sbt-server 106:dup-sbt-gone-dir " "$(reaped)"
check "a one-shot sbt run sharing a cwd is never a candidate" "no" "$(reaped | grep -qE '10[34]:' && echo yes || echo no)"
check "a server whose .part dir was renamed to the final one is not 'gone'" "no" "$(reaped | grep -q '105:' && echo yes || echo no)"

# Rule B (hog) and scripts/sbt-warm's KEPT servers: a kept server compiles and tests for hours, so its lifetime CPU share
# passes the hog bar, and 2026-10-19 15:14 the reaper killed one mid-use (pid 37041, 1h42m, 35% of a core) -- a "vanished"
# server nothing on sbt-warm's side logged. Its keeper stops it once it is idle; while one lives, the server is spared.
mkdir -p "$scratch/kept" "$scratch/unkept"
cat > "$scratch/ps" <<PS
 201 1 ?? 02:00:00 00:40:00 $SERVER
 202 1 ?? 02:00:00 00:40:00 $SERVER
 203 1 ?? 02:00:00 00:40:00 $ONESHOT
PS
printf '201 %s\n202 %s\n203 %s\n' "$scratch/kept" "$scratch/unkept" "$scratch/kept" > "$scratch/cwd"
check "a hogging server whose checkout has a live sbt-warm keeper is spared; an unkept one, or a one-shot run, is not" \
  "202:hog 203:hog " "$(KEPT_COMMAND="echo $scratch/kept" reaped)"
spec_summary
