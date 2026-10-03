#!/usr/bin/env bash
# sbt-server.sh against a stub `sbt` that records how it was asked, and a stand-in server process.
# (A real one is exercised by every convergence leg; this pins the waiting, parsing and stopping.)
# Run: bash scripts/ci/sbt-server-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m sbt-server.sh\n'

work="$(mktemp -d)"
export RUNNER_TEMP="$work"
trap 'pkill -f "kinowo.leg-sbt=server" 2>/dev/null; rm -rf "$work"' EXIT
mkdir -p "$work/bin"
# The stub: a warm-up that takes two seconds, an `export` answered the way the thin client answers
# it, and any other command exiting 1 when it names `red`. Every call is logged with SBT_OPTS.
cat > "$work/bin/sbt" <<'STUB'
#!/usr/bin/env bash
mode=client
[ "$1" = "--client" ] && shift || { mode=fresh; while [ "${1#-}" != "$1" ]; do shift; done; }
echo "$(date +%s) start $1 [$SBT_OPTS] $mode" >> "$RUNNER_TEMP/calls"
case "$1" in
  warm*)   sleep 2 ;;
  export*)
    # SBT_STUB_SERVER_SAYS=nothing: the server answers without the value (what failed run 37110767992).
    if [ "$mode" = client ] && [ "${SBT_STUB_SERVER_SAYS:-}" = hang ]; then
      sleep 30
    elif [ "$mode" = client ] && [ "${SBT_STUB_SERVER_SAYS:-}" = nothing ]; then
      printf '[info] entering thin client - BEEP WHIRR\n[success] elapsed time: 0 s\n'
    else
      printf '[info] entering thin client - BEEP WHIRR\n/w/target/classes:/c/lib.jar\n[success] elapsed time: 0 s\n\033[0J'
    fi ;;
  *red*)   echo "$(date +%s) end $1" >> "$RUNNER_TEMP/calls"; exit 1 ;;
esac
echo "$(date +%s) end $1" >> "$RUNNER_TEMP/calls"
STUB
chmod +x "$work/bin/sbt"
export PATH="$work/bin:$PATH"
script="$REPO_ROOT/scripts/ci/sbt-server.sh"

SBT_SERVER_WARM=warm bash "$script" start 6g
check "start returns at once, the server booting behind it" "true" \
  "$(grep -qs 'end warm' "$work/calls" && echo false || echo true)"
bash "$script" run 6g suiteAlias > /dev/null
warm_end=$(awk '/end warm/{print $1}' "$work/calls"); suite_start=$(awk '/start suiteAlias/{print $1}' "$work/calls")
check "a run waits for the background start rather than booting a second server" "true" \
  "$([ -n "$warm_end" ] && [ "$suite_start" -ge "$warm_end" ] && echo true || echo false)"
check "the heap reaches the server's JVM, with the marker stop finds it by" "true" \
  "$(grep -q 'start warm \[-Xmx6g -Dkinowo.leg-sbt=server\]' "$work/calls" && echo true || echo false)"
bash "$script" run 6g redAlias > /dev/null
check "a run exits with its command's status" "1" "$?"
check "classpath prints the value bare, without the client's chatter" "/w/target/classes:/c/lib.jar" \
  "$(bash "$script" classpath worker/Fixtures/fullClasspath)"
value=$(SBT_STUB_SERVER_SAYS=nothing bash "$script" classpath worker/Fixtures/fullClasspath 2> "$work/cp.err")
check "a server answer without the value falls back to a fresh sbt" "/w/target/classes:/c/lib.jar" "$value"
check "...saying what the server answered, rather than failing in silence" "true" \
  "$(grep -q 'no value for worker/Fixtures/fullClasspath from the server' "$work/cp.err" && echo true || echo false)"
check "...and the fallback is a sbt of its own, not the server" "true" \
  "$(grep -q 'start export worker/Fixtures/fullClasspath .* fresh' "$work/calls" && echo true || echo false)"

began=$SECONDS
value=$(SBT_STUB_SERVER_SAYS=hang SBT_SERVER_CLIENT_SECONDS=2 bash "$script" classpath worker/Fixtures/fullClasspath 2> /dev/null)
check "a server that never answers is given up on, and a fresh sbt asked" "/w/target/classes:/c/lib.jar" "$value"
check "...within its bound, not after the server's own 30 s" "true" "$([ $((SECONDS - began)) -lt 15 ] && echo true || echo false)"

# A portfile the build cache restored names a server that is not running: start clears it.
mkdir -p "$work/project/target"; echo '{"uri":"local:///gone"}' > "$work/project/target/active.json"
SBT_PORTFILE="$work/project/target/active.json" SBT_SERVER_WARM=warm bash "$script" start 6g
check "start clears a stale portfile before it boots the server" "gone" \
  "$([ -e "$work/project/target/active.json" ] && echo present || echo gone)"
bash "$script" stop

# A server still running a timed-out step's command is stopped, and `stop` returns once it is gone.
bash -c 'exec -a "java -Dkinowo.leg-sbt=server stand-in" sleep 60' 2>/dev/null &
standin=$!
sleep 0.5
bash "$script" stop
wait "$standin" 2>/dev/null
check "stop ends the server, whatever it is running" "gone" "$(kill -0 "$standin" 2>/dev/null && echo alive || echo gone)"
bash "$script" stop
check "...and stopping no server is no error" "0" "$?"

spec_summary
