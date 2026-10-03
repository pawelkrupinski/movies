#!/usr/bin/env bash
# ONE sbt for a convergence leg's job, instead of a cold one per step.
#
#   sbt-server.sh start <heap>            # boot it in the BACKGROUND, compiling worker/Fixtures
#   sbt-server.sh run <heap> <command>    # run <command> on it; exits with the command's status
#   sbt-server.sh classpath <key>         # print a classpath task's value, for a plain JVM
#   sbt-server.sh stop                    # end it, whatever it is running
#
# WHY. A leg ran up to three sbt JVMs one after another — the corpus capture, the sample, the
# suite — and each paid ~6 s of JVM and project load and ~6 s of task-graph and incremental-compile
# checks before its first line of work (run 37105119296): ~12 s a step, on every leg's critical
# path. `start` boots sbt's server (`sbt --client`) beside the tunnel and the corpus read, and the
# sample and suite then reach it warm through the thin client, which streams the server's output
# and exits with the command's own status — so `| tee` logs and step verdicts are unchanged.
#
# THE SERVER'S ENVIRONMENT IS THE STEP'S THAT STARTS IT, and stays so: a thin client forwards no
# environment, and every spec reads its settings from the process (`Env`). So `start` runs in a
# step carrying the suite's environment, and the corpus capture — the one step that holds the prod
# Mongo credential — never runs on the server: it asks the server for `classpath` and runs its own
# plain JVM, which takes the credential with it when it exits, as its sbt JVM did.
#
# AND IT OUTLIVES ITS STEPS: a step that times out kills its client, not the server, which goes on
# running the command. So the suite stops a server a red sample may have left busy before its own
# run, and the leg stops it (`stop`) before anything reads what it writes — the publish packs a
# tree the server would otherwise still be recording into.
set -uo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# On the server's command line, so `stop` can find it (SBT_OPTS reaches the server's JVM).
marker="-Dkinowo.leg-sbt=server"

opts() { echo "-Xmx${1:?heap} $marker"; }

# A client started while another is still BOOTING the server boots a second one; so every use
# waits for the background start, if there was one, before it connects. The start compiles only
# worker/Fixtures (all the corpus capture needs, and what its sbt compiled before): the e2e specs
# compile on the first `run`, as they did in the sample step, so a cold build cache never holds
# the corpus step — and the tunnel open beside it — for a whole-project compile.
await_start() {
  local out status
  out=$("$here/in-background.sh" wait sbt-server "${SBT_SERVER_START_SECONDS:-900}" 2>&1)
  status=$?
  [ "$status" -eq 2 ] && return 0          # never started in the background: the client boots one
  [ "$status" -ne 0 ] && printf '%s\n' "$out" >&2
  return "$status"
}

case "${1:-}" in
  start)
    heap="${2:?usage: sbt-server.sh start <heap>}"
    # sbt finds its server through this portfile, and the build cache restores `project/target`:
    # one saved while a leg's server ran points every later client at a server that is not there,
    # and Germany's corpus step hung its whole 10 minutes on it (run 37112719910). No server of
    # ours is running yet, so any portfile here is stale.
    pgrep -f -- "$marker" > /dev/null || rm -f "${SBT_PORTFILE:-project/target/active.json}"
    SBT_OPTS="$(opts "$heap")" "$here/in-background.sh" start sbt-server \
      sbt --client "${SBT_SERVER_WARM:-worker/Fixtures/compile}"
    ;;
  run)
    heap="${2:?usage: sbt-server.sh run <heap> <command>}"; shift 2
    await_start || echo "[sbt-server] the background start failed — running on a fresh server"
    SBT_OPTS="$(opts "$heap")" exec sbt --client "$*"
    ;;
  classpath)
    key="${2:?usage: sbt-server.sh classpath <key>}"
    # `export` prints the value bare, between the client's own `[info]`/`[success]` lines.
    value_in() { printf '%s\n' "$1" | sed -E 's/\x1b\[[0-9;]*[A-Za-z]//g' | grep -v '^\[' | grep -E '^[^ ]+$' | tail -1; }
    value=""
    # Bounded well inside the corpus step's 10 minutes, so a server that never answers falls back to
    # a sbt of our own below instead of timing the step out.
    if SBT_SERVER_START_SECONDS="${SBT_SERVER_CLASSPATH_WAIT_SECONDS:-240}" await_start; then
      out=$(timeout "${SBT_SERVER_CLIENT_SECONDS:-120}" sbt --client "export $key" 2>&1) && value=$(value_in "$out")
    else
      out="[sbt-server] the background start failed"
    fi
    # A server answer with no value in it used to end this script in silence: the parse's grep matched
    # nothing, `pipefail` made that exit 1, and the UK recording leg died at its corpus step with no
    # output (run 37110767992). Say what the server answered, and ask a sbt of our own instead: it
    # costs a JVM start, only when the server could not answer.
    if [ -z "$value" ]; then
      { echo "[sbt-server] no value for $key from the server; it answered:"; printf '%s\n' "$out"
        echo "[sbt-server] asking a fresh sbt instead"; } >&2
      out=$(sbt -batch -Dsbt.server.forcestart=true "export $key" 2>&1 < /dev/null) && value=$(value_in "$out")
    fi
    [ -n "$value" ] || { echo "[sbt-server] no value for $key from a fresh sbt either:" >&2; printf '%s\n' "$out" >&2; exit 1; }
    printf '%s\n' "$value"
    ;;
  stop)
    pkill -f -- "$marker" 2>/dev/null || exit 0
    for _ in $(seq 1 "${SBT_SERVER_STOP_SECONDS:-30}"); do
      pgrep -f -- "$marker" > /dev/null || exit 0
      sleep 1
    done
    pkill -9 -f -- "$marker" 2>/dev/null || true
    ;;
  *)
    echo "usage: $0 start <heap> | run <heap> <command> | classpath <key> | stop" >&2; exit 64 ;;
esac
