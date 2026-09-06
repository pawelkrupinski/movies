#!/usr/bin/env bash
#
# Boot FixtureServerMain in the background, wait for the port it writes, and
# RELAUNCH it if it exits before writing one.
#
#   fixture-server.sh launch <state-dir> <command...>   start it, return at once
#   fixture-server.sh await  <state-dir>                block until <state-dir>/port exists
#   fixture-server.sh stop   <state-dir>                kill whichever launch is current
#
# `launch` and `await` are separate so a row can start the ~100s sbt boot first
# and install Node + browsers while it runs. Everything the two share lives in
# <state-dir>: `cmd` (the command, one argument per line), `pid` (the CURRENT
# launch — a relaunch replaces it, which is why `stop` reads it rather than a
# `$!` captured at launch time), `attempt`, `log` (appended across launches, so a
# failure prints every attempt) and `port`, which the command itself must write —
# pass `<state-dir>/port` into it.
#
# The relaunch is the point. On 2026-09-06 one WebKit shard's sbt launcher hit a
# `Connection timed out` fetching a boot jar from Maven Central — a blip its 19
# sibling shards, fetching the same jar at the same moment, did not see — and
# exited within seconds. The inline wait loop this replaced only ever looked for
# the port file, so it sat out its whole 600s ceiling on a process that was
# already dead, then failed the run. Now a launch that exits without a port is
# noticed on the next poll and started again; a transient blip costs one boot, and
# a server that genuinely cannot start still fails, after FIXTURE_BOOT_ATTEMPTS
# launches or at the ceiling, whichever is first. The ceiling is wall-clock from
# `await`, never extended by a relaunch, so a deterministic failure such as a
# compile error is bounded exactly as it was.
#
# A shell FILE rather than an inline `run:` block so FixtureServerBootSpec can
# run it against a fake command that dies — the same shape
# `.github/actions/changed-paths/matches.sh` has, for the same reason. No
# `mapfile`, no `wait -n`: the spec runs it on macOS, where /bin/bash is 3.2.
set -euo pipefail

FIXTURE_BOOT_CEILING_SECONDS=${FIXTURE_BOOT_CEILING_SECONDS:-600}
FIXTURE_BOOT_ATTEMPTS=${FIXTURE_BOOT_ATTEMPTS:-3}
FIXTURE_POLL_SECONDS=${FIXTURE_POLL_SECONDS:-2}

mode=${1:?usage: fixture-server.sh launch|await|stop <state-dir> [command...]}
state=${2:?usage: fixture-server.sh launch|await|stop <state-dir> [command...]}
shift 2

# Started from a subshell so the server is reparented to init the moment this
# returns and a dead one is reaped there — a zombie child of THIS shell would
# still answer `kill -0`, and `await` would wait on a corpse.
start() {
    echo "==> launch $(cat "$state/attempt"): $*" >>"$state/log"
    (nohup "$@" >>"$state/log" 2>&1 </dev/null & echo $! >"$state/pid")
}

alive() {
    kill -0 "$(cat "$state/pid")" 2>/dev/null
}

case "$mode" in
    launch)
        [ $# -gt 0 ] || { echo "fixture-server.sh launch: no command given" >&2; exit 2; }
        mkdir -p "$state"
        rm -f "$state/port" "$state/log" "$state/pid"
        printf '%s\n' "$@" >"$state/cmd"
        echo 1 >"$state/attempt"
        start "$@"
        ;;

    await)
        [ -f "$state/cmd" ] || { echo "fixture-server.sh await: nothing launched in $state" >&2; exit 2; }
        cmd=()
        while IFS= read -r arg; do cmd+=("$arg"); done <"$state/cmd"
        deadline=$(( $(date +%s) + FIXTURE_BOOT_CEILING_SECONDS ))
        while :; do
            if [ -s "$state/port" ]; then
                echo "fixture server listening on port $(cat "$state/port") (launch $(cat "$state/attempt"))"
                exit 0
            fi
            if ! alive; then
                attempt=$(cat "$state/attempt")
                if [ "$attempt" -ge "$FIXTURE_BOOT_ATTEMPTS" ]; then
                    echo "::error::FixtureServerMain exited before writing a port on all $attempt launches; log:"
                    cat "$state/log"
                    exit 1
                fi
                echo "FixtureServerMain exited before writing a port; relaunching ($((attempt + 1)) of $FIXTURE_BOOT_ATTEMPTS)"
                echo $((attempt + 1)) >"$state/attempt"
                sleep "$FIXTURE_POLL_SECONDS"
                start "${cmd[@]}"
                continue
            fi
            if [ "$(date +%s)" -ge "$deadline" ]; then
                echo "::error::FixtureServerMain didn't write a port within ${FIXTURE_BOOT_CEILING_SECONDS}s; log:"
                cat "$state/log"
                exit 1
            fi
            sleep "$FIXTURE_POLL_SECONDS"
        done
        ;;

    stop)
        [ -f "$state/pid" ] && kill "$(cat "$state/pid")" 2>/dev/null || true
        ;;

    *)
        echo "fixture-server.sh: unknown mode '$mode' (launch|await|stop)" >&2
        exit 2
        ;;
esac
