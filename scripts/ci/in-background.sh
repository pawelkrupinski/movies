#!/usr/bin/env bash
# Run a setup chore in the BACKGROUND of a job and collect its verdict in a later step.
#
#   in-background.sh start <name> <command> [args...]   # returns at once
#   in-background.sh wait  <name> <seconds>             # its log, then its exit status
#
# WHY. A convergence leg spends its first minute on chores that each wait on the network and
# need nothing from each other: pulling and booting MongoDB (~20-35 s), installing the prod-Mongo
# tunnel's `socat` (an `apt-get update` + install, 9-17 s on run 37105119296). Run in the
# foreground they queue behind the JDK, the caches and the fixture unpack; started first in the
# background they overlap them, and the step that needs one waits only for what is left of it.
#
# Background processes outlive the step that started them (the runner reaps them only when the
# JOB ends), so the verdict is handed over through files in $RUNNER_TEMP: the command's output in
# <name>.log, its exit status in <name>.rc — written to a temporary name and renamed, so a reader
# never sees a half-written status — and <name>.started, which tells "never started" (wait exits
# 2 at once, so a caller can fall back to doing the chore itself) from "not finished yet".
#
# `wait` fails as the chore would have: its exit status, after printing its log; 1, with the
# log so far, when it has not finished within <seconds>.
set -uo pipefail

dir="${BACKGROUND_DIR:-${RUNNER_TEMP:?RUNNER_TEMP (or BACKGROUND_DIR) must name a directory}}"
action="${1:-}"; name="${2:-}"
[ -n "$action" ] && [ -n "$name" ] || { echo "usage: $0 start <name> <command...> | wait <name> <seconds>" >&2; exit 64; }
shift 2
log="$dir/$name.log"; rc="$dir/$name.rc"; started="$dir/$name.started"

case "$action" in
  start)
    [ "$#" -gt 0 ] || { echo "$0 start $name: no command" >&2; exit 64; }
    rm -f "$rc" "$rc.part"
    : > "$started"
    ( "$@"; echo $? > "$rc.part"; mv "$rc.part" "$rc" ) > "$log" 2>&1 < /dev/null &
    ;;
  wait)
    limit="${1:?seconds}"
    [ -f "$started" ] || { echo "$name was never started in the background"; exit 2; }
    deadline=$((SECONDS + limit))
    until [ -f "$rc" ]; do
      if [ "$SECONDS" -ge "$deadline" ]; then
        echo "::error::$name did not finish within $limit s"
        cat "$log" 2>/dev/null
        exit 1
      fi
      sleep 1
    done
    cat "$log"
    exit "$(cat "$rc")"
    ;;
  *)
    echo "usage: $0 start <name> <command...> | wait <name> <seconds>" >&2; exit 64 ;;
esac
