#!/usr/bin/env bash
# Kill a running local web stack: free :9000 (the `sbt web/run` | `sbt localStack`
# JVM) and reap the forked fixture worker (LocalFixtureWorkerMain). The web
# console runs this as an action, so it has ALREADY SIGTERM'd its current
# runner's process group (web JVM + sbt) before this script starts — see
# ConsoleView.run's `runner?.stop()`. This is the belt-and-suspenders reap that
# guarantees nothing keeps holding :9000 and no orphaned worker keeps projecting
# fixtures into the local Mongo. `step` (not a bare call) so the actions are
# visible in the console and assertable under DEVPANEL_PRINT_ONLY.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib.sh"

step free_port 9000
step kill_stale_worker
