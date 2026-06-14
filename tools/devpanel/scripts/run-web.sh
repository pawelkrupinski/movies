#!/usr/bin/env bash
# Run the Play web app locally on :9000 (sbt web/run). Also reaps a stale
# fixture worker from a prior `localStack` run so it can't keep mutating the
# local corpus under a plain web server.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib.sh"

free_port 9000
kill_stale_worker
dispatch "$REPO_ROOT" "Local web server (:9000)" sbt web/run
