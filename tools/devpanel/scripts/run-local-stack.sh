#!/usr/bin/env bash
# Run web + a fixture-replaying worker locally (sbt localStack). The worker
# background-runs LocalFixtureWorkerMain against fixtures/today on local Mongo
# (127.0.0.1:27018); web/run serves on :9000. See build.sbt:localStack.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib.sh"

dispatch "$REPO_ROOT" "Local web + worker (fixtures)" sbt localStack
