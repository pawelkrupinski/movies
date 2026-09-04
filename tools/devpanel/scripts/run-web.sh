#!/usr/bin/env bash
# Run the Play web app locally on :9000, via `scripts/dev-server.sh` rather than
# a bare `sbt web/run`: that wrapper narrows the heap to 2.5GB so an OOM trips
# early and leaves `target/oom-<pid>.hprof` behind. THIS script is what launched
# the 2026-09-04 dev server that died with nothing but one OutOfMemoryError line,
# so it is the path that most needs the dump. Also reaps a stale fixture worker
# from a prior `localStack` run so it can't keep mutating the local corpus under
# a plain web server.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib.sh"

reset_local_stack
dispatch "$REPO_ROOT" "Local web server (:9000)" ./scripts/dev-server.sh
