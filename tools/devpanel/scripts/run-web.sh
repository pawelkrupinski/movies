#!/usr/bin/env bash
# Run the Play web app locally on :9000 (sbt web/run).
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib.sh"

dispatch "$REPO_ROOT" "Local web server (:9000)" sbt web/run
