#!/usr/bin/env bash
# Build and launch the DevPanel floating palette (tools/devpanel).
# Pass-through args, e.g. `./runDevPanel.sh --no-open` to build without opening.
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec bash "$HERE/tools/devpanel/build.sh" "$@"
