#!/usr/bin/env bash
# One-way sync of the admin-curated titleRules collection from prod into the
# local dev kinowo_local. Runs the repo's scripts/local-mirror/sync-title-rules.sh
# (which opens its OWN read-only flyctl tunnel to prod, mongodumps titleRules,
# guards on the record count, then mongorestore --drops it into kinowo_local).
# Prod is read-only / never written.
#
# The "Reset local corpus" action already runs this at the end of a --local
# reset; this button is the standalone "just re-pull the rules" path.
set -euo pipefail
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib.sh"

dispatch "$REPO_ROOT" "Sync title rules (prod → kinowo_local)" scripts/local-mirror/sync-title-rules.sh
