#!/usr/bin/env bash
# The one place that knows how to make the LOCAL mirror Mongo reachable.
#
# Sibling of prod-tunnel.sh, deliberately separate: that file is about reaching
# the prod SOURCE over ssh, this one is about the local TARGET the mirror and the
# sync scripts write into. Same shape of question, opposite end of the pipe.
#
# WHY IT EXISTS. mirror.sh, sync-title-rules.sh and sync-enrichment-cache.sh each
# carried their own copy of the same probe-then-restart block, differing only in
# the log tag — and one of them had already drifted, hardcoding :28017 while the
# other two honoured $LOCAL_MIRROR_PORT. A copy that drifts silently is the whole
# argument for one definition.
#
# Sourcing must stay side-effect free (mirror-resilience-spec.sh asserts that
# sourcing mirror.sh reads no .env.local and starts nothing), so this file is a
# function definition and nothing else.

# Ensure the local mirror Mongo (native, brew-managed) answers, re-running the
# idempotent starter only when it does not — so a stopped service self-heals.
#
# Returns non-zero rather than crashing when the starter cannot bring it up: the
# mirror daemon's loop retries (e.g. at login, before `brew services` has it),
# while the one-shot sync scripts call it bare and let `set -e` end the run.
#
# The log tag is $TUNNEL_TAG, which every caller already sets for prod-tunnel.sh —
# reusing it is what removes the only difference the three copies had.
ensure_local_mongo() {
  local port="${LOCAL_MIRROR_PORT:-28017}"
  nc -z -w2 127.0.0.1 "$port" 2>/dev/null && return 0
  echo "[${TUNNEL_TAG:-local-mongo}] local Mongo not reachable on :$port — (re)starting"
  "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/start-local-mongo.sh" || return 1
}
