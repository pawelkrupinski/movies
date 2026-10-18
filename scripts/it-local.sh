#!/usr/bin/env bash
#
# Run the it/ layer locally against a MongoDB of this run's own.
#
#   scripts/it-local.sh                      # sbt itAll
#   scripts/it-local.sh 'worker/IntegrationTest/testOnly integration.FooSpec'
#
# Why a server of its own, not the shared mongod on :28017: scripts/own-mongod.sh (2026-10-12's seven suites failed
# behind another agent's writes). It is a single-node replica set on a free port, its data in a temporary directory,
# removed when the run ends — however it ends. MONGODB_DB is set too (`kinowo_it`): no other run can reach this server,
# so the prefix cannot collide.
set -euo pipefail

cd "$(dirname "$0")/.."

# shellcheck source=scripts/own-mongod.sh
. scripts/own-mongod.sh
trap own_mongod_stop EXIT INT TERM
own_mongod_start it || exit 1

status=0
MONGODB_URI="$OWN_MONGOD_URI" MONGODB_DB="kinowo_it" sbt "${@:-itAll}" || status=$?
exit "$status"
