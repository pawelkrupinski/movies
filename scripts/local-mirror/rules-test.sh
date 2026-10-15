#!/usr/bin/env bash
# The local mirror's pure rules, through their own specs: the re-seed decision (staleness-rule-spec.js), whether a
# reader may trust the mirror (freshness-rule-spec.js), where a tailer opens its stream (stream-start-spec.js) and the
# mirrored-collection digest (targets-fingerprint-spec.js). No Mongo: each is a pure function, run under
# `mongosh --nodb` when mongosh is installed and under Node (nodb.js) when it is not, as on a CI runner.
#
#   scripts/local-mirror/rules-test.sh
#
# Exits 0 when every spec passes, 1 otherwise, 2 with neither mongosh nor node to run them.
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$HERE" || exit 2

if command -v mongosh >/dev/null && [ -z "${RULES_TEST_NODE:-}" ]; then nodb() { mongosh --nodb --quiet "$@"; }
elif command -v node >/dev/null; then nodb() { node "$HERE/nodb.js" "$@"; }
else echo "[spec] rules: neither mongosh nor node is installed — nothing ran" >&2; exit 2
fi

failed=0
nodb --file staleness-rule.js --file staleness-rule-spec.js || failed=1
nodb --file staleness-rule.js --file freshness-rule.js --file freshness-rule-spec.js || failed=1
nodb --file stream-start.js --file stream-start-spec.js || failed=1
nodb --eval 'var FINGERPRINT_SILENT=1' --file targets-fingerprint.js --file targets-fingerprint-spec.js || failed=1
exit "$failed"
