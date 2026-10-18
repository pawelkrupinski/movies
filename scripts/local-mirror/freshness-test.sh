#!/usr/bin/env bash
# scripts/local-mirror/freshness: its arguments reach the mongosh rule scripts as the variables they read, and mongosh's
# exit maps to 0 fresh / 2 no mirror / 3 stale. mongosh is a stub: no Mongo is contacted.
# Run: bash scripts/local-mirror/freshness-test.sh
set -uo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$HERE/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

scratch="$(mktemp -d)"; trap 'rm -rf "$scratch"' EXIT
mkdir -p "$scratch/bin"
cat > "$scratch/bin/mongosh" <<STUB
#!/usr/bin/env bash
printf '%s\n' "\$*" > "$scratch/mongosh-args"
exit "\${STUB_MONGOSH_EXIT:-0}"
STUB
chmod +x "$scratch/bin/mongosh"
export PATH="$scratch/bin:$PATH"
unset LOCAL_MIRROR_PORT
fresh() { "$HERE/freshness" "$@" 2> "$scratch/err"; }
args() { cat "$scratch/mongosh-args"; }

fresh >/dev/null; rc=$?
check "mongosh exit 0 is fresh, 0" "0" "$rc"
check "no country: all five are asked" "yes" "$(args | grep -q "var CCS=\['pl','de','uk','us','es'\]" && echo yes || echo "no: $(args)")"
check "...with the 6 hour default lag, in ms" "yes" "$(args | grep -q 'var MAX_LAG_MS=21600000' && echo yes || echo no)"
check "...against the local mirror on :28017" "yes" "$(args | grep -q "mongodb://127.0.0.1:28017" && echo yes || echo no)"
check "...with the rule scripts loaded in order" "yes" \
  "$(args | grep -q -- '--file .*mirror-targets.js --file .*staleness-rule.js --file .*freshness-rule.js --file .*freshness.js' && echo yes || echo no)"

fresh DE uk --max-lag-hours 1.5 --need cinema_scrapes,venue_pages --quiet >/dev/null
check "countries are lowercased and limit the run" "yes" "$(args | grep -q "var CCS=\['de','uk'\]" && echo yes || echo no)"
check "--max-lag-hours is read in hours" "yes" "$(args | grep -q 'var MAX_LAG_MS=5400000' && echo yes || echo no)"
check "--need names the collections the caller reads" "yes" "$(args | grep -q "var NEED=\['cinema_scrapes','venue_pages'\]" && echo yes || echo no)"
check "--quiet is passed" "yes" "$(args | grep -q 'var QUIET=true' && echo yes || echo no)"

fresh pl --not-before 2026-10-06T03:05:51.123456789Z >/dev/null
check "--not-before takes a recording's nanosecond moment, as epoch ms" "yes" "$(args | grep -q 'var NOT_BEFORE_MS=1791255951123' && echo yes || echo "no: $(args)")"
fresh pl --not-before yesterday >/dev/null; rc=$?
check "--not-before that is no ISO-8601 moment is refused, 2" "2" "$rc"
fresh --wat >/dev/null; rc=$?
check "an unknown argument is refused, 2" "2" "$rc"

STUB_MONGOSH_EXIT=3 fresh pl >/dev/null; rc=$?
check "mongosh exit 3 (stale) is passed on" "3" "$rc"
STUB_MONGOSH_EXIT=2 fresh pl >/dev/null; rc=$?
check "mongosh exit 2 (no mirror database) is passed on" "2" "$rc"
STUB_MONGOSH_EXIT=1 fresh pl >/dev/null; rc=$?
check "any other mongosh failure is 'the mirror did not answer', 2, naming the starter" "2 yes" \
  "$rc $(grep -q 'start-local-mongo.sh' "$scratch/err" && echo yes || echo no)"
spec_summary
