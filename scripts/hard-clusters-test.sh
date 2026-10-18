#!/usr/bin/env bash
# scripts/hard-clusters.sh: a corpus or tree the leg did not restore comes from ONE recording in the shared cache, a
# recording that cannot be found stops it before sbt, and a failed sbt fails it. sbt and gh are stubs.
# Run: bash scripts/hard-clusters-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m hard-clusters.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/empty"
cache="$work/cache"

# recording 300, already in the cache, holds the de corpus and tree; the newest run (400) holds no de corpus
fixtures="$cache/recordings/300/pair/test/resources/fixtures"
mkdir -p "$fixtures/corpus" "$fixtures/enrichment-de"
echo corpus > "$fixtures/corpus/cinema-scrapes-de.json.gz"

# gh lists runs 400 then 300 and says 400 holds no de corpus; GH_DOWN makes every call fail
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
[ -z "${GH_DOWN:-}" ] || { echo "error connecting to api.github.com" >&2; exit 1; }
case "$1" in run) printf '400\n300\n' ;; api) ;; esac
STUB
# sbt records each call and the tree it was handed; SBT_FAILS makes it fail
cat > "$work/bin/sbt" <<'STUB'
#!/usr/bin/env bash
echo "sbt $* uri=${MONGODB_URI:-}" >> "$STUB/calls"
[ -z "${SBT_FAILS:-}" ] || { echo "[error] compilation failed"; exit 1; }
echo "[hard-clusters] recorded 3 responses"
STUB
chmod +x "$work/bin/gh" "$work/bin/sbt"

hc() { STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$cache" KINOWO_FIXTURE_ROOT="$work/empty" \
  KINOWO_HARD_CLUSTERS_CORPUS_DIR="$work/empty" "$REPO_ROOT/scripts/hard-clusters.sh" "$@" > "$work/out" 2>&1; echo $?; }

check "record: a tree the leg did not restore comes from the newest recording holding the country" "0" "$(hc record de)"
check "...handed to the recording tool as that recording's fixtures" "1" \
  "$(grep -c "IntegrationTest/runMain integration.HardClusterRecord de --fixtures $fixtures uri=" "$work/calls")"
check "...and said" "1" "$(grep -c 'the de enrichment tree: recording run 300' "$work/out")"

printf "record 'Lalka' (Some(2026)):\n" > "$work/convergence.log"
rm -f "$work/calls"
check "ratchet: the corpus and the tree come from the same recording" "0" "$(hc ratchet de "$work/convergence.log")"
check "...the corpus extended is that recording's" "1" "$(grep -c "BuildHardClusters extend $fixtures/corpus " "$work/calls")"
check "...and the tree recorded against is its tree" "1" "$(grep -c "HardClusterRecord de --fixtures $fixtures " "$work/calls")"

rm -f "$work/calls"
check "gh unreachable: no recording, stopped before sbt" "1 0" "$(GH_DOWN=1 hc record de) $(grep -c . "$work/calls" 2>/dev/null || echo 0)"
check "...naming why" "1" "$(grep -c 'no recording to fetch the de inputs from' "$work/out")"

rm -f "$work/calls"
check "no MONGODB_URI: the local Mongo, on the port the local mirror names" "0 1" \
  "$(MONGODB_URI="" LOCAL_MIRROR_PORT=28999 hc record de) $(grep -c 'uri=mongodb://127.0.0.1:28999/?directConnection=true' "$work/calls")"

check "a failed sbt fails the script" "1" "$(SBT_FAILS=1 hc record de)"
check "...showing sbt's log" "1" "$(grep -c 'compilation failed' "$work/out")"
rm -f "$work/calls"
check "a ratchet whose extend fails fails, re-recording nothing after it" "1 1" \
  "$(SBT_FAILS=1 hc ratchet de "$work/convergence.log") $(grep -c . "$work/calls")"
check "no mode prints the usage, from whatever directory it was started" "2 1" \
  "$(cd "$REPO_ROOT/scripts" && bash ./hard-clusters.sh > "$work/out" 2>&1; echo "$? $(grep -c 'hard-clusters.sh ratchet' "$work/out")")"

spec_summary
