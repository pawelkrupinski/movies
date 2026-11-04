#!/usr/bin/env bash
# convergence-setup.sh against a stub `gh` serving a fake release and recording runs.
# Run: bash .github/scripts/convergence-setup-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m convergence-setup.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
stub="$work/bin"; release="$work/release"; runs="$work/runs"
mkdir -p "$stub" "$release" "$runs"
setup="$REPO_ROOT/.github/scripts/convergence-setup.sh"

# Release assets by pattern, a run's artifact by name, a run's createdAt, the green-run listing
# (STUB_RUN_LIST), and the release's asset listing (for the fills). STUB_FAIL makes every read 5xx;
# STUB_THROTTLED makes the first that many artifact downloads answer as Actions' storage did in run 38763455103.
cat > "$stub/gh" <<'STUB'
#!/usr/bin/env bash
[ -z "${STUB_FAIL:-}" ] || { echo "HTTP 502: Bad Gateway" >&2; exit 1; }
if [ "$1 $2" = "run download" ] && [ -n "${STUB_THROTTLED:-}" ]; then
  n=$(( $(cat "$STUB_RUNS/throttled" 2>/dev/null || echo 0) + 1 )); echo "$n" > "$STUB_RUNS/throttled"
  if [ "$n" -le "$STUB_THROTTLED" ]; then
    echo "error downloading scrape-fixtures-de: HTTP 503: 503 Egress is over the account limit. (https://productionresultssa17.blob.core.windows.net/x.zip)" >&2
    exit 1
  fi
fi
dir=""; pattern=""; name=""
args=("$@")
for ((i = 0; i < ${#args[@]}; i++)); do
  case "${args[$i]}" in
    --dir) dir="${args[$((i + 1))]}" ;;
    --pattern) pattern="${args[$((i + 1))]}" ;;
    --name) name="${args[$((i + 1))]}" ;;
  esac
done
case "$1 $2" in
  "release view") ls -1 "$STUB_RELEASE"; exit 0 ;;
  "release download")
    matches=(); for f in "$STUB_RELEASE"/$pattern; do [ -e "$f" ] && matches+=("$f"); done
    [ "${#matches[@]}" -gt 0 ] || { echo "no assets match the file pattern" >&2; exit 1; }
    mkdir -p "$dir"; cp "${matches[@]}" "$dir/"; exit 0 ;;
  "run download")
    src="$STUB_RUNS/$3/$name"
    [ -d "$src" ] || { echo "no artifact matches any of the names or patterns provided" >&2; exit 1; }
    mkdir -p "$dir"; cp "$src"/* "$dir/"; exit 0 ;;
  "run view") echo "2026-10-01T01:00:00Z"; exit 0 ;;
  "run list") printf '%s\n' ${STUB_RUN_LIST:-}; exit 0 ;;
esac
echo "unexpected gh $*" >&2; exit 2
STUB
chmod +x "$stub/gh"

# A pinned pair for `de` with one fill published over it; `uk` pins nothing; `es` pins a line without a tree.
echo "42 2026-10-11T02:00:00Z enrichment-de-42.tar.zst" > "$release/hermetic-de.txt"
: > "$release/fill-de-42-77-1-convergence.tar.zst"
echo "43 2026-10-11T02:00:00Z" > "$release/hermetic-es.txt"

# step <subcommand args...>: runs in a fresh $work/ws with fresh GITHUB_ENV/GITHUB_OUTPUT; leaves
# the exit status in $status and stdout+stderr in $work/log.
step() {
  rm -rf "$work/ws"; mkdir -p "$work/ws"
  : > "$work/env"; : > "$work/output"
  (cd "$work/ws" && PATH="$stub:$PATH" STUB_RELEASE="$release" STUB_RUNS="$runs" FIXTURE_RELEASE_TAG=t \
     CONVERGENCE_FILL_LIST_BACKOFF=0 GH_DOWNLOAD_BACKOFF=0 GITHUB_ENV="$work/env" GITHUB_OUTPUT="$work/output" \
     bash "$setup" "$@") > "$work/log" 2>&1
  status=$?
}
logged() { grep -q -- "$1" "$work/log" && echo yes || echo no; }

# ---- pinned: the one reader of the pin
check "pinned prints the pinned pair" "42 2026-10-11T02:00:00Z enrichment-de-42.tar.zst" \
  "$(PATH="$stub:$PATH" STUB_RELEASE="$release" FIXTURE_RELEASE_TAG=t bash "$setup" pinned de 2>/dev/null)"
check "no pin prints nothing and succeeds" "0|" \
  "$(out=$(PATH="$stub:$PATH" STUB_RELEASE="$release" FIXTURE_RELEASE_TAG=t bash "$setup" pinned uk 2>/dev/null); echo "$?|$out")"
check "a pin without a tree is malformed (exit 2)" "2" \
  "$(PATH="$stub:$PATH" STUB_RELEASE="$release" FIXTURE_RELEASE_TAG=t bash "$setup" pinned es >/dev/null 2>&1; echo $?)"
check "a failed read is not an absent pin (exit 1)" "1" \
  "$(PATH="$stub:$PATH" STUB_FAIL=1 GH_DOWNLOAD_BACKOFF=0 FIXTURE_RELEASE_TAG=t bash "$setup" pinned de >/dev/null 2>&1; echo $?)"

# ---- pair
step pair de record "" 900
check "a recording leg pins the corpus it is given and the working tree glob" \
  "KINOWO_CONVERGENCE_PIN_CORPUS_RUN=900 KINOWO_CONVERGENCE_TREE_ASSET=enrichment-de.tar.*" "$(tr '\n' ' ' < "$work/env" | sed 's/ $//')"
step pair de record
check "a recording leg with no corpus run takes the newest" "KINOWO_CONVERGENCE_TREE_ASSET=enrichment-de.tar.*" "$(cat "$work/env")"
step pair de hermetic
check "a hermetic leg resolves the pinned pair" "0" "$status"
check "…with the fills published over its tree" "pair=42 2026-10-11T02:00:00Z enrichment-de-42.tar.zst fill-de-42-77-1-convergence.tar.zst" "$(cat "$work/output")"
check "…and exports the pair's parts" \
  "KINOWO_CONVERGENCE_PIN_CORPUS_RUN=42 KINOWO_CONVERGENCE_TREE_ASSET=enrichment-de-42.tar.zst KINOWO_CONVERGENCE_FILL_ASSETS=fill-de-42-77-1-convergence.tar.zst" \
  "$(tr '\n' ' ' < "$work/env" | sed 's/ $//')"
step pair de hermetic "7 2026-09-01T00:00:00Z enrichment-de-7.tar.zst fill-a fill-b"
check "a given pair is replayed as given, its fills included" "pair=7 2026-09-01T00:00:00Z enrichment-de-7.tar.zst fill-a fill-b|KINOWO_CONVERGENCE_FILL_ASSETS=fill-a fill-b" \
  "$(cat "$work/output")|$(grep FILL "$work/env")"
step pair de hermetic "" 528
check "a hermetic leg given a corpus run replays that recording's bare pair: its own tree, no fills" \
  "0|pair=528 2026-10-01T01:00:00Z enrichment-de-528.tar.*" "$status|$(cat "$work/output")"
check "…and exports it so" \
  "KINOWO_CONVERGENCE_PIN_CORPUS_RUN=528 KINOWO_CONVERGENCE_TREE_ASSET=enrichment-de-528.tar.* KINOWO_CONVERGENCE_FILL_ASSETS= KINOWO_CONVERGENCE_NAMED_RECORDING=528" \
  "$(tr '\n' ' ' < "$work/env" | sed 's/ $//')"
step pair uk hermetic
check "no pinned pair fails the hermetic leg" "1|yes" "$status|$(logged '::error::No recorded pair pinned for uk')"
step pair es hermetic
check "a malformed pin fails the hermetic leg" "1|yes" "$status|$(logged '::error::No recorded pair pinned for es')"

# ---- pins: the run's pairs, once
step pins de,uk,es
check "pins names each country's pair with its fills, leaving out one it cannot resolve" \
  '0|{"de":"42 2026-10-11T02:00:00Z enrichment-de-42.tar.zst fill-de-42-77-1-convergence.tar.zst"}|yes' \
  "$status|$(grep '^{' "$work/log")|$(logged "could not pin uk's pair")"
check "…and exports nothing of its own" "|" "$(cat "$work/env")|$(cat "$work/output")"

# ---- corpus
mkdir -p "$runs/42/scrape-fixtures-de" "$runs/50/scrape-fixtures-de"
echo pinned > "$runs/42/scrape-fixtures-de/scrapes-de.tar.gz"
echo newest > "$runs/50/scrape-fixtures-de/scrapes-de.tar.gz"
KINOWO_CONVERGENCE_PIN_CORPUS_RUN=42 step corpus de
check "the pinned run's corpus is downloaded" "0|pinned" "$status|$(cat "$work/ws/scrape-archive/scrapes-de.tar.gz" 2>/dev/null)"
check "…and its provenance exported" "KINOWO_CONVERGENCE_CORPUS_RUN=42 KINOWO_CONVERGENCE_CORPUS_RECORDED_AT=2026-10-01T01:00:00Z" \
  "$(tr '\n' ' ' < "$work/env" | sed 's/ $//')"
KINOWO_CONVERGENCE_PIN_CORPUS_RUN=41 step corpus de
check "an expired pinned corpus ends the leg, sending the reader to re-record" "1|yes" \
  "$status|$(logged "::error::recording run 41's corpus is no longer downloadable.*re-run Record scrape fixtures")"
rm -f "$runs/throttled"
# A landing recording whose artifact is gone is read back from the archive release (one stub release serves both).
echo archived > "$release/scrapes-de-40.tar.zst"
KINOWO_CONVERGENCE_PIN_CORPUS_RUN=40 step corpus de
check "an artifact gone, the archive release's corpus is taken, under the name unpack reads" "0|archived|yes|KINOWO_CONVERGENCE_CORPUS_RUN=40" \
  "$status|$(cat "$work/ws/scrape-archive/scrapes-de.tar.gz" 2>/dev/null)|$(logged "recording run 40's corpus from the archive release identity-recordings")|$(grep CORPUS_RUN= "$work/env")"
rm -f "$release/scrapes-de-40.tar.zst"
STUB_THROTTLED=2 KINOWO_CONVERGENCE_PIN_CORPUS_RUN=42 step corpus de
check "a throttled artifact storage (503 Egress is over the account limit) is asked again, and the corpus comes" "0|pinned" \
  "$status|$(cat "$work/ws/scrape-archive/scrapes-de.tar.gz" 2>/dev/null)"
rm -f "$runs/throttled"
STUB_THROTTLED=99 KINOWO_CONVERGENCE_PIN_CORPUS_RUN=42 step corpus de
check "a throttle outlasting every attempt ends the leg as a FAILED read, gh's error shown, never as an expired pair" "1|yes|yes|no" \
  "$status|$(logged 'Egress is over the account limit')|$(logged "::error::could not download recording run 42's corpus")|$(logged 'the pinned pair has expired')"
KINOWO_CONVERGENCE_PIN_CORPUS_RUN=77 GITHUB_RUN_ID=77 step corpus de
check "this job's own recording downloads nothing, only names itself" "0|KINOWO_CONVERGENCE_CORPUS_RUN=77|no" \
  "$status|$(head -1 "$work/env")|$([ -e "$work/ws/scrape-archive/scrapes-de.tar.gz" ] && echo yes || echo no)"
STUB_RUN_LIST="60 2026-10-03T00:00:00Z 50 2026-10-02T00:00:00Z" step corpus de
check "unpinned, the newest green run that still has the corpus is taken" "0|newest|KINOWO_CONVERGENCE_CORPUS_RUN=50" \
  "$status|$(cat "$work/ws/scrape-archive/scrapes-de.tar.gz" 2>/dev/null)|$(head -1 "$work/env")"
STUB_RUN_LIST="60 2026-10-03T00:00:00Z" step corpus de
check "no run with a corpus says so and does not fail the step" "0|yes" "$status|$(logged 'no recorded scrape corpus for de')"

# ---- unpack
background="$work/bg"; mkdir -p "$background"
unpack() { # unpack <tree status> <mode>
  rm -rf "$work/stage"
  mkdir -p "$work/stage/tree-stage/test/resources/fixtures/enrichment-de/.enrichment-cache"
  echo answer > "$work/stage/tree-stage/test/resources/fixtures/enrichment-de/one.json"
  : > "$work/stage/tree-stage/test/resources/fixtures/enrichment-de/.enrichment-cache/a.entry"
  BACKGROUND_DIR="$background" bash "$REPO_ROOT/scripts/ci/in-background.sh" start tree-restore bash -c "exit $1"
  rm -rf "$work/ws"; mkdir -p "$work/ws/test/resources/fixtures/enrichment-de"
  echo kept > "$work/ws/test/resources/fixtures/enrichment-de/kept.json"
  (cd "$work/ws" && BACKGROUND_DIR="$background" RUNNER_TEMP="$work/stage" bash "$setup" unpack de "$2") > "$work/log" 2>&1
  status=$?
}
unpack 0 hermetic
check "the staged tree is merged into the fixtures" "0|answer|kept" \
  "$status|$(cat "$work/ws/test/resources/fixtures/enrichment-de/one.json" 2>/dev/null)|$(cat "$work/ws/test/resources/fixtures/enrichment-de/kept.json" 2>/dev/null)"
check "…and its files and remembered answers counted" "yes|yes" \
  "$(logged 'fixture files available: *[1-9]')|$(logged 'remembered enrichment answers restored: *1')"
check "…with no corpus archive, says it fetches live" "yes" "$(logged 'no scrape-archive/scrapes-de.tar.gz')"
unpack 3 hermetic
check "a hermetic leg's missing pin ends the leg" "3" "$status"
unpack 3 record
check "a recording leg's missing pin falls through to fetching live" "0" "$status"
unpack 4 record
check "an unreadable release ends even a recording leg" "4" "$status"

status=$(bash "$setup" bogus >/dev/null 2>&1; echo $?)
check "an unknown subcommand is a usage error" "64" "$status"

spec_summary
