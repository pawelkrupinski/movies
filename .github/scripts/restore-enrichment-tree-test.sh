#!/usr/bin/env bash
# restore-enrichment-tree.sh against a stub `gh` whose release holds, or lacks, the tree.
# Run: bash .github/scripts/restore-enrichment-tree-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m restore-enrichment-tree.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/src/test/resources/fixtures/enrichment-uk/api.themoviedb.org"
printf 'recorded\n' > "$work/src/test/resources/fixtures/enrichment-uk/api.themoviedb.org/search"
( cd "$work/src" && tar -cf - test | zstd -q -c > "$work/enrichment-uk.tar.zst" )
printf 'not an archive' > "$work/enrichment-uk-broken.tar.zst"

# `release download` hands over $STUB_ASSET when set, as a release holding it would, and fails
# as an unreachable release does when $STUB_UNREACHABLE is set; any other
# call is logged to $work/other-calls, since the release is the tree's only source.
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
if [ "$1 $2" = "release download" ]; then
  [ -z "${STUB_UNREACHABLE:-}" ] || { echo "HTTP 502: Bad Gateway" >&2; exit 1; }
  [ ! -f "${STUB_BLIP:-/nonexistent}" ] || { rm -f "$STUB_BLIP"; echo "HTTP 500: Internal Server Error" >&2; exit 1; }
  [ -n "${STUB_ASSET:-}" ] || { echo "no assets match the file pattern" >&2; exit 1; }
  while [ "$#" -gt 0 ]; do case "$1" in --dir) dir="$2" ;; --pattern) pattern="$2" ;; esac; shift; done
  case "$pattern" in fill-*) cp "$STUB_FILL" "$dir/$pattern"; exit 0 ;; esac
  cp "$STUB_ASSET" "$dir/"
  exit 0
fi
echo "$*" >> "$(dirname "$0")/../other-calls"
[ "$1 $2" = "run list" ] && { echo 12345; exit 0; }
exit 1
STUB
chmod +x "$work/bin/gh"

# restore <label> <mode> [asset] -> exit status; the stage is $work/stage-<label>, the output $work/out-<label>
restore() {
  STUB_ASSET="${3:-}" PATH="$work/bin:$PATH" FIXTURE_RELEASE_TAG=convergence-fixtures GH_DOWNLOAD_BACKOFF=0 \
    bash "$REPO_ROOT/.github/scripts/restore-enrichment-tree.sh" uk "$2" "$work/stage-$1" > "$work/out-$1" 2>&1
  echo $?
}

check "a release holding the tree stages it" "0" "$(restore pinned hermetic "$work/enrichment-uk.tar.zst")"
check "...unpacked under its own paths, outside the workspace" "recorded" \
  "$(cat "$work/stage-pinned/test/resources/fixtures/enrichment-uk/api.themoviedb.org/search" 2>/dev/null)"
# A fill earlier legs published over the pair: one page the pinned tree lacks.
mkdir -p "$work/fill/test/resources/fixtures/enrichment-uk/www.flicks.co.uk"
printf 'filled\n' > "$work/fill/test/resources/fixtures/enrichment-uk/www.flicks.co.uk/movie"
( cd "$work/fill" && tar -cf - test | zstd -q -c > "$work/fill-uk-1-2.tar.zst" )
status="$(STUB_FILL="$work/fill-uk-1-2.tar.zst" KINOWO_CONVERGENCE_FILL_ASSETS="fill-uk-1-2.tar.zst" \
  restore filled hermetic "$work/enrichment-uk.tar.zst")"
check "a hermetic leg's pair includes the fills published over its tree" "0:filled:recorded" \
  "$status:$(cat "$work/stage-filled/test/resources/fixtures/enrichment-uk/www.flicks.co.uk/movie" 2>/dev/null):$(cat "$work/stage-filled/test/resources/fixtures/enrichment-uk/api.themoviedb.org/search" 2>/dev/null)"
status="$(STUB_FILL="$work/fill-uk-gone.tar.zst" KINOWO_CONVERGENCE_FILL_ASSETS="fill-uk-1-2.tar.zst" CONVERGENCE_FILL_LIST_BACKOFF=0 \
  restore unfilled hermetic "$work/enrichment-uk.tar.zst")"
check "...and a fill its pair names but it cannot restore fails the restore, never replayed without" "true" \
  "$([ "$status" -ne 0 ] && [ "$status" -ne 3 ] && grep -q '::error::fill fill-uk-1-2.tar.zst' "$work/out-unfilled" && echo true || echo false)"
STUB_FILL="$work/fill-uk-1-2.tar.zst" KINOWO_CONVERGENCE_FILL_ASSETS="fill-uk-1-2.tar.zst" restore recording-fill record "$work/enrichment-uk.tar.zst" > /dev/null
check "...and a recording's never does: it records them itself" "false" \
  "$([ -e "$work/stage-recording-fill/test/resources/fixtures/enrichment-uk/www.flicks.co.uk/movie" ] && echo true || echo false)"
check "a replay leg whose pinned tree is gone fails, distinctly" "3" "$(restore unpinned hermetic)"
check "...and says why" "true" "$(grep -q 'a hermetic leg replays nothing else' "$work/out-unpinned" && echo true || echo false)"
check "a recording with no tree anywhere goes on to fetch live" "0" "$(restore cold record)"
check "...having staged nothing" "0" "$(find "$work/stage-cold" -type f | wc -l | tr -d ' ')"
check "...and asked nothing but the release (no run artifacts publish a tree any more)" "" "$(cat "$work/other-calls" 2>/dev/null)"
status="$(STUB_UNREACHABLE=1 restore unreachable record)"
check "a recording whose release cannot be read fails, rather than fetching everything live" "4" "$status"
check "...and says why" "true" "$(grep -q 'HTTP 502' "$work/out-unreachable" && echo true || echo false)"
touch "$work/blip"
check "a release read that fails once (a GitHub 5xx) is asked again, not the leg's setup failed" "0 recorded" \
  "$(STUB_BLIP="$work/blip" restore blip hermetic "$work/enrichment-uk.tar.zst") $(cat "$work/stage-blip/test/resources/fixtures/enrichment-uk/api.themoviedb.org/search" 2>/dev/null)"
status="$(restore broken record "$work/enrichment-uk-broken.tar.zst")"
check "an archive that will not unpack fails, and not as a missing pin" "true" \
  "$([ "$status" -ne 0 ] && [ "$status" -ne 3 ] && echo true || echo false)"

# ci-runner's recording cache: the host has unpacked the tree (scripts/ci/recording-cache.sh), so a replay downloads and
# unpacks nothing of it -- only the fills, into the stage, for convergence-setup to lay over the overlay it mounts.
digest=$(printf '%064d' 7)
mkdir -p "$work/cache-mount/$digest/test/resources/fixtures/enrichment-uk"
cat > "$work/bin/curl" <<'STUB'
#!/usr/bin/env bash
printf '{"digest": "%s", "asset": "a", "hit": true}' "$STUB_DIGEST"
STUB
chmod +x "$work/bin/curl"
cached() {
  STUB_DIGEST="${STUB_DIGEST:-$digest}" FORGEJO_RECORDING_CACHE=http://cache FORGEJO_RECORDING_CACHE_DIR="$work/cache-mount" GH_TOKEN=t \
    GITHUB_REPOSITORY=o/movies STUB_FILL="$work/fill-uk-1-2.tar.zst" KINOWO_CONVERGENCE_FILL_ASSETS="fill-uk-1-2.tar.zst" \
    restore "$@"
}
check "a tree the host's recording cache holds is not downloaded or unpacked by the job" "0:$work/cache-mount/$digest:false" \
  "$(cached cache hermetic "$work/enrichment-uk.tar.zst"):$(cat "$work/stage-cache.cached" 2>/dev/null):$([ -e "$work/stage-cache/test/resources/fixtures/enrichment-uk/api.themoviedb.org/search" ] && echo true || echo false)"
check "...but its pair's fills still are, into the stage, to lay over it" "filled" \
  "$(cat "$work/stage-cache/test/resources/fixtures/enrichment-uk/www.flicks.co.uk/movie" 2>/dev/null)"
check "a recording extends its working tree, so it unpacks its own whatever the cache holds" "0:absent:recorded" \
  "$(cached cache-record record "$work/enrichment-uk.tar.zst"):$([ -e "$work/stage-cache-record.cached" ] && echo present || echo absent):$(cat "$work/stage-cache-record/test/resources/fixtures/enrichment-uk/api.themoviedb.org/search" 2>/dev/null)"
check "a cache that cannot answer is a job that unpacks the tree itself" "0:absent:recorded" \
  "$(STUB_DIGEST=nonsense cached cache-down hermetic "$work/enrichment-uk.tar.zst"):$([ -e "$work/stage-cache-down.cached" ] && echo present || echo absent):$(cat "$work/stage-cache-down/test/resources/fixtures/enrichment-uk/api.themoviedb.org/search" 2>/dev/null)"

spec_summary
