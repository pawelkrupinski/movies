#!/usr/bin/env bash
# recording-cache.sh against a stub `curl` (the host's cache service) and stub `sudo` / `mountpoint` (the mounts).
# Run: bash scripts/ci/recording-cache-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m recording-cache.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
digest=$(printf 'tree' | shasum -a 256 2>/dev/null | cut -d' ' -f1 || printf 'tree' | sha256sum | cut -d' ' -f1)
mkdir -p "$work/bin" "$work/mount/$digest/test/resources/fixtures/enrichment-uk/api.themoviedb.org"
printf 'recorded\n' > "$work/mount/$digest/test/resources/fixtures/enrichment-uk/api.themoviedb.org/search"
printf 'beside\n' > "$work/mount/$digest/test/resources/fixtures/enrichment-uk.refetch.tsv"

# The service: answers $STUB_ANSWER (or fails as an unreachable one does with $STUB_DOWN), and logs what it was asked.
cat > "$work/bin/curl" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$@" >> "$(dirname "$0")/../curl-args"
[ -z "${STUB_DOWN:-}" ] || { echo "curl: (7) Failed to connect" >&2; exit 7; }
printf '%s' "$STUB_ANSWER"
STUB
# Every mount is logged, never made; `mountpoint` says nothing is mounted yet.
cat > "$work/bin/sudo" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$(dirname "$0")/../mounts"
[ -z "${STUB_NO_OVERLAY:-}" ] || [ "$3" != overlay ] || exit 32
STUB
printf '#!/usr/bin/env bash\nexit 1\n' > "$work/bin/mountpoint"
chmod +x "$work/bin/curl" "$work/bin/sudo" "$work/bin/mountpoint"

hit="{\"digest\": \"$digest\", \"dir\": \"$digest\", \"asset\": \"enrichment-uk-573.tar.zst\", \"bytes\": 1, \"hit\": true}"

# tree <label> [env...] -> exit status; stdout in $work/out-<label>, stderr in $work/err-<label>
tree() {
  local label="$1"; shift
  env PATH="$work/bin:$PATH" FORGEJO_RECORDING_CACHE=http://cache FORGEJO_RECORDING_CACHE_DIR="$work/mount" \
    GH_TOKEN=job-token GITHUB_REPOSITORY=o/movies STUB_ANSWER="$hit" "$@" \
    bash "$REPO_ROOT/scripts/ci/recording-cache.sh" tree convergence-fixtures 'enrichment-uk-573.tar.*' \
    > "$work/out-$label" 2> "$work/err-$label"
  echo $?
}

check "a tree the host has unpacked is handed over by its digest" "0 $work/mount/$digest" "$(tree hit) $(cat "$work/out-hit")"
check "...asked for with the job's own token, for this repository's release and asset" "true" \
  "$(grep -qx 'Authorization: token job-token' "$work/curl-args" && grep -qx 'repo=o/movies' "$work/curl-args" &&
     grep -qx 'tag=convergence-fixtures' "$work/curl-args" && grep -qx 'asset=enrichment-uk-573.tar.*' "$work/curl-args" &&
     echo true || echo false)"
check "no service offered (github.com, a host runner) is a fall-back, not a failure to unpack" "1" \
  "$(tree github FORGEJO_RECORDING_CACHE= FORGEJO_RECORDING_CACHE_DIR=)"
check "...nor is a host whose mount is missing" "1" "$(tree unmounted FORGEJO_RECORDING_CACHE_DIR="$work/nowhere")"
check "...nor a service that is down" "1" "$(tree down STUB_DOWN=1)"
check "...nor one that answers without a digest" "1" "$(tree refused STUB_ANSWER='{"error": "no asset matches"}')"
check "...and says why" "true" "$(grep -q 'no asset matches' "$work/err-refused" && echo true || echo false)"
check "...nor a digest the mount does not hold" "1" \
  "$(tree elsewhere STUB_ANSWER="{\"digest\": \"$(printf '0%.0s' {1..64})\"}")"
check "RECORDING_CACHE=off opts a job out" "1" "$(tree off RECORDING_CACHE=off)"

# overlay
mkdir -p "$work/ws/test/resources/fixtures/corpus"
printf 'checked in\n' > "$work/ws/test/resources/fixtures/corpus/c"
overlay() {
  env PATH="$work/bin:$PATH" "$@" bash "$REPO_ROOT/scripts/ci/recording-cache.sh" overlay \
    "$work/mount/$digest/test/resources/fixtures" "$work/ws/test/resources/fixtures" "$work/scratch" 2>/dev/null
  echo $?
}
check "the cached tree is overlaid on the workspace" "0" "$(overlay)"
check "...over a RAM upper layer the job's user owns" "true" \
  "$(grep -q "^mount -t tmpfs -o size=2g,mode=0755,uid=$(id -u),gid=$(id -g) recording-upper $work/scratch$" "$work/mounts" && echo true || echo false)"
check "...each directory read-only below it, its writes in the upper layer" "true" \
  "$(grep -qx "mount -t overlay recording-overlay -o lowerdir=$work/mount/$digest/test/resources/fixtures/enrichment-uk,upperdir=$work/scratch/enrichment-uk/upper,workdir=$work/scratch/enrichment-uk/work $work/ws/test/resources/fixtures/enrichment-uk" "$work/mounts" && echo true || echo false)"
check "...and a plain file beside it copied" "beside" "$(cat "$work/ws/test/resources/fixtures/enrichment-uk.refetch.tsv")"
check "...touching nothing the checkout already holds" "checked in" "$(cat "$work/ws/test/resources/fixtures/corpus/c")"
mkdir -p "$work/mount/$digest/test/resources/fixtures/corpus"
: > "$work/mounts"
overlay > /dev/null
check "a directory the checkout already fills stays visible beneath the tree" "true" \
  "$(grep -q "lowerdir=$work/mount/$digest/test/resources/fixtures/corpus:$work/ws/test/resources/fixtures/corpus," "$work/mounts" && echo true || echo false)"
check "an overlay that will not mount fails, so the caller copies the tree out instead" "1" "$(overlay STUB_NO_OVERLAY=1)"

spec_summary
