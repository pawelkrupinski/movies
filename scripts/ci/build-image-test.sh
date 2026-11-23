#!/usr/bin/env bash
# build-image.sh against a stub `docker` whose builds succeed, or fail through the mirror, on cue.
# Run: bash scripts/ci/build-image-test.sh
# script-test-reads: Dockerfile
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m build-image.sh\n'

stub_dir="$(mktemp -d)"
trap 'rm -rf "$stub_dir"' EXIT
# STUB_BUILD: `ok` builds; `mirror-blob` fails every build whose bases come through mirror.gcr.io the way Main #2264's
# did; `mirror-buildkit` fails the same way with BuildKit's "unknown blob"; `broken` fails every build on its own
# Dockerfile; `registry-down` fails the first build as the image registry dropping away mid-build did, and
# `registry-pruned` as Main #2662's push into the 04:00 registry-prune did -- each builds the next. A build's -f Dockerfile (else ./Dockerfile) is copied to $STUB_DOCKERFILE.
cat > "$stub_dir/docker" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STUB_LOG"
dockerfile=Dockerfile
while [ "$#" -gt 0 ]; do [ "$1" = -f ] && dockerfile="$2"; shift; done
cp "$dockerfile" "$STUB_DOCKERFILE"
from_mirror=$(grep -c '^FROM mirror.gcr.io/' "$dockerfile")
case "$STUB_BUILD" in
  ok) exit 0 ;;
  mirror-blob) [ "$from_mirror" -gt 0 ] || exit 0
    echo "ERROR: failed to build: failed to solve: failed to compute cache key: failed to copy: httpReadSeeker: failed open: could not fetch content descriptor sha256:edaf (application/vnd.oci.image.layer.v1.tar+gzip) from remote: not found"
    exit 1 ;;
  mirror-buildkit) [ "$from_mirror" -gt 0 ] || exit 0; echo "ERROR: unknown blob"; exit 1 ;;
  broken) echo "ERROR: failed to solve: process \"/bin/sh -c apt-get install nope\" did not complete successfully: exit code: 100"; exit 1 ;;
  # Main #2361 (2026-11-09 23:33Z): the image registry went away mid-build, a transfer cut short, then refused.
  registry-down)
    [ -e "$STUB_LOG.pruned" ] && exit 0
    : > "$STUB_LOG.pruned"
    echo "#13 2.036 error: short read: expected 42139920 bytes but got 13714392: unexpected EOF"
    echo "ERROR: failed to build: failed to solve: failed to compute cache key: failed to copy: httpReadSeeker: failed open: failed to do request: Get \"https://registry.example/v2/movies-web/blobs/sha256:86bc\": dial tcp 2.28.47.31:443: connect: connection refused"
    exit 1 ;;
  # Main #2662 (2026-11-19 04:00:22Z): the push met the registry stopped for its daily garbage-collect.
  registry-pruned)
    [ -e "$STUB_LOG.pruned" ] && exit 0
    : > "$STUB_LOG.pruned"
    echo "#17 1.311 error: failed to copy: failed to do request: Put \"https://registry.example/v2/movies-web/blobs/uploads/01a1\": write tcp 172.17.0.2:58052->2.28.47.31:443: write: connection reset by peer"
    echo "ERROR: failed to build: failed to solve: failed to push registry.example/movies-web:abc123: unexpected status from HEAD request to https://registry.example/v2/movies-web/blobs/sha256:46e1: 502 Bad Gateway"
    exit 1 ;;
esac
STUB
chmod +x "$stub_dir/docker"
# The registry's /v2/ answers, one per probe from $STUB_ANSWERS (the last repeats); every URL probed to $STUB_LOG.curl.
cat > "$stub_dir/curl" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "${!#}" >> "$STUB_LOG.curl"
set -- $STUB_ANSWERS
n=$(($(wc -l < "$STUB_LOG.curl")))
[ "$n" -gt "$#" ] && n=$#
printf '%s' "${!n}"
STUB
cat > "$stub_dir/date" <<'STUB'
#!/usr/bin/env bash
echo "$STUB_HHMM"
STUB
chmod +x "$stub_dir/curl" "$stub_dir/date"
export STUB_ANSWERS=200 STUB_HHMM=1200 REGISTRY_WAIT_SECONDS=5 REGISTRY_POLL_SECONDS=0
export STUB_LOG="$stub_dir/log" STUB_DOCKERFILE="$stub_dir/used-dockerfile"
cp "$REPO_ROOT/Dockerfile" "$stub_dir/Dockerfile"

# build <stub state> -> "<exit status>"
build() {
  export STUB_BUILD="$1"
  : > "$STUB_LOG"; : > "$STUB_LOG.curl"; rm -f "$STUB_LOG.pruned"
  (cd "$stub_dir" && PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/build-image.sh" web registry.example/movies-web abc123) \
    > "$stub_dir/out" 2>&1
  echo "$?"
}

check "a build that succeeds succeeds" "0" "$(build ok)"
check "...building once" "1" "$(grep -c '^buildx build' "$STUB_LOG")"
check "...for amd64, pushed under the commit's SHA tag only" "1" \
  "$(grep -c -- '^buildx build --platform linux/amd64 --push --build-arg BIN=web --build-arg COMMIT_SHA=abc123 --tag registry.example/movies-web:abc123 ' "$STUB_LOG")"
check "...with the inline cache of the image last shipped" "1" \
  "$(grep -c -- '--cache-to type=inline --cache-from type=registry,ref=registry.example/movies-web:latest \.$' "$STUB_LOG")"
check "...from the repository's own Dockerfile, its bases through the mirror" "0" \
  "$(cmp -s "$STUB_DOCKERFILE" "$REPO_ROOT/Dockerfile"; echo $?)"

check "a build the mirror fails a blob of is built again from Docker Hub, and succeeds" "0" "$(build mirror-blob)"
check "...building twice" "2" "$(grep -c '^buildx build' "$STUB_LOG")"
check "...the second time without the cache import" "1" "$(tail -1 "$STUB_LOG" | grep -vc -- '--cache-from')"
check "...from a Dockerfile with no base left on the mirror" "0" "$(grep -c '^FROM mirror.gcr.io/' "$STUB_DOCKERFILE")"
check "...every base Docker Hub's name for the same pinned image" \
  "$(grep '^FROM ' "$REPO_ROOT/Dockerfile" | sed 's#mirror.gcr.io/library/#docker.io/library/#')" "$(grep '^FROM ' "$STUB_DOCKERFILE")"
check "...and the rest of it unchanged" "" \
  "$(diff <(grep -v '^FROM ' "$REPO_ROOT/Dockerfile") <(grep -v '^FROM ' "$STUB_DOCKERFILE"))"
check "...saying why on the run" "1" "$(grep -c '^::warning::the web image.s build could not fetch a blob through mirror.gcr.io' "$stub_dir/out")"
check "BuildKit's 'unknown blob' from the mirror falls back the same way" "0" "$(build mirror-buildkit)"
check "...building twice" "2" "$(grep -c '^buildx build' "$STUB_LOG")"

check "a build failing on its own Dockerfile fails" "1" "$(build broken)"
check "...without a second build" "1" "$(grep -c '^buildx build' "$STUB_LOG")"
check "...its error on the log" "1" "$(grep -c 'did not complete successfully' "$stub_dir/out")"

# A registry that went away is no build verdict: the build waits, bounded, for it to answer again and builds once more,
# saying so; one still gone at the deadline fails naming it. A failure of the build's own never waits on it.
check "a build into the 04:00 registry-prune waits for the registry and succeeds" "0" \
  "$(STUB_ANSWERS='502 502 401' STUB_HHMM=0400 build registry-pruned)"
check "...probing the registry's /v2/ until it answers" "https://registry.example/v2/ https://registry.example/v2/ https://registry.example/v2/" \
  "$(paste -sd' ' "$STUB_LOG.curl")"
check "...building twice, the second with the cache of the image last shipped" "2 1" \
  "$(grep -c '^buildx build' "$STUB_LOG") $(tail -1 "$STUB_LOG" | grep -c -- '--cache-from type=registry,ref=registry.example/movies-web:latest')"
check "...naming the prune and what the registry answered on the run" "1" \
  "$(grep -c '^::warning::the web image.s build lost registry.example: connection reset by peer -- the image registry was stopped by its daily 04:00 UTC registry-prune' "$stub_dir/out")"
check "a registry back at once, outside the prune window, is built again with the generic cause" "0" "$(build registry-down)"
check "...naming it a host switch or the prune" "1" \
  "$(grep -c '^::warning::the web image.s build lost registry.example: connection refused -- the image registry was down or restarting' "$stub_dir/out")"
check "a build the image registry stays gone from fails" "1" "$(STUB_ANSWERS=000 build registry-down)"
check "...after waiting for it" "1" "$(grep -c 'answers 000, waiting for it' "$stub_dir/out" | sed 's/^[1-9][0-9]*$/1/')"
check "...without a second build" "1" "$(grep -c '^buildx build' "$STUB_LOG")"
check "...naming the registry as the cause, with what it answered" "1" \
  "$(grep -c '^::error::the web image.s build lost registry.example: connection refused, and it still answered 000 after 5 s' "$stub_dir/out")"
check "...a build failing on its own Dockerfile names no registry and never probes it" "0 0" \
  "$(build broken >/dev/null; echo "$(grep -c '^::error::.*lost registry' "$stub_dir/out") $(($(wc -l < "$STUB_LOG.curl")))")"

spec_summary
