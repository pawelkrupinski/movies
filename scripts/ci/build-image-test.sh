#!/usr/bin/env bash
# build-image.sh against a stub `docker` whose builds succeed, or fail through the mirror, on cue.
# Run: bash scripts/ci/build-image-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m build-image.sh\n'

stub_dir="$(mktemp -d)"
trap 'rm -rf "$stub_dir"' EXIT
# STUB_BUILD: `ok` builds; `mirror-blob` fails every build whose bases come through mirror.gcr.io the way Main #2264's
# did; `mirror-buildkit` fails the same way with BuildKit's "unknown blob"; `broken` fails every build on its own
# Dockerfile. A build's -f Dockerfile (else ./Dockerfile) is copied to $STUB_DOCKERFILE.
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
esac
STUB
chmod +x "$stub_dir/docker"
export STUB_LOG="$stub_dir/log" STUB_DOCKERFILE="$stub_dir/used-dockerfile"
cp "$REPO_ROOT/Dockerfile" "$stub_dir/Dockerfile"

# build <stub state> -> "<exit status>"
build() {
  export STUB_BUILD="$1"
  : > "$STUB_LOG"
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

spec_summary
