#!/usr/bin/env bash
# Build one of the Dockerfile's images for linux/amd64 and push it under the commit's SHA tag ONLY (main.yml's
# publish jobs give it the tags that ship). Run from the repository root, the build context, with buildx's default
# builder set up by .github/actions/setup-buildx.
#
#   build-image.sh <bin: web|worker> <image repository> <commit sha>
#
# amd64 ONLY: the cluster is Hetzner x86_64. INLINE cache, restored from the image last shipped (`latest`): the layers
# worth reusing (base OS, JRE, libvips) are all in it, and inline metadata rides the push itself.
#
# THE MIRROR'S FALLBACK. The Dockerfile's bases come through mirror.gcr.io (CiImageMirrorSpec), which at times answers
# for blobs it served minutes before as if they were gone: run 38788714642 (2026-10-15, "could not fetch content
# descriptor ... not found") and Main #2264 (2026-10-28, BuildKit's pull "unknown blob", then the ubuntu base's
# 42 MB layer "could not fetch content descriptor ... not found", served again a few minutes later). A build that
# fails THAT way is built once more from the same digests on Docker Hub, and without the cache import, whose lazily
# fetched layers can name the same blobs. Any other failure is the build's verdict as it stands.
#
# Tested by scripts/ci/build-image-test.sh against a stub `docker`.
set -uo pipefail

bin="${1:?usage: $0 <bin> <image repository> <commit sha>}"
repo="${2:?usage: $0 <bin> <image repository> <commit sha>}"
sha="${3:?usage: $0 <bin> <image repository> <commit sha>}"

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# SHADOW (scripts/ci/ci-writes.sh): a github.com shadow run still BUILDS the image -- the Dockerfile and the staged
# dist are checked -- but pushes nothing to the registry (a SHA-tagged push is one manifest copy from shipping).
. "$(dirname "${BASH_SOURCE[0]}")/ci-writes.sh" || { echo "::error::$0: scripts/ci/ci-writes.sh is missing beside it -- its shadow gate cannot be read" >&2; exit 2; }
output=--push
ci_writes || { ci_writes_skipped "the $bin image push"; output=--load; }

build() {
  docker buildx build --platform linux/amd64 "$output" \
      --build-arg BIN="$bin" --build-arg COMMIT_SHA="$sha" \
      --tag "$repo:$sha" --cache-to type=inline "$@" .
}

build --cache-from "type=registry,ref=$repo:latest" 2>&1 | tee "$work/mirror.log"
status=${PIPESTATUS[0]}
[ "$status" -eq 0 ] && exit 0
grep -qE 'could not fetch content descriptor|unknown blob' "$work/mirror.log" || exit "$status"

echo "::warning::the $bin image's build could not fetch a blob through mirror.gcr.io: building it again from Docker Hub"
# The same pinned digests under Docker Hub's names; the context's .dockerignore still applies (there is no
# Dockerfile.dockerignore beside this copy).
sed 's#^FROM mirror\.gcr\.io/library/#FROM docker.io/library/#' Dockerfile > "$work/Dockerfile"
build -f "$work/Dockerfile"
