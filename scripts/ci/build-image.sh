#!/usr/bin/env bash
# Purpose: Build one of the Dockerfile's images for linux/amd64 and push it under the commit's SHA tag only (main.yml's publish jobs give it the tags that ship)
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
# fetched layers can name the same blobs. A build that lost the image registry itself waits for it to come back and builds once more (below). Any
# other failure is the build's verdict as it stands.
#
# Tested by scripts/ci/build-image-test.sh against a stub `docker`, `curl` and `date`.
set -uo pipefail

bin="${1:?usage: $0 <bin> <image repository> <commit sha>}"
repo="${2:?usage: $0 <bin> <image repository> <commit sha>}"
sha="${3:?usage: $0 <bin> <image repository> <commit sha>}"

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

output=--push

build() {
  docker buildx build --platform linux/amd64 "$output" \
      --build-arg BIN="$bin" --build-arg COMMIT_SHA="$sha" \
      --tag "$repo:$sha" --cache-to type=inline "$@" .
}

build --cache-from "type=registry,ref=$repo:latest" 2>&1 | tee "$work/mirror.log"
status=${PIPESTATUS[0]}
[ "$status" -eq 0 ] && exit 0
# THE REGISTRY GONE. A build that lost the image registry itself -- a refused connection, a reset, a 502 from the proxy
# in front of it, a transfer cut short -- is no verdict on the build: Main #2361 (2026-11-09 23:33Z) lost
# registry.kinowo.net mid-pull while a host switch rolled out ("short read ... unexpected EOF", then "connect:
# connection refused"); Main #2662 (2026-11-19 04:00:22Z) lost it mid-push to the daily 04:00 UTC `registry-prune`
# (infra/nix/modules/roles/registry.nix), which stops the registry for its garbage-collect ("write: connection reset by
# peer", then "502 Bad Gateway"). Both outages are minutes long and END: the build waits, bounded, until the registry
# answers again and builds once more (the builder's own cache makes that the push), saying on the run what it lost
# and how long it waited -- never a silent retry. A registry still gone at the deadline fails naming it.
registry=${repo%%/*}
lost=$(grep -F "$registry" "$work/mirror.log" \
  | grep -oE 'connection refused|connection reset by peer|i/o timeout|no such host|502 Bad Gateway|503 Service Unavailable' \
  | head -n 1)
[ -z "$lost" ] && grep -q 'short read: expected [0-9]* bytes' "$work/mirror.log" && grep -qF "$registry" "$work/mirror.log" \
  && lost="a transfer cut short"
if [ -n "$lost" ]; then
  hhmm=$(date -u +%H%M)
  why="the image registry was down or restarting (a host switch, or its daily prune's garbage collect)"
  [ "$hhmm" -ge 0400 ] && [ "$hhmm" -lt 0420 ] \
    && why="the image registry was stopped by its daily 04:00 UTC registry-prune (infra/nix/modules/roles/registry.nix)"
  wait_limit=${REGISTRY_WAIT_SECONDS:-600}
  poll=${REGISTRY_POLL_SECONDS:-15}
  waited_from=$SECONDS
  answer=000
  while :; do
    # 200, or 401 asking for the login docker/login-action already did: the registry is serving again.
    answer=$(curl -s -o /dev/null -w '%{http_code}' --max-time 10 "https://$registry/v2/") || answer=000
    case "$answer" in 200|401) break ;; esac
    [ $((SECONDS - waited_from)) -ge "$wait_limit" ] && break
    echo "the $bin image's build lost $registry ($lost): it answers $answer, waiting for it ($((SECONDS - waited_from)) s of ${wait_limit} s)"
    sleep "$poll"
  done
  case "$answer" in
    200|401) ;;
    *) echo "::error::the $bin image's build lost $registry: $lost, and it still answered $answer after ${wait_limit} s -- $why, not the build"
       exit "$status" ;;
  esac
  echo "::warning::the $bin image's build lost $registry: $lost -- $why; it answered again after $((SECONDS - waited_from)) s: building once more"
  build --cache-from "type=registry,ref=$repo:latest"
  exit
fi
grep -qE 'could not fetch content descriptor|unknown blob' "$work/mirror.log" || exit "$status"

echo "::warning::the $bin image's build could not fetch a blob through mirror.gcr.io: building it again from Docker Hub"
# The same pinned digests under Docker Hub's names; the context's .dockerignore still applies (there is no
# Dockerfile.dockerignore beside this copy).
sed 's#^FROM mirror\.gcr\.io/library/#FROM docker.io/library/#' Dockerfile > "$work/Dockerfile"
build -f "$work/Dockerfile"
