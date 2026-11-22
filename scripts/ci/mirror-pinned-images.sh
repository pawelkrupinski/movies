#!/usr/bin/env bash
# Purpose: Copy every image .github/pinned-images.tsv pins to its GHCR mirror by digest, and check that GHCR serves exactly the pin
# Copy every image .github/pinned-images.tsv pins to its GHCR mirror BY DIGEST, and check that GHCR serves exactly the pin.
# Run by .github/workflows/mirror-pinned-images.yml (weekly, on dispatch, on a push to main changing the pins).
#
#   mirror-pinned-images.sh
#
# For each pin: GHCR's `<mirror>:<tag>` is read (its raw manifest's sha256). The pin's digest: nothing to do. Missing,
# or another digest (DRIFT: the pin was bumped and GHCR not refreshed, or someone pushed over the tag): the pinned digest
# is copied over it with `skopeo copy --all --preserve-digests` -- every platform, the index byte for byte, so the GHCR
# image's digest IS the upstream digest -- from Docker Hub, else mirror.gcr.io. Then GHCR is read again, and anything
# but the pin fails the run, loudly: CI's pulls (scripts/ci/pull-pinned-image.sh) would skip GHCR for that image.
# The upstream TAG pointing elsewhere than the pin (an official image rebuilt under the same tag) is only a notice:
# the pin is a digest, so nothing CI runs moved; bumping it is infra/bin/nix-update's (mongo) or a deliberate edit.
#
# GHCR_TOKEN (the job's GITHUB_TOKEN, `packages: write`) logs in to ghcr.io as GITHUB_ACTOR; DOCKERHUB_USERNAME and
# DOCKERHUB_TOKEN, when both set, log in to Docker Hub for the copy's reads (anonymous otherwise, or when refused).
# Tokens go to `skopeo login` on stdin, never argv. Every registry call is bounded (MIRROR_COPY_SECONDS, default 1800,
# for a copy; 120 s for a read). Tested by scripts/ci/mirror-pinned-images-test.sh against a stub skopeo.
set -uo pipefail
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
pins=${PINNED_IMAGES:-$here/../../.github/pinned-images.tsv}
copy_seconds=${MIRROR_COPY_SECONDS:-1800}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

sha256() { if command -v sha256sum > /dev/null; then sha256sum; else shasum -a 256; fi | cut -d' ' -f1; }

# served <docker://ref>: the sha256 of the manifest the registry serves for it, empty when it serves none
served() {
  timeout -k 10 120 skopeo inspect --raw --retry-times 2 "$1" > "$work/manifest" 2> "$work/inspect.err" || return 0
  echo "sha256:$(sha256 < "$work/manifest")"
}

login() { # login <registry> <user> <token>
  printf '%s' "$3" | timeout -k 5 60 skopeo login "$1" -u "$2" --password-stdin \
    || echo "::warning::could not log in to $1 as $2"
}

[ -n "${GHCR_TOKEN:-}" ] && login ghcr.io "${GITHUB_ACTOR:-github-actions}" "$GHCR_TOKEN"
if [ -n "${DOCKERHUB_USERNAME:-}" ] && [ -n "${DOCKERHUB_TOKEN:-}" ]; then
  login docker.io "$DOCKERHUB_USERNAME" "$DOCKERHUB_TOKEN"
fi

failed=0
rows=0
while IFS=$'\t' read -r name image mirror; do
  case "$name" in ''|'#'*) continue ;; esac
  rows=$((rows + 1))
  repository=${image%%:*}; tag=${image#*:}; tag=${tag%%@*}; digest=${image#*@}
  if ! [[ "$digest" =~ ^sha256:[0-9a-f]{64}$ ]] || [ -z "$mirror" ]; then
    echo "::error::$name: the pin is not <repository>:<tag>@sha256:<digest> TAB <mirror> ($image $mirror)"
    failed=1; continue
  fi
  target="docker://$mirror:$tag"
  now=$(served "$target")
  if [ "$now" = "$digest" ]; then
    echo "$name: $mirror:$tag is the pin, $digest"
  else
    if [ -n "$now" ]; then
      echo "::warning::$name: DRIFT -- GHCR's $mirror:$tag is $now, the pin is $digest; copying the pin over it"
    else
      echo "$name: $mirror:$tag is not on GHCR yet; copying $digest"
    fi
    copied=""
    for source in "docker://$repository@$digest" "docker://mirror.gcr.io/${repository#docker.io/}@$digest"; do
      echo "$name: copying $source to $mirror:$tag"
      if timeout -k 30 "$copy_seconds" skopeo copy --all --preserve-digests --retry-times 3 "$source" "$target"; then
        copied=$source; break
      fi
      echo "::warning::$name: the copy from $source failed"
    done
    now=$(served "$target")
    if [ "$now" != "$digest" ]; then
      echo "::error::$name: GHCR's $mirror:$tag is ${now:-missing} after the copy${copied:+ from $copied}, not the pin $digest -- CI pulls of $name skip GHCR until it is"
      failed=1; continue
    fi
    echo "$name: $mirror:$tag is now the pin, $digest (from $copied)"
  fi
  upstream=$(served "docker://$repository:$tag")
  if [ -n "$upstream" ] && [ "$upstream" != "$digest" ]; then
    echo "::notice::$name: $repository:$tag now points at $upstream, not the pinned $digest (CI keeps the pin)"
  fi
done < "$pins"

[ "$rows" -gt 0 ] || { echo "::error::$pins pins no image"; exit 1; }
exit "$failed"
