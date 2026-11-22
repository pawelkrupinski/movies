#!/usr/bin/env bash
# Purpose: Pull one of .github/pinned-images.tsv's images by digest, from the first source that serves it, and refuse any other
# Pull one of .github/pinned-images.tsv's images BY DIGEST, from the first source that serves it, and refuse any other.
#
#   pull-pinned-image.sh <name>                  # pull; the local ref (<source>@sha256:...) on stdout, progress on stderr
#   pull-pinned-image.sh <name> -- <command...>  # pull, then exec <command> with PINNED_IMAGE=<that ref>
#   pull-pinned-image.sh --pin <name> <field>    # print the pin's image|repository|tag|digest|mirror; pulls nothing
#
# In CI it is run by ./.github/actions/pull-pinned-image, the one way a workflow or action pulls a pinned image
# (CiPinnedImageSpec); scripts/ci/start-mongo-replset.sh runs it itself only where nothing pulled first (locally).
#
# THE SOURCES, in order, each pull bounded and a stalled pull treated as a refused one:
#   1. the daemon's own Docker Hub mirror, when it has one (`docker info`): Docker Hub's name, served from its disk;
#   2. the GHCR mirror (this repo's package, filled by .github/workflows/mirror-pinned-images.yml), logged in with
#      GHCR_TOKEN (the job's GITHUB_TOKEN) when set -- PINNED_GHCR_PULL_SECONDS (60) a try, PINNED_GHCR_ATTEMPTS (2)
#      tries, the last one anonymous (a public package needs no token; a token without `packages: read` is refused);
#   3. mirror.gcr.io, Google's pull-through cache of Docker Hub;
#   4. Docker Hub itself, logged in as DOCKERHUB_USERNAME/DOCKERHUB_TOKEN when both are set.
# Sources 3 and 4 get PINNED_PULL_SECONDS (120) each. Every token goes to `docker login` on stdin, never argv.
#
# WHY GHCR FIRST: Identity model convergence #1286 (run 44526198751, 2026-11-19) lost a leg before any build ran --
# mirror.gcr.io stalled past 120 s, Docker Hub refused the CI account's expired token, then stalled anonymously.
# GitHub's own registry, reached from GitHub's own runners with the job's own token, depends on neither.
#
# BY DIGEST, ALWAYS: every pull names the pin's sha256, so each source can only hand over the pinned bytes, and the
# pulled image's RepoDigests must name it too before its ref is printed. A tag that moved upstream changes nothing here.
set -uo pipefail
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
pins=${PINNED_IMAGES:-$here/../../.github/pinned-images.tsv}

# pin <name> <field>
pin() {
  local row image
  row=$(awk -F'\t' -v n="$1" '$1 == n { print; exit }' "$pins")
  [ -n "$row" ] || { echo "::error::$pins pins no image named $1" >&2; return 1; }
  image=$(cut -f2 <<< "$row")
  case "$2" in
    image)      echo "$image" ;;
    repository) echo "${image%%:*}" ;;
    tag)        local t=${image#*:}; echo "${t%%@*}" ;;
    digest)     echo "${image#*@}" ;;
    mirror)     cut -f3 <<< "$row" ;;
    *) echo "unknown field $2" >&2; return 64 ;;
  esac
}

if [ "${1:-}" = --pin ]; then
  [ "$#" -eq 3 ] || { echo "usage: $0 --pin <name> image|repository|tag|digest|mirror" >&2; exit 64; }
  pin "$2" "$3"; exit
fi
name=${1:-}
[ -n "$name" ] || { echo "usage: $0 <name> [-- command...] | --pin <name> <field>" >&2; exit 64; }
shift
if [ "$#" -gt 0 ]; then
  [ "$1" = -- ] && [ "$#" -gt 1 ] || { echo "usage: $0 <name> [-- command...]" >&2; exit 64; }
  shift
fi

repository=$(pin "$name" repository) || exit 1
digest=$(pin "$name" digest) || exit 1
mirror=$(pin "$name" mirror) || exit 1
[[ "$digest" =~ ^sha256:[0-9a-f]{64}$ ]] || { echo "::error::$name's pin names no sha256 digest ($digest)" >&2; exit 1; }
hub=${repository#docker.io/}

# The pulled image carries the pin's digest, or it is not the pinned image.
verified() {
  docker image inspect --format '{{range .RepoDigests}}{{println .}}{{end}}' "$1" 2>/dev/null | grep -q "@$digest\$"
}

# pull <ref> <seconds>
pull() {
  local status pulled=$SECONDS
  echo "pulling $1 (at most ${2}s)" >&2
  timeout -k 10 "$2" docker pull -q "$1" >&2
  status=$?
  echo "the pull of $1 ended (status $status) after $((SECONDS - pulled))s" >&2
  [ "$status" -eq 124 ] && echo "::warning::the pull of $1 stalled past ${2}s" >&2
  [ "$status" -eq 0 ] || return "$status"
  verified "$1" && return 0
  echo "::error::$1 was pulled but does not carry $digest" >&2
  return 1
}

login() { # login <registry> <user> <token>
  printf '%s' "$3" | timeout -k 5 30 docker login "$1" -u "$2" --password-stdin >&2 \
    || echo "::warning::could not log in to $1; pulling from it anonymously" >&2
}

pull_seconds=${PINNED_PULL_SECONDS:-120}
ghcr_seconds=${PINNED_GHCR_PULL_SECONDS:-60}
ghcr_attempts=${PINNED_GHCR_ATTEMPTS:-2}
hub_ref="docker.io/$hub@$digest"
ghcr_ref="$mirror@$digest"
mirror_gcr_ref="mirror.gcr.io/$hub@$digest"

ref=""
# Already here (an earlier step pulled it): nothing to fetch.
for candidate in "$ghcr_ref" "$mirror_gcr_ref" "$hub_ref"; do
  verified "$candidate" && { ref=$candidate; break; }
done
if [ -z "$ref" ] && [ -n "$(docker info --format '{{range .RegistryConfig.Mirrors}}{{.}} {{end}}' 2>/dev/null)" ] &&
    pull "$hub_ref" "$pull_seconds"; then
  ref=$hub_ref
fi
if [ -z "$ref" ]; then
  [ -n "${GHCR_TOKEN:-}" ] && login "${mirror%%/*}" "${GITHUB_ACTOR:-github-actions}" "$GHCR_TOKEN"
  for attempt in $(seq 1 "$ghcr_attempts"); do
    if [ "$attempt" -eq "$ghcr_attempts" ] && [ "$attempt" -gt 1 ] && [ -n "${GHCR_TOKEN:-}" ]; then
      docker logout "${mirror%%/*}" >&2 || true   # the last try anonymous: a public package needs no token
    fi
    pull "$ghcr_ref" "$ghcr_seconds" && { ref=$ghcr_ref; break; }
  done
fi
if [ -z "$ref" ]; then
  echo "::warning::$name: the GHCR mirror ($mirror) did not serve $digest; trying mirror.gcr.io" >&2
  pull "$mirror_gcr_ref" "$pull_seconds" && ref=$mirror_gcr_ref
fi
if [ -z "$ref" ]; then
  echo "::warning::$name: mirror.gcr.io refused or stalled the pull; falling back to Docker Hub" >&2
  if [ -n "${DOCKERHUB_USERNAME:-}" ] && [ -n "${DOCKERHUB_TOKEN:-}" ]; then
    login docker.io "$DOCKERHUB_USERNAME" "$DOCKERHUB_TOKEN"
  fi
  pull "$hub_ref" "$pull_seconds" && ref=$hub_ref
fi
[ -n "$ref" ] || { echo "::error::no source served $name ($repository@$digest)" >&2; exit 1; }

echo "$name: $ref" >&2
if [ "$#" -gt 0 ]; then
  PINNED_IMAGE=$ref exec "$@"
fi
printf '%s\n' "$ref"
