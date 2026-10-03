#!/usr/bin/env bash
# Unpack an overlay convergence leg's identity overlay into the workspace, run from the repository
# root, and write the files it placed to <files-out> (what convergence-overlay-publish packs with
# whatever the leg records next).
#
#   restore-identity-overlay.sh <code> <overlay asset> <files-out>
#
# Reads FIXTURE_RELEASE_TAG and GH_TOKEN from the environment.
#
# The leg's own overlay (`identity-overlay-<code>-<corpus run>.tar.gz`) when the release holds it.
# A new recording has none yet; without one, every question only the model asks went live again
# after each recording (Wikidata rate-limiting a Poland leg for over 1,000 s), so the newest earlier
# overlay is carried forward instead (carry-overlay.sh), and with none at all the leg fills the
# model's gaps live and publishes the first.
#
# Only gh's own "no assets match" means the release lacks an overlay. Any other failure — a network
# blip, a 5xx, an expired token — is a read that did not happen: taken for "none yet", the leg asked
# every gap live and then published its overlay over the one it never read.
#
# Exit status: 0 with the overlay (or a carried one, or none to carry) in place; 4 when the release
# could not be read.
set -uo pipefail

code="${1:?usage: restore-identity-overlay.sh <code> <overlay asset> <files-out>}"
overlay="${2:?usage: restore-identity-overlay.sh <code> <overlay asset> <files-out>}"
files="${3:?usage: restore-identity-overlay.sh <code> <overlay asset> <files-out>}"
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
TAG="${FIXTURE_RELEASE_TAG:?FIXTURE_RELEASE_TAG names the rolling release}"

archives="overlay-archive"
errors="$(mktemp)"
trap 'rm -f "$errors"' EXIT
mkdir -p "$archives"
: > "$files"

unreadable() { echo "::error::could not read release $TAG for $1: $(tr '\n' ' ' < "$errors")"; exit 4; }

if gh release download "$TAG" --pattern "$overlay" --dir "$archives" --clobber 2>"$errors"; then
    tar -xzf "$archives/$overlay"
    tar -tzf "$archives/$overlay" | grep -v '/$' > "$files" || true
    echo "restored $overlay ($(wc -l < "$files" | tr -d ' ') files)"
    exit 0
fi
grep -q 'no assets match' "$errors" || unreadable "$overlay"

names=$(gh release view "$TAG" --json assets --jq '.assets[].name' 2>"$errors") || unreadable "its asset list"
previous=$(printf '%s\n' "$names" | grep -E "^identity-overlay-${code}-[0-9]+\.tar\.gz$" | sort -t- -k4 -n | tail -1 || true)
if [ -z "$previous" ]; then
    echo "no $overlay yet — this leg fills the model's gaps live and publishes the first"
    exit 0
fi
gh release download "$TAG" --pattern "$previous" --dir "$archives" --clobber 2>"$errors" || unreadable "$previous"
echo "no $overlay yet"
"$here/carry-overlay.sh" "$archives/$previous" "$files"
