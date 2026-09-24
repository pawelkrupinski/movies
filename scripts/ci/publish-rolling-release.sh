#!/usr/bin/env bash
# Point a rolling prerelease at this commit and replace its assets.
#
# An UPSERT, never delete + recreate: deleting the release (with --cleanup-tag) opens a window
# in which the tag does not exist, which is what the 2026-09-04 403 hit. `view` decides edit vs
# create; it goes through gh-release.sh so a transient failure of the check is retried, and only
# gh's "release not found" is read as missing — never a 403, which a create would only repeat
# against the existing tag.
#
# Usage: publish-rolling-release.sh <tag> <title> <notes> <asset>...
#   needs GH_TOKEN (contents: write), GITHUB_REPOSITORY and GITHUB_SHA — as in any Actions step.
# Tested by scripts/ci/gh-release-test.sh against a stub `gh`.
set -euo pipefail

tag="${1:?tag}"
title="${2:?title}"
notes="${3:?notes}"
shift 3
[ "$#" -gt 0 ] || { echo "no assets to publish" >&2; exit 2; }

release="$(dirname "${BASH_SOURCE[0]}")/gh-release.sh"
repo="${GITHUB_REPOSITORY:?}"
sha="${GITHUB_SHA:?}"

# Only gh's own "release not found" means create: any other failed view (a refused 403) is
# the error to report, not a cue to create a release whose tag already exists.
if view_err="$("$release" view "$tag" --repo "$repo" 2>&1 >/dev/null)"; then
  "$release" edit "$tag" --repo "$repo" --target "$sha" --prerelease --notes "$notes"
elif grep -q 'release not found' <<< "$view_err"; then
  "$release" create "$tag" --repo "$repo" --target "$sha" --prerelease \
    --title "$title" --notes "$notes"
else
  printf '%s\n' "$view_err" >&2
  exit 1
fi
"$release" upload "$tag" --repo "$repo" --clobber "$@"
