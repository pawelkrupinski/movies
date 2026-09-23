#!/usr/bin/env bash
# Point a rolling prerelease at this commit and replace its assets.
#
# An UPSERT, never delete + recreate: deleting the release (with --cleanup-tag) opens a window
# in which the tag does not exist, which is what the 2026-09-04 403 hit. `view` decides edit vs
# create; it goes through gh-release.sh so a transient 403 on the check is retried rather than
# read as "missing" and answered with a create that would then fail on the existing tag.
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

if "$release" view "$tag" --repo "$repo" >/dev/null 2>&1; then
  "$release" edit "$tag" --repo "$repo" --target "$sha" --prerelease --notes "$notes"
else
  "$release" create "$tag" --repo "$repo" --target "$sha" --prerelease \
    --title "$title" --notes "$notes"
fi
"$release" upload "$tag" --repo "$repo" --clobber "$@"
