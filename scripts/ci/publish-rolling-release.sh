#!/usr/bin/env bash
# Point a rolling prerelease — and its git tag — at this commit and replace its assets.
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
moved_by_create=no
if view_err="$("$release" view "$tag" --repo "$repo" 2>&1 >/dev/null)"; then
  "$release" edit "$tag" --repo "$repo" --target "$sha" --prerelease --notes "$notes"
elif grep -q 'release not found' <<< "$view_err"; then
  "$release" create "$tag" --repo "$repo" --target "$sha" --prerelease \
    --title "$title" --notes "$notes"
  moved_by_create=yes
else
  printf '%s\n' "$view_err" >&2
  exit 1
fi
"$release" upload "$tag" --repo "$repo" --clobber "$@"
# `--target` only places a tag that does not exist yet (GitHub ignores target_commitish for an
# existing one), so an edited release kept its tag on the commit it was first created at —
# android-latest sat on 199465a from 09-04 while its assets moved on. Force-move the tag last,
# once the assets are out, so a refused move cannot withhold the build — but still fails the
# step rather than leaving the tag silently stale. Through the REST refs API, never `git push`:
# GITHUB_TOKEN has no `workflows` permission, and GitHub refuses its tag PUSH whenever the
# range touches .github/workflows; the refs API is not held to that — run 36030022187 moved
# android-latest 199465a → ed300da54 over a range full of workflow edits.
if [ "$moved_by_create" = no ]; then
  gh api -X PATCH "repos/$repo/git/refs/tags/$tag" -f sha="$sha" -F force=true >/dev/null
fi
