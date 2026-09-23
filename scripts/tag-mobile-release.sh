#!/usr/bin/env bash
#
# Force-tags the commit that actually produced a shipped mobile store build, for
# the mobile-releases dashboard (infra/version-dashboard/app.py:release_commit_for)
# to anchor on instead of the "Release mobile X.Y.Z" bump commit -- see that
# function's docstring for why the bump commit alone is often wrong.
#
#   scripts/tag-mobile-release.sh <ios|android> <version> [sha]
#
# sha defaults to HEAD. Shared between ios-release.sh (local, after a
# successful altool upload) and android.yml (CI, after a successful Play
# publish) so the two callers can't drift on error handling the way they
# already had once -- one warned on a failed push, the other let it fail the
# whole job. Neither a failed local tag creation nor a failed push is fatal
# here: the store upload this tag is only bookkeeping for has already
# succeeded by the time this runs, and failing the caller's job over a
# best-effort dashboard tag would report a shipped release as broken.
set -euo pipefail

platform="${1:?usage: tag-mobile-release.sh <ios|android> <version> [sha]}"
version="${2:?usage: tag-mobile-release.sh <ios|android> <version> [sha]}"
sha="${3:-HEAD}"
repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

. "$repo_root/scripts/log.sh"

tag="mobile-$platform-$version"
if ! git -C "$repo_root" tag -f "$tag" "$sha" >/dev/null 2>&1; then
  warn "could not create tag $tag locally — not pushing"
  exit 0
fi
git -C "$repo_root" push -f origin "$tag" >/dev/null 2>&1 \
  || warn "could not push tag $tag — push it manually: git push -f origin $tag"
