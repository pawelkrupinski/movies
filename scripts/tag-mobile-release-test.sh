#!/usr/bin/env bash
#
# tag-mobile-release.sh's two failure paths (a bad local tag, a broken push) are both
# meant to warn and exit 0, never fail the caller — ios-release.sh runs this right after
# a successful store upload, and the Android CI workflow runs it after a successful Play
# publish, so a failure here would report an already-shipped release as broken. Exercised
# against a throwaway repo with its own remote, never this one.
#
#   scripts/tag-mobile-release-test.sh
#
set -uo pipefail

HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
. "$HERE/shell-spec.sh"

printf '\033[36m▸\033[0m tag-mobile-release.sh\n'

SCRATCH="$(mktemp -d)"
trap 'rm -rf "$SCRATCH"' EXIT

# The script resolves its own repo root from its own file location (see its `repo_root=`
# line), so isolating it means copying it — and the log.sh it sources — into a throwaway
# repo, not just running it with a different cwd.
REPO="$SCRATCH/repo"
mkdir -p "$REPO/scripts"
cp "$HERE/tag-mobile-release.sh" "$HERE/log.sh" "$REPO/scripts/"
git -C "$REPO" init -q
git -C "$REPO" config user.email t@t
git -C "$REPO" config user.name t
echo x > "$REPO/f"
git -C "$REPO" add f
git -C "$REPO" -c commit.gpgsign=false commit -q -m init
git init -q --bare "$SCRATCH/origin.git"
git -C "$REPO" remote add origin "$SCRATCH/origin.git"
git -C "$REPO" push -q origin HEAD:refs/heads/main

# ── the happy path: tags and pushes ────────────────────────────────────────────
bash "$REPO/scripts/tag-mobile-release.sh" ios 1.0.0 >/dev/null 2>&1
check "exits 0 on a clean tag+push" "0" "$?"
check "creates the tag locally" "mobile-ios-1.0.0" "$(git -C "$REPO" tag -l mobile-ios-1.0.0)"
check "pushes the tag to origin" "1" \
  "$(git -C "$REPO" ls-remote origin | grep -c mobile-ios-1.0.0)"

# ── a broken remote: warns, does not fail ──────────────────────────────────────
git -C "$REPO" remote set-url origin /nonexistent-path
push_output="$(bash "$REPO/scripts/tag-mobile-release.sh" android 2.0.0 2>&1)"
check "exits 0 even when the push fails" "0" "$?"
check "still creates the tag locally" "mobile-android-2.0.0" \
  "$(git -C "$REPO" tag -l mobile-android-2.0.0)"
# The tag is FORCE-moved on every re-upload of a version, so the manual retry it
# suggests must force too — a plain `git push origin <tag>` is rejected as
# "already exists" whenever origin still holds the tag's previous position.
check "suggests a FORCE push as the manual retry" "1" \
  "$(printf '%s' "$push_output" | grep -c 'git push -f origin mobile-android-2.0.0')"
git -C "$REPO" remote set-url origin "$SCRATCH/origin.git"

# ── a version string git can't use as a ref: warns, does not fail ─────────────
bash "$REPO/scripts/tag-mobile-release.sh" ios "1.0.0 with a space" >/dev/null 2>&1
check "exits 0 even when the local tag can't be created" "0" "$?"
check "creates no tag for the unusable version" "0" \
  "$(git -C "$REPO" tag -l | grep -c "1.0.0 with a space")"

# ── the optional sha argument, not just HEAD ───────────────────────────────────
first=$(git -C "$REPO" rev-parse HEAD)
echo y >> "$REPO/f"
git -C "$REPO" add f
git -C "$REPO" -c commit.gpgsign=false commit -q -m second
bash "$REPO/scripts/tag-mobile-release.sh" ios 3.0.0 "$first" >/dev/null 2>&1
check "tags the given sha argument, not HEAD" "$first" "$(git -C "$REPO" rev-parse mobile-ios-3.0.0)"

spec_summary
