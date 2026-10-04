#!/usr/bin/env bash
# scratch-git.sh: a test's scratch repository stays scratch even when the shell carries the git
# environment a hook hands its children, and even when the developer's ~/.gitconfig says otherwise.
# Run: bash scripts/scratch-git-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
# This spec's own setup must not leak either: isolate first, then hand the decoy environment to the
# subshell below and source the helper again there, as a test running under a hook would.
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m scratch-git.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

# A decoy for "the repository being pushed", set up the way git sets up a hook's environment, and a
# home whose ~/.gitconfig would sign and name every commit if it were read.
decoy="$work/decoy"
mkdir -p "$work/home"
scratch_repo "$decoy"
scratch_git "$decoy" commit -q --allow-empty -m owner
decoy_config="$(cat "$decoy/.git/config")"
decoy_head="$(scratch_git "$decoy" rev-parse HEAD)"
printf '[user]\n\tname = Leaked\n[commit]\n\tgpgsign = true\n' > "$work/home/.gitconfig"

out="$(
  export GIT_DIR="$decoy/.git" GIT_INDEX_FILE="$decoy/.git/index" GIT_WORK_TREE="$decoy" HOME="$work/home"
  . "$REPO_ROOT/scripts/scratch-git.sh"
  repo="$work/scratch"
  scratch_repo "$repo"
  echo x > "$repo/f"
  scratch_git "$repo" add f
  scratch_git "$repo" commit -qm scratch
  scratch_repo "$work/origin.git" --bare
  printf 'author=%s\n' "$(scratch_git "$repo" log -1 --format=%an)"
  printf 'branch=%s\n' "$(scratch_git "$repo" rev-parse --abbrev-ref HEAD)"
  printf 'global=%s\n' "$(scratch_git "$repo" config --global user.name 2>/dev/null)"
  printf 'leftover=%s\n' "$(compgen -e | grep -E '^GIT_(DIR|INDEX_FILE|WORK_TREE)$' | tr '\n' ' ')"
  scratch_git "$REPO_ROOT" status >/dev/null 2>&1; printf 'own-checkout=%s\n' "$?"
  scratch_git "$REPO_ROOT/scripts" status >/dev/null 2>&1; printf 'own-subdirectory=%s\n' "$?"
)"
field() { printf '%s\n' "$out" | sed -n "s/^$1=//p"; }

check "the decoy repository gains no commit" "$decoy_head" "$(scratch_git "$decoy" rev-parse HEAD)"
check "the decoy repository's config is untouched (no Spec user, no core.bare)" "$decoy_config" "$(cat "$decoy/.git/config")"
check "the scratch repository got the commit" "scratch" "$(scratch_git "$work/scratch" log -1 --format=%s 2>/dev/null)"
check "commits as Spec, not as the developer's ~/.gitconfig user" "Spec" "$(field author)"
check "a scratch repository starts on main" "main" "$(field branch)"
check "the developer's ~/.gitconfig is not read" "" "$(field global)"
check "the hook's repository variables are gone from the shell" "" "$(field leftover)"
check "a bare scratch repository is made where it was asked for" "true" \
  "$(scratch_git "$work/origin.git" rev-parse --is-bare-repository 2>/dev/null)"
check "scratch_git refuses this checkout" "2" "$(field own-checkout)"
check "...and any directory inside it" "2" "$(field own-subdirectory)"

spec_summary
