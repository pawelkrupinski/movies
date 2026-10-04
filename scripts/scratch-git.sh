#!/usr/bin/env bash
# Throwaway git repositories for the shell test scripts — and nothing else, ever.
#
# WHY IT EXISTS. Every worktree of this clone, and every agent session working one, commits through
# ONE shared `.git` (config, refs, stash, hooks path). On 2026-10-04 scripts/hooks/pre-push-test.sh
# ran as the pre-push hook's own check; git exports GIT_DIR / GIT_INDEX_FILE / GIT_WORK_TREE to a hook,
# so its "scratch repository" commands acted on the repository being PUSHED: five "Spec" commits onto
# the branch, and `user.name=Spec`, `core.bare=true` (from a `git init --bare`) written into the shared
# config every other session then committed through. `git -C <dir>` does not help — an exported
# GIT_DIR beats the directory. Neither does a per-repo `git config user.name`: it lands wherever
# GIT_DIR points.
#
# SOURCING THIS FILE isolates the calling shell and every child it starts (the script under test,
# the hook under test, a stub that shells out) — for the rest of that shell's life:
#   - every GIT_* variable is unset, so git finds its repository from the directory it runs in;
#   - the developer's ~/.gitconfig and the system config are not read (GIT_CONFIG_GLOBAL=/dev/null,
#     GIT_CONFIG_NOSYSTEM), so a signing key, a hooks path or an alias of theirs cannot change what a
#     spec sees, and no `git config --global` from a spec can write there;
#   - a fixed identity and unsigned commits are supplied as command-line-scope config, so no spec
#     needs `git config user.*` at all.
#
# Usage:
#   . "$REPO_ROOT/scripts/scratch-git.sh"
#   repo="$(mktemp -d)/repo"
#   scratch_repo "$repo"                 # git init (extra init flags may follow: --bare, -b main)
#   scratch_git "$repo" commit -qm base  # git -C "$repo" ..., refusing this checkout itself
#
# scripts/scratch-git-test.sh proves the isolation against a decoy repository; the
# ScratchGitIsolationSpec lint fails the build on a test script that calls `git` directly.

while IFS= read -r _scratch_git_var; do
  unset "$_scratch_git_var"
done < <(compgen -e | grep '^GIT_')
unset _scratch_git_var

export GIT_CONFIG_NOSYSTEM=1 GIT_CONFIG_GLOBAL=/dev/null
export GIT_CONFIG_COUNT=5 \
  GIT_CONFIG_KEY_0=user.name        GIT_CONFIG_VALUE_0=Spec \
  GIT_CONFIG_KEY_1=user.email       GIT_CONFIG_VALUE_1=spec@example.test \
  GIT_CONFIG_KEY_2=commit.gpgsign   GIT_CONFIG_VALUE_2=false \
  GIT_CONFIG_KEY_3=tag.gpgsign      GIT_CONFIG_VALUE_3=false \
  GIT_CONFIG_KEY_4=init.defaultBranch GIT_CONFIG_VALUE_4=main

# The checkout this file belongs to: the one repository a scratch command must never reach.
_scratch_git_own_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd -P)"

# scratch_git <repo> <git args...> — `git -C <repo>`, refused for this checkout or anything inside it.
scratch_git() {
  local repo="$1" resolved
  shift
  resolved="$(cd "$repo" 2>/dev/null && pwd -P)" || { printf 'scratch_git: no directory %s\n' "$repo" >&2; return 2; }
  case "$resolved/" in
    "$_scratch_git_own_root"/*)
      printf 'scratch_git: refusing %s — it is inside this checkout, not a scratch repository\n' "$repo" >&2
      return 2 ;;
  esac
  git -C "$resolved" "$@"
}

# scratch_repo <dir> [git init flags...] — creates <dir> and initialises a repository in it.
scratch_repo() {
  local repo="$1"
  shift
  mkdir -p "$repo" && scratch_git "$repo" init -q "$@"
}
