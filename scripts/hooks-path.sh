#!/usr/bin/env bash
# What an ABSOLUTE core.hooksPath names, for the two tools that mend one: scripts/install-hooks.sh and the pre-push hook.
# An IDE's git integration rewrote the shared config's relative `scripts/hooks` to an absolute path (2026-10-14); a path
# to scripts/hooks of any worktree of this clone holds only this repository's own hooks, so setting it back to relative
# loses nothing. Sourcing defines one function and runs nothing.
#
#   . scripts/hooks-path.sh
#   hooks_path_is_own <path>    true when <path> is scripts/hooks of some worktree of the repository git finds here

hooks_path_is_own() { # <path>
  local dir wt
  dir="$(cd "$1" 2>/dev/null && pwd -P)" || return 1
  while IFS= read -r wt; do
    [ "$dir" = "$(cd "$wt" 2>/dev/null && pwd -P)/scripts/hooks" ] && return 0
  done < <(git worktree list --porcelain | sed -n 's/^worktree //p')
  return 1
}
