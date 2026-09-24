#!/usr/bin/env bash
# Point this clone's git hooks at scripts/hooks (the pre-push checks — see scripts/hooks/pre-push).
#
# core.hooksPath lives in the SHARED config, so one run covers the root checkout and every
# worktree of it, present and future. The path is RELATIVE, which git resolves against each
# worktree's own top level: every worktree runs the hook from its own checkout, and a worktree on
# a branch that predates the hook simply has none to run.
#
# Undo: git config --unset core.hooksPath
set -euo pipefail

cd "$(git rev-parse --show-toplevel)"

current="$(git config --get core.hooksPath || true)"
if [ -n "$current" ] && [ "$current" != scripts/hooks ]; then
    # A hook already installed at the old path would silently stop running; say which.
    own=$(find "$current" -maxdepth 1 -type f -perm -u+x ! -name '*.sample' 2>/dev/null || true)
    if [ -n "$own" ]; then
        echo "core.hooksPath is $current, which holds hooks that would stop running:" >&2
        echo "$own" >&2
        echo "Move them into scripts/hooks (or delete them), then re-run." >&2
        exit 1
    fi
fi

git config core.hooksPath scripts/hooks
chmod +x scripts/hooks/*
echo "core.hooksPath = scripts/hooks (was: ${current:-unset}). Bypass once with 'git push --no-verify'."
