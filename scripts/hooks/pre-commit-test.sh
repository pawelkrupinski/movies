#!/usr/bin/env bash
# Self-test for scripts/hooks/pre-commit: a commit in a scratch clone's root checkout is refused (bypassable on
# purpose), and one in its linked worktree goes through.
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
# shellcheck source=scripts/scratch-git.sh
. "$REPO_ROOT/scripts/scratch-git.sh"

testroot="$(cd "$(mktemp -d)" && pwd -P)"; trap 'rm -rf "$testroot"' EXIT
fails=0
check() { # <what> <expected> <actual>
  if [ "$2" = "$3" ]; then printf '  \033[32m✓\033[0m %s\n' "$1"
  else printf '  \033[31m✗\033[0m %s\n      expected: %s\n      actual:   %s\n' "$1" "$2" "$3"; fails=$((fails + 1)); fi
}

root="$testroot/clone"; wt="$testroot/wt"
scratch_repo "$root" -b main
echo a > "$root/a.txt"; scratch_git "$root" add -A; scratch_git "$root" commit -qm base
scratch_git "$root" worktree add -q -b feature "$wt"
mkdir -p "$root/scripts/hooks" "$wt/scripts/hooks"
cp "$REPO_ROOT/scripts/hooks/pre-commit" "$root/scripts/hooks/"; cp "$REPO_ROOT/scripts/hooks/pre-commit" "$wt/scripts/hooks/"
printf 'scripts/\n' >> "$root/.git/info/exclude" # the hook copies themselves stay out of the commits below
scratch_git "$root" config core.hooksPath scripts/hooks

head_of() { scratch_git "$1" rev-parse HEAD; }
before="$(head_of "$root")"
echo b >> "$root/a.txt"
out="$(scratch_git "$root" commit -qam "in the root" 2>&1)"
check "a commit in the root checkout is refused" "$before" "$(head_of "$root")"
check "...saying where to commit instead" 1 "$(printf '%s\n' "$out" | grep -c 'git worktree add')"
ALLOW_ROOT_COMMIT=1 scratch_git "$root" commit -qam "on purpose"
check "ALLOW_ROOT_COMMIT=1 lets a person commit there on purpose" 1 "$(scratch_git "$root" log -1 --format=%s | grep -c 'on purpose')"

before="$(head_of "$wt")"
echo c >> "$wt/a.txt"; scratch_git "$wt" commit -qam "in the worktree"
check "a commit in a linked worktree goes through" 1 "$([ "$(head_of "$wt")" != "$before" ] && echo 1 || echo 0)"

if [ "$fails" -ne 0 ]; then echo "pre-commit: $fails failed"; exit 1; fi
echo "pre-commit: all checks passed"
