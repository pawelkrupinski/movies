#!/usr/bin/env bash
# Self-test for scripts/hooks/post-commit: a commit the commit rules refuse is named on stderr, with the rule's own
# reason, and still made; a commit they pass says nothing; KINOWO_POST_COMMIT_RULES=0 runs nothing.
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

repo="$testroot/repo"
scratch_repo "$repo" -b main
mkdir -p "$repo/scripts/hooks" "$repo/scripts/ci"
cp "$REPO_ROOT/scripts/hooks/post-commit" "$repo/scripts/hooks/"
# The rules stub: refuses a commit whose subject says "refuse", naming the rule and the range it was given.
cat > "$repo/scripts/ci/commit_rules.py" <<'PY'
import subprocess, sys
subject = subprocess.run(["git", "log", "-1", "--format=%s"], capture_output=True, text=True).stdout
if "refuse" in subject:
    print(f"{sys.argv[1]}: {subject.strip()} needs a trailer ({sys.argv[3]})")
    sys.exit(1)
PY
printf 'scripts/\n' >> "$repo/.git/info/exclude"
scratch_git "$repo" config core.hooksPath scripts/hooks
echo a > "$repo/a.txt"; scratch_git "$repo" add a.txt; scratch_git "$repo" commit -qm base 2> "$testroot/err"
check "a root commit (no parent) is not judged" 0 "$(wc -l < "$testroot/err" | tr -d ' ')"

echo b >> "$repo/a.txt"; scratch_git "$repo" commit -qam "passes" 2> "$testroot/err"
check "a commit the rules pass: nothing said" 0 "$(wc -l < "$testroot/err" | tr -d ' ')"

echo c >> "$repo/a.txt"; scratch_git "$repo" commit -qam "refuse me" 2> "$testroot/err"
check "a refused commit is still made" "refuse me" "$(scratch_git "$repo" log -1 --format=%s)"
check "...each refusing rule named, with its reason over HEAD^..HEAD, and the fix" "2 2 2" \
  "$(grep -c "breaks the '\(allowlists\|driftguards\)' rule" "$testroot/err") $(grep -c 'needs a trailer (HEAD^..HEAD)' "$testroot/err") $(grep -c 'git commit --amend' "$testroot/err")"

echo d >> "$repo/a.txt"; KINOWO_POST_COMMIT_RULES=0 scratch_git "$repo" commit -qam "refuse quietly" 2> "$testroot/err"
check "KINOWO_POST_COMMIT_RULES=0: nothing run" 0 "$(wc -l < "$testroot/err" | tr -d ' ')"

if [ "$fails" -ne 0 ]; then echo "post-commit: $fails failed"; exit 1; fi
echo "post-commit: all checks passed"
