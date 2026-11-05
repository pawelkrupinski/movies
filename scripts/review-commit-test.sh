#!/usr/bin/env bash
# Assertions for scripts/review-commit: it commits the review answers from a linked worktree and refuses the root checkout.
#
#   scripts/review-commit-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

SCRATCH="$(cd "$(mktemp -d "${TMPDIR:-/tmp}/kinowo-review-commit.XXXXXX")" && pwd -P)"
trap 'rm -rf "$SCRATCH"' EXIT
root="$SCRATCH/movies"; wt="$SCRATCH/wt"
scratch_repo "$root" -b main
mkdir -p "$root/scripts/ci" "$root/test/resources/fixtures"
cp "$REPO_ROOT/scripts/review-commit" "$root/scripts/review-commit"
printf '#!/usr/bin/env python3\n' > "$root/scripts/ci/identity_judgements_lint.py"
printf 'country\n' > "$root/test/resources/fixtures/identity-judgements.tsv"
echo 2026-01-01T00:00:00Z > "$root/test/resources/fixtures/identity-judgements.tsv.used-until"
scratch_git "$root" add -A; scratch_git "$root" commit -qm base
scratch_git "$root" worktree add -q -b review/labels "$wt"
printf 'pl\tKino\tFranz\ttmdb:1\tright\topen\t\t\t\t\treview page: right\n' >> "$wt/test/resources/fixtures/identity-judgements.tsv"
printf 'pl\tKino\tFranz\ttmdb:1\tright\topen\t\t\t\t\treview page: right\n' >> "$root/test/resources/fixtures/identity-judgements.tsv"
echo 2026-02-01T00:00:00Z > "$wt/test/resources/fixtures/identity-judgements.tsv.used-until"

echo "[spec] review-commit"
check "the root checkout is refused, naming scripts/review-server" 1 \
  "$( (cd "$root" && scripts/review-commit 2>&1 >/dev/null) | grep -c review-server)"
check "...and nothing was committed there" "base" "$(scratch_git "$root" log --format=%s -1)"
(cd "$wt" && scripts/review-commit >"$SCRATCH/out" 2>&1); rc=$?; cat "$SCRATCH/out" >&2
check "a linked worktree commits the answers" "0 Judgements: the user's review answers" "$rc $(scratch_git "$wt" log --format=%s -1)"
spec_summary
