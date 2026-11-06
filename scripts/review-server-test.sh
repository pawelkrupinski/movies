#!/usr/bin/env bash
# Assertions for scripts/review-server: the review pages' labels file is the one of a worktree of its own, made from
# the base on first use, caught up while it holds nothing, and left alone once it holds answers; the dev server it runs
# is that worktree's, never the checkout the script was started from.
#
#   scripts/review-server-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

SCRATCH="$(cd "$(mktemp -d "${TMPDIR:-/tmp}/kinowo-review-server.XXXXXX")" && pwd -P)"
trap 'rm -rf "$SCRATCH"' EXIT
origin="$SCRATCH/origin.git"; root="$SCRATCH/movies"; wt="$SCRATCH/movies-review-labels"
scratch_repo "$origin" --bare -b main
scratch_repo "$root" -b main
mkdir -p "$root/scripts" "$root/test/resources/fixtures"
cp "$REPO_ROOT/scripts/review-server" "$root/scripts/review-server"
printf '#!/usr/bin/env bash\necho "DEV-SERVER pwd=$(pwd -P) labels=$REVIEW_LABELS_FILE args=$*"\n' > "$root/scripts/dev-server.sh"
chmod +x "$root/scripts/dev-server.sh"
echo v1 > "$root/test/resources/fixtures/identity-judgements.tsv"
scratch_git "$root" add -A; scratch_git "$root" commit -qm base
scratch_git "$root" remote add origin "$origin"; scratch_git "$root" push -q origin main

run() { REVIEW_WORKTREE="$wt" "$root/scripts/review-server" "$@" 2>"$SCRATCH/err"; }

echo "[spec] review-server"
check "first run makes the worktree and serves from it, the labels file inside it" \
  "DEV-SERVER pwd=$wt labels=$wt/test/resources/fixtures/identity-judgements.tsv args=-J-Xmx4g" "$(run -J-Xmx4g)"
check "...on the review/labels branch" "review/labels" "$(scratch_git "$wt" rev-parse --abbrev-ref HEAD)"
check "the root checkout is untouched" "" "$(scratch_git "$root" status --porcelain)"
scratch_git "$root" commit -q --allow-empty -m next; echo v2 > "$root/test/resources/fixtures/identity-judgements.tsv"
scratch_git "$root" commit -qam v2; scratch_git "$root" push -q origin main
run >/dev/null
check "a clean worktree is caught up with the base" "v2" "$(cat "$wt/test/resources/fixtures/identity-judgements.tsv")"
echo answer >> "$wt/test/resources/fixtures/identity-judgements.tsv"
scratch_git "$root" commit -q --allow-empty -m v3; scratch_git "$root" push -q origin main
run >/dev/null
check "a worktree holding answers is not touched" "v2
answer" "$(cat "$wt/test/resources/fixtures/identity-judgements.tsv")"
check "...and says what to do" 1 "$(grep -c 'review-commit' "$SCRATCH/err")"
spec_summary
