#!/usr/bin/env bash
# scripts/ratchet-diff --measure over stub scripts/quality-report and scripts/coupling-report: it measures the checkout
# it lives in, so run from another checkout (a worktree calling the root checkout's copy) it refuses, exit 2, naming
# both -- that checkout's refs against this one's tree is a diff of two different trees; run inside its own, it measures.
# Run: bash scripts/ratchet-diff-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"
printf '\033[36m▸\033[0m ratchet-diff\n'
work="$(cd "$(mktemp -d)" && pwd -P)"
trap 'rm -rf "$work"' EXIT
for repo in root other; do
  scratch_repo "$work/$repo" -q
  mkdir -p "$work/$repo/scripts"
  echo x > "$work/$repo/a.txt"
  scratch_git "$work/$repo" add -A
  scratch_git "$work/$repo" commit -qm base
done
cp "$REPO_ROOT/scripts/ratchet-diff" "$REPO_ROOT/scripts/own-checkout" "$work/root/scripts/"
for tool in quality-report coupling-report; do
  cat > "$work/root/scripts/$tool" <<'STUB'
#!/usr/bin/env bash
echo measured >> "$(dirname "$0")/../measured"
exit 2
STUB
  chmod +x "$work/root/scripts/$tool"
done
diff_from() { ( cd "$1" && env -u GIT_DIR -u GIT_WORK_TREE -u GIT_INDEX_FILE "$work/root/scripts/ratchet-diff" --measure HEAD 2>&1 ); }

out="$(diff_from "$work/other")"; status=$?
check "--measure from another checkout is exit 2" "2" "$status"
check "...naming the checkout it measures" "1" "$(printf '%s\n' "$out" | grep -c "measures $work/root")"
check "...and measuring nothing" "no" "$([ -e "$work/root/measured" ] && echo yes || echo no)"

diff_from "$work/root" > /dev/null
check "--measure inside its own checkout measures it" "yes" "$([ -e "$work/root/measured" ] && echo yes || echo no)"

spec_summary
