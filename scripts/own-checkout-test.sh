#!/usr/bin/env bash
# scripts/own-checkout: a tool measuring its own checkout refuses (exit 2, naming the copy to run) inside another
# checkout, runs inside its own or outside any; and every tool that takes its checkout from its own path and works in it
# (`root=...BASH_SOURCE...` then `cd "$root"`) calls it -- a new such tool that forgets fails here.
# Run: bash scripts/own-checkout-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"
printf '\033[36m▸\033[0m own-checkout\n'
work="$(cd "$(mktemp -d)" && pwd -P)"
trap 'rm -rf "$work"' EXIT
for repo in own other; do scratch_repo "$work/$repo" -q; done
mkdir -p "$work/own/scripts" "$work/plain"
cp "$REPO_ROOT/scripts/own-checkout" "$work/own/scripts/"
printf '#!/usr/bin/env bash\n"$(dirname "$0")/own-checkout" "$0" || exit 2\necho ran\n' > "$work/own/scripts/tool"
chmod +x "$work/own/scripts/tool"
run_in() { ( cd "$1" && env -u GIT_DIR -u GIT_WORK_TREE -u GIT_INDEX_FILE "$work/own/scripts/tool" 2>&1 ); }

out="$(run_in "$work/other")"; status=$?
check "inside another checkout: exit 2, naming the copy to run" "2 1" \
  "$status $(grep -c "this copy measures $work/own, its own checkout, but this is $work/other: run $work/other/scripts/tool" <<< "$out")"
check "inside its own checkout: it runs" "0 ran" "$(out="$(run_in "$work/own")"; echo "$? $out")"
check "outside any checkout: it runs" "0 ran" "$(out="$(run_in "$work/plain")"; echo "$? $out")"

# every tool that works in its own checkout calls the guard; these choose their tree otherwise
exempt=" scripts/hooks/pre-push scripts/run-script-tests "  # git runs the hook in its own checkout; --root names the tree
missing=""
while IFS= read -r f; do
  rel="${f#"$REPO_ROOT"/}"
  case "$exempt" in *" $rel "*) continue ;; esac
  grep -qE '^root=.*BASH_SOURCE' "$f" && grep -qE '^\s*cd "\$root"' "$f" || continue
  grep -q 'scripts/own-checkout" "\$0"' "$f" || missing="$missing $rel"
done < <(find "$REPO_ROOT/scripts" -type f ! -name '*-test.sh' ! -name '*_test.py' ! -path '*/fixtures/*')
check "every tool working in its own checkout calls scripts/own-checkout" "" "$missing"

spec_summary
