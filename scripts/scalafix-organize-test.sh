#!/usr/bin/env bash
# scripts/scalafix-organize over a scratch repository and a stub sbt: it refuses a checkout with uncommitted changes,
# rewrites in the kept scalafix tree (made once, then moved to HEAD and reused) with SemanticDB on and scalafix asked of
# every configuration, and lays the rewrite over the checkout and its index; --check changes nothing and lists what would
# change; a source set already in order is said. Run: bash scripts/scalafix-organize-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"
printf '\033[36m▸\033[0m scalafix-organize\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

repo="$work/repo"
src=base/src/main/scala/tools/Genres.scala
mkdir -p "$repo/base/src/main/scala/tools"
printf 'package tools\n\nimport scala.util.Try\nimport java.util.Locale\n\nobject Genres\n' > "$repo/$src"
scratch_git "$repo" init -q -b main
scratch_git "$repo" add -A
scratch_git "$repo" commit -qm base

# the stub: logs the call; a scalafix command sorts the imports of the source (as OrganizeImports would)
cat > "$work/sbt" <<'STUB'
#!/usr/bin/env bash
line="$PWD"; for a in "$@"; do line="$line|$a"; done; echo "$line" >> "$STUB_CALLS"
for a in "$@"; do
  case "$a" in
    base/Compile/scalafix*)
      f=base/src/main/scala/tools/Genres.scala
      { sed -n '1,2p' "$f"; grep '^import' "$f" | sort; sed -n '/^import/!p' "$f" | sed '1,2d'; } > "$f.new" && mv "$f.new" "$f" ;;
  esac
done
STUB
chmod +x "$work/sbt"
export STUB_CALLS="$work/calls" KINOWO_SCALAFIX_REPO="$repo" KINOWO_SCALAFIX_TREE="$work/tree" KINOWO_SCALAFIX_SBT="$work/sbt"
run() { (cd "$repo" && "$REPO_ROOT/scripts/scalafix-organize" "$@") 2>&1; }

echo "// wip" >> "$repo/$src"
out=$(run); status=$?
check "uncommitted changes: refused, exit 1" "1 1" "$status $(echo "$out" | grep -c 'uncommitted changes')"
scratch_git "$repo" checkout -q -- "$src"

: > "$STUB_CALLS"
out=$(run --check); status=$?
check "--check with imports out of order: exit 1, the source listed" "1 1" "$status $(echo "$out" | grep -cx "$src")"
check "...the checkout untouched" "" "$(scratch_git "$repo" status --porcelain)"
check "...SemanticDB on, scalafix asked of every configuration" "1 1 1" \
  "$(grep -c '|set ThisBuild / semanticdbEnabled := true|' "$STUB_CALLS") $(grep -c '|worker/IntegrationTest/scalafix |' "$STUB_CALLS") $(grep -c '|web/PageTest/scalafix |' "$STUB_CALLS")"
check "...in the kept tree" "$work/tree" "$(cut -d'|' -f1 "$STUB_CALLS")"

: > "$STUB_CALLS"
out=$(run --check --main); status=$?
check "--main: only the Compile configurations asked" "0 1" \
  "$(tr '|' '\n' < "$STUB_CALLS" | grep -c '/Test/scalafix') $(tr '|' '\n' < "$STUB_CALLS" | grep -c '^base/Compile/scalafix')"

out=$(run); status=$?
check "a rewrite: exit 0" "0" "$status"
check "...laid over the checkout and staged" "M  $src" "$(scratch_git "$repo" status --porcelain)"
check "...the imports sorted" "import java.util.Locale" "$(grep -m1 '^import' "$repo/$src")"
scratch_git "$repo" commit -qm organised

out=$(run); status=$?
check "imports already in order: exit 0, said, the tree reused at the new HEAD" "0 1 $(scratch_git "$repo" rev-parse HEAD)" \
  "$status $(echo "$out" | grep -c 'imports are in order') $(scratch_git "$work/tree" rev-parse HEAD)"
check "--help prints the header" "1" "$("$REPO_ROOT/scripts/scalafix-organize" --help | grep -c '^IMPORTS ORGANISED BY SCALAFIX')"
scratch_git "$repo" worktree remove --force "$work/tree" >/dev/null 2>&1
spec_summary
