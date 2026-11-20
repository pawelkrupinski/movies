#!/usr/bin/env bash
# scripts/scalafix-move over a scratch repository and a stub sbt: it finds the class's source, asks every
# configuration for the MoveSymbol rewrite with the two names, moves the source to the new package's directory, compiles
# every configuration, and only then lays the rewrite (a rename kept) over the checkout and its index; a rewrite that
# does not compile leaves the checkout untouched; a source holding another class, an unknown class and a bad name
# refuse. Run: bash scripts/scalafix-move-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"
printf '\033[36m▸\033[0m scalafix-move\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

repo="$work/repo"
mkdir -p "$repo/base/src/main/scala/tools"
printf 'package tools\n\nfinal case class Genres(names: Seq[String])\n\nobject Genres { def none: Genres = Genres(Nil) }\n' > "$repo/base/src/main/scala/tools/Genres.scala"
printf 'package tools\n\nobject Uses { def g: Genres = Genres.none }\n' > "$repo/base/src/main/scala/tools/Uses.scala"
printf 'package tools\n\nclass Pair\nclass Other\n' > "$repo/base/src/main/scala/tools/Pair.scala"
scratch_git "$repo" init -q -b main
scratch_git "$repo" add -A
scratch_git "$repo" commit -qm base

# the stub: logs the call; the MoveSymbol command does what the rule would to these two sources; a compile fails on ask
cat > "$work/sbt" <<'STUB'
#!/usr/bin/env bash
line="$PWD"; for a in "$@"; do line="$line|$a"; done; echo "$line" >> "$STUB_CALLS"
for a in "$@"; do
  case "$a" in
    "base/Compile/scalafix --rules=file:scripts/scalafix-rules/MoveSymbol.scala"*)
      sed -i.bak 's/^package tools$/package tools.genres/' base/src/main/scala/tools/Genres.scala
      sed -i.bak 's/^package tools$/package tools\n\nimport tools.genres.Genres/' base/src/main/scala/tools/Uses.scala
      rm -f base/src/main/scala/tools/*.bak ;;
    Test/compile) [ -z "${STUB_COMPILE_FAIL:-}" ] || { echo "[error] Not found: Genres"; exit 1; } ;;
  esac
done
STUB
chmod +x "$work/sbt"
export STUB_CALLS="$work/calls" KINOWO_SCALAFIX_REPO="$repo" KINOWO_SCALAFIX_TREE="$work/tree" KINOWO_SCALAFIX_SBT="$work/sbt"
run() { (cd "$repo" && "$REPO_ROOT/scripts/scalafix-move" "$@") 2>&1; }

out=$(STUB_COMPILE_FAIL=1 run tools.Genres tools.genres.Genres); status=$?
check "a rewrite that does not compile: exit 1, its errors shown" "1 1" "$status $(echo "$out" | grep -c 'Not found: Genres')"
check "...the checkout untouched" "" "$(scratch_git "$repo" status --porcelain)"

: > "$STUB_CALLS"
out=$(run tools.Genres tools.genres.Genres); status=$?
check "a move: exit 0" "0" "$status"
check "...the rule asked of every configuration, with both names" "1" \
  "$(grep -c '|worker/IntegrationTest/scalafix --rules=file:scripts/scalafix-rules/MoveSymbol.scala --settings.MoveSymbol.from=tools.Genres --settings.MoveSymbol.to=tools.genres.Genres|' "$STUB_CALLS")"
check "...every configuration compiled afterwards" "1" "$(grep -c '|Test/compile|worker/IntegrationTest/compile|web/IntegrationTest/compile|web/PageTest/compile$' "$STUB_CALLS")"
check "...the source moved to the new package's directory, staged as a rename" "R  base/src/main/scala/tools/Genres.scala -> base/src/main/scala/tools/genres/Genres.scala" \
  "$(scratch_git "$repo" status --porcelain | grep '^R')"
check "...its package clause and its user's import rewritten" "package tools.genres import tools.genres.Genres" \
  "$(head -1 "$repo/base/src/main/scala/tools/genres/Genres.scala") $(grep '^import' "$repo/base/src/main/scala/tools/Uses.scala")"
scratch_git "$repo" reset -q --hard

out=$(run tools.Pair tools.pairs.Pair); status=$?
check "a source holding another class too: refused" "1 1" "$status $(echo "$out" | grep -c 'split it first')"
out=$(run tools.Nothing tools.x.Nothing); status=$?
check "a class no source defines: refused" "1 1" "$status $(echo "$out" | grep -c 'no source of tools defines Nothing')"
out=$(run Genres tools.genres.Genres); status=$?
check "a name without its package: a usage error, exit 2" "2" "$status"
check "--help prints the header" "1" "$("$REPO_ROOT/scripts/scalafix-move" --help | grep -c '^A CLASS MOVED TO ANOTHER PACKAGE')"
scratch_git "$repo" worktree remove --force "$work/tree" >/dev/null 2>&1
spec_summary
