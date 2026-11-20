#!/usr/bin/env bash
# scripts/mutation-check over a scratch repository and a stub sbt (scripts/touched-tree-stub.sh): it measures the head
# in a worktree of its own (removed afterwards, the caller's checkout untouched), with the plugins laid over it; mutates
# only the touched method's source and runs only the reaching specs, from the repository root, without -Werror; reports
# each touched method's killed and surviving mutants, the survivors by line; a range touching no production method, a
# module no spec reaches, too many specs and a failed Stryker run each say so. Run: bash scripts/mutation-check-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"
printf '\033[36m▸\033[0m mutation-check\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

repo="$work/repo"
src=scrapers/src/main/scala/services/cinemas/Genres.scala
mkdir -p "$repo/scrapers/src/main/scala/services/cinemas" "$repo/project" "$repo/docs"
cat > "$repo/$src" <<'SCALA'
package services.cinemas

object Genres {
  def parse(text: String): Seq[String] =
    text.split(',').map(_.trim).toSeq

  def label(genre: String): String = genre.capitalize
}
SCALA
echo "# notes" > "$repo/docs/notes.md"
scratch_git "$repo" init -q -b main
scratch_git "$repo" add -A
scratch_git "$repo" commit -qm base
sed -i.bak 's/_.trim/_.trim.toLowerCase/' "$repo/$src" && rm "$repo/$src.bak"
scratch_git "$repo" commit -qam "lower-case the genres"
echo "more" >> "$repo/docs/notes.md"
scratch_git "$repo" commit -qam "docs only"

printf '%s\t%s:Genres.parse\t4\t5\n%s\t%s:Genres.label\t7\t7\n' "$src" "$src" "$src" "$src" > "$work/spans.tsv"
printf 'scrapers services.cinemas.GenresSpec\n' > "$work/specs.txt"
printf '5 Killed MethodExpression _.trim\n5 Survived MethodExpression toUpperCase\n7 Survived StringLiteral ""\n' > "$work/mutants"
export KINOWO_TOUCHED_REPO="$repo" KINOWO_TOUCHED_TREE_DIR="$work/trees" KINOWO_TOUCHED_SBT="$REPO_ROOT/scripts/touched-tree-stub.sh"
export STUB_CALLS="$work/calls" STUB_SPANS="$work/spans.tsv" STUB_SPECS="$work/specs.txt" STUB_MUTANTS="$work/mutants" STUB_SOURCE="$src"
run() { (cd "$repo" && "$REPO_ROOT/scripts/mutation-check" "$@") 2>&1; }

: > "$STUB_CALLS"
out=$(run main~2..main~1 --out "$work/keep"); status=$?
check "a change: exit 0" "0" "$status"
check "...the touched method's mutants counted" "1" \
  "$(echo "$out" | grep -c "^| \`$src:Genres.parse\` | 1 | 1 | 0 |$")"
check "...the untouched method's mutant counted apart" "1" "$(echo "$out" | grep -c '1 more in the touched files outside the touched methods')"
check "...the survivor named by line" "1" "$(echo "$out" | grep -c "^- $src:5 \[Survived\] MethodExpression: \`toUpperCase\`  (Genres.parse)$")"
stryker=$(grep '|scrapers/stryker$' "$STUB_CALLS")
check "...Stryker mutates only the touched source" "1" "$(echo "$stryker" | grep -c "|set scrapers/strykerMutate := Seq(\"$src\")|")"
check "...runs only the reaching specs" "1" "$(echo "$stryker" | grep -c '|set scrapers/strykerTestFilter := Seq("services.cinemas.GenresSpec")|')"
check "...from the repository root, without -Werror" "2" \
  "$(echo "$stryker" | tr '|' '\n' | grep -c -e 'strykerBaseDir := (LocalRootProject / baseDirectory).value' -e 'filterNot(_ == "-Werror")')"
tree=$(echo "$stryker" | cut -d'|' -f1)
check "...in a worktree of its own, at the head" "$work/trees/mutation-check" "${tree%.*}"
check "...the plugins laid over it" "1" "$(grep -c "^$tree|touchedSpecs" "$STUB_CALLS")"
check "...the worktree removed afterwards" "absent" "$([ -d "$tree" ] && echo present || echo absent)"
check "...and gone from git's list" "1" "$(scratch_git "$repo" worktree list | grep -c .)"
check "...the caller's checkout untouched" "" "$(scratch_git "$repo" status --porcelain)"
check "...report and Stryker's json kept under --out" "report.md report-scrapers.json" \
  "$(for f in report.md report-scrapers.json; do [ -f "$work/keep/$f" ] && printf '%s ' "$f"; done | sed 's/ $//')"

out=$(run main~1..main); status=$?
check "a range touching no production method: exit 0, said" "0 1" "$status $(echo "$out" | grep -c 'touches no production method')"

: > "$work/specs.txt"
: > "$STUB_CALLS"
out=$(run main~2..main~1); status=$?
check "no spec reaches the module: exit 0" "0" "$status"
check "...said, and Stryker never run" "1 0" "$(echo "$out" | grep -c 'no spec of scrapers reaches') $(grep -c 'stryker$' "$STUB_CALLS")"

printf 'scrapers a.ASpec\nscrapers b.BSpec\n' > "$work/specs.txt"
out=$(run main~2..main~1 --max-specs 1); status=$?
check "more reaching specs than --max-specs: exit 1, naming the count" "1 1" "$status $(echo "$out" | grep -c '2 specs reach the touched methods, over --max-specs 1')"

printf 'scrapers services.cinemas.GenresSpec\n' > "$work/specs.txt"
out=$(STUB_FAIL_ON=stryker run main~2..main~1); status=$?
check "a failed Stryker run: exit 1, with its log's tail" "1 1" "$status $(echo "$out" | grep -c 'Stryker failed')"

out=$(run main~2); status=$?
check "no range: a usage error, exit 2" "2" "$status"
check "--help prints the header" "1" "$("$REPO_ROOT/scripts/mutation-check" --help | grep -c '^MUTATION TESTING OF THE METHODS A CHANGE TOUCHES')"
check "no worktree left behind" "0" "$(find "$work/trees" -mindepth 1 -maxdepth 1 | grep -c .)"
spec_summary
