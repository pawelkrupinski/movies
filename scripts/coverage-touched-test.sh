#!/usr/bin/env bash
# scripts/coverage-touched over a scratch repository and a stub sbt (scripts/touched-tree-stub.sh): in a worktree of its
# own at the head, it turns coverage on for the touched methods' module only, runs only the reaching specs, reads the
# module's scoverage report and lists each touched method's statement coverage, the uncovered first; a failing spec
# still reports, a range touching no production method says so, and the worktree goes afterwards.
# Run: bash scripts/coverage-touched-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"
printf '\033[36m▸\033[0m coverage-touched\n'
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
sed -i.bak -e 's/_.trim/_.trim.toLowerCase/' -e 's/genre.capitalize/genre.trim.capitalize/' "$repo/$src" && rm "$repo/$src.bak"
scratch_git "$repo" commit -qam "lower-case and trim the genres"
echo "more" >> "$repo/docs/notes.md"
scratch_git "$repo" commit -qam "docs only"

printf '%s\t%s:Genres.parse\t4\t5\n%s\t%s:Genres.label\t7\t7\n' "$src" "$src" "$src" "$src" > "$work/spans.tsv"
printf 'scrapers services.cinemas.GenresSpec\nworker services.WorkerGenresSpec\n' > "$work/specs.txt"
printf '5 3\n5 1\n7 0\n' > "$work/statements"
export KINOWO_TOUCHED_REPO="$repo" KINOWO_TOUCHED_TREE_DIR="$work/trees" KINOWO_TOUCHED_SBT="$REPO_ROOT/scripts/touched-tree-stub.sh"
export STUB_CALLS="$work/calls" STUB_SPANS="$work/spans.tsv" STUB_SPECS="$work/specs.txt" STUB_STATEMENTS="$work/statements" STUB_SOURCE="$src"
run() { (cd "$repo" && "$REPO_ROOT/scripts/coverage-touched" "$@") 2>&1; }

: > "$STUB_CALLS"
out=$(run main~2..main~1 --out "$work/keep"); status=$?
check "a change: exit 0" "0" "$status"
check "...the uncovered method listed below the bar" "1" \
  "$(echo "$out" | sed -n '/^## below the bar/,/^## covered/p' | grep -c "0.0% (0/1)  $src:Genres.label")"
check "...the covered one after it" "1" "$(echo "$out" | sed -n '/^## covered/,$p' | grep -c "100.0% (2/2)  $src:Genres.parse")"
tests=$(grep '/testOnly' "$STUB_CALLS")
check "...coverage on for the touched module only" "1 0" \
  "$(echo "$tests" | tr '|' '\n' | grep -c '^set scrapers/coverageEnabled := true$') $(echo "$tests" | tr '|' '\n' | grep -c '^set worker/coverageEnabled')"
check "...only the reaching specs run, in their own modules" "2" \
  "$(echo "$tests" | tr '|' '\n' | grep -c -e '^scrapers/testOnly services.cinemas.GenresSpec$' -e '^worker/testOnly services.WorkerGenresSpec$')"
check "...the report read with coverage still on" "1" "$(grep -c '|set scrapers/coverageEnabled := true|scrapers/coverageReport$' "$STUB_CALLS")"
check "...in a worktree of its own, removed afterwards" "0" "$(find "$work/trees" -mindepth 1 -maxdepth 1 | grep -c .)"
check "...report and XML kept under --out" "report.md scoverage-scrapers.xml" \
  "$(for f in report.md scoverage-scrapers.xml; do [ -f "$work/keep/$f" ] && printf '%s ' "$f"; done | sed 's/ $//')"

out=$(STUB_TEST_RC=1 run main~2..main~1); status=$?
check "a failing reaching spec: still the report, exit 0" "0 1" "$status $(echo "$out" | grep -c 'Some reaching specs FAILED')"

out=$(run main~2..main~1 --below 40); status=$?
check "--below moves the bar" "1" "$(echo "$out" | grep -c '1 below 40% statement coverage')"

out=$(run main~1..main); status=$?
check "a range touching no production method: exit 0, said" "0 1" "$status $(echo "$out" | grep -c 'touches no production method')"

out=$(STUB_FAIL_ON=coverageReport run main~2..main~1); status=$?
check "a failed coverageReport: exit 1, naming it" "1 1" "$status $(echo "$out" | grep -c '^coverage-touched: coverageReport failed')"

out=$(run main~2..main~1 --below lots); status=$?
check "a --below that is no number: a usage error, exit 2" "2" "$status"
check "--help prints the header" "1" "$("$REPO_ROOT/scripts/coverage-touched" --help | grep -c '^COVERAGE OF THE METHODS A CHANGE TOUCHES')"
spec_summary
