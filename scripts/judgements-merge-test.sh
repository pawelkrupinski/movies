#!/usr/bin/env bash
# The judgement files merge by UNION (.gitattributes): two branches that each add a line-per-entry judgement — a label,
# a known-wrong take, a suspect baseline line, an accepted move, an expected match — rebase and merge with BOTH lines
# kept and no conflict to resolve by hand. A duplicate or contradicting pair the union produces is the judgements lint's
# (scripts/ci/identity_judgements_lint.py) to refuse, not git's. Run: bash scripts/judgements-merge-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

Files=(test/resources/fixtures/identity-unmatched/labels.tsv test/resources/fixtures/identity-unmatched/known-wrong.tsv
  test/resources/fixtures/identity-suspects/baseline.tsv test/resources/fixtures/identity-takes/accepted.tsv
  test/resources/fixtures/identity-unmatched/expected-matches.tsv)

scratch="$(mktemp -d)"; trap 'rm -rf "$scratch"' EXIT
repo="$scratch/repo"
scratch_repo "$repo"
cp "$REPO_ROOT/.gitattributes" "$repo/.gitattributes"
for f in "${Files[@]}"; do
  mkdir -p "$repo/$(dirname "$f")"
  printf 'country\tvenue\trawTitle\tfilm\tverdict\tnote\nde\tUnion\tDer Diplomat\ttmdb:1\tright\t\n' > "$repo/$f"
done
scratch_git "$repo" add -A; scratch_git "$repo" commit -qm base
scratch_git "$repo" checkout -qb one
for f in "${Files[@]}"; do printf 'pl\tMuranow\tLalka\ttmdb:2\tright\tone\n' >> "$repo/$f"; done
scratch_git "$repo" commit -qam "one adds a judgement"
scratch_git "$repo" checkout -q main
scratch_git "$repo" checkout -qb two
for f in "${Files[@]}"; do printf 'uk\tFilmhouse\tRomeo\ttmdb:3\twrong\ttwo\n' >> "$repo/$f"; done
scratch_git "$repo" commit -qam "two adds another"

scratch_git "$repo" rebase -q one >/dev/null 2>&1; code=$?
check "a rebase over a branch adding other judgements needs no hand-merge" "0" "$code"
for f in "${Files[@]}"; do
  check "$f keeps both added lines, the base's once" "1 1 1 4" \
    "$(grep -c Lalka "$repo/$f") $(grep -c Romeo "$repo/$f") $(grep -c Diplomat "$repo/$f") $(wc -l < "$repo/$f" | tr -d ' ')"
done

scratch_git "$repo" checkout -q main
scratch_git "$repo" checkout -qb three
printf 'es\tYelmo\tBuho\ttmdb:4\tright\tthree\n' >> "$repo/${Files[0]}"
scratch_git "$repo" commit -qam "three"
scratch_git "$repo" merge -q --no-edit one >/dev/null 2>&1; code=$?
check "a merge of two branches each adding a label keeps both" "0 1 1" \
  "$code $(grep -c Buho "$repo/${Files[0]}") $(grep -c Lalka "$repo/${Files[0]}")"

spec_summary
