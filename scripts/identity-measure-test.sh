#!/usr/bin/env bash
# scripts/identity-measure against a stub `gh` and scratch repositories: the commit goes out on a temporary
# measure/ branch that is deleted whatever happens, the dispatch names the base and the takes-only rows, an old
# commit is measured under main's CI, and the downloaded takes artifacts become a LANDABLE / NOT LANDABLE verdict.
# Run: bash scripts/identity-measure-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m identity-measure\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
origin="$work/origin.git" repo="$work/repo"
scratch_repo "$origin" --bare -b main
scratch_repo "$repo" -b main
scratch_git "$repo" remote add origin "$origin"

tab=$'\t'
mkdir -p "$repo/.github/workflows" "$repo/scripts/ci" "$repo/test/resources/fixtures"
printf 'country\tvenue\trawTitle\tfilm\tverdict\tstate\tscope\tfilmTitle\treason\tjudged\tevidence\n' \
  > "$repo/test/resources/fixtures/identity-judgements.tsv"
printf 'pl\t*\tLalka\ttmdb:1\tright\topen\t\t\t\t2026-10-14\t\n' >> "$repo/test/resources/fixtures/identity-judgements.tsv"
echo "phase: convergence" > "$repo/.github/workflows/country-convergence-leg.yml"
echo "old" > "$repo/scripts/ci/identity-takes-check.sh"
scratch_git "$repo" add -A && scratch_git "$repo" commit -qm "old main, before takes-only"
old=$(scratch_git "$repo" rev-parse HEAD)
echo "takes-only: {}" > "$repo/.github/workflows/country-convergence-leg.yml"
echo "new" > "$repo/scripts/ci/identity-takes-check.sh"
scratch_git "$repo" commit -qam "main learns takes-only"
parent=$(scratch_git "$repo" rev-parse HEAD)
echo "rule" > "$repo/rule.txt" && scratch_git "$repo" add rule.txt && scratch_git "$repo" commit -qm "a resolver change"
tip=$(scratch_git "$repo" rev-parse HEAD)
scratch_git "$repo" push -q origin main

# One artifact: identity-takes-pl, with a right take ("Lalka") lost when $LOSE is set.
artifact() { # <dir> <lose?>
  local d="$1/identity-takes-pl"
  mkdir -p "$d/base" "$d/head"
  echo "identity takes (pl): lost ${2:-0}" > "$d/verdict.txt"
  echo '{"key":"Helios|Lalka","venue":"Helios","rawTitle":"Lalka","film":1,"filmTitle":"Lalka (2026)","basis":"OwnMatch","explanation":[],"blocker":null,"suspect":[]}' > "$d/base/decisions-pl.jsonl"
  if [ -n "${2:-}" ]; then
    echo '{"key":"Helios|Lalka","venue":"Helios","rawTitle":"Lalka","film":null,"filmTitle":null,"basis":"none","explanation":[],"blocker":"no film","suspect":[]}' > "$d/head/decisions-pl.jsonl"
  else
    cp "$d/base/decisions-pl.jsonl" "$d/head/decisions-pl.jsonl"
  fi
  for side in base head; do echo "status${tab}country${tab}venue${tab}rawTitle${tab}film${tab}title${tab}basis${tab}ids${tab}was${tab}explanation" > "$d/$side/ratchet-takes.tsv"; done
}
artifact "$work/landable"
artifact "$work/lost" 1

mkdir -p "$work/bin"
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$STUB/calls"
json=""; prev=""
for a in "$@"; do [ "$prev" = --json ] && json=$a; prev=$a; done
case "$1 $2" in
  "workflow run") echo "$*" > "$STUB/dispatch" ;;
  "run list")
    if [[ " $* " == *" --branch "* ]]; then
      # the run exists only once the commit is on origin's measure/ branch: record what was pushed
      for f in "$ORIGIN"/refs/heads/measure/*; do [ -f "$f" ] && { basename "$f" > "$STUB/branch"; cat "$f" > "$STUB/pushed"; }; done
      [ -f "$STUB/pushed" ] && echo 777
    else echo 0; fi ;;
  "run view")
    case "$json" in
      url) echo "https://example.test/runs/777" ;;
      status,conclusion) [ -n "${VIEW_FAILS:-}" ] && exit 1
                         [ -n "${VIEW_BLIPS:-}" ] && [ ! -f "$STUB/blipped" ] && { touch "$STUB/blipped"; exit 1; }
                         printf 'completed\tsuccess\n' ;;
      status,headBranch,headSha) printf 'completed\tmeasure/abc-1\t%s\n' "$HEAD_SHA" ;;
      jobs) printf 'poland / takes\tfailure\n' ;;
    esac ;;
  "run download") while [ "$#" -gt 0 ]; do [ "$1" = --dir ] && dir=$2; shift; done; cp -R "$ARTIFACT/." "$dir/" ;;
  "run cancel") echo cancelled > "$STUB/cancelled" ;;
  *) exit 1 ;;
esac
STUB
chmod +x "$work/bin/gh"

measure() { # <artifact> <args...>: exit code; output in $work/out
  local art=$1; shift
  rm -rf "$work/stub" "$work/cache"; mkdir -p "$work/stub"
  ( cd "$repo" && STUB="$work/stub" ORIGIN="$origin" ARTIFACT="$work/$art" HEAD_SHA="${HEAD_SHA:-}" \
      KINOWO_IDENTITY_CACHE="$work/cache" IDENTITY_MEASURE_POLL=0 PATH="$work/bin:$PATH" \
      "$REPO_ROOT/scripts/identity-measure" "$@" > "$work/out" 2>&1 )
  echo $?
}
measure_branches() { ls "$origin/refs/heads/measure" 2>/dev/null | wc -l | tr -d ' '; }

check "a commit that moved nothing is LANDABLE" "0" "$(measure landable --countries pl)"
check "...pushed as itself on a measure/<short sha>-<random> branch" \
  "$tip" "$(cat "$work/stub/pushed")"
check "...named after the commit" "true" "$([[ "$(cat "$work/stub/branch")" == "${tip:0:9}-"* ]] && echo true || echo false)"
check "...dispatching only the takes rows against the commit's parent (it is on main already)" \
  "workflow run identity-model-convergence.yml --ref measure/$(cat "$work/stub/branch") -f takes-base=$parent -f takes-only=true" \
  "$(cat "$work/stub/dispatch")"
check "...and the branch is deleted afterwards" "0" "$(measure_branches)"
check "...printing the verdict" "true" "$(grep -q '^LANDABLE' "$work/out" && echo true || echo false)"
check "...and the takes row's own conclusion" "true" "$(grep -Eq '^pl +model .*failure' "$work/out" && echo true || echo false)"

check "a right take lost is NOT LANDABLE" "1" "$(measure lost --base "$parent" --countries pl)"
check "...naming the bucket" "true" "$(grep -q 'pl model: 1 right-lost' "$work/out" && echo true || echo false)"
check "...with the listing, before -> after" "true" \
  "$(grep -q 'Helios | Lalka: tmdb:1 Lalka (2026) -> none' "$work/out" && echo true || echo false)"
check "...and the branch is deleted" "0" "$(measure_branches)"

check "a commit from before the takes-only dispatch is measured" "0" "$(measure landable "$old" --base "$old" --countries pl)"
pushed=$(cat "$work/stub/pushed")
check "...under a commit laying main's CI over it, whose parent is the commit" \
  "$old" "$(scratch_git "$repo" rev-parse "$pushed^")"
check "...with main's workflows" "takes-only: {}" "$(scratch_git "$repo" show "$pushed:.github/workflows/country-convergence-leg.yml")"
check "...and main's scripts/ci" "new" "$(scratch_git "$repo" show "$pushed:scripts/ci/identity-takes-check.sh")"
check "...and nothing else of main's" "false" \
  "$(scratch_git "$repo" cat-file -e "$pushed:rule.txt" 2>/dev/null && echo true || echo false)"

check "a run read that fails once is retried, not the end of the measurement" "0" "$(VIEW_BLIPS=1 measure landable --countries pl)"
check "a run that cannot be read is no measurement" "2" "$(VIEW_FAILS=1 measure landable --countries pl)"
check "...and its branch is still deleted" "0" "$(measure_branches)"

check "--no-wait dispatches and keeps the branch for --run" "0" "$(measure landable --no-wait --countries pl)"
check "...which stays on origin" "1" "$(measure_branches)"
check "...saying how to collect it" "true" "$(grep -q 'scripts/identity-measure --run 777' "$work/out" && echo true || echo false)"
scratch_git "$repo" push -q origin --delete "refs/heads/measure/$(cat "$work/stub/branch")"

scratch_git "$repo" push -q origin "$tip:refs/heads/measure/abc-1"
check "--run collects a finished run" "1" "$(HEAD_SHA=$tip measure lost --run 777 --countries pl)"
check "...and deletes its measure/ branch" "0" "$(measure_branches)"
check "...dispatching nothing" "false" "$([ -f "$work/stub/dispatch" ] && echo true || echo false)"

spec_summary
