#!/usr/bin/env bash
# identity-measure-local (the whole-corpus base-vs-head identity measure, no GitHub): each ref resolved once in a
# worktree of its own through scripts/it-local.sh (every country's dump, then the ratchet's final takes), a resolved ref
# kept, each country's model and final takes diffed, the verdict's exit. A scratch repo, with it-local.sh and the diff
# stubbed.
# Run: bash scripts/identity-measure-local-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh" # scratch_git

printf '\033[36m▸\033[0m identity-measure-local\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
repo="$work/repo"
mkdir -p "$repo/scripts/ci" "$repo/test/resources/fixtures"
cp "$REPO_ROOT/scripts/identity-measure-local" "$repo/scripts/"
# it-local.sh: logs each sbt command, and writes what each would ("--out DIR --full CC", "--measure --out DIR")
cat > "$repo/scripts/it-local.sh" <<'STUB'
#!/usr/bin/env bash
for command in "$@"; do
  echo "$(basename "$PWD") $command" >> "$STUB/calls"
  set -- $command
  case "$command" in
    *IdentityResolveDump*) while [ $# -gt 0 ]; do case "$1" in --out) out=$2 ;; --full) cc=$2 ;; esac; shift; done
                           echo '{}' > "$out/decisions-$cc.jsonl" ;;
    *UnmatchedClustersRatchet*) while [ $# -gt 0 ]; do [ "$1" = --out ] && out=$2; shift; done
                                mkdir -p "$out" && echo takes > "$out/ratchet-takes.tsv" ;;
  esac
done
STUB
cat > "$repo/scripts/ci/identity_takes_diff.py" <<'STUB'
import os, sys
open(os.path.join(os.environ["STUB"], "diffs"), "a").write(" ".join(a for a in sys.argv[1:] if not a.startswith("/")) + "\n")
sys.exit(1 if os.path.exists(os.path.join(os.environ["STUB"], "moved")) else 0)
STUB
# the classification: its directory's layout logged, its verdict the stub's
cat > "$repo/scripts/identity_measure.py" <<'STUB'
import os, sys
root = sys.argv[1]
open(os.path.join(os.environ["STUB"], "classified"), "a").write(
    " ".join(sorted(os.path.relpath(os.path.join(d, f), root) for d, _, fs in os.walk(root) for f in fs)) + "\n")
sys.exit(1 if os.path.exists(os.path.join(os.environ["STUB"], "wrong")) else 0)
STUB
chmod +x "$repo/scripts/it-local.sh"
echo "c" > "$repo/test/resources/fixtures/identity-judgements.tsv"
scratch_git "$repo" init -q && scratch_git "$repo" add -A && scratch_git "$repo" -c user.email=t@t -c user.name=t commit -qm base
echo head > "$repo/change" && scratch_git "$repo" add -A && scratch_git "$repo" -c user.email=t@t -c user.name=t commit -qm head
base=$(scratch_git "$repo" rev-parse --short=12 HEAD~1) head=$(scratch_git "$repo" rev-parse --short=12 HEAD)
# a laid recording in the cache: its corpus and two countries' trees
cache="$work/cache"; laid="$cache/recordings/41/pair/test/resources/fixtures"
mkdir -p "$laid/corpus" "$laid/enrichment-pl" "$laid/enrichment-uk" "$cache/recordings/7/pair/test/resources/fixtures/corpus"

measure() { # <args...> -> output in $work/out, exit status in $work/status
  STUB="$work" KINOWO_IDENTITY_CACHE="$cache" bash "$repo/scripts/identity-measure-local" "$@" > "$work/out" 2>&1
  echo $? > "$work/status"
}

measure --head "$head"
check "no --base is a usage error" "2" "$(cat "$work/status")"

measure --base "$base" --head "$head" --out "$work/m"
check "nothing moved: LANDABLE, exit 0" "0 yes" "$(cat "$work/status") $(grep -q '^LANDABLE (local)' "$work/out" && echo yes || echo no)"
check "...on the newest laid recording, every country it holds" "yes" \
  "$(grep -q "recording 41, countries pl uk" "$work/out" && echo yes || echo no)"
check "...each ref in its own worktree: every country's dump, then the final takes, in one session" \
  "$base 3|$head 3" "$(cut -d' ' -f1 "$work/calls" | uniq -c | awk '{print $2" "$1}' | paste -sd'|' -)"
check "...the dump reading the laid pair" "yes" "$(grep -q -- "--corpus-dir $laid/corpus --fixtures $laid" "$work/calls" && echo yes || echo no)"
check "...each country's model and final takes diffed" \
  "pl --base-name $base --head-name $head|pl --final --base-name $base --head-name $head|uk --base-name $base --head-name $head|uk --final --base-name $base --head-name $head" \
  "$(sed 's/--judgements //' "$work/diffs" | paste -sd'|' -)"

check "...classified as CI's measure is: each country's base and head takes laid out as a takes row's artifact" \
  "identity-takes-pl/base/decisions-pl.jsonl identity-takes-pl/base/ratchet-takes.tsv identity-takes-pl/head/decisions-pl.jsonl identity-takes-pl/head/ratchet-takes.tsv identity-takes-uk/base/decisions-uk.jsonl identity-takes-uk/base/ratchet-takes.tsv identity-takes-uk/head/decisions-uk.jsonl identity-takes-uk/head/ratchet-takes.tsv" \
  "$(cat "$work/classified")"
check "...and the final stage's blind spot said" "yes" "$(grep -q 'not seen, not refilled' "$work/out" && echo yes || echo no)"

rm -f "$work/calls" "$work/diffs"; touch "$work/wrong"
measure --base "$base" --head "$head" --out "$work/m" --countries pl
check "the classification finding a right take lost or a new wrong one: NOT LANDABLE, exit 1" "1 yes" \
  "$(cat "$work/status") $(grep -q '^NOT LANDABLE (local)' "$work/out" && echo yes || echo no)"
rm -f "$work/wrong"

rm -f "$work/calls" "$work/diffs"; touch "$work/moved"
measure --base "$base" --head "$head" --out "$work/m" --countries pl
check "a move nobody judged, the classification clean: landable once judged, exit 1" "1 yes" \
  "$(cat "$work/status") $(grep -q '^LANDABLE ONCE JUDGED (local)' "$work/out" && echo yes || echo no)"
check "...both refs already resolved here: kept, not resolved again" "" "$(cat "$work/calls" 2>/dev/null)"
rm -f "$work/moved"

( cd "$work" && measure --base "$base" --head "$head" --out rel --countries pl )
check "a relative --out is the caller's, not each ref's worktree's: both refs' takes land under it" "0 yes" \
  "$(cat "$work/status") $([ -s "$work/rel/$base/decisions-pl.jsonl" ] && [ -s "$work/rel/$head/decisions-pl.jsonl" ] && echo yes || echo no)"

measure --base "$base" --head "$head" --recording 99
check "a recording with no laid pair is a setup error" "2" "$(cat "$work/status")"

spec_summary
