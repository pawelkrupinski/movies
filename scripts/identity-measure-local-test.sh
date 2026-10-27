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
cp "$REPO_ROOT/scripts/identity-measure-local" "$REPO_ROOT/scripts/jvm-gate.sh" "$REPO_ROOT/scripts/tool-conventions.sh" "$REPO_ROOT/scripts/forge-gh" "$repo/scripts/"
# it-local.sh: logs each sbt command, and writes what each would ("--out DIR --full CC", "--measure --out DIR")
cat > "$repo/scripts/it-local.sh" <<'STUB'
#!/usr/bin/env bash
for command in "$@"; do
  echo "$(basename "$PWD") $command" >> "$STUB/calls"
  [ -n "${KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY:-}" ] && echo "live $KINOWO_IDENTITY_LIVE_GAPS_TMDB_KEY" >> "$STUB/live"
  set -- $command
  case "$command" in
    *IdentityResolveDump*) while [ $# -gt 0 ]; do case "$1" in --out) out=$2 ;; --full) cc=$2 ;; --corpus-dir) corpus=$2 ;; esac; shift; done
                           # the dump names the recording it was resolved over, so a diff can tell which it read
                           rec=${corpus%/pair/*}; rec=${rec##*/}
                           if [ -f "$STUB/crash-dump" ]; then   # the JVM killed mid-write: half a dump, sbt failed
                             printf '{"recording": "%s", "par' "$rec" > "$out/decisions-$cc.jsonl"; exit 137
                           fi
                           echo "{\"recording\": \"$rec\"}" > "$out/decisions-$cc.jsonl" ;;
    *UnmatchedClustersRatchet*) while [ $# -gt 0 ]; do [ "$1" = --out ] && out=$2; shift; done
                                if [ -f "$STUB/crash-ratchet" ]; then   # as sbt ran it on 2026-10-19: died, then [success]
                                  printf '%s\n' 'Exception in thread "main" java.lang.NoClassDefFoundError: modules/wiring/IdentityCutoverWiring$' \
                                    '	at tools.UnmatchedClusters$.familiesOf(UnmatchedClusters.scala:94)' '[success] Total time: 38 s'
                                else mkdir -p "$out" && echo takes > "$out/ratchet-takes.tsv"; fi ;;
  esac
done
STUB
cat > "$repo/scripts/ci/identity_takes_diff.py" <<'STUB'
import os, sys, time
stub = os.environ["STUB"]
open(os.path.join(stub, "diffs"), "a").write(" ".join(a for a in sys.argv[1:] if not a.startswith("/")) + "\n")
# a diff held open (STUB_PAUSE names a file): it says so, waits for the file to go, then reads its two sides -- the
# window another run's work falls into
pause = os.environ.get("STUB_PAUSE")
if pause and os.path.exists(pause):
    open(pause + ".reached", "w").close()
    while os.path.exists(pause): time.sleep(0.05)
if "--final" not in sys.argv:
    open(os.path.join(stub, "read"), "a").write(
        " ".join(open(f).read().strip() for f in sys.argv[2:4]) + "\n")
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
echo "c" > "$repo/test/resources/fixtures/identity-judgements.tsv"; echo target/ > "$repo/.gitignore"
scratch_git "$repo" init -q && scratch_git "$repo" add -A && scratch_git "$repo" -c user.email=t@t -c user.name=t commit -qm base
echo head > "$repo/change" && scratch_git "$repo" add -A && scratch_git "$repo" -c user.email=t@t -c user.name=t commit -qm head
base=$(scratch_git "$repo" rev-parse --short=12 HEAD~1) head=$(scratch_git "$repo" rev-parse --short=12 HEAD)
# a laid recording in the cache: its corpus and two countries' trees
cache="$work/cache"; laid="$cache/recordings/41/pair/test/resources/fixtures"
mkdir -p "$laid/corpus" "$laid/enrichment-pl" "$laid/enrichment-uk" "$cache/recordings/7/pair/test/resources/fixtures/corpus"
mkdir -p "$cache/recordings/39/pair/test/resources/fixtures/corpus" "$cache/recordings/39/pair/test/resources/fixtures/enrichment-pl"

measure() { # <args...> -> output in $work/out, exit status in $work/status; bounded: a parse that loops fails, not hangs
  STUB="$work" KINOWO_IDENTITY_CACHE="$cache" perl -e 'alarm 60; exec @ARGV' bash "$repo/scripts/identity-measure-local" "$@" > "$work/out" 2>&1
  echo $? > "$work/status"
}

measure --head "$head"
check "no --base is a usage error" "2" "$(cat "$work/status")"
measure --head "$head" --base
check "a flag with no value after it is a usage error, not a parse that loops forever" "2 yes" \
  "$(cat "$work/status") $(grep -q -- '--base needs a value' "$work/out" && echo yes || echo no)"
measure --base "$base" --head "$head" --countries pl,xx
check "a country no tool knows is a usage error, before anything resolves" "2 yes" \
  "$(cat "$work/status") $(grep -q 'unknown country xx' "$work/out" && echo yes || echo no)"
measure --help
check "--help prints the header" "0 yes" "$(cat "$work/status") $(grep -q '^THE IDENTITY MEASURE, LOCALLY' "$work/out" && echo yes || echo no)"

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

rm -f "$work/live"
TMDB_API_KEY='' measure --base "$base" --head "$head" --live-gaps
check "--live-gaps with no TMDB key is a setup error" "2" "$(cat "$work/status")"
rm -f "$work/calls"
TMDB_API_KEY=k1 measure --base "$base" --head "$head" --live-gaps
check "--live-gaps: every resolve asks live with the key, in an out dir of its own (none reused)" "0 6 yes" \
  "$(cat "$work/status") $(grep -c '^live k1$' "$work/live") $(grep -q -- "-live$" <<< "$(grep -o 'out .*' "$work/out" | head -1)" && echo yes || echo no)"

rm -f "$work/calls"
measure --base "$base" --head "$head" --out "$work/other-checkout"
check "another checkout measuring the same refs on the same recording: their shared takes reused, nothing resolved" "0 " \
  "$(cat "$work/status") $(cat "$work/calls" 2>/dev/null)"

newer=$(scratch_git "$repo" rev-parse --short=12 HEAD); echo more > "$repo/more"; scratch_git "$repo" add -A
scratch_git "$repo" -c user.email=t@t -c user.name=t commit -qm newer; newer=$(scratch_git "$repo" rev-parse --short=12 HEAD)
measure --base "$base" --head "$newer" --countries pl --out "$work/sub"
rm -f "$work/calls"
measure --base "$base" --head "$newer" --countries pl,uk --out "$work/sub2"
check "a ref resolved for some countries: only the missing country's dump and this country set's final takes resolved" \
  "$newer worker/IntegrationTest/runMain integration.IdentityResolveDump --full uk|$newer worker/IntegrationTest/runMain scripts.UnmatchedClustersRatchet --measure" \
  "$(sed -E 's/ --(out|corpus-dir|fixtures) [^ ]+//g; s/ --countries .*//' "$work/calls" | paste -sd'|' -)"

third=$(echo x > "$repo/third"; scratch_git "$repo" add -A; scratch_git "$repo" -c user.email=t@t -c user.name=t commit -qm third; scratch_git "$repo" rev-parse --short=12 HEAD)
store="$cache/measure-local/41"; mkdir -p "$store"
spec_lock_holder "$store/$third.flock"; holder=$SPEC_LOCK_HOLDER
rm -f "$work/calls"
( KINOWO_MEASURE_LOCAL_POLL=0.2 STUB="$work" KINOWO_IDENTITY_CACHE="$cache" bash "$repo/scripts/identity-measure-local" \
    --base "$base" --head "$third" --countries pl --out "$work/locked" > "$work/locked.out" 2>&1; echo $? > "$work/locked.status" ) &
runner=$!
wait_until "the run said it waits for the other's resolve" grep -q 'waiting for it' "$work/locked.out"
check "a ref another live run is resolving: waited for, not resolved twice" "yes " \
  "$(grep -q "$third is being resolved by pid $holder" "$work/locked.out" && echo yes || echo no) $(grep "^$third" "$work/calls" 2>/dev/null)"
kill -9 "$holder"; wait "$holder" 2>/dev/null; wait "$runner"
check "...and, its holder killed, resolved by the waiting run" "0 2" "$(cat "$work/locked.status") $(grep -c "^$third" "$work/calls")"

fourth=$(echo y > "$repo/fourth"; scratch_git "$repo" add -A; scratch_git "$repo" -c user.email=t@t -c user.name=t commit -qm fourth; scratch_git "$repo" rev-parse --short=12 HEAD)
mkdir -p "$cache/jvm-slots"; spec_lock_holder "$cache/jvm-slots/1.flock"; holder=$SPEC_LOCK_HOLDER
( KINOWO_JVM_SLOTS=1 KINOWO_JVM_SLOT_POLL=0.2 STUB="$work" KINOWO_IDENTITY_CACHE="$cache" bash "$repo/scripts/identity-measure-local" \
    --base "$base" --head "$fourth" --countries pl --out "$work/slots" > "$work/slots.out" 2>&1; echo $? > "$work/slots.status" ) &
runner=$!
wait_until "the run said every JVM slot is busy" grep -q 'JVM slots busy' "$work/slots.out"
check "every JVM slot on the machine busy: the resolve waits for one (scripts/jvm-gate.sh)" "yes no" \
  "$(grep -q '\[identity-measure-local\] all 1 JVM slots busy' "$work/slots.out" && echo yes || echo no) $(grep -q "^$fourth" "$work/calls" && echo yes || echo no)"
kill -9 "$holder"; wait "$holder" 2>/dev/null; wait "$runner"
check "...a slot whose holder is killed is free to it at once, and given back after" "0 yes free" \
  "$(cat "$work/slots.status") $(grep -q "^$fourth" "$work/calls" && echo yes || echo no) $(spec_lock_state "$cache/jvm-slots/1.flock")"

fifth=$(echo z > "$repo/fifth"; scratch_git "$repo" add -A; scratch_git "$repo" -c user.email=t@t -c user.name=t commit -qm fifth; scratch_git "$repo" rev-parse --short=12 HEAD)
KINOWO_MEMORY_GATE=1 KINOWO_MEMORY_GATE_PROBE='echo 99' KINOWO_MEMORY_GATE_FREE_PROBE='echo 0' KINOWO_MEMORY_GATE_MAX_WAIT=0 measure --base "$base" --head "$fifth" --countries pl --out "$work/gated"
check "a resolve waits on the memory gate while the Mac is paging (deadline 0: warned, then resolved)" "0 yes" \
  "$(cat "$work/status") $(grep -q '\[identity-measure-local\] memory gate: still 99 GB' "$work/out" && echo yes || echo no)"

sixth=$(echo w > "$repo/sixth"; scratch_git "$repo" add -A; scratch_git "$repo" -c user.email=t@t -c user.name=t commit -qm sixth; scratch_git "$repo" rev-parse --short=12 HEAD)
# 2026-10-19: the ratchet died on a NoClassDefFoundError (an orphan class a seed carried in), sbt said [success], and the
# measure printed "resolved (exit 0)", then NOT LANDABLE with no reason. A ref whose ratchet writes no takes did not
# resolve: a setup error, naming what it died of.
touch "$work/crash-ratchet"
measure --base "$base" --head "$sixth" --countries pl --out "$work/crashed"
rm -f "$work/crash-ratchet"
check "a ratchet that dies writing no takes: a setup error naming the exception, no verdict" "2 yes yes no" \
  "$(cat "$work/status") $(grep -q "the head did not resolve" "$work/out" && echo yes || echo no) \
$(grep -q 'NoClassDefFoundError: modules/wiring/IdentityCutoverWiring\$' "$work/out" && echo yes || echo no) \
$(grep -q 'LANDABLE' "$work/out" && echo yes || echo no)"
measure --base "$base" --head "$sixth" --countries pl --out "$work/recovered"
check "...and the next run resolves its ratchet again" "0 yes" \
  "$(cat "$work/status") $(grep -q '^LANDABLE (local)' "$work/out" && echo yes || echo no)"

# 2026-10-21: agents measured one base..head on several recordings at once (three heavy slots). The runs shared one out
# dir (named by the refs alone) and read the takes through its links, which the other recording's run re-pointed: a
# diff of one recording's base against another's head, thousands of listings "on one side only", NOT LANDABLE -- or,
# both links re-pointed, a verdict over the wrong recording, a regression on the right one unseen: a false LANDABLE.
race() { # <tag> -> the race's two runs: a run on recording 41 held in its first diff while one on 39 runs whole
  rm -f "$work/read" "$work/pause" "$work/pause.reached"
  touch "$work/pause"
  ( STUB_PAUSE="$work/pause" STUB="$work" KINOWO_IDENTITY_CACHE="$cache" perl -e 'alarm 60; exec @ARGV' bash "$repo/scripts/identity-measure-local" \
      --base "$base" --head "$head" --recording 41 --countries pl "$@" > "$work/race41.out" 2>&1; echo $? > "$work/race41.status" ) &
  local first=$!
  wait_until "the first run reached its pause" '[ -e "$work/pause.reached" ]'
  STUB="$work" KINOWO_IDENTITY_CACHE="$cache" perl -e 'alarm 60; exec @ARGV' bash "$repo/scripts/identity-measure-local" \
    --base "$base" --head "$head" --recording 39 --countries pl "$@" > "$work/race39.out" 2>&1; echo $? > "$work/race39.status"
  rm -f "$work/pause"; wait "$first"
}
race
check "one base..head measured on two recordings at once: each run's diff reads its own recording's takes, both sides" \
  "0 0 yes yes" "$(cat "$work/race41.status") $(cat "$work/race39.status") \
$(grep -qx '{"recording": "41"} {"recording": "41"}' "$work/read" && echo yes || echo no) \
$(grep -qx '{"recording": "39"} {"recording": "39"}' "$work/read" && echo yes || echo no)"
race --out "$work/one-out"
check "...even with one --out named for both" "0 0 yes yes" "$(cat "$work/race41.status") $(cat "$work/race39.status") \
$(grep -qx '{"recording": "41"} {"recording": "41"}' "$work/read" && echo yes || echo no) \
$(grep -qx '{"recording": "39"} {"recording": "39"}' "$work/read" && echo yes || echo no)"

seventh=$(echo v > "$repo/seventh"; scratch_git "$repo" add -A; scratch_git "$repo" -c user.email=t@t -c user.name=t commit -qm seventh; scratch_git "$repo" rev-parse --short=12 HEAD)
# a resolve killed mid-write (an agent's timeout, a kill -9) left half a dump; the next run took any non-empty file for a
# finished one, and measured against it
touch "$work/crash-dump"
measure --base "$base" --head "$seventh" --countries pl --out "$work/half"
rm -f "$work/crash-dump" "$work/calls"
check "a resolve whose sbt dies mid-dump: a setup error, no verdict" "2 no" \
  "$(cat "$work/status") $(grep -q 'LANDABLE' "$work/out" && echo yes || echo no)"
measure --base "$base" --head "$seventh" --countries pl --out "$work/half2"
check "...and its half dump is never taken for a finished one: the next run resolves it again" "0 yes" \
  "$(cat "$work/status") $(grep -q "^$seventh worker/IntegrationTest/runMain integration.IdentityResolveDump" "$work/calls" && echo yes || echo no)"

measure --base "$base" --head "$head" --recording 99
check "a recording with no laid pair is a setup error" "2" "$(cat "$work/status")"

spec_summary
