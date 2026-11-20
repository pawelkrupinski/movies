#!/usr/bin/env bash
# identity-what-if --bisect: over a scratch repository's commits, each side read from the what-if's kept-side cache (the
# seam a whole resolve fills: <cache>/what-if/<cc>/<sha>-<tools hash>-live/<snapshot>/), it names the first commit whose
# take of the listing differs from the good one's -- and, when good and bad take alike, says the code did not move it.
# No JVM, no mirror: every side is kept, and the mirror's freshness check is a stub.
# Then THE MAIN PATH over stub sbt and java: an inert change starts no head JVM and compiles no head, the base's whole
# side runs in a one-shot JVM (never a kept one), and a head that resolves compiles beside the base's resolve.
# Run: bash scripts/identity-what-if-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m identity-what-if: --bisect, and the main path over stub JVMs\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
repo="$work/repo"
mkdir -p "$repo/scripts/ci" "$repo/scripts/local-mirror"
cp "$REPO_ROOT"/scripts/identity-what-if "$REPO_ROOT"/scripts/pure-move-check "$REPO_ROOT"/scripts/identity-what-if-roots.tsv "$REPO_ROOT"/scripts/*.sh "$REPO_ROOT"/scripts/*.py "$repo/scripts/"
cp "$REPO_ROOT"/scripts/ci/*.py "$repo/scripts/ci/"
# the tool files the tool keys its sides by (TOOL_FILES), as the checkout holds them
eval "$(sed -n '/^TOOL_FILES=(/,/)/p' "$REPO_ROOT/scripts/identity-what-if")"
for f in "${TOOL_FILES[@]}"; do mkdir -p "$repo/$(dirname "$f")"; cp "$REPO_ROOT/$f" "$repo/$f"; done
# THE MIRROR'S GENERATION, as freshness --generation reports it: each call takes the next line of $work/generations
# (the seed generation) and of $work/tails (the tail position) -- the last one stays --, so a case can seed or tail a
# write mid-run; `100.1` (one finished seed) and `T0` by default -- and of $work/behind, the seconds the tail's last
# change lies behind (5 by default). Each call also notes the fences in force then.
cat > "$repo/scripts/local-mirror/freshness" <<STUB
#!/usr/bin/env bash
next() { local v; v=\$(head -1 "\$1" 2>/dev/null); [ "\$(wc -l 2>/dev/null < "\$1" || echo 0)" -le 1 ] || { tail -n +2 "\$1" > "\$1.next"; mv "\$1.next" "\$1"; }; echo "\${v:-\$2}"; }
# (the drain's --tail read takes the next line of $work/behind alone)
case " \$* " in
  *" --tail "*) gen="" tail=\$(head -1 "$work/tails" 2>/dev/null); tail=\${tail:-T0}; behind=\$(next "$work/behind" 5) ;;
  *) gen=\$(next "$work/generations" 100.1) tail=\$(next "$work/tails" T0) behind=\$(head -1 "$work/behind" 2>/dev/null); behind=\${behind:-5} ;;
esac
echo "freshness \$*" >> "$work/freshness-calls"
cat "$work"/fence/kinowo_uk/* >> "$work/fences-seen" 2>/dev/null
cat "$work"/fence/kinowo_de/* >> "$work/fences-seen-de" 2>/dev/null
case " \$* " in *" --tail "*) echo "TAIL \$1 \$tail \$(( (\$(date +%s) - behind) * 1000 ))" ;; *" --generation "*) echo "GENERATION \$1 \$gen"; echo "TAIL \$1 \$tail \$(( (\$(date +%s) - behind) * 1000 ))" ;; esac
exit 0
STUB
chmod +x "$repo/scripts/local-mirror/freshness"
scratch_repo "$repo" -q
commit() { echo "$1" > "$repo/change.txt"; scratch_git "$repo" add -A; scratch_git "$repo" commit -q -m "$1"; scratch_git "$repo" rev-parse HEAD; }
c0=$(commit "good") c1=$(commit "inert") c2=$(commit "moves the take") c3=$(commit "after it")

# the tool files' hash, as the tool keys its kept sides
hash=$(cd "$repo" && bash -c '. scripts/identity-tools.sh; identity_tools_hash "$@"' _ "${TOOL_FILES[@]}")
at=2026-11-05T20:10:00Z since=2026-11-04T20:10:00Z
key=$'Pthe venue\x1ethe film'
# the mirror generation the tool keys its sides by: the seed generation and the tail position's digest
gen=100.1-tail$(printf '%s' T0 | shasum | cut -c1-12)
side() { # <sha> <snapshot> <model take> <final take>: a kept whole side of that commit
  local d="$work/cache/what-if/uk/$1-$hash-live-mirror$gen/$2"
  mkdir -p "$d"
  printf 'key\tvenue\trawTitle\tfamily\ttake\trules\n%s\tThe Venue\tThe Film\t%s\t%s\t\n' "$key" "$key" "$3" > "$d/takes.tsv"
  printf 'key\tfinal\tbasis\n%s\t%s\tAgreed\n' "$key" "$4" > "$d/final.tsv"
  printf 'key\tprod\tbasis\n%s\t%s\t-\n' "$key" "$4" > "$d/prod.tsv"
}
bisect() {
  ( cd "$repo" && KINOWO_IDENTITY_CACHE="$work/cache" KINOWO_MIRROR_FENCE_DIR="$work/fence" KINOWO_JVM_GATE_FOLLOW_MAIN=0 TMDB_API_KEY=stub spec_bounded \
      scripts/identity-what-if --base "$c0" --bisect "$c0" "$c3" --listing 'The Venue|The Film' --at "$at" "$@" uk 2> "$work/err" )
}
side "$c0" "$at" none none; side "$c1" "$at" none none; side "$c2" "$at" tmdb:5 tmdb:5; side "$c3" "$at" tmdb:5 tmdb:5
out=$(bisect)
check "--bisect names the first commit whose take of the listing moved" "FIRST MOVED BY ${c2:0:7} moves the take" \
  "$(grep -o 'FIRST MOVED BY .*' <<<"$out" | sed -E 's/BY ([0-9a-f]{7})[0-9a-f]* /BY \1 /')"
check "...with the take before and after it" "model none, final none|model tmdb:5, final tmdb:5" \
  "$(grep -E '^ +(before|after):' <<<"$out" | sed -E 's/^ +(before|after): +//' | paste -sd'|' -)"

# the code takes alike at good and bad: the corpus moved it, read at --since
side "$c2" "$at" none none; side "$c3" "$at" none none; side "$c3" "$since" tmdb:5 tmdb:5
out=$(bisect --since "$since")
check "...and when no commit moves it, the corpus did: the bad code at --since takes otherwise" "yes" \
  "$(grep -q 'CORPUS: the corpus moved it between' <<<"$out" && echo yes || echo "no: $out $(cat "$work/err")")"


# THE MAIN PATH over stub JVMs: `sbt` prints a classpath, `java` is WhatIfResolve (a whole side, or the head's re-resolve
# of the selection: every listing taken alike), each appending what it ran to $STUB/calls, in order.
mkdir -p "$work/bin"
cat > "$work/bin/sbt" <<'STUB'
#!/usr/bin/env bash
echo $$ >> "$STUB/sbt-pids"
echo "sbt-start $(basename "$PWD")" >> "$STUB/calls"
if [ "$(basename "$PWD")" = "${STUB_SLOW_SBT:-}" ]; then sleep 15; else sleep 1; fi
echo "$PWD/worker/target/scala-3.x/test-classes:/lib/a.jar"
echo "sbt-end $(basename "$PWD")" >> "$STUB/calls"
STUB
cat > "$work/bin/java" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$STUB/calls.args"
case " $* " in *" --serve "*) echo "java-serve $(basename "$PWD")" >> "$STUB/calls"; exit 1 ;; esac
out="" select=""
while [ "$#" -gt 0 ]; do case "$1" in --out) out=$2 ;; --select) select=$2 ;; esac; shift; done
echo "java-start $(basename "$PWD")" >> "$STUB/calls"; sleep 2
key=$'Pthe venue\x1ethe film'
mkdir -p "$out"
printf 'key\tvenue\trawTitle\tfamily\ttake\trules\n' > "$out/takes.tsv"
take=tmdb:5; [ -z "$select" ] || take=${STUB_HEAD_TAKE:-tmdb:5}
if [ -z "$select" ] || [ -s "$select" ]; then printf '%s\tThe Venue\tThe Film\t%s\t%s\t\n' "$key" "$key" "$take" >> "$out/takes.tsv"; fi
printf 'key\tfinal\tbasis\n%s\ttmdb:5\tAgreed\n' "$key" > "$out/final.tsv"
if [ -z "$select" ]; then
  printf 'family\tblockKeys\n%s\t\n' "$key" > "$out/families.tsv"
  printf 'key\tprod\tbasis\n%s\ttmdb:5\t-\n' "$key" > "$out/prod.tsv"
else
  printf 'selected\tre-resolved\tgroups\trounds\n1\t1\t1\t1\n' > "$out/region.tsv"
fi
echo "${STUB_ANSWERS:-}" > "$out/answers.txt"
echo "java-end $(basename "$PWD")" >> "$STUB/calls"
STUB
chmod +x "$work/bin/sbt" "$work/bin/java"
c4=$(echo row > "$repo/common-row.txt"; mkdir -p "$repo/common/src/main/resources"; echo row > "$repo/common/src/main/resources/rows.tsv"
  scratch_git "$repo" add -A; scratch_git "$repo" commit -q -m "a resource the lookups read"; scratch_git "$repo" rev-parse HEAD)
what_if() { # <base> <head> <snapshot> [args...]
  local b=$1 h=$2 s=$3; shift 3; rm -f "$work/calls" "$work/calls.args"
  ( cd "$repo" && STUB="$work" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$work/cache" KINOWO_MIRROR_FENCE_DIR="$work/fence" KINOWO_JVM_GATE_FOLLOW_MAIN=0 \
      KINOWO_GOVERNOR=0 KINOWO_JVM_SLOTS=0 KINOWO_MEMORY_GATE=0 TMDB_API_KEY=stub spec_bounded scripts/identity-what-if \
      --base "$b" --head "$h" --at "$s" --judgements "$REPO_ROOT/test/resources/fixtures/identity-judgements.tsv" "$@" uk 2> "$work/err" )
}
calls() { sed -e "s/$c0/base/" -e "s/$c1/head/" -e "s/$c4/head/" "$work/calls" 2>/dev/null | paste -sd'|' -; }
# A function, not a `case` inside `$( )`: macOS /bin/bash 3.2 reads the pattern's `)` as the substitution's end.
head_compiled_beside_base() {
  local c; c=$(calls)
  case "$c" in *"sbt-start head"*"java-end base"*) echo yes ;; *) echo "no: $c $(cat "$work/err")" ;; esac
}

# an inert change (nothing reaches a take) over a base side not yet kept: the base resolves in a JVM of its own (never a
# kept one, which would hold a JVM slot idle for nothing), and the head is neither compiled nor started
out=$(what_if "$c0" "$c1" 2026-11-06T10:00:00Z)
check "an inert change: the base resolves in a one-shot JVM, the head starts no JVM and compiles nothing" \
  "sbt-start base|sbt-end base|java-start base|java-end base" "$(calls)"
check "...and the what-if is CLEAN, 0 moved" "yes" \
  "$(grep -q 'CLEAN' <<<"$out" && grep -q '0 take(s) moved' <<<"$out" && echo yes || echo "no: $out $(cat "$work/err")")"

# a change reaching every take over a base side not yet kept: the head compiles BESIDE the base's resolve, not after it
out=$(what_if "$c0" "$c4" 2026-11-06T11:00:00Z --no-serve)
check "a change the head resolves: its compile starts before the base's resolve ends" "yes" \
  "$(head_compiled_beside_base)"
check "...and the head resolves once both are done" "java-start head|java-end head" \
  "$(calls | tr '|' '\n' | grep 'java-.* head' | paste -sd'|' -)"

# A PURE MOVE (scripts/pure-move-check) measures nothing: no compile, no JVM, a `pure-move` verdict a country recorded for
# identity-measure's what-if gate
c5=$(mkdir -p "$repo/moved"; scratch_git "$repo" mv common-row.txt moved/common-row.txt; scratch_git "$repo" commit -q -m "a pure move"
  scratch_git "$repo" rev-parse HEAD)
out=$(what_if "$c4" "$c5" 2026-11-06T12:00:00Z)
check "a pure move: what-if says so and is skipped, compiling and starting nothing" \
  "PURE MOVE: what-if skipped (scripts/pure-move-check)|" "$(grep 'PURE MOVE' <<<"$out")|$(calls)"
check "...and records a pure-move verdict for identity-measure's gate" "uk pure-move" \
  "$(cd "$repo" && KINOWO_IDENTITY_CACHE="$work/cache" bash -c '. scripts/identity-tools.sh; what_if_verdicts "$1" "$2"' _ "$c5" "$c4")"
# ...and a change that is no move still resolves (the inert one above was no move either, and resolved)
check "a range that moves nothing says so and resolves as before" "yes" \
  "$(out=$(what_if "$c0" "$c1" 2026-11-06T10:00:00Z)
     grep -q 'not a pure move (scripts/pure-move-check exit 1' "$work/err" && grep -q CLEAN <<<"$out" && echo yes || echo "no: $(cat "$work/err")")"

# A MIRROR CHANGING UNDER THE RUN is a HARNESS ERROR (exit 2), never takes moved (2026-11-19: lane C's US what-if read
# a seeding mirror on one side and reported 87k takes moved; a rerun on a whole mirror moved none).
harness_error() { # <expected message> <exit> <stdout>: exit 2, the message on stderr, no CLEAN/BLOCKING verdict printed
  if [ "$2" = 2 ] && grep -q "$1" "$work/err" && ! grep -qE 'CLEAN|BLOCKING|moved' <<<"$3"; then echo yes
  else echo "no: exit $2, out: $3, err: $(cat "$work/err")"; fi
}
# a seed in flight (the `:seed` mark): refused before anything compiles or resolves
echo seeding > "$work/generations"
out=$(what_if "$c0" "$c4" 2026-11-06T13:00:00Z --no-serve); rc=$?
check "a seeding mirror: exit 2, 'mirror not ready', nothing reported moved" "yes" \
  "$(harness_error 'HARNESS ERROR.*mirror not ready.*seed is in flight' "$rc" "$out")"
check "...and nothing compiled or resolved" "" "$(calls)"
# a token lost, its re-seed not yet begun: refused too
echo reseed-due > "$work/generations"
out=$(what_if "$c0" "$c4" 2026-11-06T13:00:00Z --no-serve); rc=$?
check "a mirror whose re-seed is due: exit 2, mirror not ready" "yes" "$(harness_error 'HARNESS ERROR.*mirror not ready' "$rc" "$out")"
# a re-seed finishing while the base resolves: the head would read another corpus than the base did
verdicts() { cat "$(cd "$repo" && KINOWO_IDENTITY_CACHE="$work/cache" bash -c '. scripts/identity-tools.sh; what_if_verdicts_file "$1" "$2"' _ "$c4" "$c0")" 2>/dev/null | wc -l | tr -d ' '; }
verdicts_before=$(verdicts)
printf '100.1\n200.1\n' > "$work/generations"; rm -f "$work/sbt-pids"
# (the head's compile, started beside the base, outlasting the base's resolve)
out=$(STUB_SLOW_SBT=$c4 what_if "$c0" "$c4" 2026-11-06T14:00:00Z --no-serve); rc=$?
check "a re-seed between the base and the head: exit 2, naming both generations" "yes" \
  "$(harness_error 'HARNESS ERROR.*re-seeded during the run.*100.1.*200.1' "$rc" "$out")"
check "...and the base side it resolved on the old generation is not kept" "no" \
  "$(ls -d "$work/cache/what-if/uk/$c0-$hash-live-mirror$gen/2026-11-06T14:00:00Z" >/dev/null 2>&1 && echo yes || echo no)"
check "...and no verdict is recorded for identity-measure's gate" "$verdicts_before" "$(verdicts)"
# an exit leaves no compile of the head behind it: an orphan wrote into the head's tree while the next run laid it out
# again, and that run failed with no word (Order independence #90, the case below)
check "...and the head's compile it started beside the base is no longer running" "0" "$(spec_still_running "$work/sbt-pids")"
# a seed starting while the head resolves: caught by the check after the head
printf '100.1\n100.1\nseeding\n' > "$work/generations"; rm -f "$work/freshness-calls"
out=$(what_if "$c0" "$c4" 2026-11-06T15:00:00Z --no-serve); rc=$?
check "a seed starting during the head: exit 2, caught after the head" "yes" \
  "$(harness_error 'HARNESS ERROR.*re-seeded during the run' "$rc" "$out")"
check "...the generation read before the base, between the sides and after the head" "3" \
  "$(grep -c -- '--generation' "$work/freshness-calls")"
rm -f "$work/generations" "$work/freshness-calls"
# A WRITE TAILED BETWEEN THE SIDES (2026-11-20, lane A3: a DE what-if moved a take its own base-vs-base control moved too,
# a rerun none): the seed generation stands, the tail position moved -- each side read another corpus. Exit 2, never moves
printf 'T0\nT1\n' > "$work/tails"; rm -rf "$work/fence" "$work/fences-seen"
out=$(what_if "$c0" "$c4" 2026-11-06T15:30:00Z --no-serve); rc=$?
check "a write tailed between the base and the head: exit 2, the mirror tailed a write" "yes" \
  "$(harness_error 'HARNESS ERROR.*tailed a write during the run' "$rc" "$out")"
check "...and the base side it resolved off the earlier copy is not kept" "no" \
  "$(ls -d "$work/cache/what-if/uk/$c0-$hash-live-mirror$gen/2026-11-06T15:30:00Z" >/dev/null 2>&1 && echo yes || echo no)"
rm -f "$work/tails"
# THE FENCE: every read of the generation is made while the run's fence holds the tailer; the exit leaves it lingering
rm -rf "$work/fence" "$work/fences-seen"
out=$(what_if "$c0" "$c4" 2026-11-06T15:40:00Z --no-serve); rc=$?
check "the tailer is fenced for the run: each generation read sees the run's fence" "0 run run run" \
  "$rc $(awk '{ print $2 }' "$work/fences-seen" 2>/dev/null | paste -sd' ' -)"
check "...and the exit leaves it lingering, so a rerun reads the same copy" "linger" \
  "$(cat "$work"/fence/kinowo_uk/what-if-* 2>/dev/null | awk '{ print $2 }' | paste -sd' ' -)"
# THE DRAIN (back-to-back what-ifs of many agents would hold the tail for good): a mirror more than 20 min behind is
# let catch up before the run fences -- after every other run holding it has ended
rm -rf "$work/fence" "$work/fences-seen"; mkdir -p "$work/fence/kinowo_uk"
sleep 600 & other=$!   # another agent's what-if, mid-run (HUNG STUBS: it must stay alive until the case ends it)
echo "$(( $(date +%s) + 3600 )) run" > "$work/fence/kinowo_uk/what-if-$other"
printf '1800\n1800\n60\n' > "$work/behind"
( KINOWO_WHAT_IF_DRAIN_POLL_SECONDS=0.2 what_if "$c0" "$c4" 2026-11-06T15:50:00Z --no-serve > "$work/drain-out"; echo $? > "$work/drain-rc" ) &
wait_until "the run waits for the other run holding the tail" grep -q 'waiting for the runs holding the tail to end: what-if-'"$other" "$work/err"
check "...and has not fenced meanwhile" "run" "$(awk '{ print $2 }' "$work"/fence/kinowo_uk/what-if-* | paste -sd' ' -)"
kill "$other"; wait "$other" 2>/dev/null
wait_until "the run goes on once the other ended" test -s "$work/drain-rc"
check "...drains, then fences and resolves: CLEAN" "0 yes yes" \
  "$(cat "$work/drain-rc") $(grep -q 'drained (60s behind)' "$work/err" && echo yes || echo "no: $(cat "$work/err")") $(grep -q CLEAN "$work/drain-out" && echo yes || echo no)"
check "...the generation pinned only after the drain: every read of it sees the run's own fence" "run run run" \
  "$(awk '{ print $2 }' "$work/fences-seen" | tail -3 | paste -sd' ' -)"
rm -f "$work/behind"
# A RUN OF SEVERAL COUNTRIES fences each for its own sides only: a country done lingers while the next resolves
rm -rf "$work/fence" "$work/fences-seen" "$work/fences-seen-de"
out=$(what_if "$c0" "$c4" 2026-11-06T15:55:00Z --no-serve de); rc=$?
check "two countries: de's fence lingers while uk's sides resolve" "0 linger" \
  "$rc $(awk '{ print $2 }' "$work/fences-seen-de" 2>/dev/null | tail -1)"
# the kept JVM's loaded corpus is keyed by the generation too: a corpus loaded off a torn copy is never served again
out=$(what_if "$c0" "$c4" 2026-11-06T16:00:00Z)
check "every side is asked with the mirror generation it must read (the kept JVM's corpus key)" "yes" \
  "$(grep -q -- "--mirror-generation $gen" "$work/calls.args" && echo yes || echo "no: $(cat "$work/calls.args" 2>/dev/null)")"
# a side whose live asks failed past the bound (lane C: 16k failed on the base) is a HARNESS ERROR, never moves
out=$(STUB_ANSWERS="16000 live request(s) (api.themoviedb.org 16000; 16000 failed) in 300s, ~53.3 req/s" \
  what_if "$c0" "$c4" 2026-11-06T17:00:00Z --no-serve); rc=$?
check "16000 failed live asks: exit 2, 'answers failed', nothing reported moved" "yes" \
  "$(harness_error 'HARNESS ERROR.*answers failed.*16000 of 16000' "$rc" "$out")"
check "...and the base side holding those failures is not kept" "no" \
  "$(ls -d "$work/cache/what-if/uk/$c0-$hash-live-mirror$gen/2026-11-06T17:00:00Z" >/dev/null 2>&1 && echo yes || echo no)"
out=$(STUB_ANSWERS="400 live request(s) (api.themoviedb.org 400; 3 failed) in 30s, ~13.3 req/s" \
  what_if "$c0" "$c4" 2026-11-06T18:00:00Z --no-serve); rc=$?
check "a few failed asks (3, within the bound) still diff" "0 yes" "$rc $(grep -q CLEAN <<<"$out" && echo yes || echo "no: $(cat "$work/err")")"

# HEAD = BASE MUST MOVE NOTHING (2026-11-19, lane E2: a control with head = base reported the same 462 US moves as the
# change): when both sides run the same code, any moved take is the harness's, a HARNESS ERROR, never a verdict
out=$(what_if "$c0" "$c0" 2026-11-06T19:00:00Z --all --no-serve); rc=$?
check "head = base taking alike: CLEAN, 0" "0 yes" "$rc $(grep -q CLEAN <<<"$out" && echo yes || echo "no: $(cat "$work/err")")"
out=$(STUB_HEAD_TAKE=tmdb:6 what_if "$c0" "$c0" 2026-11-06T20:00:00Z --all --no-serve); rc=$?
check "head = base taking otherwise: exit 2, the same code moved takes" "yes" \
  "$(if [ "$rc" = 2 ] && grep -q 'HARNESS ERROR.*same code.*moved' "$work/err"; then echo yes; else echo "no: exit $rc, err: $(cat "$work/err")"; fi)"
check "...and the base side kept from that run is not kept" "no" \
  "$(ls -d "$work/cache/what-if/uk/$c0-$hash-live-mirror$gen/2026-11-06T20:00:00Z" >/dev/null 2>&1 && echo yes || echo no)"

# A REF IS LAID OUT ONCE (Order independence #90): the trees here hold no build.sbt, which identity_tree took for "not
# laid out yet", so every run removed and laid out again the trees another step of it was reading -- a run on a slow
# runner then died between the layout and its next step. Both refs were laid out above: a run over them lays out none.
out=$(what_if "$c0" "$c4" 2026-11-06T20:30:00Z --no-serve)
check "a ref laid out before is not laid out again" "0" "$(grep -cE "laying out [0-9a-f]{40} " "$work/err")"

# A STEP BEFORE THE RESOLVE FAILING says which one (Order independence #90: exit 2, nothing on stderr after "laying out"
# the head, so neither the case nor the cause could be told): a head whose tree cannot be laid out (its refs directory
# read-only) is a HARNESS ERROR naming the head and the step's exit status
c6=$(commit "a head laid out nowhere")
chmod 555 "$work/cache/refs"
out=$(what_if "$c0" "$c6" 2026-11-06T21:00:00Z --no-serve); rc=$?
chmod 755 "$work/cache/refs"
check "a head whose tree cannot be laid out: exit 2, naming the step and its status" "yes" \
  "$(harness_error "HARNESS ERROR.*laying out the head ${c6:0:12}.* failed (exit [0-9]*)" "$rc" "$out")"
# ...and so does a bisect step whose whole side is not kept and whose tree cannot be laid out
chmod 555 "$work/cache/refs"
out=$( cd "$repo" && KINOWO_IDENTITY_CACHE="$work/cache" TMDB_API_KEY=stub spec_bounded scripts/identity-what-if --base "$c0" \
  --bisect "$c0" "$c6" --listing 'The Venue|The Film' --at "$at" uk 2> "$work/err" ); rc=$?
chmod 755 "$work/cache/refs"
check "a bisect step whose tree cannot be laid out: exit 2, naming the commit and the status" "yes" \
  "$(harness_error "HARNESS ERROR.*laying out ${c6:0:12} for its whole side failed (exit [0-9]*)" "$rc" "$out")"

spec_summary
