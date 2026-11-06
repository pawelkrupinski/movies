#!/usr/bin/env bash
# identity-drift-check.sh against a stand-in sbt in a scratch git repository (a base commit and a head commit):
#   - divergences beyond the allow-list fail (1) whatever the base did -- the hard invariant; allowed ones pass;
#   - FLIPS ARE DIFFERENTIAL: only a flip the head shows on a seed and its base does not fails (1), naming the listing;
#     one both show is a `pre-existing (backlog)` line and passes (0), one only the base shows is `removed:`;
#   - a head whose identity code is its base's replays nothing twice and never fails on flips;
#   - a newborn check's TEMPORARY flips-allow row passes its head-only flip;
#   - a replay that wrote no report, no flips line, or no recording to replay is no verdict (2);
#   - the row's fixed seeds go to the head in one replay, to the base one replay a seed;
#   - a red names its seed (verdict, seed file, a one-line reproducer with the red listing's drift slice);
#   - a base that does not compile steps back to its parent (DRIFT_BASE_STEPS), saying so; one that never does is no verdict;
#   - DRIFT_TWICE: two replays of the same seeds that differ fail;
#   - THE SCENARIO GUARD: head and base that drew differently for a question both ask (a coupled harness: the old shared
#     rng) is a HARNESS ERROR (2), never flips; a question only the head asks is its own; a base with no scenario (from
#     before the guard) is not compared.
# Run: bash scripts/ci/identity-drift-check-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m identity-drift-check.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
repo="$work/repo" fx="$work/repo/test/resources/fixtures"
mkdir -p "$repo/scripts/ci" "$fx/corpus" "$repo/identity/src/main" "$work/bin" "$work/tmp"
cp "$REPO_ROOT/scripts/ci/identity-drift-check.sh" "$REPO_ROOT/scripts/ci/drift-replays-compare.sh" "$REPO_ROOT/scripts/ci/drift_seeds.py" "$REPO_ROOT/scripts/ci/drift_diff.py" "$REPO_ROOT/scripts/ci/drift_explorer.py" \
  "$REPO_ROOT/scripts/ci/drift_scenario.py" "$repo/scripts/ci/"
printf 'country\tvenue\trawTitle\treason\npl\tKino Nowe\tAllowed Film\tTEMPORARY owner: x; remove when y; z\n' > "$fx/identity-flips-allow.tsv"
printf 'kind\tcountry\tseed\tslice\tvenue\trawTitle\ttakes\tfirstSeen\towner\tnote\ndrift\tpl\t1\t\tRiverside\tThe Stranger\t\t2026-10-29\tthe red fixer\tx\n' > "$fx/identity-flip-backlog.tsv"
# sbt as the replay leaves it: the report on stdout, the TSVs under --out, IdentityDrift's exit. The flips are
# FAKE_HEAD_FLIPS at the head and FAKE_BASE_FLIPS in the base's checkout: `venue|title;venue|title`, on every seed. Its
# scenario: one arrangement, and the fill draws FAKE_HEAD_DRAWS / FAKE_BASE_DRAWS (`round:question=0|1,...`; default one
# draw both sides make alike); FAKE_BASE_DRAWS=none writes no scenario (a base from before the guard).
cat > "$work/bin/sbt" <<'EOF'
#!/usr/bin/env bash
args=$(printf '%s\n' "$@")
out=$(grep -oE -- '--out [^ ]+' <<< "$args" | tail -1 | cut -d' ' -f2)
list=$(grep -oE -- '--seed [0-9,]+' <<< "$args" | tail -1 | cut -d' ' -f2)
side=head; case $PWD in *drift-base-checkout*) side=base ;; esac
echo "$side $list" >> "$FAKE_LOG"
printf '%s\n' "$@" > "$out.args"
[ "${FAKE_REPORT:-}" = none ] && { echo "[error] compilation failed"; exit 1; }
# FAKE_BROKEN_BASE: a base checkout at this commit (or `all`) fails to compile, as #950's 053f867ea9 did
[ -n "${FAKE_BROKEN_BASE:-}" ] && . "$SCRATCH_GIT_LIB"
case "${FAKE_BROKEN_BASE:-}" in "") ;; all|"$(scratch_git "$PWD" rev-parse HEAD 2>/dev/null)")
  [ "$side" = base ] && { echo "[error] (worker / Test / compileIncremental) Compilation failed"; exit 1; } ;; esac
mkdir -p "$out"; printf 'country\tseed\n' > "$out/drift-pl.tsv"
flips=${FAKE_HEAD_FLIPS:-}; [ "$side" = base ] && flips=${FAKE_BASE_FLIPS:-}
n=$(( $(cat "$out/../runs" 2>/dev/null || echo 0) + 1 )); echo "$n" > "$out/../runs"
{ printf 'country\tseed\tvenue\trawTitle\tfilms\treasons\ttakes\tcause\n'
  IFS=, read -ra seeds <<< "$list"
  for s in "${seeds[@]}"; do
    IFS=';' read -ra rows <<< "$flips"
    for r in "${rows[@]}"; do [ -n "$r" ] && printf 'pl\t%s\t%s\t%s\ttmdb:1, tmdb:2\tback to a film it left\ttmdb:1 [3 scrape] -> tmdb:2 [4 answer] -> tmdb:1 [9 scrape]\tanswer\n' "$s" "${r%%|*}" "${r#*|}"; done
    [ -n "${FAKE_VARY:-}" ] && printf 'pl\t%s\tRun\t%s\ta\tb\tc\td\n' "$s" "$n"
  done; } > "$out/flips-pl.tsv"
draws=${FAKE_HEAD_DRAWS:-1:q:shared=1}; [ "$side" = base ] && draws=${FAKE_BASE_DRAWS:-1:q:shared=1}
if [ "$draws" != none ]; then
  { printf 'seed\tkind\tkey\tvalue\n'
    for s in "${seeds[@]}"; do
      printf '%s\tarrangement\tinputs\taa11\n%s\tarrangement\thalf-1\tbb22\n' "$s" "$s"
      IFS=, read -ra ds <<< "$draws"
      for d in "${ds[@]}"; do printf '%s\tfill\t%s\t%s\n' "$s" "${d%=*}" "${d##*=}"; done
    done; } > "$out/scenario-pl.tsv"
fi
echo "## pl: 10804 listings, 1 seed(s), ${FAKE_FOUND:-0} divergence(s) (1 listings), ${FAKE_ALLOWED:-0} allow-listed, in 191s"
[ "${FAKE_FOUND:-0}" -gt 0 ] && echo "  1 listing(s)  deploy: incremental-only take (whole BelowThreshold, incremental Agreed)"
[ "${FAKE_FLIPS:-}" = none ] && exit 0
echo "## pl flips: 9 flip(s) (1 listings), 0 allow-listed, 0 known"
echo "  1 listing(s)  flip: back to a film it left"
echo "      Head Row | Not The Verdict: tmdb:1 [1 boot] -> tmdb:2 [2 x] [seed 1]"
exit 0
EOF
chmod +x "$work/bin/sbt"
export FAKE_LOG="$work/sbt-calls" RUNNER_TEMP="$work/tmp" SCRATCH_GIT_LIB="$REPO_ROOT/scripts/scratch-git.sh"
scratch_repo "$repo" -q -b main
echo base > "$repo/identity/src/main/Rule.scala"; scratch_git "$repo" add -A && scratch_git "$repo" commit -qm base
echo docs > "$repo/README.md"; scratch_git "$repo" add -A && scratch_git "$repo" commit -qm "docs only"

run() { : > "$FAKE_LOG"; rm -rf "$work/out"; PATH="$work/bin:$PATH" bash "$repo/scripts/ci/identity-drift-check.sh" pl "$work/out" > "$work/stdout" 2>&1; echo $?; }

check "no recorded corpus to replay: no verdict" "2" "$(run)"
touch "$fx/corpus/cinema-scrapes-pl.json.gz"
check "no divergence, no flip passes" "0" "$(run)"
check "divergences the allow-list does not name fail" "1" "$(FAKE_FOUND=3 run)"
check "...and the verdict names them" "3 divergence(s) from a whole resolve and 0 flip(s) on the way the change adds over its base, beyond the allow-lists; 0 pre-existing (backlog) flip(s) not failed -- seed(s) 1 2" "$(cat "$work/out/verdict.txt")"
check "...and the report names their class" "1" "$(grep -c 'incremental-only take' "$work/out/report.txt")"
check "divergences the allow-list names all pass" "0" "$(FAKE_FOUND=3 FAKE_ALLOWED=3 run)"
check "a replay that never reported is no verdict" "2" "$(FAKE_REPORT=none run)"
check "a replay that reported no flips line is no verdict" "2" "$(FAKE_FLIPS=none run)"

# THE EXPLORER (DRIFT_EXPLORE): the head alone, never red, its flips no backlog row names written for agentctl
check "the explorer never fails on a flip" "0" "$(DRIFT_EXPLORE=1 DRIFT_EXPLORE_REF=run:9/a FAKE_HEAD_FLIPS='Riverside|The Stranger;Kino Nowy|New Flip' run)"
check "...replaying the head alone" "head 1,2" "$(cat "$FAKE_LOG")"
check "...writing the flip no backlog row names, once, its lowest seed" "Kino Nowy New Flip pl 1 run:9/a" \
  "$(sed 1d "$work/out/explore-pl.tsv" | cut -f1-4,6 | tr '\t' ' ')"
check "...and naming it in the log, never as a red row" "1 0" \
  "$(grep -c '^      explored: Kino Nowy | New Flip \[seed 1\]' "$work/stdout") $(grep -cE '^      Kino Nowy \| New Flip' "$work/stdout")"

# THE SAME IDENTITY CODE (the head changed docs only): every flip is pre-existing, the base is not replayed
check "a head with its base's identity code never fails on flips" "0" "$(FAKE_HEAD_FLIPS='Riverside|The Stranger' run)"
check "...replaying the head alone" "head 1,2" "$(cat "$FAKE_LOG")"
check "...naming the flip pre-existing, with its backlog row" "2" "$(grep -c '^      pre-existing (backlog): Riverside | The Stranger \[seed [12]\] -- in the base too, not failed; backlog: owner the red fixer, first seen 2026-10-29' "$work/stdout")"
check "...never as the red's row" "0" "$(grep -cE '^      Riverside \| The Stranger' "$work/stdout")"
check "...and not the head's raw flip rows" "0" "$(grep -c 'Head Row' "$work/stdout")"
check "...but divergences still fail it" "1" "$(FAKE_FOUND=1 FAKE_HEAD_FLIPS='Riverside|The Stranger' run)"

# AN IDENTITY CHANGE: the base is replayed on the same seeds, one replay a seed
echo head > "$repo/identity/src/main/Rule.scala"; scratch_git "$repo" commit -qam "an identity change"
check "a flip head and base both show passes" "0" "$(FAKE_HEAD_FLIPS='Riverside|The Stranger' FAKE_BASE_FLIPS='Riverside|The Stranger' run)"
check "...the base replayed a seed at a time" "head 1,2 base 1 base 2" "$(paste -sd' ' "$FAKE_LOG")"
check "...said pre-existing" "1" "$(grep -c 'pre-existing (backlog): Riverside | The Stranger \[seed 1\]' "$work/stdout")"
check "...the verdict counting it apart" "0 divergence(s) from a whole resolve and 0 flip(s) on the way the change adds over its base, beyond the allow-lists; 2 pre-existing (backlog) flip(s) not failed -- seed(s) 1 2" "$(cat "$work/out/verdict.txt")"
check "a flip the head adds fails" "1" "$(FAKE_HEAD_FLIPS='Riverside|The Stranger;Kino Muza|New Film' FAKE_BASE_FLIPS='Riverside|The Stranger' run)"
check "...naming the listing as the red's row" "2" "$(grep -cE '^      Kino Muza \| New Film: tmdb:1 \[3 scrape\] -> tmdb:2 \[4 answer\] -> tmdb:1 \[9 scrape\] \[seed [12]\]$' "$work/stdout")"
check "...and the old one pre-existing" "2" "$(grep -c 'pre-existing (backlog): Riverside | The Stranger' "$work/stdout")"
check "...the verdict naming the flips it adds" "1" "$(grep -c ' and 2 flip(s) on the way the change adds over its base' "$work/out/verdict.txt")"
check "...the new flips written" "New Film" "$(tail -n +2 "$work/out/new-1.tsv" | cut -f4)"
check "a flip the head removes passes, said" "0 1" "$(FAKE_BASE_FLIPS='Riverside|The Stranger' run) $(grep -c '^      removed: Riverside | The Stranger \[seed 1\]' "$work/stdout")"
check "a newborn check's TEMPORARY row passes its head-only flip" "0" "$(FAKE_HEAD_FLIPS='Kino Nowe|Allowed Film' run)"
check "...said allow-listed" "1" "$(grep -c 'allow-listed (identity-flips-allow.tsv): Kino Nowe | Allowed Film \[seed 1\]' "$work/stdout")"
check "an explicit base is the one judged against" "1" "$(DRIFT_BASE=$(scratch_git "$repo" rev-parse HEAD~2) FAKE_HEAD_FLIPS='Riverside|The Stranger' run)"
check "...resolved on each seed" "head 1,2 base 1 base 2" "$(paste -sd' ' "$FAKE_LOG")"
check "...and named" "2" "$(grep -c "head vs base $(scratch_git "$repo" rev-parse HEAD~2 | cut -c1-12) (resolved)" "$work/stdout")"
broken=$(scratch_git "$repo" rev-parse HEAD~1) parent=$(scratch_git "$repo" rev-parse HEAD~2)
check "a base that does not compile steps back to its parent" "0" \
  "$(DRIFT_BASE=$broken FAKE_BROKEN_BASE=$broken FAKE_HEAD_FLIPS='Riverside|The Stranger' FAKE_BASE_FLIPS='Riverside|The Stranger' run)"
check "...replaying the parent on every seed" "head 1,2 base 1 base 1 base 2" "$(paste -sd' ' "$FAKE_LOG")"
check "...saying so" "1" "$(grep -c "the base ${broken:0:12} does not compile; judged against its parent ${parent:0:12} instead" "$work/stdout")"
check "...and judging against the parent" "2" "$(grep -c "head vs base ${parent:0:12} (resolved)" "$work/stdout")"
check "a base that never compiles is still no verdict" "2" "$(DRIFT_BASE=$broken FAKE_BROKEN_BASE=all DRIFT_BASE_STEPS=1 FAKE_HEAD_FLIPS='Riverside|The Stranger' run)"

# THE SCENARIO GUARD: head and base must have drawn alike for what both ask, before any flip is judged
check "head and base drawing alike pass" "0" "$(FAKE_HEAD_DRAWS='1:q:a=1,1:q:b=0' FAKE_BASE_DRAWS='1:q:a=1,1:q:b=0' run)"
check "...said per seed" "2" "$(grep -c '^scenario: the same on seed [12]' "$work/stdout")"
check "a question only the head asks is its own" "0" "$(FAKE_HEAD_DRAWS='1:q:new=1,1:q:a=1' FAKE_BASE_DRAWS='1:q:a=1' run)"
check "a coupled harness (the old shared rng: one question more re-deals the draws both make) is a harness error" "2" \
  "$(FAKE_HEAD_DRAWS='1:q:new=1,1:q:a=0,1:q:b=1' FAKE_BASE_DRAWS='1:q:a=1,1:q:b=0' FAKE_HEAD_FLIPS='Kino Muza|New Film' run)"
check "...the verdict a harness error, never flips" "1 0" \
  "$(grep -c '^harness error: head and base replayed different scenarios on seed(s) 1 2' "$work/out/verdict.txt") $(grep -c 'flip(s) on the way' "$work/out/verdict.txt")"
check "...loudly, each seed named" "1 2" "$(grep -c '^::error title=identity drift harness (pl, seed 1)::harness error' "$work/stdout") $(grep -c 'HARNESS: 2 of the 2 fill draw(s) both sides made differ' "$work/stdout")"
check "...and no flip judged" "0" "$(grep -c 'Kino Muza | New Film' "$work/stdout")"
check "a base from before the guard (no scenario) is not compared" "0" "$(FAKE_BASE_DRAWS=none run)"
check "...said" "2" "$(grep -c '^scenario: not compared on seed [12] -- the base wrote no scenario' "$work/stdout")"

# THE ROW'S FIXED SEEDS, and a red names its seed
mkdir -p "$fx/drift-slices/pl/muza-new-film-1"
python3 -c 'import gzip,json,sys; json.dump({"country":"pl","core":["N\0Kino Muza\0https://x\0New Film"],"context":[]}, gzip.open(sys.argv[1],"wt"))' \
  "$fx/drift-slices/pl/muza-new-film-1/slice.json.gz"
check "a row's fixed seeds: the head in one replay" "1" "$(DRIFT_SEED_LIST='2 47' FAKE_HEAD_FLIPS='Kino Muza|New Film' run)"
check "...the base a seed at a time" "head 2,47 base 2 base 47" "$(paste -sd' ' "$FAKE_LOG")"
check "...its verdict naming the seeds" "1" "$(grep -c ' -- seed(s) 2 47$' "$work/out/verdict.txt")"
check "...the seed file a red seed first" "2 47" "$(paste -sd' ' "$work/out/seed")"
check "...a one-line reproducer, with the red listing's drift slice" \
  "IdentityDrift pl --seed 2,47; slice: IdentityDrift pl --slice test/resources/fixtures/drift-slices/pl/muza-new-film-1 --seed 2,47" \
  "$(cat "$work/out/reproduce.txt")"
check "...said in the log" "1" "$(grep -c '^identity drift (pl): red on seed(s) 2 47 -- reproduce: IdentityDrift pl --seed 2,47' "$work/stdout")"
check "one seed alone (DRIFT_SEED)" "0 head 38" "$(DRIFT_SEED=38 run) $(head -1 "$FAKE_LOG")"
check "a green run says no red" "0" "$(grep -c 'red on seed' "$work/stdout")"

check "the daily double run: two replays alike pass" "0" "$(DRIFT_TWICE=true run)"
check "...having replayed twice" "1" "$(grep -c 'deterministic -- a second replay' "$work/stdout")"
check "two replays that differ fail" "1" "$(DRIFT_TWICE=true FAKE_VARY=1 FAKE_BASE_FLIPS='Run|1;Run|2;Run|3;Run|4' run)"
check "...and the verdict says so" "1" "$(grep -c '^not deterministic' "$work/out/verdict.txt")"

spec_summary
