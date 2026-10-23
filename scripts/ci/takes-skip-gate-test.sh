#!/usr/bin/env bash
# takes-skip-gate.sh against a scratch repository and a stub proof: SKIP for a title-rule change the proof clears, and
# RUN for every other case -- each condition failing on its own, every failure mode of the proof, every input missing.
# The gate must NEVER answer SKIP when the takes should run. (scripts/ci/takes-skip-gate-mutation-test.sh runs this file
# against mutants of the gate, each with one check removed, and requires it to fail on every one.)
# shellcheck disable=SC2034 # the case commits are each read by name (${!c})
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"
gate=${TAKES_SKIP_GATE_UNDER_TEST:-$REPO_ROOT/scripts/ci/takes-skip-gate.sh}
printf '\033[36m▸\033[0m takes-skip-gate.sh\n'
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# THE REPOSITORY: a base, then one commit per case, each on the base (a branch of its own).
scratch_repo "$work/repo"
put() { mkdir -p "$work/repo/$(dirname "$1")" && printf '%s\n' "${2:-x}" > "$work/repo/$1"; }
for f in common/src/main/resources/title-rules.tsv common/src/main/scala/services/titlerules/Rules.scala \
         common/src/main/scala/services/titlerules/TitleRulePlaceholders.scala common/src/main/scala/services/movies/TitleNormalizer.scala \
         identity/src/main/scala/services/identity/Resolver.scala common/src/main/scala/services/movies/Matcher.scala \
         worker/src/main/scala/services/Projection.scala docs/a.md README.md scripts/ci/takes-skip-gate.sh scripts/ci/identity-takes-check.sh \
         testkit/src/main/scala/services/movies/NormalizerTrace.scala testkit/src/main/scala/services/movies/NormalizerTraceCheck.scala \
         test/resources/fixtures/identity-judgements.tsv worker/src/test/scala/scripts/UnmatchedClustersRatchet.scala \
         .github/workflows/x.yml; do put "$f" base; done
scratch_git "$work/repo" add -A && scratch_git "$work/repo" commit -q -m base
base=$(scratch_git "$work/repo" rev-parse HEAD)
case_commit() { # <name> <file>... -> the commit changing those files on the base
  local name=$1 f; shift
  scratch_git "$work/repo" checkout -q -B "$name" "$base"
  for f in "$@"; do put "$f" "changed $f"; done
  scratch_git "$work/repo" add -A && scratch_git "$work/repo" commit -q -m "$name" --allow-empty
  scratch_git "$work/repo" rev-parse HEAD
}
rules=$(case_commit rules common/src/main/resources/title-rules.tsv)
rules_and_docs=$(case_commit rules-docs common/src/main/resources/title-rules.tsv common/src/main/scala/services/titlerules/Rules.scala docs/a.md README.md)
normalizer=$(case_commit normalizer common/src/main/scala/services/movies/TitleNormalizer.scala)
placeholders=$(case_commit placeholders common/src/main/scala/services/titlerules/TitleRulePlaceholders.scala)
resolver=$(case_commit resolver common/src/main/resources/title-rules.tsv identity/src/main/scala/services/identity/Resolver.scala)
matcher=$(case_commit matcher common/src/main/resources/title-rules.tsv common/src/main/scala/services/movies/Matcher.scala)
projection=$(case_commit projection common/src/main/resources/title-rules.tsv worker/src/main/scala/services/Projection.scala)
judgements=$(case_commit judgements common/src/main/resources/title-rules.tsv test/resources/fixtures/identity-judgements.tsv)
ratchet=$(case_commit ratchet common/src/main/resources/title-rules.tsv worker/src/test/scala/scripts/UnmatchedClustersRatchet.scala)
workflow=$(case_commit workflow common/src/main/resources/title-rules.tsv .github/workflows/x.yml)
unknown=$(case_commit unknown common/src/main/resources/title-rules.tsv some/new/file.txt)
docs_only=$(case_commit docs-only docs/a.md README.md)
empty=$(case_commit empty)
tool_trace=$(case_commit tool-trace common/src/main/resources/title-rules.tsv testkit/src/main/scala/services/movies/NormalizerTrace.scala)
tool_check=$(case_commit tool-check common/src/main/resources/title-rules.tsv testkit/src/main/scala/services/movies/NormalizerTraceCheck.scala)
tool_gate=$(case_commit tool-gate common/src/main/resources/title-rules.tsv scripts/ci/takes-skip-gate.sh)
tool_takes=$(case_commit tool-takes common/src/main/resources/title-rules.tsv scripts/ci/identity-takes-check.sh)
identity_dir_rule=$(case_commit identity-rule common/src/main/scala/services/titlerules/IdentityRules.scala)

# THE TRACE: a base trace with two readings, an empty one, a broken gzip.
printf '{"c":"pl","m":"clean","a":["x"],"o":"x"}\n{"c":"pl","m":"clean","a":["y"],"o":"y"}\n' | gzip > "$work/trace.jsonl.gz"
printf '' | gzip > "$work/empty-trace.jsonl.gz"
printf 'not a gzip' > "$work/broken-trace.jsonl.gz"

# THE PROOF: STUB_PROOF says how it ends -- pass (0, all alike), differ (1), unreplayable (3), crash (2), hang (never
# answers), liar (exits 0 but says some differ), silent (exits 0, says nothing), zero (exits 0 having read nothing),
# listed (exits 0, all alike, but writes a differ row). It logs its arguments.
cat > "$work/proof" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$PROOF_LOG"
out=$2
: > "$out"
case "${STUB_PROOF:-pass}" in
  pass)   echo "[normalizer-trace] 2 reading(s) made again: 0 read otherwise, 0 could not be made again, 0 decision-trace rule name(s) otherwise (no take reads them)" ;;
  differ) echo "[normalizer-trace] 2 reading(s) made again: 1 read otherwise, 0 could not be made again, 0 decision-trace"; exit 1 ;;
  unreplayable) echo "[normalizer-trace] 2 reading(s) made again: 0 read otherwise, 1 could not be made again, 0 decision-trace"; exit 3 ;;
  crash)  echo "[normalizer-trace] could not read the trace: boom" >&2; exit 2 ;;
  hang)   echo $$ >> "$PROOF_HUNG"; exec sleep 86400 ;;
  liar)   echo "[normalizer-trace] 2 reading(s) made again: 1 read otherwise, 0 could not be made again, 0 decision-trace" ;;
  silent) ;;
  zero)   echo "[normalizer-trace] 0 reading(s) made again: 0 read otherwise, 0 could not be made again, 0 decision-trace" ;;
  listed) echo "[normalizer-trace] 2 reading(s) made again: 0 read otherwise, 0 could not be made again, 0 decision-trace"
          printf 'pl\tclean\tx\tx\ty\n' > "$out" ;;
esac
exit 0
STUB
chmod +x "$work/proof"
export PROOF_LOG="$work/proof.log" PROOF_HUNG="$work/hung"
: > "$PROOF_HUNG"

# decide <head> [trace] -> "<stdout first line> <exit>"; the mode is shadow unless MODE says otherwise
decide() {
  local head=$1 trace=${2:-$work/trace.jsonl.gz} answer rc
  : > "$PROOF_LOG"
  answer=$(cd "$work/repo" && TAKES_SKIP_MODE=${MODE-shadow} TAKES_SKIP_PROOF_TIMEOUT=2 \
    spec_bounded bash "$gate" "$base" "$head" "$trace" -- "$work/proof" 2> "$work/err")
  rc=$?
  echo "$(head -1 <<< "$answer") $rc"
}

# because <text>: 1 when the last decision's reasons (stderr) name <text> -- every RUN case below names ITS reason, so a
# check taken out of the gate shows even where a later check still answers RUN (the mutation test relies on it)
because() { grep -qF -- "$1" "$work/err" && echo 1 || echo 0; }

# raw <dir> <base> <head> <trace> [-- proof...] -> "<stdout first line> <exit>", the gate called as given
raw() {
  local dir=$1 answer rc; shift
  answer=$(cd "$dir" && TAKES_SKIP_MODE=shadow spec_bounded bash "$gate" "$@" 2> "$work/err"); rc=$?
  echo "$(head -1 <<< "$answer") $rc"
}

# SKIP: the one case that holds
check "a title-rule change the proof clears: SKIP" "SKIP 0" "$(decide "$rules")"
check "...the proof handed the base trace" "1" "$(grep -c -- "--out .*/differ.tsv $work/trace.jsonl.gz$" "$PROOF_LOG")"
check "...saying so as the footprint measure does" "measure: footprint, 0 readings differ (2 readings)" \
  "$(cd "$work/repo" && TAKES_SKIP_MODE=shadow bash "$gate" "$base" "$rules" "$work/trace.jsonl.gz" -- "$work/proof" 2>/dev/null | sed -n 2p)"
check "rules and their docs: SKIP" "SKIP 0" "$(decide "$rules_and_docs")"
check "the normaliser itself: SKIP" "SKIP 0" "$(decide "$normalizer")"
check "in enforce mode as in shadow: SKIP" "SKIP 0" "$(MODE=enforce decide "$rules")"

# RUN: the mode
check "mode off: RUN" "RUN 1" "$(MODE=off decide "$rules")"
check "mode unset: RUN" "RUN 1" "$(MODE='' decide "$rules")"
check "mode anything else: RUN" "RUN 1" "$(MODE=yes decide "$rules")"
check "...without running the proof" "0" "$(wc -l < "$PROOF_LOG" | tr -d ' ')"

# RUN: the commits
check "an abbreviated base: RUN" "RUN 1" "$(raw "$work/repo" "${base:0:9}" "$rules" "$work/trace.jsonl.gz" -- "$work/proof")"
check "an unknown head: RUN" "RUN 1 1" "$(decide 1111111111111111111111111111111111111111) $(because 'git could not diff')"
check "base and head one commit: RUN" "RUN 1 1" "$(decide "$base") $(because 'base and head are one commit')"
check "no change at all: RUN" "RUN 1 1" "$(decide "$empty") $(because 'no file changed')"
check "a gate called without its proof command: RUN" "RUN 1 1" "$(raw "$work/repo" "$base" "$rules" "$work/trace.jsonl.gz") $(because 'usage:')"
check "outside a repository: RUN" "RUN 1" "$(raw "$work" "$base" "$rules" "$work/trace.jsonl.gz" -- "$work/proof")"

# RUN: the files
for c in placeholders resolver matcher projection judgements ratchet workflow unknown identity_dir_rule; do
  check "a change with $c: RUN" "RUN 1" "$(decide "${!c}")"
done
check "...the resolver's named" "1" "$(decide "$resolver" > /dev/null; grep -c 'identity/src/main/scala/services/identity/Resolver.scala' "$work/err")"
check "docs alone (no rule to prove): RUN" "RUN 1 1" "$(decide "$docs_only") $(because 'no title-rule file changed')"
for c in tool_trace tool_check tool_gate tool_takes; do
  check "the trace tooling changed ($c): RUN" "RUN 1 1" "$(decide "${!c}") $(because 'the trace tooling changed')"
done

# RUN: the trace
check "no base trace: RUN" "RUN 1 1" "$(decide "$rules" "$work/no-such-trace.jsonl.gz") $(because 'no base trace at')"
check "an empty base trace: RUN" "RUN 1 1" "$(decide "$rules" "$work/empty-trace.jsonl.gz") $(because 'holds no reading')"
check "a base trace that is no gzip: RUN" "RUN 1 1" "$(decide "$rules" "$work/broken-trace.jsonl.gz") $(because 'is not a whole gzip')"

# RUN: the proof
for p in differ unreplayable crash liar silent zero listed; do
  check "a proof that ends '$p': RUN" "RUN 1" "$(STUB_PROOF=$p decide "$rules")"
done
check "a proof that hangs is cut off: RUN" "RUN 1" "$(STUB_PROOF=hang decide "$rules")"
check "...leaving no proof running" "0" "$(spec_still_running "$PROOF_HUNG")"
check "...saying it timed out" "1" "$(grep -c 'the proof exited 124' "$work/err")"
check "a proof command that does not exist: RUN" "RUN 1" "$(raw "$work/repo" "$base" "$rules" "$work/trace.jsonl.gz" -- "$work/no-such-proof")"

spec_summary
