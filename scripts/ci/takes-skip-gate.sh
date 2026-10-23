#!/usr/bin/env bash
# THE TAKES SKIP GATE: whether a convergence `takes` row may leave out its takes diff -- the base's and the head's resolve
# and final takes over the recording -- because the commit PROVABLY moves no take. FAIL-CLOSED: it answers SKIP only when
# every condition below holds and was positively checked; anything else -- a condition that fails, a read or a command
# that errors or hangs, an input missing, an answer it cannot parse -- is RUN. It never decides a verdict itself.
#
# SKIP needs ALL of:
#   1. TAKES_SKIP_MODE is shadow or enforce (off, unset, anything else: RUN);
#   2. base and head are distinct full commit names, and git can diff them (an error: RUN), and the diff is not empty;
#   3. every changed file is either footprint-eligible -- a title-rule file the resolve reads only through the title
#      normaliser's readings (title-rules.tsv, services/titlerules/*.scala but TitleRulePlaceholders.scala, which
#      FormatTags reads directly, and TitleNormalizer.scala: identity-measure's footprint_rule_path) -- or inert (docs/,
#      .claude/, a Markdown file). Any other file -- a spec, a judgement, a capture, a workflow, a script -- is RUN;
#   4. no changed path names identity, matcher, resolver or projection code (checked on its own, whatever 3 said);
#   5. the trace tooling (NormalizerTrace, NormalizerTraceCheck, this gate, the takes check) is the same at base and head;
#   6. the base's trace exists, is a readable gzip with at least one reading, and was stored under the base's EXACT inputs
#      (its identity code, the recording, its tree, its fills and the capture: the caller names that file);
#   7. the proof -- the base's every title reading read again under the head's rules (services.movies.NormalizerTraceCheck,
#      the command the caller hands in) -- exits 0 within TAKES_SKIP_PROOF_TIMEOUT seconds (default 900), its last
#      `[normalizer-trace]` line reading N > 0 readings made again, 0 read otherwise, 0 could not be made again.
#
# usage: takes-skip-gate.sh <base sha> <head sha> <base trace .jsonl.gz> -- <proof command...>
#   (the proof command is run with `--out <differ.tsv> <trace>` appended)
# Prints SKIP or RUN on stdout, then -- on SKIP -- `measure: footprint, 0 readings differ (<N> readings)`; the reasons on
# stderr. Exit 0 for SKIP, 1 for RUN. Tested by scripts/ci/takes-skip-gate-test.sh, whose mutation test
# (scripts/ci/takes-skip-gate-mutation-test.sh) proves that removing any one check lets a RUN case through.
set -uo pipefail

run() { echo "takes-skip-gate: RUN -- $*" >&2; echo RUN; exit 1; }

case "${TAKES_SKIP_MODE:-off}" in shadow|enforce) ;; *) run "TAKES_SKIP_MODE is ${TAKES_SKIP_MODE:-unset}" ;; esac
[ "$#" -ge 5 ] && [ "$4" = -- ] || run "usage: <base> <head> <trace> -- <proof command...>"
base=$1 head=$2 trace=$3
shift 4
[[ "$base" =~ ^[0-9a-f]{40}$ ]] && [[ "$head" =~ ^[0-9a-f]{40}$ ]] || run "base and head must be full commit names"
[ "$base" != "$head" ] || run "base and head are one commit"

# 2. what changed (an error is RUN, not "nothing")
changed=$(git diff --name-only --no-renames "$base" "$head" --) || run "git could not diff $base and $head"
[ -n "$changed" ] || run "no file changed: nothing for a proof to stand for"

# 5. the trace tooling unchanged (asked first: its files are no rule, and 3 would refuse them under another name)
tooling=(testkit/src/main/scala/services/movies/NormalizerTrace.scala testkit/src/main/scala/services/movies/NormalizerTraceCheck.scala
         scripts/ci/takes-skip-gate.sh scripts/ci/identity-takes-check.sh)
tooling_diff=$(git diff --name-only "$base" "$head" -- "${tooling[@]}") || run "git could not diff the trace tooling"
[ -z "$tooling_diff" ] || run "the trace tooling changed: $(head -1 <<< "$tooling_diff")"
for f in "${tooling[@]:0:2}"; do git cat-file -e "$head:$f" 2> /dev/null || run "the head has no $f"; done

# 3. eligible or inert, every one
eligible() {
  case $1 in
    common/src/main/scala/services/titlerules/TitleRulePlaceholders.scala) return 1 ;; # FormatTags reads it directly
    common/src/main/resources/title-rules.tsv) return 0 ;;
    common/src/main/scala/services/titlerules/*.scala) return 0 ;;
    common/src/main/scala/services/movies/TitleNormalizer.scala) return 0 ;;
  esac
  return 1
}
inert() { case $1 in docs/*|.claude/*|*.md) return 0 ;; esac; return 1; }
rules=0
while IFS= read -r file; do
  [ -n "$file" ] || continue
  if eligible "$file"; then rules=$((rules + 1)); continue; fi
  inert "$file" || run "$file is neither a title-rule file nor inert"
done <<< "$changed"
[ "$rules" -gt 0 ] || run "no title-rule file changed: nothing for the proof to prove"

# 4. no identity, matcher, resolver or projection code, whatever 3 said
denied=$(grep -iE '(^|/)(identity|matcher|resolver|projection)|identity[^/]*\.|matcher[^/]*\.|resolver[^/]*\.|projection[^/]*\.' <<< "$changed" \
  | grep -vE '^(docs/|\.claude/)|\.md$' || true)
[ -z "$denied" ] || run "identity, matcher, resolver or projection code changed: $(head -1 <<< "$denied")"

# 6. the base's trace, under its exact inputs (the caller names it), readable and not empty
[ -s "$trace" ] || run "no base trace at $trace (none stored under the base's exact inputs)"
gzip -t "$trace" 2> /dev/null || run "the base trace $trace is not a whole gzip"
lines=$(gzip -dc "$trace" 2> /dev/null | grep -c . || true)
[ "$lines" -gt 0 ] || run "the base trace $trace holds no reading"

# 7. the proof, bounded
out=$(mktemp -d) || run "no temporary directory"
trap 'rm -rf "$out"' EXIT
timeout "${TAKES_SKIP_PROOF_TIMEOUT:-900}" "$@" --out "$out/differ.tsv" "$trace" > "$out/proof.txt" 2>&1
rc=$?
cat "$out/proof.txt" >&2
[ "$rc" = 0 ] || run "the proof exited $rc (1: readings differ, 3: some cannot be made again, 124: timed out)"
verdict=$(grep '^\[normalizer-trace\] ' "$out/proof.txt" | tail -1)
[[ "$verdict" =~ ^\[normalizer-trace\]\ ([0-9]+)\ reading\(s\)\ made\ again:\ 0\ read\ otherwise,\ 0\ could\ not\ be\ made\ again ]] \
  || run "the proof said no '0 read otherwise, 0 could not be made again' (${verdict:-nothing})"
readings=${BASH_REMATCH[1]}
[ "$readings" -gt 0 ] || run "the proof read nothing again"
[ ! -s "$out/differ.tsv" ] || run "the proof listed readings in $out/differ.tsv though it exited 0"

echo "takes-skip-gate: SKIP -- $rules title-rule file(s) changed, nothing else that can move a take; $readings readings alike" >&2
echo SKIP
echo "measure: footprint, 0 readings differ ($readings readings)"
exit 0
