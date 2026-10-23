#!/usr/bin/env bash
# scripts/identity-landing-check over a stub identity-measure and a stub identity-ci-run (each group's sbt log from a
# fixture): one verdict, LANDABLE only when the measure and every group pass, BLOCKED naming the failing spec, its test
# and the hint for its usual cause, and no verdict when a group could not run.
# Run: bash scripts/identity-landing-check-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

printf '\033[36m▸\033[0m identity-landing-check\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
repo="$work/repo"
scratch_repo "$repo" -b main
mkdir -p "$repo/worker/src/main/scala" "$repo/test/resources/fixtures"
echo base > "$repo/worker/src/main/scala/Resolver.scala"
echo "country" > "$repo/test/resources/fixtures/identity-judgements.tsv"
scratch_git "$repo" add -A && scratch_git "$repo" commit -qm base
base=$(scratch_git "$repo" rev-parse HEAD)
echo head > "$repo/worker/src/main/scala/Resolver.scala"
scratch_git "$repo" commit -qam "a resolver change"
code=$(scratch_git "$repo" rev-parse HEAD)
echo "country	judged" > "$repo/test/resources/fixtures/identity-judgements.tsv"
scratch_git "$repo" commit -qam "a judgement"
judged=$(scratch_git "$repo" rev-parse HEAD)

mkdir -p "$work/bin" "$work/logs"
# identity-ci-run: the group's sbt log is $LOGS/<first word of the command after the module>.log; exit from it
cat > "$work/bin/ci-run" <<'STUB'
#!/usr/bin/env bash
out="" cmd=""
while [ "$#" -gt 0 ]; do case "$1" in --out) out=$2; shift 2 ;; --) shift; cmd="$*"; break ;; *) shift ;; esac; done
echo "$cmd" >> "$LOGS/commands"
case "$cmd" in testOnly*) g=unit ;; worker/IntegrationTest*) g=it ;; *FilmSchedule*) g=e2e-corpus ;; *) g=e2e-rest ;; esac
[ -f "$LOGS/$g.unreachable" ] && exit 2
mkdir -p "$out"
if [ -f "$LOGS/$g.log" ]; then cp "$LOGS/$g.log" "$out/sbt.log"; exit 1; fi
printf '[info] All tests passed.\n' > "$out/sbt.log"
STUB
cat > "$work/bin/measure" <<'STUB'
#!/usr/bin/env bash
echo "$*" > "$LOGS/measure-args"
case "${MEASURE:-0}" in
  0) echo "LANDABLE: no right take lost or turned wrong, no new wrong take" ;;
  1) printf 'NOT LANDABLE:\n  - pl model: 1 right-lost\n' ;;
  3) echo "identity-measure: --footprint-only: not measured in full" >&2 ;;
  *) echo "identity-measure: could not fetch" >&2 ;;
esac
exit "${MEASURE:-0}"
STUB
chmod +x "$work/bin/ci-run" "$work/bin/measure"

landing() { # <args...>: exit code; output in $work/out
  ( cd "$repo" && LOGS="$work/logs" IDENTITY_LANDING_OUT="$work/check" IDENTITY_LANDING_CI_RUN="$work/bin/ci-run" \
      IDENTITY_LANDING_MEASURE="$work/bin/measure" "$REPO_ROOT/scripts/identity-landing-check" "$@" > "$work/out" 2> "$work/err" )
  echo $?
}
has() { grep -qF -- "$1" "$work/out" && echo true || echo false; }

check "every group passing and the measure LANDABLE is LANDABLE" "0" "$(landing --head "$code" --base "$base")"
check "...the verdict last" "LANDABLE" "$(tail -1 "$work/out")"
check "...the measure asked for the footprint when eligible, the full measure otherwise (identity-measure's own fallback)" \
  "--head $code --base $base --footprint" "$(cat "$work/logs/measure-args")"
check "...and every group Main's rows run for a resolver change, e2e included" "4" "$(wc -l < "$work/logs/commands" | tr -d ' ')"
check "...the unit group naming the identity specs Main runs" "true" \
  "$(grep -q '^testOnly services.identity.\* services.titlerules.\* scripts.\* tools.DocCommentPlacementSpec tools.HardClusterRatchetSpec$' "$work/logs/commands" && echo true || echo false)"
check "...the hard-cluster convergence among them" "true" \
  "$(grep -q 'integration.HardClusterConvergenceIntegrationSpec' "$work/logs/commands" && echo true || echo false)"

rm -f "$work/logs/commands"
check "a judgements-only change skips the e2e rows, as Main's scope does" "0 2" \
  "$(landing --head "$judged" --base "$code") $(wc -l < "$work/logs/commands" | tr -d ' ')"
check "--full asks the full measure" "--head $judged --base $code" "$(landing --head "$judged" --base "$code" --full > /dev/null; cat "$work/logs/measure-args")"
check "--footprint-only asks only the footprint" "true" \
  "$(landing --head "$judged" --base "$code" --footprint-only > /dev/null; grep -q -- '--footprint-only' "$work/logs/measure-args" && echo true || echo false)"
check "--no-cache reaches the measure" "true" \
  "$(landing --head "$judged" --base "$code" --no-cache > /dev/null; grep -q -- '--no-cache' "$work/logs/measure-args" && echo true || echo false)"

cat > "$work/logs/unit.log" <<'LOG'
[info] MatchedTakesLabelSpec:
[info] The model's matched takes
[info] - should take no film a judgement denies or contradicts, beyond those known-wrong in scope rows *** FAILED ***
[info]   uk Pavilions Teignmouth | Patrick Monahan -> tmdb:123 judged wrong (MatchedTakesLabelSpec.scala:41)
[info] IdentitySliceCasesSpec:
[info] - should take identity-judgements.tsv's right film for each listing of uk-relay-1 *** FAILED ***
[error] Failed tests:
[error] 	services.identity.MatchedTakesLabelSpec
[error] 	services.identity.IdentitySliceCasesSpec
[error] 	services.identity.UnmatchedClustersRatchetSpec
[error] (worker / Test / testOnly) sbt.TestsFailedException: Tests unsuccessful
LOG
check "a judgement the unified rows contradict is BLOCKED, the measure LANDABLE notwithstanding" "1" "$(landing --head "$judged" --base "$code")"
check "...the verdict" "true" "$(has 'BLOCKED')"
check "...naming the spec and its test" "true" \
  "$(has '- MatchedTakesLabelSpec: should take no film a judgement denies or contradicts, beyond those known-wrong in scope rows')"
check "...with the hint for its usual cause" "true" "$(has 'hint: judgement rows stale -> known-wrong scope rows')"
check "...a slice case, its own hint" "true" "$(has 'the case'"'"'s expected.tsv is stale / asks unpinned questions -> re-pin from the recording')"
check "...a suite the summary names without a test, too" "true" "$(has '- UnmatchedClustersRatchetSpec: (the suite failed: see the log)')"
rm -f "$work/logs/unit.log"

printf '[error] /w/worker/src/main/scala/X.scala:3:5: not found: value y\n[error] (worker / Compile / compileIncremental) Compilation failed\n' > "$work/logs/it.log"
check "a commit that does not compile is BLOCKED, saying so" "1 true" \
  "$(landing --head "$code" --base "$base") $(has '- compile: /w/worker/src/main/scala/X.scala:3:5: not found: value y')"
rm -f "$work/logs/it.log"

check "a measure NOT LANDABLE is BLOCKED, with its reasons" "1 true" \
  "$(MEASURE=1 landing --head "$code" --base "$base") $(has '- measure: NOT LANDABLE: pl model: 1 right-lost')"
check "a measure the footprint could not answer under --footprint-only is BLOCKED" "1" \
  "$(MEASURE=3 landing --head "$code" --base "$base" --footprint-only)"
check "a measure that could not be made is no verdict" "2 true" \
  "$(MEASURE=2 landing --head "$code" --base "$base") $(has 'NO VERDICT: could not check measure')"
touch "$work/logs/e2e-corpus.unreachable"
check "a group that could not run is no verdict either" "2 true" \
  "$(landing --head "$code" --base "$base") $(has 'could not check e2e-corpus')"
rm -f "$work/logs/e2e-corpus.unreachable"
check "--no-measure and --no-specs together is a usage error" "2" "$(landing --no-measure --no-specs)"

spec_summary
