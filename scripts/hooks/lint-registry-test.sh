#!/usr/bin/env bash
# THE LINT REGISTRY (scripts/hooks/lint-registry.tsv): the pre-push `sbt` check selects its lint specs from that table,
# by the pushed paths each row names, never by a spec's name. This fails when a repo-reading spec has no row -- the gap
# that let LoadIndependentTestsSpec and InMemoryStoresRoundTripSpec turn Main red 7 times (review 2026-11-06, D3) -- or
# a row names a spec that does not exist, and checks the selection the hook makes from it.
# Run: bash scripts/hooks/lint-registry-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
cd "$REPO_ROOT" || exit 1
registry=scripts/hooks/lint-registry.tsv

printf '\033[36m▸\033[0m the registry names every repo-reading spec, and only specs that exist\n'
rows() { [ -f "$registry" ] && grep -v '^#' "$registry" | grep -v '^$' | cut -f1; }
# A repo-reading spec: a tools/ No*/*Lint* spec, any deploy/ spec, or a spec using a source-scan helper (the one
# reading, scripts/hooks/lint-registry-missing, is also pre-push's sbt check's).
missing="$(scripts/hooks/lint-registry-missing | tr '\n' ' ' | sed 's/ $//')"
check "every repo-reading spec has a row (missing: add one)" "" "$missing"
gone=""
for spec in $(rows); do
    f="${spec%%:*}/src/test/scala/$(printf '%s' "${spec#*:}" | tr . /).scala"
    [ -f "$f" ] || gone+="$spec "
done
check "every row names a spec that exists (stale rows: drop them)" "" "${gone% }"
check "no spec has two rows" "" "$(rows | LC_ALL=C sort | uniq -d | tr '\n' ' ' | sed 's/ $//')"
check "every row's reads are *, @repo, @gitops, - or a path pattern" "" \
  "$(grep -v '^#' "$registry" 2>/dev/null | grep -v '^$' | awk -F'\t' 'NF < 2 || $2 == "" { print $1 }' | tr '\n' ' ' | sed 's/ $//')"

printf '\033[36m▸\033[0m the hook selects from it\n'
lints() { printf '%s\n' "$@" | scripts/hooks/pre-push --lint-plan | tr '\n' ' ' | sed 's/ $//'; }
has() { case " $1 " in *" $2 "*) echo yes ;; *) echo no ;; esac; }
out="$(lints worker/src/main/scala/services/A.scala)"
check "a production Scala source runs the source scans the name patterns missed, and the standing lints" "yes yes yes" \
  "$(has "$out" worker:tools.LoadIndependentTestsSpec) $(has "$out" worker:tools.InMemoryStoresRoundTripSpec) $(has "$out" worker:tools.NoWallClockInTestsSpec)"
check "...but not a spec reading only scripts and workflows" "no" "$(has "$out" worker:deploy.WorkflowPlanSpec)"
out="$(lints scripts/x.sh)"
check "a script runs the @repo specs, not a @gitops one without the manifests, never a '-' one" "yes no no" \
  "$(has "$out" worker:deploy.WorkflowPlanSpec) $(has "$out" worker:deploy.NodeCpuBudgetSpec) $(has "$out" worker:deploy.FixtureServerBootOverlapSpec)"
check "...the manifests in place (PREPUSH_GITOPS=1), the @gitops specs too" "yes" \
  "$(has "$(printf 'scripts/x.sh\n' | PREPUSH_GITOPS=1 scripts/hooks/pre-push --lint-plan | tr '\n' ' ')" worker:deploy.NodeCpuBudgetSpec)"
check "every selected spec is a registry row" "" \
  "$(comm -23 <(printf '%s\n' scripts/x.sh worker/src/main/scala/A.scala | PREPUSH_GITOPS=1 scripts/hooks/pre-push --lint-plan | LC_ALL=C sort -u) <(rows | LC_ALL=C sort -u) | tr '\n' ' ' | sed 's/ $//')"
printf '\033[36m▸\033[0m a row selects its spec for every path the spec reads\n'
# scripts/ci/lint_registry_reads.py: the repo paths named by string literals in each registered spec, each put to the
# hook as a push (a spec reading tools/devpanel/scripts/devpanel.py with an @repo-only row turned Main #2236 red).
check "no spec reads a path its row would not select (add the path's glob to the row)" "" \
  "$(python3 scripts/ci/lint_registry_reads.py | tr '\t\n' ': ' | sed 's/ $//')"
check "a push of tools/devpanel/scripts/devpanel.py selects DevServerHeapCanarySpec" "yes" \
  "$(has "$(lints tools/devpanel/scripts/devpanel.py)" worker:deploy.DevServerHeapCanarySpec)"
spec_summary
