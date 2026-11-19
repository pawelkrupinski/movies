#!/usr/bin/env bash
# THE LINT REGISTRY (scripts/hooks/lint-registry.tsv): the pre-push `sbt` check selects its lint specs from that table,
# by the pushed paths each row names, never by a spec's name. This fails when a repo-reading spec has no row -- the gap
# that let LoadIndependentTestsSpec and InMemoryStoresRoundTripSpec turn Main red 7 times (review 2026-11-06, D3) -- or
# a row names a spec that does not exist, and checks the selection the hook makes from it.
# Run: bash scripts/hooks/lint-registry-test.sh
# script-test-reads: */src/test/scala/* scripts/hooks/lint-registry.tsv
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
cd "$REPO_ROOT" || exit 1
registry=scripts/hooks/lint-registry.tsv
# The selection is checked without the GitOps manifests unless a case sets PREPUSH_GITOPS=1 itself: land-precheck
# (and CI's check) export it for their lints, and this test, run after them, read it and failed the "not a @gitops
# one without the manifests" case (2026-11-11, a precheck refusing a sound registry row).
unset PREPUSH_GITOPS

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
check "...but not a spec reading only scripts and workflows" "no" "$(has "$out" repochecks:deploy.WorkflowPlanSpec)"
out="$(lints scripts/x.sh)"
check "a script runs the @repo specs, not a @gitops one without the manifests, never a '-' one" "yes no no" \
  "$(has "$out" repochecks:deploy.WorkflowPlanSpec) $(has "$out" repochecks:deploy.NodeCpuBudgetSpec) $(has "$out" repochecks:deploy.FixtureServerBootOverlapSpec)"
check "...the manifests in place (PREPUSH_GITOPS=1), the @gitops specs too" "yes" \
  "$(has "$(printf 'scripts/x.sh\n' | PREPUSH_GITOPS=1 scripts/hooks/pre-push --lint-plan | tr '\n' ' ')" repochecks:deploy.NodeCpuBudgetSpec)"
check "every selected spec is a registry row" "" \
  "$(comm -23 <(printf '%s\n' scripts/x.sh worker/src/main/scala/A.scala | PREPUSH_GITOPS=1 scripts/hooks/pre-push --lint-plan | LC_ALL=C sort -u) <(rows | LC_ALL=C sort -u) | tr '\n' ' ' | sed 's/ $//')"
printf '\033[36m▸\033[0m a row selects its spec for every path the spec reads\n'
# scripts/ci/lint_registry_reads.py: the repo paths named by string literals in each registered spec, each put to the
# hook as a push (a spec reading tools/devpanel/scripts/devpanel.py with an @repo-only row turned Main #2236 red).
check "no spec reads a path its row would not select (add the path's glob to the row)" "" \
  "$(python3 scripts/ci/lint_registry_reads.py | tr '\t\n' ': ' | sed 's/ $//')"
check "a push of tools/devpanel/scripts/devpanel.py selects DevServerHeapCanarySpec" "yes" \
  "$(has "$(lints tools/devpanel/scripts/devpanel.py)" repochecks:deploy.DevServerHeapCanarySpec)"

printf '\033[36m▸\033[0m what a spec reads is derived, and a reader without a row fails (THE READER LINT)\n'
scratch="$(mktemp -d)"
trap 'rm -rf "$scratch"' EXIT
reads_of() { python3 scripts/ci/lint_registry_reads.py --reads "$1" | cut -f3 | tr '\n' ' '; }
check "a root file a spec names is a read (JdkVersionParitySpec's RepoFile.read(\"Dockerfile\"))" "yes" \
  "$(has "$(reads_of repochecks:deploy.JdkVersionParitySpec)" Dockerfile)"
check "a classpath resource a spec opens is a read, found under its module's resources (\"/routes\")" "yes" \
  "$(has "$(reads_of web:controllers.BrowseFilterParamsSpec)" web/src/main/resources/routes)"
grep -v -E '^(worker:tools\.AotClassListSpec|web:controllers\.BrowseFilterParamsSpec)	' "$registry" > "$scratch/registry.tsv"
rowless="$(python3 scripts/ci/lint_registry_reads.py --registry "$scratch/registry.tsv" --missing | tr '\n' ' ')"
check "a spec opening a resource by a name no literal gives, rowless, is named (AotClassListSpec)" "yes" \
  "$(has "$rowless" worker:tools.AotClassListSpec)"
check "...and one opening a resource by literal (BrowseFilterParamsSpec)" "yes" "$(has "$rowless" web:controllers.BrowseFilterParamsSpec)"

# THE REDS THE SELECTION MISSED (the owner, 2026-11-18): each red commit's paths, replayed through the hook's selection,
# select the spec that went red -- and the sbt check that runs it.
printf '\033[36m▸\033[0m the reds the selection missed now select their spec\n'
plan_of() { printf '%s\n' "$@" | scripts/hooks/pre-push --plan | tr '\n' ' '; }
gitops_lints() { printf '%s\n' "$@" | PREPUSH_GITOPS=1 scripts/hooks/pre-push --lint-plan | tr '\n' ' '; }
# Main #2564: movies-gitops 5ec2eee (Flux's notification-controller) pushed NodeMemoryBudgetSpec over its reserve. A
# gitops push runs scripts/gitops-specs (its pre-push); here, the budget's own inputs select it too.
check "#2564: a gitops push runs NodeMemoryBudgetSpec (scripts/gitops-specs --plan)" "yes" \
  "$(has "$(scripts/gitops-specs --plan | tr '\n' ' ')" deploy.NodeMemoryBudgetSpec)"
check "#2564: host-memory.json and k3s-server.nix run the sbt check, and select NodeMemoryBudgetSpec" "yes yes yes" \
  "$(has "$(plan_of infra/nix/files/host-memory.json)" sbt) $(has "$(plan_of infra/nix/modules/roles/k3s-server.nix)" sbt) $(has "$(gitops_lints infra/nix/files/host-memory.json infra/nix/modules/roles/k3s-server.nix)" repochecks:deploy.NodeMemoryBudgetSpec)"
# 2026-11-19 (landing 668a4cced, Linux): the case above failed for host-memory.json alone. The hook matched a path
# against its ~6 KB registry file list with `printf | grep -q` under pipefail: grep exiting at a match in printf's first
# write let the second die of SIGPIPE, and the pipeline read false -- a race only a loaded host lost. A registry padded
# past the pipe buffer, the hook copied beside it, loses it every time.
mkdir -p "$scratch/hook/scripts"
cp -R scripts/hooks "$scratch/hook/scripts/" && cp scripts/hooks-path.sh scripts/tool-conventions.sh "$scratch/hook/scripts/"
awk 'BEGIN { for (i = 1; i <= 3000; i++) printf "x:Padding%dSpec\tpadding/file-%d.txt\n", i, i }' >> "$scratch/hook/scripts/hooks/lint-registry.tsv"
check "...however long the registry's file list (no SIGPIPE in the match)" "yes" \
  "$(has "$(printf 'infra/nix/files/host-memory.json\n' | "$scratch/hook/scripts/hooks/pre-push" --plan 2>/dev/null | tr '\n' ' ')" sbt)"
# Main #2547/#2550: the weekly bump (251bbc29a) moved the Temurin digest; the image's JAVA_VERSION check failed. A pin's
# restate (infra/bin/image_pins.py) writes the Dockerfile alone: it runs the sbt check, the JDK and image specs, and the
# script tests reading the Dockerfile.
red2="$(plan_of Dockerfile)"
check "#2547/#2550: a Dockerfile-only change runs the sbt check and the script tests" "yes yes" "$(has "$red2" sbt) $(has "$red2" scripttests)"
out="$(lints Dockerfile)"
check "...selecting JdkVersionParitySpec and the image specs" "yes yes yes" \
  "$(has "$out" repochecks:deploy.JdkVersionParitySpec) $(has "$out" repochecks:deploy.CiImageMirrorSpec) $(has "$out" repochecks:deploy.ImagePathGateSpec)"
out="$(lints .github/actions/setup-buildx/action.yml Dockerfile infra/flake.lock infra/nix/modules/roles/mongodb.nix scripts/ci/start-mongo-replset.sh scripts/convergence-local.sh scripts/run-script-tests)"
check "...and the bump's own paths (251bbc29a) select JdkVersionParitySpec" "yes" "$(has "$out" repochecks:deploy.JdkVersionParitySpec)"
# Main #2601: 2721992e8 added MitchellTheatresClient, whose `collect { case ... }` compiles to an anonymous class the AOT
# list lacked. A scraper change (a new source, or a new anonymous or inner class in a changed one) selects the list's spec.
out="$(lints scrapers/src/main/scala/services/cinemas/CinemaScraperCatalog.scala scrapers/src/main/scala/services/cinemas/us/MitchellTheatresClient.scala \
  scrapers/src/main/scala/services/cinemas/us/UsIndependentVenues.scala scrapers/src/test/scala/clients/CinemaScraperCatalogSpec.scala \
  scrapers/src/test/scala/clients/mitchelltheatres/MitchellTheatresClientSpec.scala scrapers/src/test/scala/clients/tools/WriteMitchellTheatres.scala \
  test/resources/fixtures/mitchelltheatres/www.southgate6.com/index worker/src/main/resources/aot-classes.txt)"
check "#2601: 2721992e8's paths select AotClassListSpec" "yes" "$(has "$out" worker:tools.AotClassListSpec)"
check "...as does a new scraper source alone, and the list alone" "yes yes" \
  "$(has "$(lints scrapers/src/main/scala/services/cinemas/de/NewKinoClient.scala)" worker:tools.AotClassListSpec) $(has "$(lints worker/src/main/resources/aot-classes.txt)" worker:tools.AotClassListSpec)"
check "...but a worker-only change does not (the spec checks the scrapers' classes)" "no" \
  "$(has "$(lints worker/src/main/scala/services/A.scala)" worker:tools.AotClassListSpec)"
spec_summary
