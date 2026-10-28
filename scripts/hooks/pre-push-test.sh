#!/usr/bin/env bash
# pre-push's path selection: which checks a push's changed paths run, and that an unrelated push
# runs none of them. Run: bash scripts/hooks/pre-push-test.sh
set -uo pipefail
# The caller's PREPUSH_SKIP is the PUSH's: a push skipping `redgate` ran this test with the gate skipped in every scratch
# push below, and its red-gate cases failed (2026-10-14). Each case names what it skips itself.
unset PREPUSH_SKIP
# Every case re-pushes the same scratch trees under different stubs: a stored pass (the hook's REUSE) would answer the
# next case instead of its checks. Reuse is off here, and on only in its own cases, with a cache of their own.
export PREPUSH_NO_REUSE=1
unset PREPUSH_REUSE_DIR
# run by the hook's scripttests check, it inherits the hook's stall marks (stall-watch.sh): its own stalls are no stall of
# that check
unset KINOWO_STALL_FILE KINOWO_PROGRESS_FILE
# Nor do the caller's Forgejo token and remote reach the scratch pushes: each Forgejo case below names its own.
unset FORGEJO_API_TOKEN PREPUSH_REMOTE_URL PREPUSH_FORGEJO_URL PREPUSH_GATE_ATTEMPTS
# The gate's retries come back at once: a failing read is asked 3 times without a pause between.
export PREPUSH_GATE_RETRY_SECONDS=0
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
export REPO_ROOT  # the quoted-heredoc stub gh below sources scratch-git.sh from it
. "$REPO_ROOT/scripts/shell-spec.sh"
# Run by pre-push's `scripttests` check (scripts/run-script-tests), this inherits the GIT_DIR the hook was handed; the scratch
# repositories below would otherwise BE the repository being pushed (it once committed this spec's
# "Spec base" history onto a pushed branch). See scratch-git.sh.
. "$REPO_ROOT/scripts/scratch-git.sh"

# Every case below runs the hook as a push to main, which reads the gating workflows' verdicts first: a stub `gh`
# answers `gh run list --workflow W` from $GH_MAIN / $GH_IMC / $GH_REC (Verify scrape recording; JSON, default none) through the hook's own
# --jq filter, so no case reads the real repository's runs.
# One temporary root for everything below, removed however the spec exits.
testroot="$(mktemp -d)"
ghstub="$testroot/gh"; mkdir -p "$ghstub"
cat > "$ghstub/gh" <<'STUB'
#!/usr/bin/env bash
[ -n "${GH_FAILS:-}" ] && { echo "HTTP 502: Bad Gateway" >&2; exit 1; }
# a gh that never answers: it records its pid in $GH_HANGS and blocks until something kills it
[ -n "${GH_HANGS:-}" ] && { echo $$ >> "$GH_HANGS"; exec sleep 86400; }
# a gh that answers but says something on stderr first (a deprecation, an update notice)
[ -n "${GH_WARNS:-}" ] && echo "! warning: this gh is old" >&2
# a check that rewrites core.hooksPath of the repository it runs in (what moved the shared config to an absolute path)
[ -n "${GH_REWRITES_HOOKSPATH:-}" ] && { . "$REPO_ROOT/scripts/scratch-git.sh"; scratch_git "$PWD" config core.hooksPath "$GH_REWRITES_HOOKSPATH"; }
# `gh run download RUN -p … -D DIR`: a convergence leg's served takes, as the judgements check fetches them
if [ "$1 $2" = "run download" ]; then
  while [ $# -gt 0 ]; do [ "$1" = -D ] && dir="$2"; shift; done
  mkdir -p "$dir/convergence-diagnostics-de-convergence/de-takes"; echo takes > "$dir/convergence-diagnostics-de-convergence/de-takes/de.json.gz"
  # another agent's hook, fetching the same run, finishes first: the run's cache directory appears meanwhile
  if [ -n "${GH_DOWNLOAD_RACE:-}" ]; then
    final="${dir%.part*}"; mkdir -p "$final/convergence-diagnostics-de-convergence/de-takes"
    echo takes > "$final/convergence-diagnostics-de-convergence/de-takes/de.json.gz"
  fi
  exit 0
fi
workflow="" jqexpr=""
while [ $# -gt 0 ]; do
  case "$1" in --workflow) workflow="$2"; shift ;; --jq) jqexpr="$2"; shift ;; esac; shift
done
case "$workflow" in
  Main) runs="${GH_MAIN:-[]}" ;; "Identity model convergence") runs="${GH_IMC:-[]}" ;; "Verify scrape recording") runs="${GH_REC:-[]}" ;;
  *) echo "unknown workflow $workflow" >&2; exit 1 ;;
esac
printf '%s' "$runs" | jq -r "$jqexpr"
STUB
chmod +x "$ghstub/gh"
# A push to a Forgejo remote reads Forgejo's actions runs API by curl instead: a stub `curl` answers
# GET …/actions/runs?workflow_id=F from $FJ_MAIN / $FJ_IMC / $FJ_REC (Forgejo's run JSON, default none) and GET
# …/actions/runs/<id> with $FJ_RUN (one run's JSON, default {}), records each
# call's argv in $FJ_ARGV and the config it was handed on stdin (the token's header) in $FJ_CONFIG, and answers 401
# without that header. $FJ_FAILS: unreachable (curl's exit 7); $FJ_FAILS_ONCE=<file>: unreachable on the first call only
# (the file marks it made). A stub `secrets` stands for the Touch ID vault: `status`
# says $SECRETS_STATE (default locked); `get` prints a token and counts itself in $SECRETS_GETS.
cat > "$ghstub/curl" <<'STUB'
#!/usr/bin/env bash
config="$(cat)"
[ -n "${FJ_CONFIG:-}" ] && printf '%s\n' "$config" >> "$FJ_CONFIG"
[ -n "${FJ_ARGV:-}" ] && printf '%s\n' "$*" >> "$FJ_ARGV"
[ -n "${FJ_FAILS:-}" ] && { echo "curl: (7) Failed to connect to forgejo.internal port 3000" >&2; exit 7; }
if [ -n "${FJ_FAILS_ONCE:-}" ] && [ ! -e "$FJ_FAILS_ONCE" ]; then
  : > "$FJ_FAILS_ONCE"; echo "curl: (7) Failed to connect to forgejo.internal port 3000" >&2; exit 7
fi
out="" url=""
while [ $# -gt 0 ]; do case "$1" in -o) out="$2"; shift ;; http*) url="$1" ;; esac; shift; done
case "$config" in *"Authorization: token "?*) ;; *) echo '{"message":"Unauthorized"}' > "$out"; printf 401; exit 0 ;; esac
case "$url" in
  "${FJ_EXPECT_BASE:-http://forgejo.internal:3000/api/v1/repos/pawelkrupinski/movies}/actions/runs/"[0-9]*)
    printf '%s' "${FJ_RUN:-{\}}" > "$out"; printf 200; exit 0 ;;
  "${FJ_EXPECT_BASE:-http://forgejo.internal:3000/api/v1/repos/pawelkrupinski/movies}/actions/runs?"*) ;;
  *) echo '{"message":"Not Found"}' > "$out"; printf 404; exit 0 ;;
esac
case "$url" in
  *workflow_id=main.yml\&*) runs="${FJ_MAIN:-[]}" ;; *workflow_id=identity-model-convergence.yml\&*) runs="${FJ_IMC:-[]}" ;;
  *workflow_id=verify-scrape-recording.yml\&*) runs="${FJ_REC:-[]}" ;; *) echo '{}' > "$out"; printf 404; exit 0 ;;
esac
printf '{"workflow_runs":%s,"total_count":0}' "$runs" > "$out"; printf 200
STUB
cat > "$ghstub/secrets" <<'STUB'
#!/usr/bin/env bash
case "$1 $2" in
  "status movies") echo "${SECRETS_STATE:-locked}" ;;
  "get movies") [ "$3" = FORGEJO_API_TOKEN ] || exit 1; [ -n "${SECRETS_GETS:-}" ] && echo x >> "$SECRETS_GETS"; echo vaulttoken ;;
  *) exit 1 ;;
esac
STUB
chmod +x "$ghstub/curl" "$ghstub/secrets"
export PATH="$ghstub:$PATH"
trap 'rm -rf "$testroot"' EXIT

printf '\033[36m▸\033[0m pre-push --plan\n'

# plan <path>... -> the selected checks, space-separated
plan() { printf '%s\n' "$@" | "$REPO_ROOT/scripts/hooks/pre-push" --plan | tr '\n' ' ' | sed 's/ $//'; }

check "production JS runs ESLint, and the page area's tests" "eslint sbt page" "$(plan web/src/main/assets/js/shared.js)"
check "a page spec's JS runs ESLint, and the page area's tests" "eslint sbt page" "$(plan web/src/page/resources/landing/hold-picker-panes.js)"
check "the ESLint config runs ESLint" "eslint" "$(plan eslint.config.js)"
check "a repo-local ESLint rule runs ESLint" "eslint" "$(plan eslint-rules/storage-access-in-try.js)"
check "a roster source runs the generated-artefact drift check, and the roster generators' self-tests" "drift scripttests" "$(plan data/pl/venues.json)"
check "a model file runs the drift check AND the sbt lints" "drift sbt" "$(plan common/src/main/scala/models/Cinema.scala)"
check "a generated geodata table runs the drift check AND the sbt lints" "drift sbt" "$(plan geodata/src/main/scala/models/VenueTowns.scala)"
check "the iOS City list runs the drift check and the iOS compile" "drift ios" "$(plan ios/Kinowo/Models/City.swift)"
check "a recorded fixture runs the snapshot stamp check" "snapshot" \
  "$(plan test/resources/fixtures/08-06-2026/www.bilety24.pl/kino/organizator/x)"
check "any Scala source runs the sbt lints" "sbt" "$(plan worker/src/test/scala/tools/FooSpec.scala)"
check "a Grafana dashboard runs the dashboard tests and the metric-coverage lint" "dashboards sbt" \
  "$(plan infra/nix/files/monitoring/grafana/dashboards/apps/kinowo-http.json)"
check "a workflow runs actionlint, and the silenced-read lint" "actionlint sbt" "$(plan .github/workflows/ci.yml)"
check "a composite action runs actionlint (its run blocks' shellcheck), the self-tests, and the lint specs" "scripttests actionlint sbt" \
  "$(plan .github/actions/convergence-setup/action.yml)"
check "the local-tests list selects shellcheck by its directory (which skips it) and the lint specs" "scripttests shellcheck sbt" "$(plan scripts/hooks/local-tests.txt)"
check "a shell script runs its self-tests, shellcheck, and the lint specs" "scripttests shellcheck sbt" "$(plan scripts/ci/order-seed.sh)"
check "the hook itself runs shellcheck and the self-tests, its own among them" "scripttests shellcheck sbt" "$(plan scripts/hooks/pre-push)"
check "the hooks-path helper and install-hooks.sh, both exercised here, run the self-tests" "scripttests shellcheck sbt scripttests shellcheck sbt" \
  "$(plan scripts/hooks-path.sh) $(plan scripts/install-hooks.sh)"
check "an iOS view runs the iOS compile check" "ios" "$(plan ios/Kinowo/Views/FilmCard.swift)"
check "an Android-only change runs nothing" "" "$(plan android/app/src/main/java/pl/kinowo/ui/X.kt)"
check "a mobile or fleet script runs the self-tests" "scripttests shellcheck|scripttests shellcheck ios|scripttests" \
  "$(plan android/scripts/store-graphics.sh)|$(plan ios/scripts/x.sh)|$(plan infra/bin/x)"
check "a roster generator runs the drift check and its self-tests" "drift scripttests" "$(plan data/uk/scripts/build_pages.py)"
check "docs run nothing" "" "$(plan docs/readme.md README.md)"
check "checks come out once each, in run order" "eslint snapshot scripttests actionlint shellcheck sbt page" \
  "$(plan web/src/main/assets/js/a.js build.sbt scripts/x.sh .github/workflows/a.yml worker/src/main/scala/A.scala test/resources/fixtures/08-06-2026/y)"

check "a judgements change runs the judgements lint, and the identity area's tests" "judgements sbt identity" "$(plan test/resources/fixtures/identity-judgements.tsv)"
check "an expected match runs them" "judgements sbt identity" "$(plan test/resources/fixtures/identity-unmatched/expected-matches.tsv)"
check "a hard-cluster expectation runs it" "judgements" "$(plan test/resources/fixtures/corpus/expected-hard-clusters-pl.txt)"

# THE AREAS (the owner, 2026-10-26: no pre-merge CI; the basic tests, plus an area's own local tests when it is touched).
check "the resolver runs the identity area's tests" "sbt identity" "$(plan identity/src/main/scala/services/identity/Resolver.scala)"
check "a worker identity adapter runs them" "sbt identity" "$(plan worker/src/main/scala/services/identity/Adapter.scala)"
check "a title rule runs them" "sbt identity|sbt identity" \
  "$(plan common/src/main/resources/title-rules.tsv)|$(plan common/src/main/scala/services/titlerules/Rule.scala)"
check "an identity spec runs them" "sbt identity" "$(plan identity/src/test/scala/services/identity/agreement/MemoSpec.scala)"
check "a template runs the page area's tests" "sbt page" "$(plan web/src/main/twirl/views/repertoire.scala.html)"
check "a stylesheet runs them" "sbt page" "$(plan web/src/main/assets/css/main.css)"
check "a Playwright spec runs ESLint and them" "eslint sbt page" "$(plan page-tests-playwright/tests/card-tap.spec.ts)"
check "a page spec runs them" "sbt page" "$(plan web/src/page/scala/views/PageJsBehaviourSpec.scala)"
check "a change in both areas runs both" "eslint sbt identity page" \
  "$(plan identity/src/main/scala/services/identity/R.scala web/src/main/assets/js/a.js)"
check "a web controller is neither area: the basic sbt check only" "sbt" "$(plan web/src/main/scala/controllers/MovieController.scala)"
check "a worker source outside identity is neither" "sbt" "$(plan worker/src/main/scala/services/movies/A.scala)"
check "docs run nothing, no area" "" "$(plan docs/landing-process.md docs/design/x.md)"
check "the lint itself runs it, the judgements' reader and the take files'" "judgements scripttests sbt judgements scripttests sbt judgements scripttests sbt" \
  "$(plan scripts/ci/identity_judgements_lint.py) $(plan scripts/ci/identity_judgements.py) $(plan scripts/ci/identity_takes.py)"
check "an unmatched-cluster capture runs the identity area's tests, not the lint" "sbt identity" "$(plan test/resources/fixtures/identity-unmatched/pl.json.gz)"
# A file identity-slice or title-trace drops into old refs (their TOOL_FILES) compiles into each tool's floor ref.
check "a file identity-slice drops into old refs compiles the tools into their floors" "sbt oldrefs" \
  "$(plan worker/src/fixtures/scala/tools/MemoLookups.scala)"
check "...and one title-trace drops in (an identity test: the identity area's tests too)" "sbt identity oldrefs" "$(plan worker/src/test/scala/services/identity/TitleTrace.scala)"
check "a fixtures-config file neither tool drops in does not" "sbt" "$(plan worker/src/fixtures/scala/tools/EnrichmentCache.scala)"

# `sbt compile` skips the Test and It configs: f20a6b5dd's new `protected def wire` made a worker
# it/ spec's local val ambiguous and reached CI unseen. The sbt check compiles the test configs of
# every module the push touches AND every module that depends on one, in its one sbt run.
printf '\033[36m▸\033[0m pre-push --compile-plan\n'
compiles() { printf '%s\n' "$@" | "$REPO_ROOT/scripts/hooks/pre-push" --compile-plan | tr '\n' ' ' | sed 's/ $//'; }
check "a worker source compiles worker's Test and It configs, and e2e's tests over it" \
  "worker/Test/compile worker/It/compile e2e/Test/compile" "$(compiles worker/src/main/scala/A.scala)"
check "a worker it/ spec compiles the same" \
  "worker/Test/compile worker/It/compile e2e/Test/compile" "$(compiles worker/src/it/scala/ASpec.scala)"
check "a web source compiles web's Test, It and Page configs, and e2e's tests" \
  "web/Test/compile web/It/compile web/Page/compile e2e/Test/compile" "$(compiles web/app/A.scala)"
check "worker's fixtures config also compiles web's, whose Page config builds on it" \
  "worker/Test/compile worker/It/compile web/Test/compile web/It/compile web/Page/compile e2e/Test/compile" \
  "$(compiles worker/src/fixtures/scala/tools/EnrichmentCache.scala)"
check "a scraper compiles the scrapers' tests and the worker's and e2e's over them" \
  "scrapers/Test/compile worker/Test/compile worker/It/compile e2e/Test/compile" "$(compiles scrapers/src/main/scala/services/cinemas/A.scala)"
check "an e2e spec compiles e2e alone" "e2e/Test/compile" "$(compiles e2e/src/test/scala/A.scala)"
check "common compiles every module's test configs" \
  "common/Test/compile identity/Test/compile testkit/Test/compile scrapers/Test/compile worker/Test/compile worker/It/compile web/Test/compile web/It/compile web/Page/compile e2e/Test/compile" \
  "$(compiles common/src/main/scala/A.scala)"
check "a generated geodata table compiles what common does, every module over it" \
  "common/Test/compile identity/Test/compile testkit/Test/compile scrapers/Test/compile worker/Test/compile worker/It/compile web/Test/compile web/It/compile web/Page/compile e2e/Test/compile" \
  "$(compiles geodata/src/main/scala/models/UsRosterData.scala)"
check "the build compiles every module's test configs" \
  "common/Test/compile identity/Test/compile testkit/Test/compile scrapers/Test/compile worker/Test/compile worker/It/compile web/Test/compile web/It/compile web/Page/compile e2e/Test/compile" \
  "$(compiles build.sbt)"
check "an identity source compiles identity and every module built on it" \
  "identity/Test/compile testkit/Test/compile scrapers/Test/compile worker/Test/compile worker/It/compile web/Test/compile web/It/compile web/Page/compile e2e/Test/compile" \
  "$(compiles identity/src/main/scala/services/identity/A.scala)"
check "a dashboard alone compiles nothing extra" "" \
  "$(compiles infra/nix/files/monitoring/grafana/dashboards/apps/kinowo-http.json)"

# The repo-reading lint specs ride the same sbt run, for the paths they read (every deploy/ spec and the No*/*Lint* source
# lints, minus the ones that need the GitOps manifests or boot a server).
printf '\033[36m▸\033[0m pre-push --lint-plan\n'
lints() { printf '%s\n' "$@" | "$REPO_ROOT/scripts/hooks/pre-push" --lint-plan | tr '\n' ' ' | sed 's/ $//'; }
Always="worker:tools.NoWallClockInTestsSpec scrapers:services.cinemas.NoWallClockInClientsSpec worker:deploy.GrafanaMetricCoverageSpec worker:deploy.NoSilencedGhCurlSpec web:deploy.GrafanaWebMetricCoverageSpec"
has() { case " $1 " in *" $2 "*) echo yes ;; *) echo no ;; esac; }
check "a production Scala source runs the standing lints only" "$Always" "$(lints worker/src/main/scala/A.scala)"
for changed in scripts/x.sh .github/workflows/x.yml .claude/skills/x/SKILL.md CLAUDE.md build.sbt web/src/test/scala/ASpec.scala \
  worker/src/fixtures/scala/tools/X.scala worker/src/it/scala/XSpec.scala; do
  out="$(lints "$changed")"
  # The script self-test and dispatch-gate specs spawn bash and git, so they are `it` specs now (unit specs reach no
  # real dependency, 2026-10-27): main's integration-test row runs them, this unit lint plan no longer can.
  check "$changed runs the literal-wait, raw-git, sleep and real-dependency lints, not the it-side self-test and dispatch-gate specs" \
    "yes yes yes yes no no" "$(has "$out" worker:tools.NoLiteralWaitBoundSpec) $(has "$out" worker:deploy.SharedMachineStateLintSpec) $(has "$out" worker:tools.NoSleepInTestsSpec) $(has "$out" worker:tools.NoRealDependenciesInUnitSpecsSpec) $(has "$out" worker:deploy.ScriptSelfTestsRunInCiSpec) $(has "$out" worker:deploy.ConvergenceDispatchGateSpec)"
done
out="$(lints scripts/x.sh)"
check "...and the common source lints" "yes" "$(has "$out" common:tools.SourceControlCharactersLintSpec)"
check "...but not a spec that needs the GitOps manifests, nor one that boots a server" "no no" \
  "$(has "$out" worker:deploy.NodeCpuBudgetSpec) $(has "$out" worker:deploy.FixtureServerBootSpec)"
out="$(PREPUSH_GITOPS=1 lints scripts/x.sh)"
check "...the GitOps manifests in place (PREPUSH_GITOPS=1), the specs reading them too, still none that boots a server" "yes no" \
  "$(has "$out" worker:deploy.NodeCpuBudgetSpec) $(has "$out" worker:deploy.FixtureServerBootSpec)"
check "PREPUSH_SKIP=lintspecs drops them, keeping the standing lints" "$Always" \
  "$(PREPUSH_SKIP=lintspecs lints scripts/x.sh)"
check "an iOS-only push selects no sbt check" "ios" "$(plan ios/Kinowo/Views/A.swift)"
check "a Python self-test runs the self-tests, and the sbt check for the self-test lint" "scripttests sbt" "$(plan scripts/ci/x_test.py)"
check "worker's fixtures config selects the sbt check" "sbt" "$(plan worker/src/fixtures/resources/x.json)"

# ── Running for real, over a scratch clone: what is judged, and what is not silently passed ──
# The plan modes never read the root checkout's state: its full `git status` made each plan call cost a whole-tree rehash
# in a Linux container's bind mount (pre-push-test.sh ran past its 600 s bound there, 2026-10-18).
printf '\033[36m▸\033[0m pre-push plan modes\n'
planrepo="$testroot/planrepo"; mkdir -p "$planrepo/scripts/hooks"
scratch_repo "$planrepo"
for f in hooks/pre-push hooks-path.sh tool-conventions.sh; do cp "$REPO_ROOT/scripts/$f" "$planrepo/scripts/$f"; done
printf '#!/usr/bin/env bash\necho checked >> "%s/root-checked"\n' "$planrepo" > "$planrepo/scripts/root-checkout-check"
chmod +x "$planrepo/scripts/root-checkout-check"
scratch_git "$planrepo" add -A; scratch_git "$planrepo" commit -qm base
for mode in --plan --compile-plan --lint-plan; do
  (cd "$planrepo" && printf 'scripts/x.sh\n' | scripts/hooks/pre-push "$mode" > /dev/null 2>&1)
done
check "the plan modes never run the root-checkout check" "0" "$(cat "$planrepo/root-checked" 2>/dev/null | wc -l | tr -d ' ')"
check "with no local-tests.txt beside the hook, no area is selected" "sbt" \
  "$(cd "$planrepo" && printf 'identity/src/main/scala/A.scala\n' | scripts/hooks/pre-push --plan | tr '\n' ' ' | sed 's/ $//')"

printf '\033[36m▸\033[0m pre-push over a scratch repository\n'
if command -v shellcheck >/dev/null; then
  scratch="$testroot/scratch"; mkdir -p "$scratch"
  repo="$scratch/repo"
  mkdir -p "$repo/scripts/hooks"
  scratch_repo "$repo"
  cp "$REPO_ROOT/scripts/hooks/pre-push" "$repo/scripts/hooks/pre-push"; cp "$REPO_ROOT/scripts/hooks-path.sh" "$repo/scripts/hooks-path.sh"; cp "$REPO_ROOT/scripts/tool-conventions.sh" "$repo/scripts/tool-conventions.sh"
  for f in sbt-warm jvm-gate.sh zinc_orphans.py; do cp "$REPO_ROOT/scripts/$f" "$repo/scripts/$f"; done
  printf '#!/usr/bin/env bash\necho "$1"\n' > "$repo/a.sh"
  scratch_git "$repo" add -A; scratch_git "$repo" commit -qm base
  base="$(scratch_git "$repo" rev-parse HEAD)"
  # The PUSHED commit adds a warning (an unused variable); the working tree then fixes it.
  printf '#!/usr/bin/env bash\nunused=1\necho "$1"\n' > "$repo/a.sh"
  scratch_git "$repo" commit -qam "adds a warning"
  head="$(scratch_git "$repo" rev-parse HEAD)"
  printf '#!/usr/bin/env bash\necho "$1"\n' > "$repo/a.sh"

  (cd "$repo" && bash scripts/hooks/pre-push --range "$base..$head" >/dev/null 2>&1); code=$?
  check "judges the commits being pushed, not a working tree that has moved on" "1" "$code"
  check "leaves the working tree as it was" "0" "$(grep -c unused "$repo/a.sh")"
  check "leaves no temporary worktree behind" "1" "$(scratch_git "$repo" worktree list | wc -l | tr -d ' ')"

  # The other way round: the pushed commit is clean, the working tree is dirty with a warning.
  scratch_git "$repo" commit -qam "fixes it"; fixed="$(scratch_git "$repo" rev-parse HEAD)"
  printf '#!/usr/bin/env bash\nunused=1\necho "$1"\n' > "$repo/a.sh"
  (cd "$repo" && bash scripts/hooks/pre-push --range "$base..$fixed" >/dev/null 2>&1); code=$?
  check "passes a clean pushed commit whatever the working tree holds" "0" "$code"
  # the hook's test list lives beside it but is no shell script: shellcheck skips it (SC2148 on its prose otherwise)
  scratch_git "$repo" checkout -q -- a.sh
  echo '# prose, no shebang' > "$repo/scripts/hooks/local-tests.txt"
  scratch_git "$repo" add scripts/hooks/local-tests.txt; scratch_git "$repo" commit -qm "a test list"
  (cd "$repo" && PREPUSH_SKIP="scripttests sbt" bash scripts/hooks/pre-push --range "HEAD~1..HEAD" >/dev/null 2>&1); code=$?
  check "shellcheck skips scripts/hooks/local-tests.txt" "0" "$code"
  scratch_git "$repo" reset -q --hard HEAD~1
  check "...and removes its temporary worktree on success too" "1" "$(scratch_git "$repo" worktree list | wc -l | tr -d ' ')"
  scratch_git "$repo" checkout -q -- a.sh

  # A symbolic end (`--range A..HEAD`) is the same commit as the checkout: checked in place.
  printf '#!/usr/bin/env bash\necho "$1"\n' > "$repo/b.sh"
  scratch_git "$repo" add b.sh; scratch_git "$repo" commit -qm "adds b"
  out="$(cd "$repo" && bash scripts/hooks/pre-push --range "$fixed..HEAD" 2>&1)"
  check "checks a clean checkout in place, even when the range names it symbolically" "1 0" \
    "$(printf '%s\n' "$out" | grep -c 'shellcheck ok') $(printf '%s\n' "$out" | grep -c 'temporary checkout')"

  # A LIVE sbt in the checkout (the DevPanel's `sbt web/run`) recompiles into the same target/
  # the moment a request arrives after a merge. Two Zincs over one classes dir delete and rewrite
  # each other's class files, and the hook's run then loaded a class the other had just deleted
  # (NoClassDefFoundError: services/movies/MovieRepository$, twice on 2026-09-26). So with one
  # running, the sbt check moves to a temporary checkout of its own.
  mkdir -p "$scratch/bin"
  # a java that runs: the sbt check wants one beside sbt (CI's script-test container has none)
  printf '#!/bin/sh\nexit 0\n' > "$scratch/bin/java"; chmod +x "$scratch/bin/java"
  # The test-config compiles ride the lints' ONE sbt run, ahead of them.
  printf '#!/usr/bin/env bash\nprintf "%%s\\n" "$@" > "%s/sbt-args"\n' "$scratch" > "$scratch/bin/sbt"; chmod +x "$scratch/bin/sbt"
  mkdir -p "$repo/worker/src/it/scala"; echo 'object B' > "$repo/worker/src/it/scala/B.scala"
  scratch_git "$repo" add worker; scratch_git "$repo" commit -qm "adds a worker it/ source"
  (cd "$repo" && PATH="$scratch/bin:$PATH" bash scripts/hooks/pre-push --range "HEAD~1..HEAD" >/dev/null 2>&1)
  check "a worker push compiles its Test and It configs in the lints' one sbt invocation" \
    "worker/Test/compile worker/It/compile e2e/Test/compile" \
    "$(grep -E '^[a-z0-9]+/[A-Za-z]+/compile$' "$scratch/sbt-args" 2>/dev/null | tr '\n' ' ' | sed 's/ $//')"
  check "...ahead of the lints, which still run" "1" "$(grep -c 'worker/Test/testOnly' "$scratch/sbt-args" 2>/dev/null)"
  # The specs reading the GitOps manifests run once infra/bin/fetch-gitops has put them in place; when it cannot (Forgejo
  # and GitHub unreachable), the push is blocked, never checked against a stale checkout.
  mkdir -p "$repo/worker/src/test/scala/deploy" "$repo/infra/bin"
  echo 'class NodeCpuBudgetSpec' > "$repo/worker/src/test/scala/deploy/NodeCpuBudgetSpec.scala"
  printf '#!/usr/bin/env bash\necho "$KINOWO_GITOPS_DEST" > "%s/gitops-dest"\n' "$scratch" > "$repo/infra/bin/fetch-gitops"
  chmod +x "$repo/infra/bin/fetch-gitops"
  scratch_git "$repo" add worker infra; scratch_git "$repo" commit -qm "adds a GitOps-reading spec"
  rm -f "$scratch/sbt-args"
  out="$(cd "$repo" && PATH="$scratch/bin:$PATH" bash scripts/hooks/pre-push --range "HEAD~1..HEAD" 2>&1)"
  check "fetches the GitOps manifests into the checkout the lints run in" "$(cd "$repo" && pwd -P)/infra/kubernetes" \
    "$(cat "$scratch/gitops-dest" 2>/dev/null | sed "s|^$repo|$(cd "$repo" && pwd -P)|")"
  check "...and runs the specs reading them" "1" "$(grep -c 'deploy.NodeCpuBudgetSpec' "$scratch/sbt-args" 2>/dev/null)"
  printf '#!/usr/bin/env bash\nexit 1\n' > "$repo/infra/bin/fetch-gitops"
  echo '// unreachable' >> "$repo/worker/src/test/scala/deploy/NodeCpuBudgetSpec.scala"
  scratch_git "$repo" commit -qam "the manifests unreachable"
  rm -f "$scratch/sbt-args"
  out="$(cd "$repo" && PATH="$scratch/bin:$PATH" bash scripts/hooks/pre-push --range "HEAD~1..HEAD" 2>&1)"
  rc="$(cd "$repo" && PATH="$scratch/bin:$PATH" bash scripts/hooks/pre-push --range "HEAD~1..HEAD" >/dev/null 2>&1; echo $?)"
  # a failed fetch leaves the checkout STALE (movies-gitops 24a5162, 2026-10-21: specs read a probe main no longer had)
  check "unreachable: BLOCKS the push, naming the stale manifests and the fix, never runs the specs on them" "1 0 1 1" \
    "$rc $(grep -c 'deploy.NodeCpuBudgetSpec' "$scratch/sbt-args" 2>/dev/null) $(printf '%s\n' "$out" | grep -c 'GitOps manifests STALE') $(printf '%s\n' "$out" | grep -c 'PREPUSH_SKIP=lintspecs')"
  printf '#!/usr/bin/env bash\npwd -P > "%s/sbt-ran-in"\n' "$scratch" > "$scratch/bin/sbt"
  mkdir -p "$repo/src"; echo 'object A' > "$repo/src/A.scala"
  scratch_git "$repo" add src/A.scala; scratch_git "$repo" commit -qm "adds a Scala source"
  scala="$(scratch_git "$repo" rev-parse HEAD)"
  (cd "$repo" && PATH="$scratch/bin:$PATH" bash scripts/hooks/pre-push --range "HEAD~1..HEAD" >/dev/null 2>&1)
  check "with no other sbt in the checkout, the sbt check runs in place, warm" \
    "$(cd "$repo" && pwd -P)" "$(cat "$scratch/sbt-ran-in")"
  # git runs the hook with the pushed repository's GIT_DIR / GIT_INDEX_FILE / GIT_WORK_TREE exported;
  # a check that inherited them would aim every scratch repository of its own at this one.
  printf '#!/usr/bin/env bash\necho "${GIT_DIR-unset} ${GIT_INDEX_FILE-unset} ${GIT_WORK_TREE-unset}" > "%s/sbt-saw"\n' "$scratch" \
    > "$scratch/bin/sbt"
  (cd "$repo" && GIT_DIR="$repo/.git" GIT_INDEX_FILE="$repo/.git/index" GIT_WORK_TREE="$repo" \
     PATH="$scratch/bin:$PATH" bash scripts/hooks/pre-push --range "HEAD~1..HEAD" >/dev/null 2>&1)
  check "a check does not inherit the git environment the hook was handed" "unset unset unset" \
    "$(cat "$scratch/sbt-saw" 2>/dev/null)"
  printf '#!/usr/bin/env bash\npwd -P > "%s/sbt-ran-in"\n' "$scratch" > "$scratch/bin/sbt"

  (cd "$repo" && exec -a "java -jar /fake/sbt-launch.jar web/run" sleep 300) &
  live_sbt=$!
  # The hook finds it by name: wait until the background shell has exec'd under it, or a busy
  # runner's hook looks before it has, and checks in place (Main run 36374330375).
  wait_until "the fake sbt exec'd under its launcher name" 'ps -o args= -p "$live_sbt" 2>/dev/null | grep -q "fake/sbt-launch.jar"'
  rm -f "$scratch/sbt-ran-in"
  out="$(cd "$repo" && PATH="$scratch/bin:$PATH" bash scripts/hooks/pre-push --range "$scala~1..$scala" 2>&1)"
  kill "$live_sbt" 2>/dev/null; wait "$live_sbt" 2>/dev/null
  ran_in="$(cat "$scratch/sbt-ran-in" 2>/dev/null)"
  check "with a live sbt in the checkout, the sbt check runs in a checkout of its own" \
    "elsewhere" "$([ -n "$ran_in" ] && [ "$ran_in" != "$(cd "$repo" && pwd -P)" ] && echo elsewhere || echo "in place: $ran_in")"
  check "...and says which process it is stepping around" "1" \
    "$(printf '%s\n' "$out" | grep -c "sbt (pid $live_sbt)")"

  # The checkout's KEPT sbt server (scripts/sbt-warm, `sbt --client`) is no rival compiler: the check runs through it,
  # in place and warm, instead of in a cold temporary checkout -- and through scripts/sbt-warm itself, holding the
  # checkout's busy lock as any call does: a bare `sbt --client` held none, so the server's keeper, seeing it idle, could
  # stop it mid-check ("sbt server disconnected", 2026-10-19). sbt-warm's state, slots and keeper are the spec's own.
  export KINOWO_SBT_WARM_STATE="$scratch/sbt-warm" KINOWO_SBT_CACHE="$scratch/sbt-cache" KINOWO_IDENTITY_CACHE="$scratch/identity" KINOWO_JVM_GATE_FOLLOW_MAIN=0 \
    KINOWO_SBT_WARM_KEEPER_POLL=0.2 KINOWO_SBT_WARM_KEEPER_GRACE_SECONDS=1
  cat > "$scratch/bin/sbt" <<STUB
#!/usr/bin/env bash
echo "\$(pwd -P) \$1" > "$scratch/sbt-ran-in"
python3 -I -c 'import fcntl, glob, sys
for f in glob.glob(sys.argv[1] + "/*.busy"):
    try:
        fcntl.flock(open(f, "a"), fcntl.LOCK_EX | fcntl.LOCK_NB); print("free")
    except OSError:
        print("held")' "$scratch/sbt-warm" > "$scratch/sbt-busy"
STUB
  (cd "$repo" && exec -a "java -jar /fake/sbt-launch.jar --detach-stdio" sleep 300) &
  live_sbt=$!
  wait_until "the fake kept sbt server exec'd under its launcher name" 'ps -o args= -p "$live_sbt" 2>/dev/null | grep -q "fake/sbt-launch.jar --detach"'
  rm -f "$scratch/sbt-ran-in"
  out="$(cd "$repo" && PATH="$scratch/bin:$PATH" bash scripts/hooks/pre-push --range "$scala~1..$scala" 2>&1)"
  kill "$live_sbt" 2>/dev/null; wait "$live_sbt" 2>/dev/null
  check "with the checkout's kept sbt server, the sbt check runs through it, in place" \
    "$(cd "$repo" && pwd -P) --client" "$(cat "$scratch/sbt-ran-in" 2>/dev/null)"
  check "...through scripts/sbt-warm, holding the checkout's busy lock while it runs" "held" "$(cat "$scratch/sbt-busy" 2>/dev/null)"
  printf '#!/usr/bin/env bash\npwd -P > "%s/sbt-ran-in"\n' "$scratch" > "$scratch/bin/sbt"

  # Pushed from a worktree, git hands the hook GIT_DIR for THAT worktree. Left exported into the
  # temporary checkout, every git a check runs there read the pushing worktree's HEAD and index:
  # the drift check's `git status` and the snapshot stamp's `git ls-files` judged the wrong tree.
  echo c > "$repo/c.txt"; scratch_git "$repo" add c.txt; scratch_git "$repo" commit -qm "moves HEAD past the pushed commit"
  printf '#!/usr/bin/env bash\ngit rev-parse HEAD > "%s/sbt-saw-head"\n' "$scratch" > "$scratch/bin/sbt"
  (cd "$repo" && GIT_DIR="$(pwd)/.git" PATH="$scratch/bin:$PATH" bash scripts/hooks/pre-push --range "$scala~1..$scala" >/dev/null 2>&1)
  check "with git's GIT_DIR exported, a check in the temporary checkout sees the pushed commit" \
    "$scala" "$(cat "$scratch/sbt-saw-head" 2>/dev/null)"

  # A judgements change: the judgements lint reads the last convergence leg's served takes, and replays the takes the
  # push newly judges wrong at the PUSHED code (--replay-head), so a push that judges a take wrong and cures it lands.
  mkdir -p "$repo/scripts/ci" "$repo/test/resources/fixtures"
  printf '#!/usr/bin/env python3\nimport sys\nopen("%s/lint-args", "w").write(" ".join(sys.argv[1:]))\n' "$scratch" \
    > "$repo/scripts/ci/identity_judgements_lint.py"
  printf 'country\tvenue\trawTitle\tfilm\tverdict\tstate\tscope\tfilmTitle\treason\tjudged\tevidence\n' \
    > "$repo/test/resources/fixtures/identity-judgements.tsv"
  scratch_git "$repo" add -A; scratch_git "$repo" commit -qm "judgements"
  printf 'de\tUnion\tDer Diplomat\ttmdb:1\twrong\topen\t\t\t\t2026-10-14\t\n' >> "$repo/test/resources/fixtures/identity-judgements.tsv"
  scratch_git "$repo" commit -qam "judges a take wrong"
  (cd "$repo" && GH_IMC="[$(printf '{"databaseId":7,"status":"completed","conclusion":"success","url":"u"}')]" \
     PATH="$scratch/bin:$PATH" bash scripts/hooks/pre-push --range "HEAD~1..HEAD" >/dev/null 2>&1)
  check "a judgements push replays its newly-wrong served takes at the pushed code" "1 1 1" \
    "$(grep -c -- '--served .*de.json.gz' "$scratch/lint-args") $(grep -c -- '--before' "$scratch/lint-args") $(grep -c -- '--replay-head' "$scratch/lint-args")"
  # Agents push at once, and their hooks share the served-takes cache in the common git dir: a download finishing
  # second must neither nest itself inside the first one's directory nor hand the lint the same leg's takes twice.
  (cd "$repo" && GH_DOWNLOAD_RACE=1 GH_IMC="[$(printf '{"databaseId":8,"status":"completed","conclusion":"success","url":"u"}')]" \
     PATH="$scratch/bin:$PATH" bash scripts/hooks/pre-push --range "HEAD~1..HEAD" >/dev/null 2>&1)
  check "a served-takes download racing another hook's for the same run hands the lint one copy of each leg" "1 0" \
    "$(tr ' ' '\n' < "$scratch/lint-args" | grep -c 'de.json.gz$') $(find "$repo/.git/kinowo-cache/served-takes" -name '*.part*' | wc -l | tr -d ' ')"

  # git's own stdin, with no origin/main to find a new branch's base from: said, not silently passed.
  out="$(cd "$repo" && printf 'refs/heads/x %s refs/heads/x %s\n' "$head" 0000000000000000000000000000000000000000 \
           | bash scripts/hooks/pre-push origin url 2>&1)"
  check "warns when origin/main is missing, instead of checking nothing in silence" "1" \
    "$(printf '%s\n' "$out" | grep -c 'origin/main')"
else
  printf '  (shellcheck not installed — scratch-repository cases skipped)\n'
fi

# ── THE BASIC AND AREA TESTS RUN (scripts/hooks/local-tests.txt), AND NO LANDING CHECK IS ASKED FOR ───────────────
# The owner, 2026-10-26: no pre-merge CI. An identity change runs the identity set, a page change the page set, a docs
# change neither (nor the basic set, which rides the sbt check); identity-landing-check's LANDABLE is required nowhere.
# A fake scripts/sbt-warm records each call; a fake sbt and java stand in for need_jvm.
printf '\033[36m▸\033[0m pre-push basic and area tests\n'
area="$testroot/area"; areabin="$testroot/areabin"; mkdir -p "$area/scripts/hooks" "$areabin"
scratch_repo "$area"
for f in hooks/pre-push hooks-path.sh tool-conventions.sh; do cp "$REPO_ROOT/scripts/$f" "$area/scripts/$f"; done
cat > "$area/scripts/hooks/local-tests.txt" <<'TESTS'
# a comment
basic sbt Test/compile
identity sbt identity/test
identity sbt worker/testOnly services.identity.*
page sbt web/PageTest/test
page sh echo playwright-chromium >> "$AREA_CALLS"
TESTS
cat > "$area/scripts/sbt-warm" <<'STUB'
#!/usr/bin/env bash
printf 'sbt-warm' >> "$AREA_CALLS"; printf ' [%s]' "$@" >> "$AREA_CALLS"; echo >> "$AREA_CALLS"
case "$*" in *"${SBTWARM_FAILS:-no such}"*) echo "[error] Failed: Total 3, Failed 1"; exit 1 ;; esac
echo "[info] All tests passed."
STUB
printf '#!/usr/bin/env bash\nprintf sbt >> "$AREA_CALLS"; printf " [%%s]" "$@" >> "$AREA_CALLS"; echo >> "$AREA_CALLS"\n' > "$areabin/sbt"
printf '#!/usr/bin/env bash\nexit 0\n' > "$areabin/java"
chmod +x "$area/scripts/sbt-warm" "$areabin/sbt" "$areabin/java"
scratch_git "$area" add -A; scratch_git "$area" commit -qm base
export AREA_CALLS="$testroot/area-calls"
area_push() { # <path> [env...]: commits <path>, runs the hook over that commit; prints the exit code
  local path="$1"; shift
  mkdir -p "$area/$(dirname "$path")"; echo "x $RANDOM" >> "$area/$path"
  scratch_git "$area" add -A; scratch_git "$area" commit -qm "touches $path"
  : > "$AREA_CALLS"
  (cd "$area" && env PATH="$areabin:$PATH" PREPUSH_SKIP="eslint lintspecs" "$@" \
     bash scripts/hooks/pre-push --range HEAD~1..HEAD > "$testroot/area-out" 2>&1); echo $?
}
calls() { grep -c -- "$1" "$AREA_CALLS"; }
basic_ran() { grep -cE '(\[|; )Test/compile(\]|;)' "$AREA_CALLS"; } # the basic line, in the sbt check's run (plain or warm)
code="$(area_push identity/src/main/scala/services/identity/Resolver.scala)"
check "an identity change runs the identity set in one sbt-warm call, and the basic set in the sbt check's run" "0 1 1 0 0" \
  "$code $(calls '\[identity/test\] \[worker/testOnly services.identity.\*\]') $(basic_ran) $(calls PageTest) $(calls playwright)"
check "...and no longer asks whether identity-landing-check said LANDABLE" "0 0" \
  "$(grep -c 'LANDABLE' "$testroot/area-out") $(grep -c 'identity-landing-check' "$testroot/area-out")"
code="$(area_push web/src/main/assets/js/a.js)"
check "a page change runs the page set -- its sbt lines, then its sh lines -- and the basic set, not the identity set" \
  "0 1 1 1 0" "$code $(calls '\[web/PageTest/test\]') $(calls playwright-chromium) $(basic_ran) $(calls identity/test)"
code="$(area_push docs/landing-process.md)"
check "a docs change runs neither area nor the basic set" "0 0" "$code $(wc -l < "$AREA_CALLS" | tr -d ' ')"
code="$(area_push worker/src/main/scala/services/movies/A.scala)"
check "a code change outside the areas runs the basic set only" "0 1 0 0" \
  "$code $(basic_ran) $(calls identity/test) $(calls PageTest)"
code="$(area_push identity/src/main/scala/services/identity/Resolver.scala SBTWARM_FAILS=identity/test)"
check "a failing identity set blocks the push, naming the check and the failure" "1 1 1" \
  "$code $(grep -c 'blocked by: identity' "$testroot/area-out") $(grep -c 'Failed 1' "$testroot/area-out")"
code="$(area_push web/src/main/twirl/views/x.scala.html PREPUSH_SKIP="eslint lintspecs page")"
check "PREPUSH_SKIP=page skips the page set, said out loud" "0 0 1" \
  "$code $(calls PageTest) $(grep -c 'page SKIPPED' "$testroot/area-out")"
unset AREA_CALLS

# ── AN ABSOLUTE core.hooksPath STILL RUNS THE PUSHING WORKTREE'S OWN HOOK ──────────────────────────
# core.hooksPath=scripts/hooks is relative, so each worktree runs its own checkout's hook. Something (very likely an
# IDE's git integration) rewrote it to the ROOT checkout's absolute path three times on 2026-10-14, and every worktree's
# push then ran the root checkout's OLD hook. The hook hands itself over to the pushing worktree's copy.
printf '\033[36m▸\033[0m pre-push under an absolute core.hooksPath\n'
hp="$testroot/hp"; mkdir -p "$hp"
scratch_repo "$hp/origin.git" --bare
scratch_repo "$hp/clone"
mkdir -p "$hp/clone/scripts/hooks"
# stamp <copy> <word>: the hook with a line, right after its hand-over, recording which copy went on to run
stamp() { awk -v w="$2" -v m="$hp/ran" '{ print } /^# ── end of the hand-over ──/ { print "echo " w " >> " m }' \
  "$REPO_ROOT/scripts/hooks/pre-push" > "$1"; chmod +x "$1"; cp "$REPO_ROOT/scripts/hooks-path.sh" "$REPO_ROOT/scripts/tool-conventions.sh" "$REPO_ROOT/scripts/root-checkout-check" "$(dirname "$1")/../"; }
stamp "$hp/clone/scripts/hooks/pre-push" old
echo base > "$hp/clone/README"; scratch_git "$hp/clone" add -A; scratch_git "$hp/clone" commit -qm base
scratch_git "$hp/clone" remote add origin "$hp/origin.git"; scratch_git "$hp/clone" push -q origin HEAD:main 2>/dev/null
scratch_git "$hp/clone" fetch -q origin
scratch_git "$hp/clone" worktree add -q -b feature "$hp/wt" origin/main
stamp "$hp/wt/scripts/hooks/pre-push" new
echo doc > "$hp/wt/doc.md"; scratch_git "$hp/wt" add -A; scratch_git "$hp/wt" commit -qm "a newer hook"
scratch_git "$hp/clone" config core.hooksPath "$hp/clone/scripts/hooks"
rm -f "$hp/ran"
out="$(scratch_git "$hp/wt" push origin HEAD:refs/heads/feature 2>&1)"
check "an absolute hooksPath at the root's OLD hook still runs the pushing worktree's own" "new" "$(cat "$hp/ran" 2>/dev/null | tr '\n' ' ' | sed 's/ $//')"
check "...and warns that the hooks path is absolute" "1" "$(printf '%s\n' "$out" | grep -c 'core.hooksPath is absolute')"
rm -f "$hp/ran"
scratch_git "$hp/clone" config core.hooksPath scripts/hooks
echo more >> "$hp/wt/doc.md"; scratch_git "$hp/wt" commit -qam more
out="$(scratch_git "$hp/wt" push origin HEAD:refs/heads/feature 2>&1)"
check "the relative hooksPath runs the worktree's own hook once, no hand-over and no warning" "new 0" \
  "$(cat "$hp/ran" 2>/dev/null | tr '\n' ' ' | sed 's/ $//') $(printf '%s\n' "$out" | grep -c 'core.hooksPath is absolute')"
# the shared root checkout left detached: every push says so, and none is blocked for it
scratch_git "$hp/clone" checkout -q --detach
rm -f "$hp/ran"
echo detached >> "$hp/wt/doc.md"; scratch_git "$hp/wt" commit -qam detached
# the checks this scratch clone cannot run (no run-script-tests, no sbt) skipped: what is left is the warning's verdict
out="$(PREPUSH_SKIP="scripttests shellcheck sbt" scratch_git "$hp/wt" push origin HEAD:refs/heads/feature 2>&1)"; code=$?
check "a detached root checkout is warned about on a push from a worktree" "1" \
  "$(printf '%s\n' "$out" | grep -c "WARNING: the root checkout .* is on a DETACHED HEAD.*Fix: .* checkout main")"
check "...and the push still lands" "0 new" "$code $(cat "$hp/ran" 2>/dev/null | tr '\n' ' ' | sed 's/ $//')"
scratch_git "$hp/clone" checkout -q main
# a worktree whose hook is the very file the path names, or one handed over already, never hands over again
rm -f "$hp/ran"
scratch_git "$hp/clone" config core.hooksPath "$hp/clone/scripts/hooks"
echo again >> "$hp/wt/doc.md"; scratch_git "$hp/wt" commit -qam again
out="$(PREPUSH_DELEGATED_FROM=/elsewhere scratch_git "$hp/wt" push origin HEAD:refs/heads/feature 2>&1)"
check "a hook already handed over to runs itself (no recursion)" "old" "$(cat "$hp/ran" 2>/dev/null | tr '\n' ' ' | sed 's/ $//')"
# install-hooks.sh puts the relative path back: an absolute path to THIS checkout's scripts/hooks is the same directory,
# never "hooks that would stop running"; another directory holding hooks still is
mkdir -p "$hp/wt/scripts"; cp "$REPO_ROOT/scripts/install-hooks.sh" "$hp/wt/scripts/install-hooks.sh"; cp "$REPO_ROOT/scripts/hooks-path.sh" "$hp/wt/scripts/hooks-path.sh"
scratch_git "$hp/clone" config core.hooksPath "$hp/wt/scripts/hooks"
out="$(cd "$hp/wt" && bash scripts/install-hooks.sh 2>&1)"; code=$?
check "install-hooks rewrites an absolute path to this checkout's own scripts/hooks to the relative one" "0 scripts/hooks" \
  "$code $(scratch_git "$hp/clone" config --get core.hooksPath)"
scratch_git "$hp/clone" config core.hooksPath "$hp/clone/scripts/hooks"
(cd "$hp/wt" && bash scripts/install-hooks.sh >/dev/null 2>&1); code=$?
check "...and so is one to another worktree's scripts/hooks of this clone (what the IDE wrote)" "0 scripts/hooks" \
  "$code $(scratch_git "$hp/clone" config --get core.hooksPath)"
mkdir -p "$hp/foreign"; printf '#!/bin/sh\n' > "$hp/foreign/pre-commit"; chmod +x "$hp/foreign/pre-commit"
scratch_git "$hp/clone" config core.hooksPath "$hp/foreign"
(cd "$hp/wt" && bash scripts/install-hooks.sh >/dev/null 2>&1); code=$?
check "...but a foreign directory's hooks, which would stop running, are still refused" "1 $hp/foreign" \
  "$code $(scratch_git "$hp/clone" config --get core.hooksPath)"
rm -rf "$hp"

# ── RED BUILDS GATE A PUSH TO MAIN: only a commit declared a fix for a red run lands ───────────────
printf '\033[36m▸\033[0m pre-push: red builds gate a push to main\n'
gate="$testroot/gate"; mkdir -p "$gate"
grepo="$gate/repo"; mkdir -p "$grepo"
scratch_repo "$grepo"
# touch <path>... and commit them with the given message lines; prints the new commit.
commit_paths() { # <message> [<trailer line>] -- <path>...
  local msg=() p
  while [ "$1" != -- ]; do msg+=(-m "$1"); shift; done; shift
  for p in "$@"; do mkdir -p "$grepo/$(dirname "$p")"; echo "$RANDOM" >> "$grepo/$p"; scratch_git "$grepo" add "$p"; done
  scratch_git "$grepo" commit -q "${msg[@]}"; scratch_git "$grepo" rev-parse HEAD; }
gbase="$(commit_paths base -- README web/src/main/scala/A.scala)"
fixing="$(commit_paths "a production fix" "Fixes-Red: 38520439037" -- web/src/main/scala/A.scala)"
# Forgejo's API run id for the red run fj_run makes below (1000 + its number in the URL).
fixing_by_id="$(commit_paths "a production fix by run id" "Fixes-Red: $((1000 + 38520439037))" -- web/src/main/scala/A.scala)"
plain="$(commit_paths "an undeclared production change" -- web/src/main/scala/A.scala)"
run_json() { printf '{"databaseId":%s,"status":"%s","conclusion":"%s","url":"https://x/%s"}' "$1" "$2" "$3" "$1"; }
red="$(run_json 38520439037 completed failure 38520439037)"
green="$(run_json 38487614044 completed success 38487614044)"
cancelled="$(run_json 38470049143 completed cancelled 38470049143)"
running="$(run_json 38529260149 in_progress "" 38529260149)"
# Only the gate is under test here: the path checks the pushed paths select are skipped (the cases above cover them).
pathchecks="judgements eslint drift snapshot dashboards sbt scripttests actionlint shellcheck ios"
push() { # <range> -> exit code; stderr in $gate/err
  (cd "$grepo" && PREPUSH_SKIP="${PREPUSH_SKIP:-} $pathchecks" bash "$REPO_ROOT/scripts/hooks/pre-push" --range "$1" >/dev/null 2>"$gate/err"); echo $?; }
check "every workflow green: the push lands" "0" "$(GH_MAIN="[$green]" GH_IMC="[$green]" GH_REC="[$green]" push "$gbase..$plain")"
check "a gh warning on stderr beside a green answer is no red" "0" \
  "$(GH_WARNS=1 GH_MAIN="[$green]" GH_IMC="[$green]" GH_REC="[$green]" push "$gbase..$plain")"
check "a red Identity model convergence refuses an undeclared commit" "1" "$(GH_IMC="[$red,$green]" push "$gbase..$plain")"
check "...naming the red run" "1" "$(grep -c 'Identity model convergence is RED on main: run 38520439037' "$gate/err")"
check "...and the commit without the trailer" "1" "$(grep -c "${plain:0:9} carries no Fixes-Red" "$gate/err")"
check "a red Main refuses it too" "1" "$(GH_MAIN="[$red]" push "$gbase..$plain")"
# THE GATE is Main and Identity model convergence only (the owner, 2026-10-27): the recording's verify no longer gates
check "a red verify of the nightly recording no longer refuses it" "0" "$(GH_MAIN="[$green]" GH_IMC="[$green]" GH_REC="[$red]" push "$gbase..$plain")"
check "a commit naming the red run in Fixes-Red: lands" "0" "$(GH_IMC="[$red,$green]" push "$gbase..$fixing")"
check "every pushed commit must name it, not just one" "1" "$(GH_IMC="[$red,$green]" push "$gbase..$plain")"
check "a Fixes-Red: naming another run is no declaration" "1" "$(GH_IMC="[$(run_json 1 completed failure 1)]" push "$gbase..$fixing")"
check "a cancelled or running newest run says nothing: the decided one before it counts" "1" \
  "$(GH_IMC="[$running,$cancelled,$red,$green]" push "$gbase..$plain")"
check "...and a green one before them lets it land" "0" "$(GH_IMC="[$cancelled,$green,$red]" push "$gbase..$plain")"
# The user's rule (2026-10-14): tools may land while red; production waits for green. A commit touching no production
# path (*/src/main/, conf/, build.sbt, project/, infra/nix/, infra/kubernetes/, Dockerfile) needs no Fixes-Red: trailer.
tools="$(scratch_git "$grepo" rev-parse HEAD)"
for p in scripts/x.sh docs/x.md .claude/skills/x/SKILL.md worker/src/test/scala/XSpec.scala worker/src/it/scala/XSpec.scala \
         worker/src/fixtures/x.json "test/resources/x.json" .github/workflows/x.yml README CLAUDE.md; do
  tools="$(commit_paths "tools-only: $p" -- "$p")"
done
check "a red build lets tools-only commits land without a Fixes-Red: trailer" "0" \
  "$(GH_IMC="[$red]" GH_MAIN="[$red]" push "$plain..$tools")"
check "...said out loud, once a commit" "10" "$(grep -c 'touch no production path' "$gate/err")"
for p in web/src/main/scala/B.scala common/src/main/resources/x.conf conf/application.conf build.sbt project/plugins.sbt \
         infra/nix/hosts/x.nix infra/kubernetes/apps/x.yaml Dockerfile; do
  mixed="$(commit_paths "tools and $p" -- scripts/y.sh "$p")"
  check "a commit also touching $p is production: refused while red" "1" "$(GH_IMC="[$red]" push "$tools..$mixed")"
  scratch_git "$grepo" reset -q --hard "$tools"
done
check "a tools-only push with one undeclared production commit in it is refused" "1" \
  "$(GH_IMC="[$red]" push "$gbase..$tools")"
check "...naming only the production commit" "1 0" \
  "$(grep -c "${plain:0:9} carries no Fixes-Red" "$gate/err") $(grep -c "${tools:0:9} carries no Fixes-Red" "$gate/err")"
# --checks-only (agentctl's `waiting` branch while main is red): every check, no red gate -- it is judged at the push.
check "--checks-only runs the checks without the red gate, said out loud" "0 1 0" \
  "$(cd "$grepo" && GH_IMC="[$red]" PREPUSH_SKIP="$pathchecks" bash "$REPO_ROOT/scripts/hooks/pre-push" --checks-only "$gbase..$plain" >/dev/null 2>"$gate/err"; echo $?) $(grep -c 'red gate is not judged' "$gate/err") $(grep -c 'carries no Fixes-Red' "$gate/err")"
check "...while --range over the same commits is refused" "1" \
  "$(cd "$grepo" && GH_IMC="[$red]" PREPUSH_SKIP="$pathchecks" bash "$REPO_ROOT/scripts/hooks/pre-push" --range "$gbase..$plain" >/dev/null 2>&1; echo $?)"
check "PREPUSH_SKIP=redgate bypasses it, said out loud" "0 1" \
  "$(GH_IMC="[$red]" PREPUSH_SKIP=redgate push "$gbase..$plain") $(grep -c 'redgate SKIPPED' "$gate/err")"
check "gh failing is said for each workflow, and refuses a production push (THE GATE FAILS CLOSED)" "1 2" "$(GH_FAILS=1 push "$gbase..$plain") $(grep -c 'HTTP 502' "$gate/err")"
# gh hanging: the stub never ends on its own, so the hook gets past it only by its own bound cutting gh off. That
# mechanism is the proof -- each workflow's gh cut off (bounded's 124) and said so, no stub left running -- never
# elapsed wall time, which counts every process the hook starts: a "within 10 s" assertion failed whenever the machine
# ran the spec ~10x slower (a load of ~86: 2026-10-15, 2026-10-19). Nor does it count the stubs that started: that slow,
# a 1 s bound can end one before bash has read it. The outer bound only makes a hook that does hang a failure instead
# of a stuck spec.
hung="$gate/gh-hung"; : > "$hung"
code="$(cd "$grepo" && GH_HANGS="$hung" PREPUSH_GH_SECONDS=1 PREPUSH_SKIP="$pathchecks" \
         spec_bounded bash "$REPO_ROOT/scripts/hooks/pre-push" --range "$gbase..$plain" >/dev/null 2>"$gate/err"; echo $?)"
check "gh hanging is cut off by its bound for each workflow, said out loud, and refuses a production push" "1 2 0" \
  "$code $(grep -c "gh did not answer .* within 1s (3 attempts): cut off" "$gate/err") $(spec_still_running "$hung")"
# ── The same gate on a Forgejo remote: Forgejo's actions runs API, read by curl, not gh ────────────────────────────
fj_run() { printf '{"id":%s,"index_in_repo":%s,"status":"%s","workflow_id":"x.yml","html_url":"http://forgejo.internal:3000/pawelkrupinski/movies/actions/runs/%s"}' "$((1000 + $1))" "$1" "$2" "$1"; }
fjred="$(fj_run 38520439037 failure)"; fjgreen="$(fj_run 164 success)"
fjcancelled="$(fj_run 170 cancelled)"; fjrunning="$(fj_run 172 running)"
fjremote=ssh://forgejo-git/pawelkrupinski/movies.git
# gh fails throughout: a Forgejo push that read it would say so
fjpush() { GH_FAILS=1 PREPUSH_REMOTE_URL="$fjremote" SECRETS_STATE="${SECRETS_STATE:-unlocked}" push "$@"; }
check "Forgejo: every workflow green, the push lands, gh unread" "0 0" \
  "$(FJ_MAIN="[$fjgreen]" FJ_IMC="[$fjgreen]" FJ_REC="[$fjgreen]" fjpush "$gbase..$plain") $(grep -c 'HTTP 502' "$gate/err")"
check "Forgejo: a red Identity model convergence refuses an undeclared commit" "1" "$(FJ_IMC="[$fjred,$fjgreen]" fjpush "$gbase..$plain")"
check "...naming the red run by its number in the run's URL" "1" \
  "$(grep -c 'Identity model convergence is RED on main: run 38520439037 (failure) http://forgejo.internal:3000/pawelkrupinski/movies/actions/runs/38520439037' "$gate/err")"
check "Forgejo: a red Main refuses it too" "1" "$(FJ_MAIN="[$fjred]" fjpush "$gbase..$plain")"
check "Forgejo: a red verify of the nightly recording no longer refuses it" "0" \
  "$(FJ_MAIN="[$fjgreen]" FJ_IMC="[$fjgreen]" FJ_REC="[$fjred]" fjpush "$gbase..$plain")"
check "Forgejo: a commit naming the red run in Fixes-Red: lands" "0" "$(FJ_IMC="[$fjred]" fjpush "$gbase..$fixing")"
check "Forgejo: a commit naming the red run by its API run id (forge-gh's) lands too" "0" \
  "$(FJ_IMC="[$fjred]" fjpush "$fixing..$fixing_by_id")"
check "...and names both ids when it refuses" "1" "$(FJ_IMC="[$fjred]" fjpush "$gbase..$plain" >/dev/null; grep -c 'runs/38520439037 (run id 38520440037)' "$gate/err")"
check "Forgejo: a running or cancelled newest run says nothing: the decided one before it counts" "1 0" \
  "$(FJ_IMC="[$fjrunning,$fjcancelled,$fjred,$fjgreen]" fjpush "$gbase..$plain") $(FJ_IMC="[$fjcancelled,$fjgreen,$fjred]" fjpush "$gbase..$plain")"
check "Forgejo: tools-only commits land while red" "0" "$(FJ_IMC="[$fjred]" fjpush "$plain..$tools")"
check "Forgejo: PREPUSH_SKIP=redgate bypasses it" "0" "$(FJ_IMC="[$fjred]" PREPUSH_SKIP=redgate fjpush "$gbase..$plain")"
rm -f "$gate/argv" "$gate/config"
FJ_ARGV="$gate/argv" FJ_CONFIG="$gate/config" FJ_IMC="[$fjgreen]" fjpush "$gbase..$plain" >/dev/null
check "Forgejo: the vault's token is sent as a header on stdin, never in curl's argv" "2 0" \
  "$(grep -c '^header = "Authorization: token vaulttoken"$' "$gate/config") $(grep -c vaulttoken "$gate/argv")"
check "Forgejo: \$FORGEJO_API_TOKEN, when set, is used without the vault" "1 0" \
  "$(rm -f "$gate/gets"; SECRETS_GETS="$gate/gets" FORGEJO_API_TOKEN=envtoken FJ_IMC="[$fjred]" fjpush "$gbase..$plain") $(cat "$gate/gets" 2>/dev/null | wc -l | tr -d ' ')"
rm -f "$gate/gets"
# THE GATE FAILS CLOSED (2026-10-27 08:05Z: a gate skipped for want of a token let agentctl's waiting processor push onto
# a red Main): unread -- no token, an API error, no answer -- refuses a production push; a tools-only one still lands.
check "Forgejo: the vault locked is never unlocked (no Touch ID on a push): no token REFUSES a production push" "1 0 1 0" \
  "$(SECRETS_GETS="$gate/gets" SECRETS_STATE=locked FJ_IMC="[$fjgreen]" FJ_MAIN="[$fjgreen]" fjpush "$gbase..$plain") $(cat "$gate/gets" 2>/dev/null | wc -l | tr -d ' ') $(grep -c 'red-build gate UNREAD (no Forgejo token.*REFUSED' "$gate/err") $(grep -c 'SKIPPED' "$gate/err")"
check "...saying how to hand the hook the token, and that agentctl hands it to its processes" "1 1 1" \
  "$(grep -c 'secrets unlock movies' "$gate/err") $(grep -c 'agentctl env --print' "$gate/err") $(grep -c 'agentctl land hands the token to the processes it starts' "$gate/err")"
check "...naming the production commit it refuses" "1" "$(grep -c "production commits land only on a green.*${plain:0:9}" "$gate/err")"
check "Forgejo: no token, a tools-only push lands, said out loud" "0 1" \
  "$(SECRETS_STATE=locked fjpush "$plain..$tools") $(grep -c 'red-build gate UNREAD (no Forgejo token.*every pushed commit is tools-only' "$gate/err")"
rm -f "$gate/argv"
check "Forgejo: an API error is asked 3 times a workflow, said for each, and REFUSES a production push" "1 2 6 1" \
  "$(FJ_ARGV="$gate/argv" FJ_FAILS=1 FORGEJO_API_TOKEN=envtoken fjpush "$gbase..$plain") $(grep -c 'from Forgejo (curl: (7).* in 3 attempts' "$gate/err") $(wc -l < "$gate/argv" | tr -d ' ') $(grep -c 'red-build gate UNREAD.*REFUSED' "$gate/err")"
check "...while a tools-only push lands" "0" "$(FJ_FAILS=1 FORGEJO_API_TOKEN=envtoken fjpush "$plain..$tools")"
check "Forgejo: a failure that a retry answers is no failure: green read, the push lands" "0 0" \
  "$(FJ_FAILS_ONCE="$gate/failed-once" FORGEJO_API_TOKEN=envtoken FJ_MAIN="[$fjgreen]" FJ_IMC="[$fjgreen]" fjpush "$gbase..$plain") $(grep -c 'UNREAD' "$gate/err")"
rm -f "$gate/failed-once"
check "Forgejo: a gate read red (token in the environment, as agentctl hands it) refuses a production push" "1 1" \
  "$(FORGEJO_API_TOKEN=envtoken FJ_MAIN="[$fjred]" FJ_IMC="[$fjgreen]" fjpush "$gbase..$plain") $(grep -c 'Main is RED on main' "$gate/err")"
check "Forgejo: a gate read green (token in the environment) lets it land" "0 0" \
  "$(FORGEJO_API_TOKEN=envtoken FJ_MAIN="[$fjgreen]" FJ_IMC="[$fjgreen]" fjpush "$gbase..$plain") $(grep -c 'RED on main\|UNREAD' "$gate/err")"
check "Forgejo: an HTTP refusal is said for each workflow, and refuses a production push" "1 2" \
  "$(FJ_EXPECT_BASE=http://elsewhere FJ_IMC="[$fjred]" fjpush "$gbase..$plain") $(grep -c 'from Forgejo (HTTP 404)' "$gate/err")"
check "Forgejo: an http remote's own host is read" "1" \
  "$(FJ_EXPECT_BASE=http://forgejo.internal:3000/api/v1/repos/pawelkrupinski/movies FJ_IMC="[$fjred]" fjremote=http://forgejo.internal:3000/pawelkrupinski/movies.git fjpush "$gbase..$plain")"
check "Forgejo: an https remote with credentials reads its host, without them" "1" \
  "$(FJ_EXPECT_BASE=https://forgejo.example.org/api/v1/repos/o/r FJ_IMC="[$fjred]" fjremote=https://u:p@forgejo.example.org/o/r.git fjpush "$gbase..$plain")"
check "a GitHub remote still reads gh, not Forgejo" "1 1" \
  "$(GH_IMC="[$red]" FJ_FAILS=1 PREPUSH_REMOTE_URL=git@github.com:pawelkrupinski/movies.git push "$gbase..$plain") $(grep -c 'Identity model convergence is RED on main: run 38520439037 (failure) https://x/' "$gate/err")"
out="$(cd "$grepo" && printf 'refs/heads/main %s refs/heads/main %s\n' "$plain" "$gbase" \
        | FJ_IMC="[$fjred]" GH_FAILS=1 SECRETS_STATE=unlocked PREPUSH_SKIP="$pathchecks" bash "$REPO_ROOT/scripts/hooks/pre-push" forgejo "$fjremote" 2>&1)"; code=$?
check "a real push to a Forgejo remote is gated by Forgejo's runs, from the URL git hands the hook" "1 1" \
  "$code $(printf '%s\n' "$out" | grep -c 'Identity model convergence is RED on main: run 38520439037 (failure) http://forgejo')"
out="$(cd "$grepo" && printf 'refs/heads/x %s refs/heads/x %s\n' "$plain" "$gbase" \
        | GH_IMC="[$red]" PREPUSH_SKIP="$pathchecks" bash "$REPO_ROOT/scripts/hooks/pre-push" origin url 2>&1)"; code=$?
check "a push to another branch than main is not gated" "0 0" "$code $(printf '%s\n' "$out" | grep -c 'RED on main')"
out="$(cd "$grepo" && printf 'refs/heads/m %s refs/heads/measure/u/abc-1 %s\n' "$plain" 0000000000000000000000000000000000000000 \
        | GH_IMC="[$red]" bash "$REPO_ROOT/scripts/hooks/pre-push" forgejo url 2>&1)"; code=$?
check "a measurement's measure/ branch alone is checked for nothing: it never lands" "0 1" \
  "$code $(printf '%s\n' "$out" | grep -c "measure/ or check/ branch: a CI run's ref")"
out="$(cd "$grepo" && printf 'refs/heads/m %s refs/heads/check/20261027-1200-movies-wt-x-3 %s\n' "$plain" 0000000000000000000000000000000000000000 \
        | GH_IMC="[$red]" bash "$REPO_ROOT/scripts/hooks/pre-push" forgejo url 2>&1)"; code=$?
check "...nor is a remote check's check/ branch: its run checks it on ci-runner" "0 1" \
  "$code $(printf '%s\n' "$out" | grep -c "measure/ or check/ branch: a CI run's ref")"
out="$(cd "$grepo" && printf 'refs/heads/m %s refs/heads/land/20261028-1200-movies-wt-x-3 %s\n' "$plain" 0000000000000000000000000000000000000000 \
        | GH_IMC="[$red]" bash "$REPO_ROOT/scripts/hooks/pre-push" forgejo url 2>&1)"; code=$?
check "...nor is a submitted change's land/ ref: the reconciler checks it (agentctl, design step 14)" "0 1" \
  "$code $(printf '%s\n' "$out" | grep -c "measure/ or check/ branch: a CI run's ref")"
out="$(cd "$grepo" && printf 'refs/heads/m %s refs/heads/measure/u/abc-1 %s\nrefs/heads/main %s refs/heads/main %s\n' "$plain" 0000000000000000000000000000000000000000 "$plain" "$gbase" \
        | GH_IMC="[$red]" PREPUSH_SKIP="$pathchecks" bash "$REPO_ROOT/scripts/hooks/pre-push" origin url 2>&1)"; code=$?
check "…but beside a push to main, main is gated as ever" "1" "$code"
out="$(cd "$grepo" && printf 'refs/heads/main %s refs/heads/main %s\n' "$plain" "$gbase" \
        | GH_IMC="[$red]" PREPUSH_SKIP="$pathchecks" bash "$REPO_ROOT/scripts/hooks/pre-push" origin url 2>&1)"; code=$?
check "a real push to main reads git's stdin and is gated" "1" "$code"
# ── core.hooksPath: the hook leaves it as it found it, mends an absolute path to its own clone, and catches a mover ──
# The shared config's relative core.hooksPath turned absolute between two pushes on 2026-10-14 with no IDE running. The
# hook runs with git's GIT_* variables (this spec's scratch_git clears them for its children, as the hook does for its
# own); whatever it runs may write to the repository it was started in. It must not leave that written.
mkdir -p "$grepo/scripts/hooks"
hooks_path() { scratch_git "$grepo" config --local --get core.hooksPath; }
all_green() { GH_MAIN="[$green]" GH_IMC="[$green]" GH_REC="[$green]" "$@"; }
scratch_git "$grepo" config core.hooksPath scripts/hooks
all_green push "$gbase..$plain" >/dev/null
check "a push that moves nothing leaves the relative core.hooksPath as it was" "scripts/hooks" "$(hooks_path)"
GH_REWRITES_HOOKSPATH="$grepo/scripts/hooks" all_green push "$gbase..$plain" >/dev/null
check "a check that rewrites core.hooksPath to an absolute path is undone at the hook's exit" "scripts/hooks" "$(hooks_path)"
check "...and the hook names the move" "1" "$(grep -c 'core.hooksPath moved from "scripts/hooks"' "$gate/err")"
scratch_git "$grepo" config core.hooksPath "$grepo/scripts/hooks"
all_green push "$gbase..$plain" >/dev/null
check "an absolute core.hooksPath naming this clone's own scripts/hooks is set back to relative" "scripts/hooks" "$(hooks_path)"
check "...with a warning naming what it was" "1" "$(grep -c "core.hooksPath was $grepo/scripts/hooks" "$gate/err")"
scratch_git "$grepo" config core.hooksPath "$gate/foreign-hooks"
all_green push "$gbase..$plain" >/dev/null
check "an absolute core.hooksPath to some other directory is left alone" "$gate/foreign-hooks" "$(hooks_path)"
# ── scripttests: the runner over what the push changed, from its base, blocking on a failure ──
printf '#!/usr/bin/env bash\necho "$*" >> "$STUB_RUNNER_ARGS"\ncase "$*" in *--bash*) exit "${STUB_FLOOR_EXIT:-0}" ;; esac; exit "${STUB_RUNNER_EXIT:-0}"\n' > "$grepo/scripts/run-script-tests"
chmod +x "$grepo/scripts/run-script-tests"
runner_base="$(scratch_git "$grepo" rev-parse HEAD)"
scratch_git "$grepo" add scripts/run-script-tests && scratch_git "$grepo" commit -qm "a runner stub"
runner_head="$(scratch_git "$grepo" rev-parse HEAD)"
# the Mac's /bin/bash, faked: a bash reporting the major version given
floor_bash() { printf '#!/bin/sh\necho %s\n' "$1" > "$gate/bash$1"; chmod +x "$gate/bash$1"; echo "$gate/bash$1"; }
script_push() { rm -f "$gate/args"; (cd "$grepo" && all_green env PREPUSH_SKIP="${pathchecks/scripttests/}" STUB_RUNNER_ARGS="$gate/args" \
  PREPUSH_FLOOR_BASH="${floor:-$gate/no-such-bash}" "$@" \
  bash "$REPO_ROOT/scripts/hooks/pre-push" --range "$runner_base..$runner_head" >/dev/null 2>"$gate/err"); echo $?; }
check "a script push runs the self-tests its changes select, from the push's base" "0 --changed $runner_base" \
  "$(script_push) $(cat "$gate/args")"
check "...and a failing one blocks the push" "1 1" "$(script_push STUB_RUNNER_EXIT=1) $(grep -c 'scripttests FAILED' "$gate/err")"
floor=$(floor_bash 3)
check "on a Mac (a /bin/bash 3) they run under it too, the floor the tools promise" "0 --changed $runner_base|--changed $runner_base --bash $floor" \
  "$(script_push) $(paste -sd'|' - < "$gate/args")"
check "...and a failure only there blocks the push" "1 1" "$(script_push STUB_FLOOR_EXIT=1) $(grep -c 'scripttests FAILED' "$gate/err")"
floor=$(floor_bash 5)
check "a /bin/bash 4 or newer (Linux) runs them once" "0 --changed $runner_base" "$(script_push) $(cat "$gate/args")"
# A pushed commit from before the runner took --bash: the hook is the working tree's, the runner the commit's.
printf '#!/usr/bin/env bash\necho "$*" >> "$STUB_RUNNER_ARGS"\nexit 0\n' > "$grepo/scripts/run-script-tests"
scratch_git "$grepo" add scripts/run-script-tests && scratch_git "$grepo" commit -qm "an older runner"
runner_head="$(scratch_git "$grepo" rev-parse HEAD)"
floor=$(floor_bash 3)
check "a runner that predates --bash runs them once, even on a Mac" "0 --changed $runner_base" "$(script_push) $(cat "$gate/args")"
floor=""
# A runner with --linux: the same tests on Linux, in the background beside the native run.
printf '#!/usr/bin/env bash\necho "$*" >> "$STUB_RUNNER_ARGS"\ncase "$*" in *--linux*) [ -z "${STUB_NO_DOCKER:-}" ] || { echo "run-script-tests: --linux needs a running Docker" >&2; exit 2; }; exit "${STUB_LINUX_EXIT:-0}" ;; esac; exit 0\n' > "$grepo/scripts/run-script-tests"
scratch_git "$grepo" add scripts/run-script-tests && scratch_git "$grepo" commit -qm "a runner with --linux"
runner_head="$(scratch_git "$grepo" rev-parse HEAD)"
check "with --linux, the same tests run on Linux too" "0 --changed $runner_base|--linux --changed $runner_base" \
  "$(script_push) $(LC_ALL=C sort "$gate/args" | paste -sd'|' -)"
check "...and a failure only there blocks the push" "1 1" "$(script_push STUB_LINUX_EXIT=1) $(grep -c 'scripttests FAILED' "$gate/err")"
check "...Docker not running: never blocking" "0 1" "$(script_push STUB_NO_DOCKER=1) $(grep -c -- '--linux' "$gate/args")"
check "...and PREPUSH_SKIP=linux skips just that" "0 --changed $runner_base" \
  "$(script_push PREPUSH_SKIP="${pathchecks/scripttests/} linux") $(cat "$gate/args")"
# ── oldrefs: a push changing a tool's TOOL_FILES runs the old-ref compile test, read from the tools of the pushed tree ──
printf '#!/usr/bin/env bash\nTOOL_FILES=(tools/A.scala\n  tools/B.scala)\n' > "$grepo/scripts/identity-slice"
printf '#!/usr/bin/env bash\nTOOL_FILES=(tools/T.scala)\n' > "$grepo/scripts/title-trace"
printf '#!/usr/bin/env bash\necho ran >> "$STUB_OLDREFS_RAN"\necho "${STUB_OLDREFS_LINE:-FAIL: tools compile into c0ffee}" \nexit "${STUB_OLDREFS_EXIT:-0}"\n' \
  > "$grepo/scripts/identity-slice-old-ref-test.sh"
scratch_git "$grepo" add scripts && scratch_git "$grepo" commit -qm "the tools, stubbed"
# the check wants an sbt and a java to compile with (CI's script-test container has neither: stubbed); the stubbed test needs neither
mkdir -p "$gate/sbt"; printf '#!/bin/sh\nexit 1\n' > "$gate/sbt/sbt"; printf '#!/bin/sh\nexit 0\n' > "$gate/sbt/java"
chmod +x "$gate/sbt/sbt" "$gate/sbt/java"
oldrefs_base="$(scratch_git "$grepo" rev-parse HEAD)"
oldrefs_push() { # <path> [<env>...] -> exit code, then how often the old-ref test ran; stderr in $gate/err
  local path=$1 head; shift
  scratch_git "$grepo" reset -q --hard "$oldrefs_base"; head="$(commit_paths "change $path" -- "$path")"; rm -f "$gate/ran"
  (cd "$grepo" && all_green env PATH="$gate/sbt:$PATH" PREPUSH_SKIP="$pathchecks" STUB_OLDREFS_RAN="$gate/ran" "$@" \
    bash "$REPO_ROOT/scripts/hooks/pre-push" --range "$oldrefs_base..$head" >/dev/null 2>"$gate/err"); echo "$? $(cat "$gate/ran" 2>/dev/null | wc -l | tr -d ' ')"; }
check "a push changing identity-slice's tool file runs the old-ref compile" "0 1" "$(oldrefs_push tools/B.scala)"
check "...and title-trace's" "0 1" "$(oldrefs_push tools/T.scala)"
check "...a failing compile blocks the push, its output shown" "1 1 1 1" \
  "$(oldrefs_push tools/A.scala STUB_OLDREFS_EXIT=1) $(grep -c 'oldrefs FAILED' "$gate/err") $(grep -c 'FAIL: tools compile into c0ffee' "$gate/err")"
check "a file neither tool drops in does not run it" "0 0" "$(oldrefs_push tools/C.scala)"
check "PREPUSH_SKIP=oldrefs skips it, said out loud" "0 0 1" \
  "$(oldrefs_push tools/A.scala PREPUSH_SKIP="$pathchecks oldrefs") $(grep -c 'oldrefs SKIPPED' "$gate/err")"

# ── THE KNOWN-RED BASELINE: a check failing only as it fails on plain main does not block ─────────────────────────
# 2026-10-27: every land failed on main's own breakage, the fix included. With PREPUSH_BASELINE_DIR (agentctl sets it),
# <dir>/<base sha> holds the hook's output of the same checks on the base; a failure the base shares is said, not held
# against the push. Fail-closed: no baseline for the base, every failure blocks.
printf '\033[36m▸\033[0m pre-push: the known-red baseline\n'
known="$gate/known"; mkdir -p "$known"
# the baseline of oldrefs_base: the hook's own output of a run whose old-ref compile fails with the default line
{ echo "pre-push baseline $oldrefs_base"; oldrefs_push tools/A.scala STUB_OLDREFS_EXIT=1 >/dev/null; cat "$gate/err"; } \
  > "$known/$oldrefs_base.out"
check "the baseline run itself failed the old-ref compile" "1" "$(grep -c 'oldrefs FAILED' "$known/$oldrefs_base.out")"
mv "$known/$oldrefs_base.out" "$known/$oldrefs_base"
check "a failure plain main shares is said, not blocking" "0 1 1 1" \
  "$(oldrefs_push tools/A.scala STUB_OLDREFS_EXIT=1 PREPUSH_BASELINE_DIR="$known") $(grep -c 'KNOWN RED: oldrefs' "$gate/err") $(grep -c 'FAIL: tools compile into c0ffee' "$gate/err")"
check "...a failure plain main does not share blocks" "1 1 0" \
  "$(oldrefs_push tools/A.scala STUB_OLDREFS_EXIT=1 STUB_OLDREFS_LINE="Error: a new break" PREPUSH_BASELINE_DIR="$known") $(grep -c 'KNOWN RED' "$gate/err")"
check "...a change fixing the baseline's failure passes" "0 1 0" \
  "$(oldrefs_push tools/A.scala PREPUSH_BASELINE_DIR="$known") $(grep -c 'KNOWN RED' "$gate/err")"
check "...no baseline for the base: every failure blocks, said" "1 1 1" \
  "$(oldrefs_push tools/A.scala STUB_OLDREFS_EXIT=1 PREPUSH_BASELINE_DIR="$known/none") $(grep -c 'no known-red baseline for base' "$gate/err")"
sed '1s/.*/pre-push baseline of something else/' "$known/$oldrefs_base" > "$known/x" && mv "$known/x" "$known/$oldrefs_base"
check "...a baseline file not naming the base: every failure blocks" "1 1 1" \
  "$(oldrefs_push tools/A.scala STUB_OLDREFS_EXIT=1 PREPUSH_BASELINE_DIR="$known") $(grep -c 'does not start' "$gate/err")"
check "...and without PREPUSH_BASELINE_DIR, as ever" "1 1" "$(oldrefs_push tools/A.scala STUB_OLDREFS_EXIT=1)"
rm -rf "$known"

# ── A CHECK'S TOOL MISSING BLOCKS THE PUSH, WITH THE FIX: never a silent skip ───────────────────────────────────────
# 2026-10-21: an agent's push, sbt off its PATH, skipped the sbt check ("sbt not installed — SKIPPED") and landed
# unchecked. A local tool missing is fixed locally, so the push fails and says how (user, 2026-10-21); only an external
# service unreachable (the red gate's Forgejo API, the GitOps clone) stays a warning.
printf '\033[36m▸\033[0m pre-push: a missing tool blocks the push, with the fix\n'
# $PATH with <tool>... gone: a directory holding one is replaced by links to its other commands.
path_without() { # <tool>...
  local d f t hit out="" shim
  shim="$(mktemp -d "$gate/without.XXXXXX")"
  while IFS= read -r d; do
    [ -n "$d" ] || continue
    hit=0; for t in "$@"; do [ -x "$d/$t" ] && hit=1; done
    if [ $hit = 0 ]; then out="$out:$d"; continue; fi
    for f in "$d"/*; do
      t="${f##*/}"; case " $* " in *" $t "*) continue ;; esac
      [ -e "$shim/$t" ] || ln -s "$f" "$shim/$t"
    done
    case ":$out:" in *":$shim:"*) ;; *) out="$out:$shim" ;; esac
  done < <(printf '%s\n' "$PATH" | tr ':' '\n')
  printf '%s' "${out#:}"; }
# a push changing <path> with only <checks> run (every other path check and the red gate skipped) -> exit code; stderr in
# $gate/err
tool_push() { # <path> <checks> [<env>...]
  local path=$1 skip=" $pathchecks oldrefs redgate " c head; shift
  for c in $1; do skip="${skip/ $c / }"; done; shift
  scratch_git "$grepo" reset -q --hard "$oldrefs_base"; head="$(commit_paths "change $path" -- "$path")"
  (cd "$grepo" && all_green env PREPUSH_SKIP="$skip" "$@" \
    bash "$REPO_ROOT/scripts/hooks/pre-push" --range "$oldrefs_base..$head" >/dev/null 2>"$gate/err"); echo $?; }
nosbt="$(path_without sbt)"
check "sbt not on PATH: the sbt check BLOCKS the push" "1 1" "$(tool_push src/X.scala sbt PATH="$nosbt") $(grep -c 'sbt FAILED' "$gate/err")"
check "...saying how to put it there (sdkman's sbt and java, or its init line)" "1 1" \
  "$(grep -c 'sdkman/candidates/sbt/current/bin' "$gate/err") $(grep -c 'sdkman-init.sh' "$gate/err")"
check "...and never that it skipped" "0" "$(grep -c 'sbt SKIPPED\|sbt not installed\|— SKIPPED' "$gate/err")"
check "PREPUSH_SKIP=sbt still skips it, said out loud" "0 1" \
  "$(tool_push src/X.scala "" PATH="$nosbt") $(grep -c 'sbt SKIPPED (PREPUSH_SKIP)' "$gate/err")"
mkdir -p "$gate/nojava"; printf '#!/bin/sh\necho "Unable to locate a Java Runtime." >&2\nexit 1\n' > "$gate/nojava/java"
chmod +x "$gate/nojava/java"
check "sbt there but no java that runs (macOS's /usr/bin/java stub): BLOCKED, with the fix" "1 1" \
  "$(tool_push src/X.scala sbt PATH="$gate/nojava:$gate/sbt:$PATH" JAVA_HOME=) $(grep -c 'java.*sdkman/candidates/java/current/bin' "$gate/err")"
check "the old-ref compile with no sbt: BLOCKED" "1 1" \
  "$(tool_push tools/A.scala oldrefs PATH="$nosbt") $(grep -c 'oldrefs FAILED' "$gate/err")"
check "shellcheck missing: the shellcheck check BLOCKS, with the fix" "1 1" \
  "$(tool_push x.sh shellcheck PATH="$(path_without shellcheck)") $(grep -c 'brew install shellcheck' "$gate/err")"
check "actionlint missing: a workflow push BLOCKS, with the fix" "1 1" \
  "$(tool_push .github/workflows/x.yml actionlint PATH="$(path_without actionlint)") $(grep -c 'brew install actionlint' "$gate/err")"
check "npm missing: the ESLint check BLOCKS, with the fix" "1 1" \
  "$(tool_push eslint.config.js eslint PATH="$(path_without npm)") $(grep -c 'npm not found' "$gate/err")"
mkdir -p "$gate/notzf"; printf '#!/bin/sh\n[ "$1" = -c ] && { echo "ModuleNotFoundError: No module named timezonefinder" >&2; exit 1; }\nexit 0\n' \
  > "$gate/notzf/python3"; chmod +x "$gate/notzf/python3"
check "timezonefinder missing: the roster drift check BLOCKS, with the fix" "1 1" \
  "$(tool_push data/pl/venues.json drift PATH="$gate/notzf:$PATH") $(grep -c 'pip install timezonefinder' "$gate/err")"
check "python3 missing: the dashboards check BLOCKS, with the fix" "1 1" \
  "$(tool_push infra/test/test_dashboards.py dashboards PATH="$(path_without python3)") $(grep -c 'python3 not found' "$gate/err")"
check "gh missing on a judgements push: BLOCKED (the served-takes check needs it), with the fix" "1 1" \
  "$(tool_push test/resources/fixtures/identity-judgements.tsv judgements PATH="$(path_without gh)") $(grep -c 'gh not found' "$gate/err")"
# NO STEP HANGS (2026-10-27: a land's hook sat 2.5 h in one sbt-warm call, the landing lock held): a check making no
# progress -- no CPU, no log, no sbt-warm progress mark -- for PREPUSH_STALL_SECONDS is ended, the hook failing with
# `pre-push STALLED: <check>` and exit 75, the stall signal agentctl land reads
mkdir -p "$gate/hungpy"; printf '#!/bin/sh\n[ "$1" = -c ] && exit 0\necho $$ >> "$PY_HANGS"\nexec sleep 86400\n' > "$gate/hungpy/python3"
chmod +x "$gate/hungpy/python3"; : > "$gate/py-hung"
scratch_git "$grepo" reset -q --hard "$oldrefs_base"; thung="$(commit_paths "change dashboards" -- infra/test/test_dashboards.py)"
code="$(cd "$grepo" && all_green spec_bounded env PREPUSH_SKIP="${pathchecks/dashboards/} oldrefs redgate" PATH="$gate/hungpy:$PATH" \
  PY_HANGS="$gate/py-hung" PREPUSH_STALL_SECONDS=2 KINOWO_STALL_POLL_SECONDS=1 \
  bash "$REPO_ROOT/scripts/hooks/pre-push" --range "$oldrefs_base..$thung" >/dev/null 2>"$gate/err"; echo $?)"
check "a check making no progress is ended: exit 75, \`pre-push STALLED: <check>\`, no hung step left" "75 1 0" \
  "$code $(grep -c '^pre-push STALLED: dashboards' "$gate/err") $(spec_still_running "$gate/py-hung")"
# ...while one busy past the bound (CPU in its processes) is no stall, however loaded the machine: busy, it is runnable at
# every poll, whatever CPU share it gets (it loops in bash builtins, forking nothing)
printf '#!/usr/bin/env bash\n[ "$1" = -c ] && exit 0\nend=$((SECONDS + 4)); while [ "$SECONDS" -lt "$end" ]; do :; done\n' > "$gate/hungpy/python3"
code="$(cd "$grepo" && all_green spec_bounded env PREPUSH_SKIP="${pathchecks/dashboards/} oldrefs redgate" PATH="$gate/hungpy:$PATH" \
  PREPUSH_STALL_SECONDS=2 KINOWO_STALL_POLL_SECONDS=1 KINOWO_STALL_CPU_CENTIS=10 \
  bash "$REPO_ROOT/scripts/hooks/pre-push" --range "$oldrefs_base..$thung" >/dev/null 2>"$gate/err"; echo $?)"
check "...while a check busy past the bound passes" "0 0" "$code $(grep -c 'STALLED' "$gate/err")"
# The red gate: its TOOLS missing block like any check's; Forgejo itself unreachable blocks a production push too.
scratch_git "$grepo" reset -q --hard "$oldrefs_base"; tplain="$(commit_paths "a production change" -- web/src/main/scala/A.scala)"
gate_push() { (cd "$grepo" && PREPUSH_SKIP="$pathchecks" "$@" bash "$REPO_ROOT/scripts/hooks/pre-push" --range "$oldrefs_base..$tplain" \
  >/dev/null 2>"$gate/err"); echo $?; }
check "jq missing on a Forgejo push: the red gate BLOCKS, with the fix" "1 1" \
  "$(gate_push env PATH="$(path_without jq)" PREPUSH_REMOTE_URL="$fjremote" FORGEJO_API_TOKEN=t) $(grep -c 'brew install jq' "$gate/err")"
check "gh missing on a GitHub push: the red gate BLOCKS, with the fix" "1 1" \
  "$(gate_push env PATH="$(path_without gh)") $(grep -c 'brew install gh' "$gate/err")"
check "...Forgejo unreachable: the gate unread, a production push is refused" "1 1" \
  "$(gate_push env FJ_FAILS=1 GH_FAILS=1 PREPUSH_REMOTE_URL="$fjremote" FORGEJO_API_TOKEN=t) $(grep -c 'red-build gate UNREAD' "$gate/err")"
# ── THE REUSE: a passed run answers a re-push of the SAME tree, and nothing else ─────────────────────────────────────
# A re-push after a no-content rebase or a reworded message ran every check again. A pass is stored under a key of the
# pushed tree, the base's, the changed paths, the checks, the hook's bytes, the red-gate state and PREPUSH_SKIP; the
# expensive check here is the counting script-test runner. What must hold: ANY difference runs everything.
printf '\033[36m▸\033[0m pre-push: a passed run is reused for the same tree only\n'
reuse="$gate/reuse"; mkdir -p "$reuse"
scratch_git "$grepo" reset -q --hard "$oldrefs_base"
printf '#!/usr/bin/env bash\necho ran >> "$STUB_RUNS"\nexit "${STUB_RUNNER_EXIT:-0}"\n' > "$grepo/scripts/run-script-tests"
chmod +x "$grepo/scripts/run-script-tests"
scratch_git "$grepo" add scripts/run-script-tests && scratch_git "$grepo" commit -qm "a counting runner"
rbase="$(scratch_git "$grepo" rev-parse HEAD)"
rhead="$(commit_paths "a script change" -- scripts/z.sh)"
# <range> [<env>...] -> "<exit> <runner runs so far>"; stderr in $gate/err. $HOOK: the hook to run (default this checkout's).
reuse_push() {
  local range=$1; shift
  (cd "$grepo" && all_green env PREPUSH_NO_REUSE= PREPUSH_REUSE_DIR="$reuse/cache" PREPUSH_SKIP="${pathchecks/scripttests/}" \
    STUB_RUNS="$reuse/runs" PREPUSH_FLOOR_BASH="$gate/no-such-bash" "$@" \
    bash "${HOOK:-$REPO_ROOT/scripts/hooks/pre-push}" --range "$range" >/dev/null 2>"$gate/err"); echo "$? $(cat "$reuse/runs" 2>/dev/null | wc -l | tr -d ' ')"; }
check "a first push runs the checks" "0 1" "$(reuse_push "$rbase..$rhead")"
check "the same push again reuses the pass: nothing runs, said once" "0 1 1" \
  "$(reuse_push "$rbase..$rhead") $(grep -c 'reused, not run again' "$gate/err")"
reworded="$(scratch_git "$grepo" commit -q --amend -m "the same change, reworded" && scratch_git "$grepo" rev-parse HEAD)"
check "a new commit of the SAME tree (no-content rebase, reworded message) reuses it" "0 1" "$(reuse_push "$rbase..$reworded")"
check "PREPUSH_NO_REUSE=1 runs everything regardless" "0 2" "$(reuse_push "$rbase..$reworded" PREPUSH_NO_REUSE=1)"
changed_tree="$(commit_paths "one more byte" -- scripts/z.sh)"
check "a changed tree runs everything" "0 3" "$(reuse_push "$rbase..$changed_tree")"
docs_only="$(commit_paths "a change outside what the checks read" -- docs/x.md)"
check "...even a change outside every checked path" "0 4" "$(reuse_push "$rbase..$docs_only")"
check "...and once passed, that tree is reused in turn" "0 4" "$(reuse_push "$rbase..$docs_only")"
check "another base (same tree) runs everything" "0 5" "$(reuse_push "$rhead..$docs_only")"
check "a red-gate state that differs runs everything (a red run appeared; tools land while red)" "0 6" \
  "$(reuse_push "$rbase..$docs_only" GH_IMC="[$red]")"
check "another PREPUSH_SKIP runs everything" "0 7" "$(reuse_push "$rbase..$docs_only" PREPUSH_SKIP="${pathchecks/scripttests/} linux")"
# --fast-only: the `waiting` batch's combined check (sbt alone: compiles, lints, the `fast` set), no red gate; a push
# with PREPUSH_REUSE_FAST=1 (agentctl's batch push) of that tree on that base reuses its pass after the gate
fast_head="$(commit_paths "a batch of script changes" -- scripts/z.sh)"
fast_run() { (cd "$grepo" && env PREPUSH_NO_REUSE= PREPUSH_REUSE_DIR="$reuse/cache" PREPUSH_SKIP="${pathchecks/scripttests/}" \
  STUB_RUNS="$reuse/runs" GH_IMC="[$red]" bash "$REPO_ROOT/scripts/hooks/pre-push" --fast-only "$rbase..$fast_head" >/dev/null 2>"$gate/err")
  echo "$? $(cat "$reuse/runs" 2>/dev/null | wc -l | tr -d ' ')"; }
check "--fast-only judges no red gate and runs no path check but sbt (scripttests not run)" "0 7 1" \
  "$(fast_run) $(grep -c 'fast-only: the red gate is not judged' "$gate/err")"
check "a batch push with PREPUSH_REUSE_FAST=1 reuses the fast pass: nothing runs, said once" "0 7 1" \
  "$(reuse_push "$rbase..$fast_head" PREPUSH_REUSE_FAST=1) $(grep -c "the waiting batch's fast check passed" "$gate/err")"
check "...an ordinary push of it runs its checks" "0 8" "$(reuse_push "$rbase..$fast_head")"
# A REMOTE PASS: agentctl pushes a change its checks passed on ci-runner with PREPUSH_REMOTE_RUN=<run id>; the run stands
# for the checks only when Forgejo says it is precommit-check.yml, a success, on exactly the pushed commit
remote_head="$(commit_paths "a change checked on ci-runner" -- scripts/z.sh)"
fj_check_run() { printf '{"id":%s,"workflow_id":"%s","status":"%s","commit_sha":"%s"}' "$1" "$2" "$3" "$4"; }
remote_push() { reuse_push "$rbase..$remote_head" PREPUSH_NO_REUSE=1 PREPUSH_REMOTE_URL="$fjremote" FORGEJO_API_TOKEN=envtoken \
  FJ_MAIN="[$fjgreen]" FJ_IMC="[$fjgreen]" PREPUSH_REMOTE_RUN=55 "$@"; }
check "a push whose remote run Forgejo confirms (precommit-check.yml, success, this commit) runs no check here" "0 8 1" \
  "$(remote_push FJ_RUN="$(fj_check_run 55 precommit-check.yml success "$remote_head")") $(grep -c 'passed on ci-runner' "$gate/err")"
vouches() { echo "$1 $(grep -c 'does not vouch' "$gate/err")"; }
check "...a failed run: every check runs here, said" "0 9 1" \
  "$(vouches "$(remote_push FJ_RUN="$(fj_check_run 55 precommit-check.yml failure "$remote_head")")")"
check "...another workflow's success: the same" "0 10 1" \
  "$(vouches "$(remote_push FJ_RUN="$(fj_check_run 55 main.yml success "$remote_head")")")"
check "...a success on another commit: the same" "0 11 1" \
  "$(vouches "$(remote_push FJ_RUN="$(fj_check_run 55 precommit-check.yml success "$rbase")")")"
check "...Forgejo not answering: the same" "0 12 1" "$(vouches "$(remote_push FJ_FAILS=1)")"
head -7 "$reuse/runs" > "$reuse/runs.kept" && mv "$reuse/runs.kept" "$reuse/runs"  # the counts below start from 7
scratch_git "$grepo" reset -q --hard "$docs_only"
# A hook that differs by one byte: a copy beside the helpers it sources, a comment appended.
mkdir -p "$reuse/hook/scripts/hooks"
cp "$REPO_ROOT/scripts/hooks-path.sh" "$REPO_ROOT/scripts/tool-conventions.sh" "$reuse/hook/scripts/"
cp "$REPO_ROOT/scripts/hooks/pre-push" "$reuse/hook/scripts/hooks/pre-push"; echo "# one more byte" >> "$reuse/hook/scripts/hooks/pre-push"
check "a hook that differs runs everything" "0 8" "$(HOOK="$reuse/hook/scripts/hooks/pre-push" reuse_push "$rbase..$docs_only")"
echo "# and a helper" >> "$reuse/hook/scripts/tool-conventions.sh"
check "...as does a helper it sources" "0 9" "$(HOOK="$reuse/hook/scripts/hooks/pre-push" reuse_push "$rbase..$docs_only")"
failing="$(commit_paths "a change whose checks fail" -- scripts/z.sh)"
check "a failed run stores nothing: the same tree runs again" "1 10 0 11" \
  "$(reuse_push "$rbase..$failing" STUB_RUNNER_EXIT=1) $(reuse_push "$rbase..$failing")"
# THE MUTANT: the same hook keyed without the pushed tree. It would answer a changed tree from the stored pass -- so
# "a changed tree runs everything" above is a check that fails on a hook that could skip a changed tree.
sed 's/^\(    tree=\)".*head_sha^{tree}")"/\1mutant/' "$REPO_ROOT/scripts/hooks/pre-push" > "$reuse/hook/scripts/hooks/pre-push"
cp "$REPO_ROOT/scripts/tool-conventions.sh" "$reuse/hook/scripts/"
check "the mutant differs from the hook in the tree's key only" "1" \
  "$(diff "$REPO_ROOT/scripts/hooks/pre-push" "$reuse/hook/scripts/hooks/pre-push" | grep -c '^>  *tree=mutant && ')"
HOOK="$reuse/hook/scripts/hooks/pre-push" reuse_push "$rbase..$docs_only" >/dev/null # the same paths as the next push
mutated="$(commit_paths "a tree the mutant has not seen" -- scripts/z.sh)"
check "...and it reuses a changed tree, which the check above refuses" "0 12" \
  "$(HOOK="$reuse/hook/scripts/hooks/pre-push" reuse_push "$rbase..$mutated")"
# CHECK FIRST: git opens the push's connection before the hook runs, and a long hook outlives it (the Forgejo tunnel
# restarting under load, 2026-10-23). A dry run of `<remote>/main..HEAD` reads that remote's runs, as the push to it
# will, so the push that follows reuses its pass and holds the connection for seconds.
scratch_git "$grepo" remote add fj "$fjremote"
scratch_git "$grepo" update-ref refs/remotes/fj/main "$rbase"
checkfirst="$(commit_paths "a change checked before its push" -- scripts/z.sh)"
fjgreens() { GH_FAILS=1 SECRETS_STATE=unlocked FJ_MAIN="[$fjgreen]" FJ_IMC="[$fjgreen]" FJ_REC="[$fjgreen]" "$@"; }
check "a dry run of fj/main..HEAD runs the checks, reading Forgejo's runs, not gh's" "0 13 0" \
  "$(fjgreens reuse_push "fj/main..$checkfirst") $(grep -c 'HTTP 502' "$gate/err")"
out="$(cd "$grepo" && printf 'refs/heads/main %s refs/heads/main %s\n' "$checkfirst" "$rbase" \
        | fjgreens env PREPUSH_NO_REUSE= PREPUSH_REUSE_DIR="$reuse/cache" PREPUSH_SKIP="${pathchecks/scripttests/}" \
          STUB_RUNS="$reuse/runs" PREPUSH_FLOOR_BASH="$gate/no-such-bash" bash "$REPO_ROOT/scripts/hooks/pre-push" fj "$fjremote" 2>&1)"; code=$?
check "...and the push to fj that follows reuses that pass: nothing runs again" "0 13 1" \
  "$code $(cat "$reuse/runs" | wc -l | tr -d ' ') $(printf '%s\n' "$out" | grep -c 'reused, not run again')"
# A hook that ran long says the connection git opened before it may be gone, and how to re-push cheaply; a quick one,
# and a dry run (no connection), do not.
long_push() { # <threshold seconds> <local sha> -> the warning's count
  (cd "$grepo" && printf 'refs/heads/main %s refs/heads/main %s\n' "$2" "$rbase" \
    | fjgreens env PREPUSH_LONG_SECONDS="$1" PREPUSH_SKIP="${pathchecks/scripttests/}" STUB_RUNS="$reuse/runs" \
      PREPUSH_FLOOR_BASH="$gate/no-such-bash" bash "$REPO_ROOT/scripts/hooks/pre-push" fj "$fjremote" 2>&1) \
    | grep -c 'with the push.s connection open .*run the same git push again.*scripts/hooks/pre-push --range fj/main..HEAD'; }
check "a push whose hook ran past PREPUSH_LONG_SECONDS says to re-push, and to check first next time" "1" "$(long_push 0 "$checkfirst")"
check "...a quick one does not" "0" "$(long_push 3600 "$checkfirst")"
check "...nor does a dry run, which holds no connection" "0 0" \
  "$(fjgreens reuse_push "fj/main..$checkfirst" PREPUSH_NO_REUSE=1 PREPUSH_LONG_SECONDS=0 | cut -d' ' -f1) $(grep -c 'run the same git push again' "$gate/err")"
rm -rf "$gate"

spec_summary
