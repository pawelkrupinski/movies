#!/usr/bin/env bash
# shellcheck disable=SC2154 # the stubs' and the fixture's variables, set in pre-push-test-lib.sh
# pre-push's path selection: which checks a push's changed paths run, and that an unrelated push
# runs none of them. Run: bash scripts/hooks/pre-push-test.sh
# (one of four, run side by side: pre-push-test-lib.sh holds the shared harness and names the others)
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/pre-push-test-lib.sh"

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

# ── THE IDENTITY DRIFT CHECK: a Fixes-Red push touching a drift path is judged on the red's drift rows, on CI ──────────────
# A fake scripts/identity-drift-ci notes its arguments; DRIFT_RC is its exit.
printf '\033[36m▸\033[0m pre-push identity drift check\n'
mkdir -p "$area/.github/actions/changed-paths"
cp "$REPO_ROOT/.github/actions/changed-paths/matches.sh" "$area/.github/actions/changed-paths/matches.sh"
cp "$REPO_ROOT/.github/identity-drift-paths.txt" "$area/.github/identity-drift-paths.txt"
cp "$REPO_ROOT/.github/identity-drift-gate-paths.txt" "$area/.github/identity-drift-gate-paths.txt"
cp "$REPO_ROOT/.github/allow-lists.txt" "$area/.github/allow-lists.txt"
mkdir -p "$area/scripts/ci" "$area/test/resources/fixtures"
cp "$REPO_ROOT/scripts/ci/allow_list_ratchet.py" "$area/scripts/ci/allow_list_ratchet.py"
printf '# reviewed flips\ncountry\tvenue\trawTitle\treason\n' > "$area/test/resources/fixtures/identity-flips-allow.tsv"
cat > "$area/scripts/identity-drift-ci" <<'STUB'
#!/usr/bin/env bash
echo "identity-drift-ci $*" >> "$AREA_CALLS"
[ "${DRIFT_RC:-0}" = 1 ] && echo "DRIFT RED: takes-drift-2 (germany)"
exit "${DRIFT_RC:-0}"
STUB
chmod +x "$area/scripts/identity-drift-ci"
scratch_git "$area" add -A; scratch_git "$area" commit -qm "drift tooling"
drift_push() { # <path> <message> [env...]: commits <path> with <message>, runs the hook over it; prints the exit code
  local path="$1" message="$2"; shift 2
  mkdir -p "$area/$(dirname "$path")"; echo "x $RANDOM" >> "$area/$path"
  scratch_git "$area" add -A; scratch_git "$area" commit -qm "$message"
  : > "$AREA_CALLS"
  # GITHUB_ACTIONS unset: in a CI job (this test in agentctl's remote checks) the check is skipped, said -- tested below
  (cd "$area" && env -u GITHUB_ACTIONS PATH="$areabin:$PATH" PREPUSH_SKIP="eslint lintspecs" "$@" \
     bash scripts/hooks/pre-push --range HEAD~1..HEAD > "$testroot/area-out" 2>&1); echo $?
}
code="$(drift_push identity/src/main/scala/services/identity/Resolver.scala $'a fix\n\nFixes-Red: 3726' GITHUB_ACTIONS=true)"
check "in a CI job the drift check dispatches nothing, and says main's run judges the fix" "0 0 1" \
  "$code $(calls identity-drift-ci) $(grep -c 'not run from a CI job' "$testroot/area-out")"
code="$(drift_push identity/src/main/scala/services/identity/Resolver.scala $'a fix\n\nFixes-Red: #2517; 3726')"
check "a Fixes-Red push touching a drift path runs the red's drift rows, each claimed run once, at the pushed commit" "0 1 1" \
  "$code $(calls "identity-drift-ci --head $(scratch_git "$area" rev-parse HEAD) --for-red 2517") $(calls 'for-red 3726')"
code="$(drift_push identity/src/main/scala/services/identity/Resolver.scala $'a fix\n\nFixes-Red: 3726' DRIFT_RC=1)"
check "...a red drift row refuses the push, naming the check" "1 1" "$code $(grep -c 'blocked by: identitydrift' "$testroot/area-out")"
code="$(drift_push identity/src/main/scala/services/identity/Resolver.scala $'a fix\n\nFixes-Red: 3726' DRIFT_RC=2)"
check "...a run that could not be made warns and passes" "0 1" "$code $(grep -c 'no verdict for Fixes-Red: 3726' "$testroot/area-out")"
code="$(drift_push identity/src/main/scala/services/identity/Resolver.scala $'a speed-up\n\nFixes-Red: 3726\nRed-Unrelated: CI speed-up')"
check "a Red-Unrelated commit claims no fix: no drift run" "0 0" "$code $(calls identity-drift-ci)"
code="$(drift_push identity/src/main/scala/services/identity/Resolver.scala 'no claim')"
check "a push claiming no red runs no drift check" "0 0" "$code $(calls identity-drift-ci)"
code="$(drift_push docs/fix.md $'a doc\n\nFixes-Red: 3726')"
check "a Fixes-Red push touching no drift path runs none" "0 0" "$code $(calls identity-drift-ci)"
# A CHECK'S BIRTH LANDS GREEN: the gate's own files, or New-Check:, run every drift row against main's
code="$(drift_push worker/src/test/scala/scripts/IdentityDrift.scala 'the harness, tightened')"
check "a push touching the gate itself runs every drift row against main's newest run" "0 1" \
  "$code $(calls "identity-drift-ci --head $(scratch_git "$area" rev-parse HEAD) --against main$")"
code="$(drift_push identity/src/main/scala/services/identity/Resolver.scala $'a new check\n\nNew-Check: settled takes')"
check "...and so does a New-Check: push touching a drift path" "0 1" "$code $(calls 'against main')"
code="$(drift_push worker/src/test/scala/scripts/IdentityDrift.scala 'the harness, tightened' DRIFT_RC=1)"
check "...a row worse than main's refuses it, saying how a check lands green" "1 1" \
  "$code $(grep -c 'a new or tightened check lands green' "$testroot/area-out")"
# THE RATCHET: an allow-list row lands only with its check's birth
code="$(drift_push docs/x.md 'docs')"
printf 'de\t*\tCAMOUFLAGE\tlooks intended\n' >> "$area/test/resources/fixtures/identity-flips-allow.tsv"
scratch_git "$area" commit -qam "list a flip"
: > "$AREA_CALLS"
code="$(cd "$area" && env PATH="$areabin:$PATH" PREPUSH_SKIP="eslint lintspecs identitydrift" bash scripts/hooks/pre-push --range HEAD~1..HEAD > "$testroot/area-out" 2>&1; echo $?)"
check "a row added to a registered allow-list outside a check's birth is refused" "1 1 1" \
  "$code $(grep -c 'blocked by: allowlists' "$testroot/area-out") $(grep -c 'an allow-list only shrinks' "$testroot/area-out")"
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

spec_summary
