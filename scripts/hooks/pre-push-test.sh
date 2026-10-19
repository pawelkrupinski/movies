#!/usr/bin/env bash
# pre-push's path selection: which checks a push's changed paths run, and that an unrelated push
# runs none of them. Run: bash scripts/hooks/pre-push-test.sh
set -uo pipefail
# The caller's PREPUSH_SKIP is the PUSH's: a push skipping `redgate` ran this test with the gate skipped in every scratch
# push below, and its red-gate cases failed (2026-10-14). Each case names what it skips itself.
unset PREPUSH_SKIP
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
export REPO_ROOT  # the quoted-heredoc stub gh below sources scratch-git.sh from it
. "$REPO_ROOT/scripts/shell-spec.sh"
# Run by pre-push's `scripttests` check (scripts/run-script-tests), this inherits the GIT_DIR the hook was handed; the scratch
# repositories below would otherwise BE the repository being pushed (it once committed this spec's
# "Spec base" history onto a pushed branch). See scratch-git.sh.
. "$REPO_ROOT/scripts/scratch-git.sh"

# Every case below runs the hook as a push to main, which reads the gating workflows' verdicts first: a stub `gh`
# answers `gh run list --workflow W` from $GH_MAIN / $GH_IMC / $GH_REC (JSON, default none) through the hook's own
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
  Main) runs="${GH_MAIN:-[]}" ;; "Identity model convergence") runs="${GH_IMC:-[]}" ;; "Record scrape fixtures") runs="${GH_REC:-[]}" ;;
  *) echo "unknown workflow $workflow" >&2; exit 1 ;;
esac
printf '%s' "$runs" | jq -r "$jqexpr"
STUB
chmod +x "$ghstub/gh"
export PATH="$ghstub:$PATH"
trap 'rm -rf "$testroot"' EXIT

printf '\033[36m▸\033[0m pre-push --plan\n'

# plan <path>... -> the selected checks, space-separated
plan() { printf '%s\n' "$@" | "$REPO_ROOT/scripts/hooks/pre-push" --plan | tr '\n' ' ' | sed 's/ $//'; }

check "production JS runs ESLint" "eslint" "$(plan web/src/main/assets/js/shared.js)"
check "a page spec's JS runs ESLint" "eslint" "$(plan web/src/page/resources/landing/hold-picker-panes.js)"
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
check "checks come out once each, in run order" "eslint snapshot scripttests actionlint shellcheck sbt" \
  "$(plan web/src/main/assets/js/a.js build.sbt scripts/x.sh .github/workflows/a.yml worker/src/main/scala/A.scala test/resources/fixtures/08-06-2026/y)"

check "a judgements change runs the judgements lint" "judgements" "$(plan test/resources/fixtures/identity-judgements.tsv)"
check "an expected match runs it" "judgements" "$(plan test/resources/fixtures/identity-unmatched/expected-matches.tsv)"
check "a hard-cluster expectation runs it" "judgements" "$(plan test/resources/fixtures/corpus/expected-hard-clusters-pl.txt)"
check "the lint itself runs it, the judgements' reader and the take files'" "judgements scripttests sbt judgements scripttests sbt judgements scripttests sbt" \
  "$(plan scripts/ci/identity_judgements_lint.py) $(plan scripts/ci/identity_judgements.py) $(plan scripts/ci/identity_takes.py)"
check "an unmatched-cluster capture does not" "" "$(plan test/resources/fixtures/identity-unmatched/pl.json.gz)"
# A file identity-slice or title-trace drops into old refs (their TOOL_FILES) compiles into each tool's floor ref.
check "a file identity-slice drops into old refs compiles the tools into their floors" "sbt oldrefs" \
  "$(plan worker/src/fixtures/scala/tools/MemoLookups.scala)"
check "...and one title-trace drops in" "sbt oldrefs" "$(plan worker/src/test/scala/services/identity/TitleTrace.scala)"
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
  check "$changed runs the self-test, dispatch-gate, literal-wait, raw-git and sleep lints" \
    "yes yes yes yes yes" "$(has "$out" worker:deploy.ScriptSelfTestsRunInCiSpec) $(has "$out" worker:deploy.ConvergenceDispatchGateSpec) $(has "$out" worker:tools.NoLiteralWaitBoundSpec) $(has "$out" worker:deploy.SharedMachineStateLintSpec) $(has "$out" worker:tools.NoSleepInTestsSpec)"
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
  # The test-config compiles ride the lints' ONE sbt run, ahead of them.
  printf '#!/usr/bin/env bash\nprintf "%%s\\n" "$@" > "%s/sbt-args"\n' "$scratch" > "$scratch/bin/sbt"; chmod +x "$scratch/bin/sbt"
  mkdir -p "$repo/worker/src/it/scala"; echo 'object B' > "$repo/worker/src/it/scala/B.scala"
  scratch_git "$repo" add worker; scratch_git "$repo" commit -qm "adds a worker it/ source"
  (cd "$repo" && PATH="$scratch/bin:$PATH" bash scripts/hooks/pre-push --range "HEAD~1..HEAD" >/dev/null 2>&1)
  check "a worker push compiles its Test and It configs in the lints' one sbt invocation" \
    "worker/Test/compile worker/It/compile e2e/Test/compile" \
    "$(grep -E '^[a-z0-9]+/[A-Za-z]+/compile$' "$scratch/sbt-args" 2>/dev/null | tr '\n' ' ' | sed 's/ $//')"
  check "...ahead of the lints, which still run" "1" "$(grep -c 'worker/Test/testOnly' "$scratch/sbt-args" 2>/dev/null)"
  # The specs reading the GitOps manifests run once infra/bin/fetch-gitops has put them in place, and are skipped, with a
  # warning, when it cannot (neither GitHub nor the local clone reachable).
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
  check "unreachable, skips them with a warning, and runs the other lints" "0 1 1" \
    "$(grep -c 'deploy.NodeCpuBudgetSpec' "$scratch/sbt-args" 2>/dev/null) $(printf '%s\n' "$out" | grep -c 'GitOps manifests unreachable') $(grep -c 'worker/Test/testOnly' "$scratch/sbt-args" 2>/dev/null)"
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
  for _ in $(seq 1 100); do grep -q 'fake/sbt-launch.jar' <<<"$(ps -o args= -p "$live_sbt" 2>/dev/null)" && break; sleep 0.1; done
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
  export KINOWO_SBT_WARM_STATE="$scratch/sbt-warm" KINOWO_IDENTITY_CACHE="$scratch/identity" KINOWO_JVM_GATE_FOLLOW_MAIN=0 \
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
  for _ in $(seq 1 100); do grep -q 'fake/sbt-launch.jar --detach' <<<"$(ps -o args= -p "$live_sbt" 2>/dev/null)" && break; sleep 0.1; done
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
check "a red nightly recording refuses it too" "1" "$(GH_REC="[$red]" push "$gbase..$plain")"
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
check "PREPUSH_SKIP=redgate bypasses it, said out loud" "0 1" \
  "$(GH_IMC="[$red]" PREPUSH_SKIP=redgate push "$gbase..$plain") $(grep -c 'redgate SKIPPED' "$gate/err")"
check "gh failing is said for each workflow, and does not block" "0 3" "$(GH_FAILS=1 push "$gbase..$plain") $(grep -c 'HTTP 502' "$gate/err")"
# gh hanging: the stub never ends on its own, so the hook gets past it only by its own bound cutting gh off. That
# mechanism is the proof -- each workflow's gh cut off (bounded's 124) and said so, no stub left running -- never
# elapsed wall time, which counts every process the hook starts: a "within 10 s" assertion failed whenever the machine
# ran the spec ~10x slower (a load of ~86: 2026-10-15, 2026-10-19). Nor does it count the stubs that started: that slow,
# a 1 s bound can end one before bash has read it. The outer bound only makes a hook that does hang a failure instead
# of a stuck spec.
hung="$gate/gh-hung"; : > "$hung"
code="$(cd "$grepo" && GH_HANGS="$hung" PREPUSH_GH_SECONDS=1 PREPUSH_SKIP="$pathchecks" \
         spec_bounded bash "$REPO_ROOT/scripts/hooks/pre-push" --range "$gbase..$plain" >/dev/null 2>"$gate/err"; echo $?)"
check "gh hanging is cut off by its bound for each workflow, said out loud, and does not block" "0 3 0" \
  "$code $(grep -c "gh did not answer .* within 1s: cut off" "$gate/err") $(spec_still_running "$hung")"
out="$(cd "$grepo" && printf 'refs/heads/x %s refs/heads/x %s\n' "$plain" "$gbase" \
        | GH_IMC="[$red]" PREPUSH_SKIP="$pathchecks" bash "$REPO_ROOT/scripts/hooks/pre-push" origin url 2>&1)"; code=$?
check "a push to another branch than main is not gated" "0 0" "$code $(printf '%s\n' "$out" | grep -c 'RED on main')"
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
printf '#!/usr/bin/env bash\necho ran >> "$STUB_OLDREFS_RAN"\necho "FAIL: tools compile into c0ffee" \nexit "${STUB_OLDREFS_EXIT:-0}"\n' \
  > "$grepo/scripts/identity-slice-old-ref-test.sh"
scratch_git "$grepo" add scripts && scratch_git "$grepo" commit -qm "the tools, stubbed"
# the check wants an sbt to compile with (none, as in CI's script-test container: skipped, warned); the stubbed test needs none
mkdir -p "$gate/sbt"; printf '#!/bin/sh\nexit 1\n' > "$gate/sbt/sbt"; chmod +x "$gate/sbt/sbt"
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
rm -rf "$gate"

spec_summary
