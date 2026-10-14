#!/usr/bin/env bash
# pre-push's path selection: which checks a push's changed paths run, and that an unrelated push
# runs none of them. Run: bash scripts/hooks/pre-push-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
# Run as pre-push's own `hooktest` check, this inherits the GIT_DIR the hook was handed; the scratch
# repositories below would otherwise BE the repository being pushed (it once committed this spec's
# "Spec base" history onto a pushed branch). See scratch-git.sh.
. "$REPO_ROOT/scripts/scratch-git.sh"

# Every case below runs the hook as a push to main, which reads the gating workflows' verdicts first: a stub `gh`
# answers `gh run list --workflow W` from $GH_MAIN / $GH_IMC / $GH_REC (JSON, default none) through the hook's own
# --jq filter, so no case reads the real repository's runs.
ghstub="$(mktemp -d)"
cat > "$ghstub/gh" <<'STUB'
#!/usr/bin/env bash
[ -n "${GH_FAILS:-}" ] && { echo "HTTP 502: Bad Gateway" >&2; exit 1; }
[ -n "${GH_HANGS:-}" ] && exec sleep 30
# `gh run download RUN -p … -D DIR`: a convergence leg's served takes, as the judgements check fetches them
if [ "$1 $2" = "run download" ]; then
  while [ $# -gt 0 ]; do [ "$1" = -D ] && dir="$2"; shift; done
  mkdir -p "$dir/convergence-diagnostics-de-convergence/de-takes"; echo takes > "$dir/convergence-diagnostics-de-convergence/de-takes/de.json.gz"
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
trap 'rm -rf "$ghstub"' EXIT

printf '\033[36m▸\033[0m pre-push --plan\n'

# plan <path>... -> the selected checks, space-separated
plan() { printf '%s\n' "$@" | "$REPO_ROOT/scripts/hooks/pre-push" --plan | tr '\n' ' ' | sed 's/ $//'; }

check "production JS runs ESLint" "eslint" "$(plan web/src/main/assets/js/shared.js)"
check "a page spec's JS runs ESLint" "eslint" "$(plan web/src/page/resources/landing/hold-picker-panes.js)"
check "the ESLint config runs ESLint" "eslint" "$(plan eslint.config.js)"
check "a repo-local ESLint rule runs ESLint" "eslint" "$(plan eslint-rules/storage-access-in-try.js)"
check "a roster source runs the generated-artefact drift check" "drift" "$(plan data/pl/venues.json)"
check "a model file runs the drift check AND the sbt lints" "drift sbt" "$(plan common/src/main/scala/models/Cinema.scala)"
check "the iOS City list runs the drift check and the iOS compile" "drift ios" "$(plan ios/Kinowo/Models/City.swift)"
check "a recorded fixture runs the snapshot stamp check" "snapshot" \
  "$(plan test/resources/fixtures/08-06-2026/www.bilety24.pl/kino/organizator/x)"
check "any Scala source runs the sbt lints" "sbt" "$(plan worker/src/test/scala/tools/FooSpec.scala)"
check "a Grafana dashboard runs the dashboard tests and the metric-coverage lint" "dashboards sbt" \
  "$(plan infra/nix/files/monitoring/grafana/dashboards/apps/kinowo-http.json)"
check "a workflow runs actionlint" "actionlint" "$(plan .github/workflows/ci.yml)"
check "a shell script runs shellcheck" "shellcheck" "$(plan scripts/ci/order-seed.sh)"
check "the hook itself runs shellcheck and its own test" "hooktest shellcheck" "$(plan scripts/hooks/pre-push)"
check "an iOS view runs the iOS compile check" "ios" "$(plan ios/Kinowo/Views/FilmCard.swift)"
check "an Android-only change runs nothing" "" "$(plan android/app/src/main/java/pl/kinowo/ui/X.kt)"
check "docs run nothing" "" "$(plan docs/readme.md README.md)"
check "checks come out once each, in run order" "eslint snapshot actionlint shellcheck sbt" \
  "$(plan web/src/main/assets/js/a.js build.sbt scripts/x.sh .github/workflows/a.yml worker/src/main/scala/A.scala test/resources/fixtures/08-06-2026/y)"

check "a labels.tsv change runs the judgements lint" "judgements" "$(plan test/resources/fixtures/identity-unmatched/labels.tsv)"
check "an accepted move or a suspect baseline line runs it" "judgements" \
  "$(plan test/resources/fixtures/identity-takes/accepted.tsv test/resources/fixtures/identity-suspects/baseline.tsv)"
check "a hard-cluster expectation runs it" "judgements" "$(plan test/resources/fixtures/corpus/expected-hard-clusters-pl.txt)"
check "the lint itself runs it" "judgements" "$(plan scripts/ci/identity_judgements_lint.py)"
check "an unmatched-cluster capture does not" "" "$(plan test/resources/fixtures/identity-unmatched/pl.json.gz)"

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
check "an e2e spec compiles e2e alone" "e2e/Test/compile" "$(compiles e2e/src/test/scala/A.scala)"
check "common compiles every module's test configs" \
  "common/Test/compile testkit/Test/compile worker/Test/compile worker/It/compile web/Test/compile web/It/compile web/Page/compile e2e/Test/compile" \
  "$(compiles common/src/main/scala/A.scala)"
check "the build compiles every module's test configs" \
  "common/Test/compile testkit/Test/compile worker/Test/compile worker/It/compile web/Test/compile web/It/compile web/Page/compile e2e/Test/compile" \
  "$(compiles build.sbt)"
check "a dashboard alone compiles nothing extra" "" \
  "$(compiles infra/nix/files/monitoring/grafana/dashboards/apps/kinowo-http.json)"

# ── Running for real, over a scratch clone: what is judged, and what is not silently passed ──
printf '\033[36m▸\033[0m pre-push over a scratch repository\n'
if command -v shellcheck >/dev/null; then
  scratch="$(mktemp -d)"
  trap 'rm -rf "$scratch" "$ghstub"' EXIT
  repo="$scratch/repo"
  mkdir -p "$repo/scripts/hooks"
  scratch_repo "$repo"
  cp "$REPO_ROOT/scripts/hooks/pre-push" "$repo/scripts/hooks/pre-push"
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
  printf '#!/usr/bin/env bash\npwd -P > "%s/sbt-ran-in"\n' "$scratch" > "$scratch/bin/sbt"
  chmod +x "$scratch/bin/sbt"
  # The test-config compiles ride the lints' ONE sbt run, ahead of them.
  printf '#!/usr/bin/env bash\nprintf "%%s\\n" "$@" > "%s/sbt-args"\n' "$scratch" > "$scratch/bin/sbt"
  mkdir -p "$repo/worker/src/it/scala"; echo 'object B' > "$repo/worker/src/it/scala/B.scala"
  scratch_git "$repo" add worker; scratch_git "$repo" commit -qm "adds a worker it/ source"
  (cd "$repo" && PATH="$scratch/bin:$PATH" bash scripts/hooks/pre-push --range "HEAD~1..HEAD" >/dev/null 2>&1)
  check "a worker push compiles its Test and It configs in the lints' one sbt invocation" \
    "worker/Test/compile worker/It/compile e2e/Test/compile" \
    "$(grep -E '^[a-z0-9]+/[A-Za-z]+/compile$' "$scratch/sbt-args" 2>/dev/null | tr '\n' ' ' | sed 's/ $//')"
  check "...ahead of the lints, which still run" "1" "$(grep -c 'worker/Test/testOnly' "$scratch/sbt-args" 2>/dev/null)"
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
  for _ in $(seq 1 100); do pgrep -f 'fake/sbt-launch.jar' >/dev/null && break; sleep 0.1; done
  rm -f "$scratch/sbt-ran-in"
  out="$(cd "$repo" && PATH="$scratch/bin:$PATH" bash scripts/hooks/pre-push --range "$scala~1..$scala" 2>&1)"
  kill "$live_sbt" 2>/dev/null; wait "$live_sbt" 2>/dev/null
  ran_in="$(cat "$scratch/sbt-ran-in" 2>/dev/null)"
  check "with a live sbt in the checkout, the sbt check runs in a checkout of its own" \
    "elsewhere" "$([ -n "$ran_in" ] && [ "$ran_in" != "$(cd "$repo" && pwd -P)" ] && echo elsewhere || echo "in place: $ran_in")"
  check "...and says which process it is stepping around" "1" \
    "$(printf '%s\n' "$out" | grep -c "sbt (pid $live_sbt)")"

  # Pushed from a worktree, git hands the hook GIT_DIR for THAT worktree. Left exported into the
  # temporary checkout, every git a check runs there read the pushing worktree's HEAD and index:
  # the drift check's `git status` and the snapshot stamp's `git ls-files` judged the wrong tree.
  echo c > "$repo/c.txt"; scratch_git "$repo" add c.txt; scratch_git "$repo" commit -qm "moves HEAD past the pushed commit"
  printf '#!/usr/bin/env bash\ngit rev-parse HEAD > "%s/sbt-saw-head"\n' "$scratch" > "$scratch/bin/sbt"
  (cd "$repo" && GIT_DIR="$(pwd)/.git" PATH="$scratch/bin:$PATH" bash scripts/hooks/pre-push --range "$scala~1..$scala" >/dev/null 2>&1)
  check "with git's GIT_DIR exported, a check in the temporary checkout sees the pushed commit" \
    "$scala" "$(cat "$scratch/sbt-saw-head" 2>/dev/null)"

  # A labels.tsv change: the judgements lint reads the last convergence leg's served takes, and replays the takes the
  # push newly labels wrong at the PUSHED code (--replay-head), so a push that labels a take wrong and cures it lands.
  mkdir -p "$repo/scripts/ci" "$repo/test/resources/fixtures/identity-unmatched"
  printf '#!/usr/bin/env python3\nimport sys\nopen("%s/lint-args", "w").write(" ".join(sys.argv[1:]))\n' "$scratch" \
    > "$repo/scripts/ci/identity_judgements_lint.py"
  printf 'country\tvenue\trawTitle\tfilm\tverdict\tnote\n' > "$repo/test/resources/fixtures/identity-unmatched/labels.tsv"
  scratch_git "$repo" add -A; scratch_git "$repo" commit -qm "labels"
  printf 'de\tUnion\tDer Diplomat\ttmdb:1\twrong\t\n' >> "$repo/test/resources/fixtures/identity-unmatched/labels.tsv"
  scratch_git "$repo" commit -qam "labels a take wrong"
  (cd "$repo" && GH_IMC="[$(printf '{"databaseId":7,"status":"completed","conclusion":"success","url":"u"}')]" \
     PATH="$scratch/bin:$PATH" bash scripts/hooks/pre-push --range "HEAD~1..HEAD" >/dev/null 2>&1)
  check "a labels.tsv push replays its newly-wrong served takes at the pushed code" "1 1 1" \
    "$(grep -c -- '--served .*de.json.gz' "$scratch/lint-args") $(grep -c -- '--labels-before' "$scratch/lint-args") $(grep -c -- '--replay-head' "$scratch/lint-args")"

  # git's own stdin, with no origin/main to find a new branch's base from: said, not silently passed.
  out="$(cd "$repo" && printf 'refs/heads/x %s refs/heads/x %s\n' "$head" 0000000000000000000000000000000000000000 \
           | bash scripts/hooks/pre-push origin url 2>&1)"
  check "warns when origin/main is missing, instead of checking nothing in silence" "1" \
    "$(printf '%s\n' "$out" | grep -c 'origin/main')"
else
  printf '  (shellcheck not installed — scratch-repository cases skipped)\n'
fi

# ── RED BUILDS GATE A PUSH TO MAIN: only a commit declared a fix for a red run lands ───────────────
printf '\033[36m▸\033[0m pre-push: red builds gate a push to main\n'
gate="$(mktemp -d)"
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
pathchecks="judgements eslint drift snapshot dashboards sbt hooktest actionlint shellcheck ios"
push() { # <range> -> exit code; stderr in $gate/err
  (cd "$grepo" && PREPUSH_SKIP="${PREPUSH_SKIP:-} $pathchecks" bash "$REPO_ROOT/scripts/hooks/pre-push" --range "$1" >/dev/null 2>"$gate/err"); echo $?; }
check "every workflow green: the push lands" "0" "$(GH_MAIN="[$green]" GH_IMC="[$green]" GH_REC="[$green]" push "$gbase..$plain")"
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
start=$SECONDS
code="$(GH_HANGS=1 PREPUSH_GH_SECONDS=1 push "$gbase..$plain")"
check "gh hanging is cut off within its bound, and does not block" "0 yes" "$code $([ $((SECONDS - start)) -lt 10 ] && echo yes || echo no)"
out="$(cd "$grepo" && printf 'refs/heads/x %s refs/heads/x %s\n' "$plain" "$gbase" \
        | GH_IMC="[$red]" PREPUSH_SKIP="$pathchecks" bash "$REPO_ROOT/scripts/hooks/pre-push" origin url 2>&1)"; code=$?
check "a push to another branch than main is not gated" "0 0" "$code $(printf '%s\n' "$out" | grep -c 'RED on main')"
out="$(cd "$grepo" && printf 'refs/heads/main %s refs/heads/main %s\n' "$plain" "$gbase" \
        | GH_IMC="[$red]" PREPUSH_SKIP="$pathchecks" bash "$REPO_ROOT/scripts/hooks/pre-push" origin url 2>&1)"; code=$?
check "a real push to main reads git's stdin and is gated" "1" "$code"
rm -rf "$gate"

spec_summary
