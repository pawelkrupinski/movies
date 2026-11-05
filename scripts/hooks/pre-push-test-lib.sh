#!/usr/bin/env bash
# shellcheck disable=SC2034 # the fixture's variables are read by the specs that source it
# THE SHARED HARNESS of the pre-push hook's self-tests (sourced, never run on its own): the stub gh,
# the scratch root, and the red gate's fixture. The hook's spec was one ~1,070-line file that ran ~2-5 min on a laptop
# and past run-script-tests' 600 s on a loaded CI host (precommit-check run 3813, load 175, 2026-10-29): its sections
# are four specs now, run side by side with the same assertions -- pre-push-test.sh (the plans, the areas, the drift
# check, hooksPath), pre-push-gate-test.sh (the red gate, script tests, old refs), pre-push-tools-test.sh (the
# known-red baseline, a missing tool, a stall) and pre-push-reuse-test.sh (the reuse) -- each sourcing this file.
set -uo pipefail
# The caller's PREPUSH_SKIP is the PUSH's: a push skipping `redgate` ran this test with the gate skipped in every scratch
# push below, and its red-gate cases failed (2026-10-14). Each case names what it skips itself.
unset PREPUSH_SKIP
# ...and so are the landing check's shares (precommit-check.yml): run inside its `rest` job, PREPUSH_ELSEWHERE=page left
# every scratch push's page set to "the other job", and the page cases failed (landing check run 4059).
unset PREPUSH_ONLY PREPUSH_ELSEWHERE PREPUSH_HEAVY_ROWS
# Every case re-pushes the same scratch trees under different stubs: a stored pass (the hook's REUSE) would answer the
# next case instead of its checks. Reuse is off here, and on only in its own cases, with a cache of their own.
export PREPUSH_NO_REUSE=1
unset PREPUSH_REUSE_DIR
# run by the hook's scripttests check, it inherits the hook's stall marks (stall-watch.sh): its own stalls are no stall of
# that check
unset KINOWO_STALL_FILE KINOWO_PROGRESS_FILE
unset PREPUSH_GATE_ATTEMPTS
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
# `gh run view RUN --json jobs`: a run's jobs, as gating_checks.py report-only reads them
# ...or run RUN's own jobs, $STUB_JOBS_DIR/RUN.json when there is one, through the caller's --jq (the red gate's episode)
if [ "$1 $2" = "run view" ]; then
  rid="$3" jqexpr=""; while [ $# -gt 0 ]; do [ "$1" = --jq ] && jqexpr="$2"; shift; done
  if [ -n "${STUB_JOBS_DIR:-}" ] && [ -f "$STUB_JOBS_DIR/$rid.json" ]; then
    if [ -n "$jqexpr" ]; then jq -r "$jqexpr" "$STUB_JOBS_DIR/$rid.json"; else cat "$STUB_JOBS_DIR/$rid.json"; fi
    exit
  fi
  [ -n "${STUB_JOBS_DIR:-}" ] && { echo "HTTP 404: no run $rid" >&2; exit 1; }
  printf '%s' "${STUB_JOBS:-}"; exit 0
fi
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
workflow="" jqexpr="" sub="$1"
while [ $# -gt 0 ]; do
  case "$1" in --workflow) workflow="$2"; shift ;; --jq) jqexpr="$2"; shift ;; esac; shift
done
# `gh api repos/{owner}/{repo}/actions/runs/RUN --jq …`: one run, $STUB_RUN (JSON), through the hook's own filter
[ "$sub" = api ] && { printf '%s' "${STUB_RUN:-{\}}" | jq -r "$jqexpr"; exit; }
case "$workflow" in
  Main) runs="${GH_MAIN:-[]}" ;; "Identity model convergence") runs="${GH_IMC:-[]}" ;; "Verify scrape recording") runs="${GH_REC:-[]}" ;;
  *) echo "unknown workflow $workflow" >&2; exit 1 ;;
esac
printf '%s' "$runs" | jq -r "$jqexpr"
STUB
chmod +x "$ghstub/gh"
export PATH="$ghstub:$PATH"
trap 'rm -rf "$testroot"' EXIT

# THE RED GATE'S FIXTURE: the scratch repository the hook is pushed from, its commits and helpers, and the run JSON the
# stub answers with.
gate_setup() {
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
run_json() { # <run id> <status> <conclusion> <url tail> [<run number>]
  printf '{"databaseId":%s,"number":%s,"status":"%s","conclusion":"%s","url":"https://x/%s"}' "$1" "${5:-null}" "$2" "$3" "$1"; }
red="$(run_json 38520439037 completed failure 38520439037)"
green="$(run_json 38487614044 completed success 38487614044)"
cancelled="$(run_json 38470049143 completed cancelled 38470049143)"
running="$(run_json 38529260149 in_progress "" 38529260149)"
# Only the gate is under test here: the path checks the pushed paths select are skipped (the cases above cover them),
# driftguards too: the `fixing` commit's Fixes-Red: trailer selects it whatever the pushed paths.
pathchecks="judgements eslint drift driftguards snapshot dashboards sbt scripttests actionlint shellcheck ios newchecks"
push() { # <range> -> exit code; stderr in $gate/err
  (cd "$grepo" && PREPUSH_SKIP="${PREPUSH_SKIP:-} $pathchecks" bash "$REPO_ROOT/scripts/hooks/pre-push" --range "$1" >/dev/null 2>"$gate/err"); echo $?; }
  all_green() { GH_MAIN="[$green]" GH_IMC="[$green]" GH_REC="[$green]" "$@"; }
}

# The repository as pre-push-gate-test.sh's cases leave it for the tools and reuse specs: a hooks directory and a foreign
# core.hooksPath, the script-test runner with --linux, the tools' TOOL_FILES and their old-ref test stubbed, sbt and java
# stubbed -- `oldrefs_base` that commit, and `oldrefs_push` a push changing one path on it.
gate_fixture() {
  mkdir -p "$grepo/scripts/hooks"
  scratch_git "$grepo" config core.hooksPath "$gate/foreign-hooks"
printf '#!/usr/bin/env bash\necho "$*" >> "$STUB_RUNNER_ARGS"\ncase "$*" in *--linux*) [ -z "${STUB_NO_DOCKER:-}" ] || { echo "run-script-tests: --linux needs a running Docker" >&2; exit 2; }; exit "${STUB_LINUX_EXIT:-0}" ;; esac; exit 0\n' > "$grepo/scripts/run-script-tests"
  chmod +x "$grepo/scripts/run-script-tests"
  scratch_git "$grepo" add scripts/run-script-tests && scratch_git "$grepo" commit -qm "a runner with --linux"
printf '#!/usr/bin/env bash\nTOOL_FILES=(tools/A.scala\n  tools/B.scala)\n' > "$grepo/scripts/identity-slice"
printf '#!/usr/bin/env bash\nTOOL_FILES=(tools/T.scala)\n' > "$grepo/scripts/title-trace"
printf '#!/usr/bin/env bash\necho ran >> "$STUB_OLDREFS_RAN"\necho "${STUB_OLDREFS_LINE:-FAIL: tools compile into c0ffee}" \nexit "${STUB_OLDREFS_EXIT:-0}"\n' \
  > "$grepo/scripts/identity-slice-old-ref-test.sh"
scratch_git "$grepo" add scripts && scratch_git "$grepo" commit -qm "the tools, stubbed"
# the check wants an sbt and a java to compile with (CI's script-test container has neither: stubbed); the stubbed test needs neither
mkdir -p "$gate/sbt"; printf '#!/bin/sh\nexit 1\n' > "$gate/sbt/sbt"; printf '#!/bin/sh\nexit 0\n' > "$gate/sbt/java"
chmod +x "$gate/sbt/sbt" "$gate/sbt/java"
  oldrefs_base="$(scratch_git "$grepo" rev-parse HEAD)"
}
oldrefs_push() { # <path> [<env>...] -> exit code, then how often the old-ref test ran; stderr in $gate/err
  local path=$1 head; shift
  scratch_git "$grepo" reset -q --hard "$oldrefs_base"; head="$(commit_paths "change $path" -- "$path")"; rm -f "$gate/ran"
  (cd "$grepo" && all_green env PATH="$gate/sbt:$PATH" PREPUSH_SKIP="$pathchecks" STUB_OLDREFS_RAN="$gate/ran" "$@" \
    bash "$REPO_ROOT/scripts/hooks/pre-push" --range "$oldrefs_base..$head" >/dev/null 2>"$gate/err"); echo "$? $(cat "$gate/ran" 2>/dev/null | wc -l | tr -d ' ')"; }
