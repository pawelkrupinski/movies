#!/usr/bin/env bash
# shellcheck disable=SC2154 # the stubs' and the fixture's variables, set in pre-push-test-lib.sh
# pre-push's red gate -- only a commit declared a fix for a red run lands -- and the script tests and old-ref compile a
# push's paths select.
# Run: bash scripts/hooks/pre-push-gate-test.sh (the shared harness: pre-push-test-lib.sh)
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/pre-push-test-lib.sh"

# ── RED BUILDS GATE A PUSH TO MAIN: only a commit declared a fix for a red run lands ───────────────
printf '\033[36m▸\033[0m pre-push: red builds gate a push to main\n'
gate_setup
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
# A STRANDED TRAILER: a Fixes-Red: line in a paragraph of its own above Co-Authored-By: is no trailer to git, so neither the
# gate nor agentctl reads it (Main #2164, #2547: the fixes landed unlinked and fixers were sent at fixed reds). Refused on
# every push to main -- green, the commit tools-only, a dry run too; one in the closing block, or in prose mid-line, lands.
stray="$(commit_paths "a tools fix" "Fixes-Red: 38520439037" "Co-Authored-By: A <a@b.c>" -- scripts/z.sh)"
check "a Fixes-Red: line outside the closing trailer block is refused, main green, the commit tools-only" "1" \
  "$(all_green push "$fixing..$stray")"
check "...naming the commit, and where the line belongs" "1 1" \
  "$(grep -c "${stray:0:9}: a Fixes-Red: line outside the message's closing trailer block" "$gate/err") $(grep -c 'beside' "$gate/err")"
check "...with --fast-only too (a change about to be checked)" "1" \
  "$(cd "$grepo" && PREPUSH_SKIP="$pathchecks" bash "$REPO_ROOT/scripts/hooks/pre-push" --fast-only "$fixing..$stray" >/dev/null 2>&1; echo $?)"
check "PREPUSH_SKIP=trailers bypasses it, said out loud" "0 1" \
  "$(PREPUSH_SKIP=trailers all_green push "$fixing..$stray") $(grep -c 'trailers SKIPPED' "$gate/err")"
scratch_git "$grepo" reset -q --hard "$fixing"
stranded_ru="$(commit_paths "an unrelated tools change" "Red-Unrelated: 38520439037" "Co-Authored-By: A <a@b.c>" -- scripts/z.sh)"
check "a stranded Red-Unrelated: is refused as well" "1" "$(all_green push "$fixing..$stranded_ru")"
scratch_git "$grepo" reset -q --hard "$fixing"
trailed="$(commit_paths "a tools fix" $'Fixes-Red: 38520439037\nCo-Authored-By: A <a@b.c>' -- scripts/z.sh)"
check "a Fixes-Red: in the closing trailer block lands" "0" "$(all_green push "$fixing..$trailed")"
prose="$(commit_paths "a note on the gate" "It explains the Fixes-Red: trailer in prose, mid-line." -- scripts/z.sh)"
check "a Fixes-Red: mentioned mid-line is no trailer line: the push lands" "0" "$(all_green push "$trailed..$prose")"
scratch_git "$grepo" reset -q --hard "$plain"
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
# A CHECK GATES ONLY ONCE IT HAS BEEN GREEN: a red run whose failed jobs are all unproven in .github/gating-checks.tsv
# (scripts/ci/gating_checks.py, its jobs read through `gh run view`, stubbed here) reports and holds no push
mkdir -p "$grepo/scripts/ci" "$grepo/.github"
cp "$REPO_ROOT/scripts/ci/gating_checks.py" "$grepo/scripts/ci/"
printf 'kind\tworkflow\tname\tproven\tnote\njob\tmain.yml\ttest (a)\t5\njob\tmain.yml\tci\taggregate\njob\tmain.yml\tnew check\tunproven\tborn today; owner: x\n' > "$grepo/.github/gating-checks.tsv"
unproven='{"jobs":[{"name":"new check","conclusion":"failure"},{"name":"ci","conclusion":"failure"},{"name":"test (a)","conclusion":"success"}]}'
proven='{"jobs":[{"name":"test (a)","conclusion":"failure"},{"name":"new check","conclusion":"failure"}]}'
check "a Main red only in an unproven check and its aggregate holds no push, said" "0 1" \
  "$(STUB_JOBS="$unproven" GH_MAIN="[$red]" push "$gbase..$plain") $(grep -c 'red only in checks no green run has proven yet' "$gate/err")"
check "...a red in a proven check refuses it as ever" "1" "$(STUB_JOBS="$proven" GH_MAIN="[$red]" push "$gbase..$plain")"
check "...and so does a run whose jobs cannot be read" "1" "$(STUB_JOBS="not json" GH_MAIN="[$red]" push "$gbase..$plain")"
rm -rf "$grepo/scripts/ci/gating_checks.py" "$grepo/.github/gating-checks.tsv"
# THE GATE FAILS CLOSED (2026-10-27 08:05Z: a gate skipped for want of a token let agentctl's waiting processor push onto
# a red Main): unread -- an API error, no answer -- refuses a production push; a tools-only one still lands.
check "gh failing refuses a production push, naming the commit it refuses" "1 1" \
  "$(GH_FAILS=1 push "$gbase..$plain") $(grep -c "production commits land only on a green.*${plain:0:9}" "$gate/err")"
check "...while a tools-only push lands" "0" "$(GH_FAILS=1 push "$plain..$tools")"
out="$(cd "$grepo" && printf 'refs/heads/x %s refs/heads/x %s\n' "$plain" "$gbase" \
        | GH_IMC="[$red]" PREPUSH_SKIP="$pathchecks" bash "$REPO_ROOT/scripts/hooks/pre-push" origin url 2>&1)"; code=$?
check "a push to another branch than main is not gated" "0 0" "$code $(printf '%s\n' "$out" | grep -c 'RED on main')"
out="$(cd "$grepo" && printf 'refs/heads/m %s refs/heads/measure/u/abc-1 %s\n' "$plain" 0000000000000000000000000000000000000000 \
        | GH_IMC="[$red]" bash "$REPO_ROOT/scripts/hooks/pre-push" origin url 2>&1)"; code=$?
check "a measurement's measure/ branch alone is checked for nothing: it never lands" "0 1" \
  "$code $(printf '%s\n' "$out" | grep -c "measure/ or check/ branch: a CI run's ref")"
out="$(cd "$grepo" && printf 'refs/heads/m %s refs/heads/check/20261027-1200-movies-wt-x-3 %s\n' "$plain" 0000000000000000000000000000000000000000 \
        | GH_IMC="[$red]" bash "$REPO_ROOT/scripts/hooks/pre-push" origin url 2>&1)"; code=$?
check "...nor is a remote check's check/ branch: its run checks it on ci-runner" "0 1" \
  "$code $(printf '%s\n' "$out" | grep -c "measure/ or check/ branch: a CI run's ref")"
out="$(cd "$grepo" && printf 'refs/heads/m %s refs/heads/land/20261028-1200-movies-wt-x-3 %s\n' "$plain" 0000000000000000000000000000000000000000 \
        | GH_IMC="[$red]" bash "$REPO_ROOT/scripts/hooks/pre-push" origin url 2>&1)"; code=$?
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


spec_summary
