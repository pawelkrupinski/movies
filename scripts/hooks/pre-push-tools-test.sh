#!/usr/bin/env bash
# shellcheck disable=SC2154 # the stubs' and the fixture's variables, set in pre-push-test-lib.sh
# pre-push: the known-red baseline, a check's tool missing (it blocks, with the fix) and a check making no progress.
# Run: bash scripts/hooks/pre-push-tools-test.sh (the shared harness: pre-push-test-lib.sh)
. "$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/pre-push-test-lib.sh"

gate_setup
gate_fixture
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
# ...wherever the host's mktemp puts the hook's temp files (2026-10-31: GNU mktemp honours $TMPDIR, on a GitHub runner
# /home/runner/work/_temp/...; the check's `full log: <tmp>/oldrefs.log` line differed run to run and no Linux land's
# known red held, while macOS's mktemp, ignoring $TMPDIR, passed). A GNU-like mktemp and a $TMPDIR outside /tmp and
# /var/folders, so this case fails the same way on a Mac.
mkdir -p "$gate/gnumktemp"; realmktemp="$(command -v mktemp)"
printf '#!/bin/sh\n[ "$*" = -d ] && exec "%s" -d "${TMPDIR%%/}/tmp.XXXXXXXXXX"\nexec "%s" "$@"\n' "$realmktemp" "$realmktemp" \
  > "$gate/gnumktemp/mktemp"; chmod +x "$gate/gnumktemp/mktemp"
mkdir -p "${XDG_CACHE_HOME:-$HOME/.cache}"; hosttmp="$(mktemp -d "${XDG_CACHE_HOME:-$HOME/.cache}/kinowo-hook-tmpdir.XXXXXX")"
gnu=(PATH="$gate/gnumktemp:$gate/sbt:$PATH" TMPDIR="$hosttmp")
cp "$known/$oldrefs_base" "$gate/baseline.kept"
{ echo "pre-push baseline $oldrefs_base"; oldrefs_push tools/A.scala STUB_OLDREFS_EXIT=1 "${gnu[@]}" >/dev/null; cat "$gate/err"; } \
  > "$known/x" && mv "$known/x" "$known/$oldrefs_base"
check "...its temp files under a \$TMPDIR the host's mktemp honours (GNU): the baseline names them there" "1" \
  "$(grep -c "full log: $hosttmp/tmp\." "$known/$oldrefs_base")"
check "...and the same failure, its temp paths differing, is still said, not blocking" "0 1 1" \
  "$(oldrefs_push tools/A.scala STUB_OLDREFS_EXIT=1 "${gnu[@]}" PREPUSH_BASELINE_DIR="$known") $(grep -c 'KNOWN RED: oldrefs' "$gate/err")"
rm -rf "$hosttmp"; mv "$gate/baseline.kept" "$known/$oldrefs_base"
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

spec_summary
