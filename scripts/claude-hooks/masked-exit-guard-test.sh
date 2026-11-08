#!/usr/bin/env bash
# Self-test for scripts/claude-hooks/masked-exit-guard: the hook's JSON fed to it the way Claude Code does. A test
# runner piped into a filter is denied (exit 2), because the pipe exits with the filter's status; a run written to a
# log, a pipe that keeps the runner's status, and a filter of anything else are allowed. The first case is the shape
# agents ran on 2026-11-08 and read as "sbt printed FAILED yet exited 0".
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
guard="$REPO_ROOT/scripts/claude-hooks/masked-exit-guard"
fails=0
check() { # <what> <expected> <actual>
  if [ "$2" = "$3" ]; then printf '  \033[32m✓\033[0m %s\n' "$1"
  else printf '  \033[31m✗\033[0m %s\n      expected: %s\n      actual:   %s\n' "$1" "$2" "$3"; fails=$((fails + 1)); fi
}
verdict() { # <command> -> the guard's exit code for a Bash call running it
  python3 -c 'import json, sys; print(json.dumps({"tool_name": "Bash", "tool_input": {"command": sys.argv[1]}}))' "$1" |
    python3 "$guard" 2>/dev/null
  echo $?
}

# The premise: a pipeline's status is its last command's, so a failed run piped into a filter exits 0.
check "a failing command piped into head exits 0 (why the guard exists)" 0 "$(set +o pipefail; (exit 1) | head -1 >/dev/null; echo $?)"

printf '\033[36m▸\033[0m denied\n'
check "sbt-warm piped into grep and head (2026-11-08)" 2 \
  "$(verdict 'cd /x && eval "$(~/bin/agentctl env --print)" >/dev/null 2>&1; timeout 1500 scripts/sbt-warm "worker/testOnly deploy.*" 2>&1 | grep -E "FAIL|Tests:" | head -30')"
check "sbt-warm testUnit piped into tail" 2 "$(verdict 'scripts/sbt-warm testUnit 2>&1 | tail -40')"
check "a plain sbt batch piped into tail" 2 "$(verdict 'sbt -batch "worker/Test/test" | tail')"
check "sbt-warm by absolute path, with an env assignment, through |&" 2 \
  "$(verdict 'KINOWO_TEST_FORKS=2 /Users/p/movies/scripts/sbt-warm "web/Test/test" |& tail -5')"
check "sbt-warm in a subshell piped out" 2 "$(verdict '(cd /x && scripts/sbt-warm compile) | tail -3')"
check "sbt-warm in a brace group piped out" 2 "$(verdict '{ scripts/sbt-warm compile; } 2>&1 | tail -3')"
check "the script tests piped into grep" 2 "$(verdict 'scripts/run-script-tests scripts/ci | grep -v ok')"
check "it-local.sh piped into tee" 2 "$(verdict 'scripts/it-local.sh itAll | tee "$log"')"
check "a runner on a later line of a multi-line command" 2 "$(verdict $'cd /x\nscripts/sbt-warm compile 2>&1 | tail -2')"
check "a runner after a heredoc" 2 "$(verdict $'cat > "$out" <<\'EOF\'\nhello | world\nEOF\nscripts/sbt-warm compile | tail')"
check "the denial says why and what to do instead" 1 \
  "$(python3 -c 'import json; print(json.dumps({"tool_input": {"command": "scripts/sbt-warm compile | tail"}}))' |
    python3 "$guard" 2>&1 | grep -c 'LAST command.*log')"

printf '\033[36m▸\033[0m allowed\n'
check "a run written to a log, its status echoed, the log grepped" 0 \
  "$(verdict 'scripts/sbt-warm testUnit > "$log" 2>&1; echo "exit=$?"; grep -E "FAILED|Tests:" "$log" | tail -20')"
check "a pipe under set -o pipefail" 0 "$(verdict 'set -o pipefail; scripts/sbt-warm compile 2>&1 | tail -3')"
check "a pipe whose status is read from PIPESTATUS" 0 "$(verdict 'scripts/sbt-warm compile | tail -3; echo "${PIPESTATUS[0]}"')"
check "a runner whose group ends in another command, piped out" 0 "$(verdict '(scripts/sbt-warm compile; echo done) | tail -1')"
check "a runner checked with || before an unrelated pipe" 0 "$(verdict 'scripts/sbt-warm compile || exit 1; ls -l | head')"
check "a runner whose output is the LAST stage" 0 "$(verdict 'echo compile | scripts/sbt-warm compile')"
check "a filter of a file that merely mentions sbt" 0 "$(verdict 'grep -n sbt-warm scripts/hooks/pre-push | head')"
check "sbt named as an argument, not run" 0 "$(verdict 'which sbt | head -1')"
check "a runner inside a heredoc body is not run by the shell reading it" 0 \
  "$(verdict $'cat > "$out" <<\'EOF\'\nscripts/sbt-warm compile | tail\nEOF')"
check "input that is not JSON never blocks" 0 "$(echo 'not json' | python3 "$guard" 2>/dev/null; echo $?)"
check "a command shlex cannot read never blocks" 0 "$(verdict 'echo "unterminated | scripts/sbt-warm x | tail')"

[ "$fails" = 0 ] && printf '\033[32mall passed\033[0m\n' || { printf '\033[31m%s failed\033[0m\n' "$fails"; exit 1; }
