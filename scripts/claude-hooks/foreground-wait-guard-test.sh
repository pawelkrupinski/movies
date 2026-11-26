#!/usr/bin/env bash
# Self-test for scripts/claude-hooks/foreground-wait-guard: the hook's JSON fed to it the way Claude Code does. A long
# wait (wait-for, ci-wait, agentctl wait) without run_in_background is denied (exit 2); the same in the background, a
# wait bounded to a minute, --help, and a command that only names the tool are allowed.
# The commands under test are literal text for the guard to read, never run here: nothing in them should expand.
# shellcheck disable=SC2016,SC2088
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
guard="$REPO_ROOT/scripts/claude-hooks/foreground-wait-guard"
fails=0
check() { # <what> <expected> <actual>
  if [ "$2" = "$3" ]; then printf '  \033[32m✓\033[0m %s\n' "$1"
  else printf '  \033[31m✗\033[0m %s\n      expected: %s\n      actual:   %s\n' "$1" "$2" "$3"; fails=$((fails + 1)); fi
}
verdict() { # <command> [background] -> the guard's exit code for a Bash call running it
  python3 -c 'import json, sys
tool_input = {"command": sys.argv[1]}
if len(sys.argv) > 2: tool_input["run_in_background"] = True
print(json.dumps({"tool_name": "Bash", "tool_input": tool_input}))' "$@" | python3 "$guard" 2>/dev/null
  echo $?
}

printf '\033[36m▸\033[0m denied\n'
check "wait-for in the foreground with its 170 s slice (the shape of 2026-11)" 2 \
  "$(verdict 'scripts/wait-for "$log" "exit=" --timeout 170')"
check "wait-for with no timeout (default 2 h)" 2 "$(verdict 'cd /x && scripts/wait-for /tmp/l "done"')"
check "wait-for by absolute path after an env prefix" 2 "$(verdict 'FOO=1 /Users/p/movies/scripts/wait-for f x --poll 5')"
check "ci-wait, its bare --timeout in minutes" 2 "$(verdict 'scripts/ci-wait 123 --timeout 5')"
check "agentctl wait run" 2 "$(verdict '~/bin/agentctl wait run 4242 --until landable')"
check "agentctl wait file with a long timeout" 2 "$(verdict 'agentctl wait file /x --timeout 600')"
check "a wait after a timeout prefix" 2 "$(verdict "timeout 900 scripts/wait-for f x; echo done")"
check "a wait after a heredoc" 2 "$(verdict $'cat > f <<\'EOF\'\nscripts/wait-for is text here\nEOF\nscripts/wait-for f x')"

printf '\033[36m▸\033[0m allowed\n'
check "the same wait in the background" 0 "$(verdict 'scripts/wait-for "$log" "exit=" --timeout 7200' background)"
check "agentctl wait in the background" 0 "$(verdict '~/bin/agentctl wait run 4242' background)"
check "wait-for bounded to a minute" 0 "$(verdict 'scripts/wait-for f x --timeout 60')"
check "ci-wait bounded to 30 s" 0 "$(verdict 'scripts/ci-wait 1 --timeout 30s')"
check "wait-for bounded by a timeout prefix of a minute (its own default 2 h)" 0 "$(verdict 'timeout 45 scripts/wait-for f x')"
check "...with timeout's own options before its duration" 0 "$(verdict 'gtimeout -k 5 30s scripts/ci-wait 1')"
check "...whose -k duration is not the bound" 2 "$(verdict 'timeout -k 5 900 scripts/wait-for f x')"
check "...but a prefix's bound holds only its own command" 2 "$(verdict 'timeout 30 true; scripts/wait-for f x')"
check "...a duration of 0, which disables timeout, bounds nothing" 2 "$(verdict 'timeout 0 scripts/wait-for f x')"
check "...a named signal before the duration is no command" 0 "$(verdict 'timeout -s KILL 30 scripts/wait-for f x')"
check "...nor does it hide an unbounded one" 2 "$(verdict 'timeout --signal TERM 900 scripts/wait-for f x')"
check "...a day's duration is a day" 2 "$(verdict 'timeout 1d scripts/ci-wait 1')"
check "ci-wait bounded with --timeout=30s" 0 "$(verdict 'scripts/ci-wait 1 --timeout=30s')"
check "ci-wait with --timeout=5 (minutes) is still denied" 2 "$(verdict 'scripts/ci-wait 1 --timeout=5')"
check "a bounded wait-for continued over lines" 0 "$(verdict $'scripts/wait-for f x \\\n  --timeout 30')"
check "an unbounded wait-for continued over lines" 2 "$(verdict $'scripts/wait-for f x \\\n  --poll 5')"
check "wait-for --help" 0 "$(verdict 'scripts/wait-for --help')"
check "a command that only names the tool" 0 "$(verdict 'grep -n wait-for scripts/agent-time; cat scripts/ci-wait')"
check "agentctl's other subcommands" 0 "$(verdict '~/bin/agentctl land && ~/bin/agentctl inbox --unread')"
check "a broken event never blocks" 0 "$(printf 'not json' | python3 "$guard" 2>/dev/null; echo $?)"

if [ "$fails" -ne 0 ]; then echo "foreground-wait-guard: $fails failed"; exit 1; fi
echo "foreground-wait-guard: all passed"
