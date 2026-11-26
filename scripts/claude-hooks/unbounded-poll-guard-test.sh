#!/usr/bin/env bash
# Self-test for scripts/claude-hooks/unbounded-poll-guard: the hook's JSON fed to it the way Claude Code does. A
# background poll loop with no bound (in the command, a `bash -c` script, a heredoc it runs, or a scratchpad script it
# runs) is denied (exit 2) with a fix; the same loop bounded, a server script, a loop that never sleeps, and anything
# in the foreground without `&` are allowed.
# The commands under test are literal text for the guard to read, never run here: nothing in them should expand.
# shellcheck disable=SC2016,SC2088
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
guard="$REPO_ROOT/scripts/claude-hooks/unbounded-poll-guard"
fails=0
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
check() { # <what> <expected> <actual>
  if [ "$2" = "$3" ]; then printf '  \033[32m✓\033[0m %s\n' "$1"
  else printf '  \033[31m✗\033[0m %s\n      expected: %s\n      actual:   %s\n' "$1" "$2" "$3"; fails=$((fails + 1)); fi
}
event() { # <command> [foreground] -> the hook's JSON for a Bash call running it (in the background by default)
  python3 -c 'import json, sys
tool_input = {"command": sys.argv[1]}
if len(sys.argv) < 3: tool_input["run_in_background"] = True
print(json.dumps({"tool_name": "Bash", "cwd": "/", "tool_input": tool_input}))' "$@"
}
verdict() { event "$@" | python3 "$guard" 2>/dev/null; echo $?; }
advice() { { event "$1" | python3 "$guard" > /dev/null; } 2>&1; }

T=/private/tmp/claude-501/p/sess/tasks
# 2026-11-18: seven background watchers of this shape ran 22-46 h after their agents ended. Each is denied, and allowed
# once bounded.
orphans=(
  'until grep -q DONE log; do sleep 10; done'
  "S=/private/tmp/claude-501/p/sess; until [ -s \$S/tasks/bkrpf1xal.output ]; do sleep 10; done; cut -c1-400 \$S/tasks/bkrpf1xal.output"
  "P=/private/tmp/claude-501/p/sess/scratchpad/probe; until grep -qE '^DONE|^\\[error\\]' \$P/out.log; do sleep 10; done; grep -c PROBE \$P/out.log"
  "until grep -q bash32= $T/barqbllbr.output 2>/dev/null; do sleep 10; done; cat $T/barqbllbr.output"
  'until ~/bin/agentctl inbox 2>&1 | grep "movies-relay" | grep -v "goes to" | grep -q -E "LANDED|landed"; do sleep 20; done; ~/bin/agentctl inbox | cut -c1-300'
  "cd ~ && until ssh -o ConnectTimeout=10 ci-runner 'journalctl -u agentctl-autopin -n 30 --no-pager' 2>/dev/null | grep -q \"ff7eb6a\"; do sleep 30; done; echo seen"
  "until ! pgrep -f waitland.sh >/dev/null; do sleep 15; done; tail -2 $T/b37jb5q3h.output"
)
printf '\033[36m▸\033[0m the seven orphaned watchers of 2026-11-18: denied, then allowed once bounded\n'
for index in "${!orphans[@]}"; do
  line=${orphans[$index]}
  check "orphan $((index + 1)): denied in the background" 2 "$(verdict "$line")"
  check "orphan $((index + 1)): ...allowed under timeout 7200 bash -c" 0 "$(verdict "timeout 7200 bash -c $(printf %q "$line")")"
  check "orphan $((index + 1)): ...allowed as an until with a \$SECONDS deadline" 0 \
    "$(verdict "${line/; do sleep/ || [ \$SECONDS -gt 7200 ]; do sleep}")"
done

printf '\033[36m▸\033[0m the fix it prints is one command\n'
check "until grep -q TEXT FILE -> wait-for --contains, with its poll" 1 \
  "$(advice 'until grep -q DONE log; do sleep 10; done' | grep -c '`scripts/wait-for log --contains DONE --timeout 2h --poll 10`')"
check "until grep -qE REGEX FILE -> wait-for with the regex" 1 \
  "$(advice "until grep -qE '^exit=' /x/l.log; do sleep 5; done" | grep -c "\`scripts/wait-for /x/l.log '^exit=' --timeout 2h\`")"
check "until [ -s FILE ] -> wait-for FILE (any content)" 1 \
  "$(advice "until [ -s $T/b.output ]; do sleep 30; done" | grep -c "\`scripts/wait-for $T/b.output --timeout 2h --poll 30\`")"
check "a compound condition gets the general advice only" 0 \
  "$(advice 'until [ -f a ] && grep -q X a; do sleep 5; done' | grep -c 'Here:')"

printf '\033[36m▸\033[0m denied\n'
check "a loop that merely prints \$SECONDS has no bound" 2 "$(verdict 'while true; do sleep 5; echo $SECONDS; done')"
check "a while true poll" 2 "$(verdict 'while true; do curl -s localhost:9000/health && break; sleep 2; done')"
check "a loop backgrounded with & from the foreground" 2 "$(verdict '(until grep -q x f; do sleep 5; done; echo y) & echo started' foreground)"
check "nohup ... & of an unbounded bash -c" 2 "$(verdict "nohup bash -c 'while :; do date; sleep 60; done' > /dev/null 2>&1 &" foreground)"
check "a comparison with nothing counting (a count that may never come)" 2 \
  "$(verdict 'until [ $(~/bin/agentctl inbox | grep -c landed) -ge 3 ]; do sleep 60; done')"
check "for ((;;)) with a sleep" 2 "$(verdict 'for ((;;)); do date; sleep 5; done')"
check "tail -f" 2 "$(verdict 'tail -f /x/log | grep --line-buffered ERROR')"
check "tail -F -n 0" 2 "$(verdict 'tail -n 0 -F /x/log')"
check "a heredoc piped to bash" 2 "$(verdict $'bash <<\'EOF\'\nuntil [ -s /x/f ]; do sleep 5; done\nEOF')"
check "a scratchpad script written by the command, then run" 2 \
  "$(verdict $'cat > /private/tmp/claude-501/p/s/scratchpad/w.sh <<\'EOF\'\nuntil grep -q x f; do sleep 5; done\nEOF\nbash /private/tmp/claude-501/p/s/scratchpad/w.sh')"
mkdir -p "$work/scratchpad"
printf '#!/bin/bash\nwhile ! grep -q LANDED "$1"; do\n  sleep 30\ndone\n' > "$work/scratchpad/watch.sh"
check "a scratchpad script on disk, run by bash" 2 "$(verdict "bash $work/scratchpad/watch.sh land.log")"
check "...run by path through a variable" 2 "$(verdict "W=$work/scratchpad; \$W/watch.sh land.log")"

printf '\033[36m▸\033[0m allowed\n'
printf '#!/bin/bash\nset -uo pipefail\nfor i in $(seq 1 120); do\n  grep -q LANDED "$1" && exit 0\n  sleep 30\ndone\nexit 2\n' \
  > "$work/scratchpad/waitland.sh"
check "a counted scratchpad script (waitland.sh's shape)" 0 "$(verdict "bash $work/scratchpad/waitland.sh land.log")"
check "...the uncounted one under timeout" 0 "$(verdict "timeout 3600 bash $work/scratchpad/watch.sh land.log")"
check "scripts/wait-for (bounded by itself)" 0 "$(verdict 'scripts/wait-for /x/log --contains DONE --timeout 2h')"
check "scripts/ci-wait" 0 "$(verdict 'scripts/ci-wait --workflow main.yml --sha abc')"
check "agentctl's inbox wait" 0 "$(verdict '~/bin/agentctl inbox wait --since-ack --audience owner')"
check "sbt-warm --serve (a server, not a loop)" 0 "$(verdict 'scripts/sbt-warm --serve')"
check "mirror.sh (a repository script is never read)" 0 "$(verdict 'scripts/local-mirror/mirror.sh > mirror.log 2>&1')"
check "a counter with -lt" 0 "$(verdict 'n=0; while [ $n -lt 60 ]; do grep -q x f && break; n=$((n+1)); sleep 5; done')"
check "an arithmetic counter" 0 "$(verdict 'i=0; until grep -q x f || (( i > 100 )); do i=$((i+1)); sleep 5; done')"
check "a date +%s deadline" 0 "$(verdict 'end=$(( $(date +%s) + 600 )); until grep -q x f || [ $(date +%s) -gt $end ]; do sleep 5; done')"
check "a loop that never sleeps (while read)" 0 "$(verdict 'while read -r line; do echo "$line"; done < f')"
check "for over a list with a sleep" 0 "$(verdict 'for i in {1..30}; do curl -s x && break; sleep 10; done')"
check "a quoted done/until in a grep is no keyword" 0 "$(verdict "grep -q 'until done' f; sleep 5")"
check "tail -f bounded by --pid" 0 "$(verdict 'tail -f --pid=1234 /x/log')"
check "tail -f under timeout" 0 "$(verdict 'timeout 600 tail -f /x/log')"
check "tail -n (no follow)" 0 "$(verdict 'tail -n 50 /x/log')"
check "an unbounded loop in the FOREGROUND (no &): not this guard's" 0 "$(verdict 'until grep -q x f; do sleep 5; done' foreground)"
check "a && chain in the foreground is no &" 0 "$(verdict 'until grep -q x f; do sleep 5; done && echo ok 2>&1' foreground)"
check "a broken event never blocks" 0 "$(printf 'not json' | python3 "$guard" 2>/dev/null; echo $?)"
check "an unclosed quote never blocks" 0 "$(verdict "until grep -q 'x f; do sleep 5; done")"

if [ "$fails" -ne 0 ]; then echo "unbounded-poll-guard: $fails failed"; exit 1; fi
echo "unbounded-poll-guard: all passed"
