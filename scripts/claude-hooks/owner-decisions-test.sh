#!/usr/bin/env bash
# Self-test for scripts/claude-hooks/owner-decisions: a pending owner decision in agentctl's snapshot (state.json's
# structured `asks`) is put to the model (question, category, the exact `! ~/bin/agentctl decide ...` command) once per
# session, again only when its question changes or the reminder interval passes, or after a compaction; an inbox entry
# QUOTING "WAITING ON YOU" (2026-11-19's false decision), no asks, a stale, missing or garbage snapshot and garbage
# input are silent and exit 0; a stdin that never closes or a terminal never holds it past its bound. SessionStart says to start the inbox notifier after a
# compaction, and on startup/resume only in the root checkout.
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
hook="$REPO_ROOT/scripts/claude-hooks/owner-decisions"
. "$REPO_ROOT/scripts/scratch-git.sh"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
fails=0
check() { # <what> <expected> <actual>
  if [ "$2" = "$3" ]; then printf '  \033[32m✓\033[0m %s\n' "$1"
  else printf '  \033[31m✗\033[0m %s\n      expected: %s\n      actual:   %s\n' "$1" "$2" "$3"; fails=$((fails + 1)); fi
}

# agentctl's snapshot (state.json): its structured `asks` are the pending owner decisions (~/bin ask.py owner_asks)
how='answer: `! ~/bin/agentctl decide e1be6bd9fd46 "<your answer>"` (look: `~/bin/agentctl red status -v`)'
snapshot() { # <file> <age seconds> [<question>...] -> a state.json whose asks are one told ask per question
  local f=$1 age=$2; shift 2; python3 -c '
import json, sys, time
f, age, how, qs = sys.argv[1], float(sys.argv[2]), sys.argv[3], sys.argv[4:]
asks = [{"block": "block/k%d" % i, "keys": ["k%d" % i], "reds": ["Main #2515"], "category": "policy-no-rule", "q": q,
         "how": how, "until": "a newer Main run passes", "since": 0} for i, q in enumerate(qs)]
json.dump({"tick_at": time.time() - age, "asks": asks}, open(f, "w"))' "$f" "$age" "$how" "$@"; }
question='approve adding those 4 rows to `identity-flip-backlog.tsv` as a known flip, or have stage 5 measured; rule: scripts/ci/lane-policy.tsv: x'
snapshot "$tmp/one.json" 0 "$question"
snapshot "$tmp/changed.json" 0 "${question/stage 5/stage 6}"
snapshot "$tmp/empty.json" 0
snapshot "$tmp/stale.json" 7200 "$question"
python3 -c 'import json, time; json.dump({"tick_at": time.time()}, open("'"$tmp"'/old-agentctl.json", "w"))'

export OWNER_DECISIONS_STATE_DIR="$tmp/state"
# THE FALSE DECISION (2026-11-19): an inbox holding an open agentctl issue that QUOTES a red's question. The hook must
# not read it: only the snapshot's asks are decisions.
mkdir -p "$tmp/inbox/acked" "$tmp/inbox.alerts"
python3 - "$tmp/inbox" <<'PY'
import json, os, sys
d = sys.argv[1]
v = ("AGENTCTL ISSUE 20261104T044122-55799d: red c494fec65b9d (Main #2128) read \"WAITING ON YOU: either design and "
     "measure...') and 13 min later RED ESCALATE 'still red after 4 attempts\" -- `! ~/bin/agentctl decide c494fec65b9d \"x\"`")
json.dump({"id": "i1", "item": "issue:55799d", "kind": "OTHER", "audience": "owner", "verdict": v},
          open(os.path.join(d, "i1.json"), "w"))
json.dump({"v": 1, "items": {"issue:55799d": "i1"}}, open(d + ".alerts/index.json", "w"))
PY
export AGENTCTL_INBOX_DIR="$tmp/inbox"
prompt() { # <snapshot file> [session] -> the hook's stdout; exit code in $tmp/code
  printf '{"session_id": "%s", "hook_event_name": "UserPromptSubmit", "prompt": "hi"}' "${2:-s1}" |
    AGENTCTL_STATE_FILE="$1" python3 "$hook" prompt 2>"$tmp/stderr"
  echo $? >"$tmp/code"
}
context() { python3 -c 'import json, sys; d = json.load(sys.stdin); print(d["hookSpecificOutput"]["additionalContext"])'; }

printf '\033[36m▸\033[0m a pending decision is shown\n'
out=$(prompt "$tmp/one.json")
check "exit 0" 0 "$(cat "$tmp/code")"
check "event name" UserPromptSubmit "$(echo "$out" | python3 -c 'import json, sys; print(json.load(sys.stdin)["hookSpecificOutput"]["hookEventName"])')"
ctx=$(echo "$out" | context)
check "the question" 1 "$(echo "$ctx" | grep -c 'approve adding those 4 rows')"
check "its category and red" 1 "$(echo "$ctx" | grep -c 'WAITING ON YOU \[policy-no-rule\]: Main #2515: approve')"
check "the exact decide command" 1 "$(echo "$ctx" | grep -cF '! ~/bin/agentctl decide e1be6bd9fd46 "<your answer>"')"
check "tells the model to put it to the user verbatim at the top" 1 "$(echo "$ctx" | grep -c 'verbatim at the top')"
check "one decision counted" 1 "$(echo "$ctx" | grep -c '^agentctl has 1 decision')"
check "an inbox entry quoting WAITING ON YOU is no decision" 0 "$(echo "$ctx" | grep -c 'either design and measure')"

printf '\033[36m▸\033[0m dedupe\n'
check "the same item, same session: silent" "" "$(prompt "$tmp/one.json")"
check "another session sees it" 1 "$(prompt "$tmp/one.json" s2 | context | grep -c 'approve adding')"
check "its question changed: shown again" 1 "$(prompt "$tmp/changed.json" | context | grep -c 'stage 6')"
check "after the reminder interval: shown again" 1 "$(OWNER_DECISIONS_REMIND_SECONDS=0 prompt "$tmp/changed.json" | context | grep -c 'stage 6')"

printf '\033[36m▸\033[0m silent\n'
check "nothing pending, an issue quoting WAITING ON YOU in the inbox" "" "$(prompt "$tmp/empty.json" s3)"
check "a stale snapshot is not believed" "" "$(prompt "$tmp/stale.json" s3)"
check "a snapshot from an agentctl without asks" "" "$(prompt "$tmp/old-agentctl.json" s3)"
check "no snapshot: silent, exit 0" " 0" "$(prompt "$tmp/nope.json" s5) $(cat "$tmp/code")"
check "a garbage snapshot: silent, exit 0" " 0" "$(prompt "$tmp/stderr" s6) $(cat "$tmp/code")"
check "garbage hook input: exit 0, nothing" "exit 0" "$(echo 'not json' | AGENTCTL_STATE_FILE="$tmp/one.json" python3 "$hook" prompt 2>/dev/null; echo "exit $?")"

printf '\033[36m▸\033[0m bounded: never waits on stdin\n'
bounded() { # <seconds> <cmd...>: "done" when the command ends within the bound, else "hung"
  local s=$1; shift
  ( "$@" >/dev/null 2>&1 ) & local pid=$!
  for _ in $(seq $((s * 10))); do kill -0 "$pid" 2>/dev/null || { echo "done"; return; }; sleep 0.1; done
  kill "$pid" 2>/dev/null; echo hung
}
held_open() { AGENTCTL_STATE_FILE="$tmp/one.json" python3 "$hook" prompt < <(sleep 30); }
check "a writer that never closes stdin (a shell's terminal): ends within 5 s" "done" "$(bounded 5 held_open)"
# BSD script(1): a pty as the hook's stdin, script's own stdin held open
held_tty() { AGENTCTL_STATE_FILE="$tmp/one.json" script -q /dev/null python3 "$hook" prompt < <(sleep 30); }
[ "$(uname)" = Darwin ] && check "run from a terminal: ends within 5 s" "done" "$(bounded 5 held_tty)"

printf '\033[36m▸\033[0m session start\n'
start() { # <source> <cwd> [session]
  printf '{"session_id": "%s", "hook_event_name": "SessionStart", "source": "%s", "cwd": "%s"}' "${3:-s1}" "$1" "$2" |
    python3 "$hook" session-start 2>/dev/null
}
scratch_repo "$tmp/root" && scratch_git "$tmp/root" commit -q --allow-empty -m x && scratch_git "$tmp/root" worktree add -q "$tmp/wt" 2>/dev/null
check "compact: start the notifier, a one-shot inbox wait in the background" 1 \
  "$(start compact "$tmp/wt" | context | grep -c 'inbox wait --since-ack --audience owner --timeout 43200` with Bash run_in_background')"
check "...never a Monitor's endless --forever" 0 "$(start compact "$tmp/wt" | context | grep -c -- '--forever')"
check "compact: the session's pending decisions are shown again" 1 "$(prompt "$tmp/changed.json" | context | grep -c 'stage 6')"
check "startup in the root checkout: start the notifier" 1 "$(start startup "$tmp/root" | context | grep -c 'notifier')"
check "resume in the root checkout: start the notifier" 1 "$(start resume "$tmp/root" | context | grep -c 'notifier')"
check "startup in a worktree: silent" "" "$(start startup "$tmp/wt")"
check "startup outside git: silent" "" "$(start startup "$tmp/state")"

if [ "$fails" -eq 0 ]; then echo "all passed"; else echo "$fails failed"; exit 1; fi
