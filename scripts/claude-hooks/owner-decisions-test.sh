#!/usr/bin/env bash
# Self-test for scripts/claude-hooks/owner-decisions: a pending owner decision in agentctl's inbox is put to the model
# (question, why, the exact `! ~/bin/agentctl decide ...` command) once per session, again only when its verdict
# changes or the reminder interval passes, or after a compaction; non-decision owner alerts, an empty inbox, a missing
# or failing agentctl and garbage input are silent and exit 0. SessionStart says to start the inbox notifier after a
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

# A fake agentctl: prints $FAKE_INBOX for `inbox ... --json`, or exits 1 when FAKE_FAIL is set.
cat >"$tmp/agentctl" <<'EOF'
#!/usr/bin/env bash
[ -n "${FAKE_FAIL:-}" ] && { echo "boom" >&2; exit 1; }
[ "$1" = inbox ] || exit 64
cat "$FAKE_INBOX"
EOF
chmod +x "$tmp/agentctl"
decision='RED BLOCKED ON OWNER: WAITING ON YOU: approve adding those 4 rows to `identity-flip-backlog.tsv` as a known flip, or have stage 5 measured -- Main #2515 waits on it. Answer: `! ~/bin/agentctl decide e1be6bd9fd46 "<your answer>"` (its fixer is dispatched with your answer). Why: DIAGNOSED: 97d260d67 committed the drift slice with only a `drift` backlog row. Look: `~/bin/agentctl red status -v`. Auto-clears when a newer Main run passes'
inbox() { # <file> <verdict>... -> a JSON list of unread owner entries, one per verdict
  python3 -c '
import json, sys
print(json.dumps([{"id": "e%d" % i, "item": "red:k%d" % i, "kind": "BLOCKED", "audience": "owner", "read": False,
                   "verdict": v} for i, v in enumerate(sys.argv[2:])]))' "$@" >"$1"
}
inbox "$tmp/one.json" "$decision"
inbox "$tmp/changed.json" "${decision/stage 5/stage 6}"
inbox "$tmp/invariant.json" 'INVARIANT W1 violated: oldest unlanded change is 4 h old'
inbox "$tmp/empty.json"

export OWNER_DECISIONS_AGENTCTL="$tmp/agentctl" OWNER_DECISIONS_STATE_DIR="$tmp/state"
export AGENTCTL_INBOX_DIR="$tmp/no-inbox"  # no alert index: the CLI answers (the index has its own section below)
prompt() { # <inbox file> [session] -> the hook's stdout; exit code in $tmp/code
  printf '{"session_id": "%s", "hook_event_name": "UserPromptSubmit", "prompt": "hi"}' "${2:-s1}" |
    FAKE_INBOX="$1" python3 "$hook" prompt 2>"$tmp/stderr"
  echo $? >"$tmp/code"
}
context() { python3 -c 'import json, sys; d = json.load(sys.stdin); print(d["hookSpecificOutput"]["additionalContext"])'; }

printf '\033[36m▸\033[0m a pending decision is shown\n'
out=$(prompt "$tmp/one.json")
check "exit 0" 0 "$(cat "$tmp/code")"
check "event name" UserPromptSubmit "$(echo "$out" | python3 -c 'import json, sys; print(json.load(sys.stdin)["hookSpecificOutput"]["hookEventName"])')"
ctx=$(echo "$out" | context)
check "the question" 1 "$(echo "$ctx" | grep -c 'approve adding those 4 rows')"
check "the why" 1 "$(echo "$ctx" | grep -c 'Why: DIAGNOSED: 97d260d67')"
check "the exact decide command" 1 "$(echo "$ctx" | grep -cF '! ~/bin/agentctl decide e1be6bd9fd46 "<your answer>"')"
check "tells the model to put it to the user verbatim at the top" 1 "$(echo "$ctx" | grep -c 'verbatim at the top')"

printf '\033[36m▸\033[0m dedupe\n'
check "the same item, same session: silent" "" "$(prompt "$tmp/one.json")"
check "another session sees it" 1 "$(prompt "$tmp/one.json" s2 | context | grep -c 'approve adding')"
check "its verdict changed: shown again" 1 "$(prompt "$tmp/changed.json" | context | grep -c 'stage 6')"
check "after the reminder interval: shown again" 1 "$(OWNER_DECISIONS_REMIND_SECONDS=0 prompt "$tmp/changed.json" | context | grep -c 'stage 6')"

printf '\033[36m▸\033[0m silent\n'
check "nothing pending" "" "$(prompt "$tmp/empty.json" s3)"
check "an owner alert that is no decision" "" "$(prompt "$tmp/invariant.json" s3)"
check "agentctl fails: silent" "" "$(FAKE_FAIL=1 prompt "$tmp/one.json" s4)"
check "agentctl fails: exit 0" 0 "$(cat "$tmp/code")"
check "agentctl missing: silent, exit 0" " 0" "$(OWNER_DECISIONS_AGENTCTL="$tmp/nope" prompt "$tmp/one.json" s5) $(cat "$tmp/code")"
check "agentctl prints garbage: silent, exit 0" " 0" "$(prompt "$tmp/stderr" s6) $(cat "$tmp/code")"
check "garbage hook input: exit 0, nothing" "exit 0" "$(echo 'not json' | FAKE_INBOX="$tmp/one.json" python3 "$hook" prompt 2>/dev/null; echo "exit $?")"

printf '\033[36m▸\033[0m read from agentctl'"'"'s alert index, not the CLI\n'
mkdir -p "$tmp/inbox/acked" "$tmp/inbox.alerts"
python3 - "$tmp/inbox" "$decision" <<'PY'
import json, os, sys
d, verdict = sys.argv[1], sys.argv[2]
entries = {"d1": ("owner", verdict), "d2": ("owner", verdict.replace("stage 5", "ACKED ONE")),
           "d3": ("fixer", verdict.replace("stage 5", "A FIXER ONE")), "d4": ("owner", "INVARIANT W1 violated")}
for i, (audience, v) in entries.items():
    json.dump({"id": i, "item": "red:" + i, "audience": audience, "verdict": v}, open(os.path.join(d, i + ".json"), "w"))
open(os.path.join(d, "acked", "d2"), "w").close()
json.dump({"v": 1, "items": {"red:" + i: i for i in entries}}, open(d + ".alerts/index.json", "w"))
PY
ctx=$(AGENTCTL_INBOX_DIR="$tmp/inbox" FAKE_FAIL=1 prompt "$tmp/empty.json" s7 | context)
check "the unread owner decision, with the CLI failing" 1 "$(echo "$ctx" | grep -c 'stage 5')"
check "an acked entry is not shown" 0 "$(echo "$ctx" | grep -c 'ACKED ONE')"
check "another audience's entry is not shown" 0 "$(echo "$ctx" | grep -c 'A FIXER ONE')"
check "one decision counted" 1 "$(echo "$ctx" | grep -c '^agentctl has 1 decision')"

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
