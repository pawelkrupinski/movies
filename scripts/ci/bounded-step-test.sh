#!/usr/bin/env bash
# bounded-step.sh against stand-in steps: a step script that sources it the way a workflow step does, run as the
# runner runs it (`bash -e -o pipefail <file>`), finishing, failing, and "timing out" (the runner giving up while its
# processes run on).
# Run: bash scripts/ci/bounded-step-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m bounded-step.sh\n'

work="$(cd "$(mktemp -d)" && pwd -P)"
export RUNNER_TEMP="$work/temp"
mkdir -p "$RUNNER_TEMP"
trap 'pkill -f "$work" 2>/dev/null; rm -rf "$work"' EXIT
alive() { kill -0 "$1" 2>/dev/null && echo alive || echo gone; }
records() { find "$RUNNER_TEMP/bounded-steps" -type f 2>/dev/null | wc -l | tr -d ' '; }

# A step script: sources the wrapper, then $1 is its body.
step() {
  local file="$work/step-$RANDOM.sh"
  printf '. %q\n%s\n' "$REPO_ROOT/scripts/ci/bounded-step.sh" "$1" > "$file"
  echo "$file"
}

# 1. A step that finishes runs its script once, keeps its exit status, and leaves no record.
ok=$(step "echo ran-once >> '$work/ran'; exit 0")
bash --noprofile --norc -eo pipefail "$ok"
check "a passing step exits 0" "0" "$?"
check "...runs its body once" "1" "$(grep -c ran-once "$work/ran")"
check "...and leaves no record" "0" "$(records)"

bad=$(step "exit 7")
bash --noprofile --norc -eo pipefail "$bad"
check "a failing step keeps its exit status" "7" "$?"
check "...and leaves no record" "0" "$(records)"

# The runner's options carry over: the default shell's `bash -e` has no pipefail, `shell: bash` does.
pipe=$(step "false | true; echo reached")
check "under bash -e, a pipeline's last command decides, as unbounded" "reached" "$(bash -e "$pipe")"
bash -eo pipefail "$pipe" > /dev/null
check "under -eo pipefail, a failing pipeline still ends the step" "1" "$?"

# 2. What a step starts in the background on purpose and then ends normally is left alone (a tunnel, mongod).
bg=$(step "bash -c 'while :; do sleep 1; done' '$work' & echo \$! > '$work/daemon'")
bash --noprofile --norc -eo pipefail "$bg"
wait_until "the daemon recorded its pid" '[ -s "$work/daemon" ]'
check "a finished step's background daemon survives the next reap" "alive" \
  "$(bash "$REPO_ROOT/scripts/ci/bounded-step.sh" --reap > /dev/null; alive "$(cat "$work/daemon")")"
kill "$(cat "$work/daemon")" 2>/dev/null

# 3. The runner gives up on a step (timeout-minutes): its shell and children run on. The next reap ends them all.
hung=$(step "bash -c 'while :; do sleep 1; done' '$work' & echo \$! > '$work/child'; while :; do sleep 1; done")
bash --noprofile --norc -eo pipefail "$hung" > /dev/null 2>&1 &
runner_gave_up_on=$!
wait_until "the step's child recorded its pid" '[ -s "$work/child" ]'
check "a step still running holds a record" "1" "$(records)"
out=$(bash "$REPO_ROOT/scripts/ci/bounded-step.sh" --reap 2>&1)
check "the reap succeeds" "0" "$?"
check "...ends the timed-out step's child" "gone" "$(alive "$(cat "$work/child")")"
check "...says so" "1" "$(grep -c '^::warning::ending what a timed-out step left running' <<< "$out")"
wait "$runner_gave_up_on" 2>/dev/null
check "...the abandoned step's own shell then ends, removing nothing it should keep" "gone" "$(alive "$runner_gave_up_on")"
check "...and no record is left" "0" "$(records)"

# 4. A daemon that left the step's process group (setsid, an sbt server) is found by its environment (Linux only).
if [ -e /proc/self/environ ] && command -v setsid > /dev/null; then
  : > "$work/child"
  escaped=$(step "setsid bash -c 'while :; do sleep 1; done' '$work' & echo \$! > '$work/child'; while :; do sleep 1; done")
  bash --noprofile --norc -eo pipefail "$escaped" > /dev/null 2>&1 &
  wait_until "the step's child recorded its pid" '[ -s "$work/child" ]'
  bash "$REPO_ROOT/scripts/ci/bounded-step.sh" --reap > /dev/null 2>&1
  check "a daemon that left the group is ended by its BOUNDED_STEP mark" "gone" "$(alive "$(cat "$work/child")")"

  # A daemon that leaves the group while the reap runs: one look at the marks, taken at the TERM, misses what is
  # mid-exec then (its environ reads empty) or starts a moment later (Tool self-tests #2866). Here the step's shell
  # answers TERM by starting one a second later: after that first look, before the group is gone. It answers the reap's
  # later TERMs with a no-op (bash would start the trap over) and its waits carry no mark, so those end only the daemon.
  : > "$work/child"
  late=$(step "trap \"trap : TERM; env -u BOUNDED_STEP sleep 1; setsid bash -c 'while :; do sleep 1; done' '$work' & echo \\\$! > '$work/child'; env -u BOUNDED_STEP sleep 0.5; exit 0\" TERM
echo started > '$work/late'; while :; do sleep 1; done")
  bash --noprofile --norc -eo pipefail "$late" > /dev/null 2>&1 &
  wait_until "the late step started" '[ -s "$work/late" ] && [ "$(records)" = 1 ]'
  bash "$REPO_ROOT/scripts/ci/bounded-step.sh" --reap > /dev/null 2>&1
  check "...and so is one that leaves the group while the reap runs" "gone" \
    "$([ -s "$work/child" ] && alive "$(cat "$work/child")" || echo "never started")"
fi

# 5. The next bounded step reaps before it starts, so two bounded steps in a row need no reap step between them.
: > "$work/child"
hung=$(step "bash -c 'while :; do sleep 1; done' '$work' & echo \$! > '$work/child'; while :; do sleep 1; done")
bash --noprofile --norc -eo pipefail "$hung" > /dev/null 2>&1 &
wait_until "the step's child recorded its pid" '[ -s "$work/child" ]'
next=$(step "true")
bash --noprofile --norc -eo pipefail "$next" > /dev/null 2>&1
check "a bounded step ends what the previous one left before it runs" "gone" "$(alive "$(cat "$work/child")")"

check "nothing recorded: the reap exits 0" "0" "$(bash "$REPO_ROOT/scripts/ci/bounded-step.sh" --reap > /dev/null 2>&1; echo $?)"
check "an unknown argument is a usage error" "2" "$(bash "$REPO_ROOT/scripts/ci/bounded-step.sh" --nope > /dev/null 2>&1; echo $?)"

spec_summary
