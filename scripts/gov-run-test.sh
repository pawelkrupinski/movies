#!/usr/bin/env bash
# scripts/gov-run and jvm-gate.sh's gov_hold / gov_run, against a stub governor: the command waits until it is admitted
# (the governor's own wait, nothing polled here), runs holding the ticket, a gated child rides it (no second ticket, no
# wait on its own parent), a red fixer's checkout asks as red, and every ungated path -- KINOWO_GOVERNOR=0, Linux, no
# agentctl, inside a check (KINOWO_GOV_CPU_HELD) -- runs the command at once with no call to the governor.
# Run: bash scripts/gov-run-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/jvm-gate.sh"
printf '\033[36m▸\033[0m gov-run\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

spec_fake_governor "$work/agentctl"
export GOV_CALLS="$work/calls"
governed() { spec_governed "$work/agentctl" "$@"; }
# what a command sees: whether it holds a ticket (descriptor 7 open, named in KINOWO_JVM_SLOT_HELD, its lock held)
cat > "$work/probe" <<'PROBE'
#!/usr/bin/env bash
. "$1/scripts/jvm-gate.sh"
if jvm_slot_riding; then echo "rides $(lock_busy "$KINOWO_JVM_SLOT_HELD" && echo held || echo free)"; else echo none; fi
PROBE
chmod +x "$work/probe"

: > "$GOV_CALLS"
( cd "$work" && GOV_HOLD="$work/admit" governed "$REPO_ROOT/scripts/gov-run" --job script-tests --cores 6 --mem 2 -- \
    bash -c 'touch "$1/ran"; "$2" "$3" > "$1/seen"' _ "$work" "$work/probe" "$REPO_ROOT" ) > "$work/out" 2>&1 &
runner=$!
wait_until "the governor was asked" '[ -e "$work/admit.waiting" ]'
check "not admitted (a starved laptop): the command waits, not started" "absent" "$([ -e "$work/ran" ] && echo ran || echo absent)"
touch "$work/admit"; wait "$runner"; status=$?
check "...admitted: it starts by itself and exits as the command did" "0 ran" "$status $([ -e "$work/ran" ] && echo ran)"
check "...holding the ticket: descriptor 7 open, its lock held, named for its children" "rides held" "$(cat "$work/seen")"
check "...asked once, as an agent's job, charged its job, cores and memory" "1" \
  "$(grep -c -- '^gov admit --fd 7 --ticket .* --class agent --mem 2 --label script-tests --pid [0-9]* --cwd .* --job script-tests --cores 6$' "$GOV_CALLS")"

: > "$GOV_CALLS"
( cd "$work" && governed "$REPO_ROOT/scripts/gov-run" --job page-tests --label outer -- \
    "$REPO_ROOT/scripts/gov-run" --job inner -- "$work/probe" "$REPO_ROOT" ) > "$work/out" 2>&1
check "a gated command run by a gated command rides its parent's ticket: one ticket, no wait on itself" "rides held|1" \
  "$(cat "$work/out")|$(grep -c '^gov admit' "$GOV_CALLS")"
check "...no --cores: the job's own figure, the label as given" "1" "$(grep -c -- '--label outer .*--job page-tests$' "$GOV_CALLS")"

: > "$GOV_CALLS"
( governed bash -c '. "$1/scripts/jvm-gate.sh"; gov_run t page-tests 5 2 "$2" "$1"; gov_run t2 x "" "" true; exit 7' \
    _ "$REPO_ROOT" "$work/probe" ) > "$work/out" 2>&1; status=$?
check "gov_run: the command (a function too) under its own ticket, given back after: the next takes one of its own" \
  "rides held|2|7" "$(head -1 "$work/out")|$(grep -c '^gov admit' "$GOV_CALLS")|$status"
check "...exit status the command's" "3" "$(governed bash -c '. "$1/scripts/jvm-gate.sh"; gov_run t j 1 1 false; gov_run t j 1 1 bash -c "exit 3"' _ "$REPO_ROOT"; echo $?)"

: > "$GOV_CALLS"
mkdir -p "$work/movies-wt-red-4242"
( cd "$work/movies-wt-red-4242" && governed "$REPO_ROOT/scripts/gov-run" --job backtest -- true
  cd "$work" && governed "$REPO_ROOT/scripts/gov-run" --class measure --job backtest -- true ) > /dev/null 2>&1
check "classes: a red fixer's checkout asks as red (admitted at once), --class as given" "red measure" \
  "$(sed -n 's/.* --class \([a-z]*\) .*/\1/p' "$GOV_CALLS" | paste -sd' ' -)"

: > "$GOV_CALLS"
{ KINOWO_GOVERNOR=0 KINOWO_GOVERNOR_BIN="$work/agentctl" KINOWO_JVM_GATE_OS=Darwin "$REPO_ROOT/scripts/gov-run" --job j -- "$work/probe" "$REPO_ROOT"
  governed env KINOWO_JVM_GATE_OS=Linux "$REPO_ROOT/scripts/gov-run" --job j -- "$work/probe" "$REPO_ROOT"
  governed env KINOWO_GOVERNOR_BIN="$work/no-agentctl" "$REPO_ROOT/scripts/gov-run" --job j -- "$work/probe" "$REPO_ROOT"
  governed env KINOWO_GOV_CPU_HELD=1 "$REPO_ROOT/scripts/gov-run" --job j -- "$work/probe" "$REPO_ROOT"
} > "$work/out" 2>&1
check "ungated, the command run at once with no ticket: KINOWO_GOVERNOR=0, Linux (CI), no agentctl, inside a check" \
  "none none none none|0" "$(paste -sd' ' "$work/out")|$(grep -c '^gov admit' "$GOV_CALLS")"

printf '#!/usr/bin/env bash\n[ "$1 $2" = "gov probe" ] && { echo "agentctl governor: admit cores"; exit 0; }\nprintf "%%s\\n" "$*" >> "$GOV_CALLS"; exit 1\n' > "$work/refusing"
chmod +x "$work/refusing"
out=$(governed env KINOWO_GOVERNOR_BIN="$work/refusing" "$REPO_ROOT/scripts/gov-run" --job j -- echo ran 2>&1)
check "a governor that fails: warned, and the command runs all the same" "1 ran" \
  "$(printf '%s\n' "$out" | grep -c 'failed: running without a ticket') $(printf '%s\n' "$out" | tail -1)"
check "usage: no --job or no command is 64" "64 64" \
  "$("$REPO_ROOT/scripts/gov-run" -- true > /dev/null 2>&1; echo $?) $("$REPO_ROOT/scripts/gov-run" --job j > /dev/null 2>&1; echo $?)"

# THE TICKET'S MEASURED CPU: a command whose children burn 3 cores records about 3 in its ticket's job, not its width (1)
: > "$GOV_CALLS"
( cd "$work" && KINOWO_CPU_METER_POLL=0.2 governed "$REPO_ROOT/scripts/gov-run" --job script-tests --cores 1 -- bash -c '
    for _ in 1 2 3; do python3 -I -B -c "import time; t = time.process_time()
while time.process_time() - t < 1.0: pass" & done; wait; exit 5' ) > /dev/null 2>&1
status=$?
usage=$(grep '^gov usage' "$GOV_CALLS")
check "metered: a tree of 3 busy children records its CPU (3 s over about 1 s wall) as the job's, with the tree meter" "5 1 tree" \
  "$status $(printf '%s\n' "$usage" | grep -c -- '--job script-tests') $(printf '%s\n' "$usage" | sed -n 's/.*--meter //p')"
cpu=$(printf '%s\n' "$usage" | sed -n 's/.*--cpu \([0-9]*\).*/\1/p'); wall=$(printf '%s\n' "$usage" | sed -n 's/.*--wall \([0-9]*\).*/\1/p')
check "...at least 2 cores' worth, as measured" "yes" "$([ "${cpu:-0}" -ge 2 ] && echo yes || echo "no: cpu=$cpu wall=$wall")"
: > "$GOV_CALLS"
( cd "$work" && governed "$REPO_ROOT/scripts/gov-run" --job outer -- "$REPO_ROOT/scripts/gov-run" --job inner -- true ) > /dev/null 2>&1
check "...a command riding its parent's ticket records nothing: only the ticket's holder reports its tree" "outer" \
  "$(sed -n 's/^gov usage --job \([a-z]*\) .*/\1/p' "$GOV_CALLS" | paste -sd' ' -)"

spec_summary
