#!/usr/bin/env bash
# jvm-gate.sh: a JVM start waits while too much memory is paged out (the reading stubbed), then for one of the
# machine's slots; a child rides its parent's slot; a slot whose holder is gone is taken over; each gives up warned at
# its deadline; a tool's JVM is sized for a share of the cores on macOS only.
# Run: bash scripts/jvm-gate-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/jvm-gate.sh"

printf '\033[36m▸\033[0m jvm-gate\n'
work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
export KINOWO_MEMORY_GATE=1 KINOWO_MEMORY_GATE_POLL=0.05 KINOWO_MEMORY_GATE_GB=20 KINOWO_MEMORY_GATE_FREE_GB=6 KINOWO_MEMORY_GATE_FREE_PROBE='echo 1'

# the probe reads the next value from a file per call: the paged-out GB as each poll would see it
probe() { printf '%s\n' "$@" > "$work/readings"; : > "$work/calls"; }
export KINOWO_MEMORY_GATE_PROBE='echo x >> "'"$work"'/calls"; n=$(wc -l < "'"$work"'/calls"); sed -n "${n}p" "'"$work"'/readings"'

probe 5
memory_gate t 2> "$work/err"
check "under the budget: starts at once, silently" "1|" "$(wc -l < "$work/calls" | tr -d ' ')|$(cat "$work/err")"

probe 48
KINOWO_MEMORY_GATE_FREE_PROBE='echo 9' memory_gate t 2> "$work/err"
check "paged out past the budget but RAM free for a heap (stale swap): starts at once, silently" "1|" \
  "$(wc -l < "$work/calls" | tr -d ' ')|$(cat "$work/err")"

probe 48 48 30 12
memory_gate t 2> "$work/err"
check "over the budget: waits until the paged-out memory drains under it" "4" "$(wc -l < "$work/calls" | tr -d ' ')"
check "...saying so once, and when it opens" "1 1" \
  "$(grep -c 'waiting for other JVMs' "$work/err") $(grep -c 'open after' "$work/err")"

probe 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48
KINOWO_MEMORY_GATE_MAX_WAIT=0 memory_gate t 2> "$work/err"; status=$?
check "past its deadline: goes ahead, warned, never fails the tool" "0 1" "$status $(grep -c 'starting anyway' "$work/err")"

probe 48
KINOWO_MEMORY_GATE=0 memory_gate t 2> "$work/err"
check "KINOWO_MEMORY_GATE=0: off, the reading never taken" "0" "$(wc -l < "$work/calls" | tr -d ' ')"

probe ""
memory_gate t 2> "$work/err"; status=$?
check "nothing readable (Linux CI): starts at once" "0|" "$status|$(cat "$work/err")"

# --- the machine's JVM slots
unset KINOWO_MEMORY_GATE_PROBE; export KINOWO_MEMORY_GATE=0 KINOWO_IDENTITY_CACHE="$work/cache" KINOWO_JVM_SLOTS=2 KINOWO_JVM_SLOT_POLL=0.05
slots="$work/cache/jvm-slots"
( jvm_slot_take t; echo "$JVM_SLOT $KINOWO_JVM_SLOT_HELD" > "$work/held"; cat "$slots/1/pid" > "$work/pid1"; echo $$ > "$work/me"
  bash -c '. "'"$REPO_ROOT"'/scripts/jvm-gate.sh"; jvm_slot_take child; echo "${JVM_SLOT:-rode}"' > "$work/child"
  jvm_slot_release; [ -d "$slots/1" ] && echo held > "$work/after" || echo free > "$work/after" )
check "a free slot is taken, held by this process, and named to its children" "$slots/1 $slots/1 yes" \
  "$(cat "$work/held") $([ "$(cat "$work/pid1")" = "$(cat "$work/me")" ] && echo yes || echo no)"
check "...a child tool rides it rather than waiting on its own parent" "rode" "$(cat "$work/child")"
check "...and it is given back on release" "free" "$(cat "$work/after")"

sleep 60 & a=$!; sleep 60 & b=$!
mkdir -p "$slots/1" "$slots/2"; echo "$a" > "$slots/1/pid"; echo "$b" > "$slots/2/pid"
( jvm_slot_take t; echo "${JVM_SLOT##*/}" > "$work/got" ) 2> "$work/err" &
waiter=$!
for _ in $(seq 1 50); do grep -q 'JVM slots busy' "$work/err" 2>/dev/null && break; sleep 0.05; done
check "every slot busy: waits, saying so with the holders" "yes no" \
  "$(grep -q "all 2 JVM slots busy (KINOWO_JVM_SLOTS; holders: $a,$b)" "$work/err" && echo yes || echo no) $([ -s "$work/got" ] && echo yes || echo no)"
kill "$b"; wait "$b" 2>/dev/null; wait "$waiter"
check "...and a slot whose holder is gone is taken over" "2" "$(cat "$work/got")"
mkdir "$work/pidless" && touch -t 202001010000 "$work/pidless"
check "a lock left with no pid (its holder killed between taking it and writing one), a minute old: taken over" "0 $$" \
  "$(jvm_lock "$work/pidless"; echo "$? $(cat "$work/pidless/pid" 2>/dev/null)")"
mkdir "$work/fresh-pidless"
check "...one just taken, its pid not written yet: still held" "1" "$(jvm_lock "$work/fresh-pidless"; echo $?)"
( KINOWO_JVM_SLOT_MAX_WAIT=0 jvm_slot_take t; echo "${JVM_SLOT:-none}" > "$work/got" ) 2> "$work/err"
check "past the slot deadline: goes ahead without one, warned" "none 1" "$(cat "$work/got") $(grep -c 'starting anyway' "$work/err")"
kill "$a"; wait "$a" 2>/dev/null
( KINOWO_JVM_SLOTS=0 jvm_slot_take t; echo "${JVM_SLOT:-none}" > "$work/got" )
check "KINOWO_JVM_SLOTS=0: no slot taken" "none" "$(cat "$work/got")"

# --- a tool JVM's cores
check "on macOS a tool's JVM is sized for KINOWO_JVM_CPUS cores (default 4)" "-XX:ActiveProcessorCount=4|-XX:ActiveProcessorCount=6" \
  "$(unset KINOWO_JVM_CPUS; KINOWO_JVM_GATE_OS=Darwin jvm_cpu_options)|$(KINOWO_JVM_CPUS=6 KINOWO_JVM_GATE_OS=Darwin jvm_cpu_options)"
check "...on Linux (CI's runner, the JVM's alone) and with KINOWO_JVM_CPUS=0: left to the JVM" "|" \
  "$(unset KINOWO_JVM_CPUS; KINOWO_JVM_GATE_OS=Linux jvm_cpu_options)|$(KINOWO_JVM_CPUS=0 KINOWO_JVM_GATE_OS=Darwin jvm_cpu_options)"

# --- a worktree follows local main's gate
. "$REPO_ROOT/scripts/scratch-git.sh"
root="$work/root"; mkdir -p "$root/scripts"
cp "$REPO_ROOT/scripts/jvm-gate.sh" "$root/scripts/"
scratch_git "$root" init -q && scratch_git "$root" add -A && scratch_git "$root" -c user.email=t@t -c user.name=t commit -qm gate
scratch_git "$root" worktree add -q -b old "$work/wt"
printf '\njvm_cpu_options() { echo from-main; }\n' >> "$root/scripts/jvm-gate.sh"
check "a worktree's gate re-reads the root checkout's (local main's): a fix there reaches running agents" "from-main" \
  "$(KINOWO_JVM_GATE_FOLLOW_MAIN=1 bash -c '. "$1/scripts/jvm-gate.sh"; jvm_cpu_options' _ "$work/wt")"
check "...KINOWO_JVM_GATE_FOLLOW_MAIN=0 keeps the worktree's own" "" \
  "$(KINOWO_JVM_GATE_FOLLOW_MAIN=0 KINOWO_JVM_GATE_OS=Linux bash -c '. "$1/scripts/jvm-gate.sh"; jvm_cpu_options' _ "$work/wt")"
check "...and the root checkout's own gate is read once, not again" "from-main" \
  "$(KINOWO_JVM_GATE_FOLLOW_MAIN=1 bash -c '. "$1/scripts/jvm-gate.sh"; jvm_cpu_options' _ "$root")"

spec_summary
