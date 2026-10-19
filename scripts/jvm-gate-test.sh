#!/usr/bin/env bash
# jvm-gate.sh: a JVM start waits while too much memory is paged out (the reading stubbed), then for one of the
# machine's slots; a child rides its parent's slot; a slot (a kernel lock) is free the moment its holder is killed, and
# held by one process at a time however many race for it; each gives up warned at its deadline; a tool's JVM is sized for a share of the cores on macOS only.
# Run: bash scripts/jvm-gate-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
# this checkout's gate, not the root checkout's (local main's) a worktree's copy would otherwise follow
export KINOWO_JVM_GATE_FOLLOW_MAIN=0
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
( jvm_slot_take t; echo "$JVM_SLOT $KINOWO_JVM_SLOT_HELD" > "$work/held"; cat "$slots/1.flock" > "$work/pid1"; echo $$ > "$work/me"
  bash -c '. "'"$REPO_ROOT"'/scripts/jvm-gate.sh"; jvm_slot_take child; echo "${JVM_SLOT:-rode}"' > "$work/child"
  lock_busy "$slots/1.flock" && echo held > "$work/during"
  jvm_slot_release; lock_busy "$slots/1.flock" && echo held > "$work/after" || echo free > "$work/after" )
check "a free slot is taken, held by this process, and named to its children" "$slots/1.flock $slots/1.flock yes held" \
  "$(cat "$work/held") $([ "$(cat "$work/pid1")" = "$(cat "$work/me")" ] && echo yes || echo no) $(cat "$work/during")"
check "...a child tool rides it rather than waiting on its own parent" "rode" "$(cat "$work/child")"
check "...and it is given back on release" "free" "$(cat "$work/after")"

spec_lock_holder "$slots/1.flock"; a=$SPEC_LOCK_HOLDER; spec_lock_holder "$slots/2.flock"; b=$SPEC_LOCK_HOLDER
( jvm_slot_take t; echo "${JVM_SLOT##*/}" > "$work/got" ) 2> "$work/err" &
waiter=$!
for _ in $(seq 1 100); do grep -q 'JVM slots busy' "$work/err" 2>/dev/null && break; sleep 0.05; done
check "every slot busy: waits, saying so with the holders" "yes no" \
  "$(grep -q "all 2 JVM slots busy (KINOWO_JVM_SLOTS; holders: $a,$b)" "$work/err" && echo yes || echo no) $([ -s "$work/got" ] && echo yes || echo no)"
kill -9 "$b"; wait "$b" 2>/dev/null; wait "$waiter"
check "...and a slot whose holder is killed (kill -9: no trap, no clean-up) is free to the waiter at once" "2.flock" "$(cat "$work/got")"
spec_lock_holder "$slots/2.flock"; c=$SPEC_LOCK_HOLDER
( KINOWO_JVM_SLOT_MAX_WAIT=0 jvm_slot_take t; echo "${JVM_SLOT:-none}" > "$work/got" ) 2> "$work/err"
check "past the slot deadline: goes ahead without one, warned" "none 1" "$(cat "$work/got") $(grep -c 'starting anyway' "$work/err")"
kill "$a" "$c"; wait "$a" "$c" 2>/dev/null
spec_lock_holder "$work/killed"; held=$SPEC_LOCK_HOLDER
kill -9 "$held"; wait "$held" 2>/dev/null
check "a lock is free the moment its holder is killed: no take-over, nothing to wait out" "0 $$" \
  "$(jvm_lock "$work/killed"; echo "$? $(cat "$work/killed")")"
spec_lock_holder "$work/live"; live=$SPEC_LOCK_HOLDER
check "...while a live holder's is not taken" "1 busy" "$(jvm_lock "$work/live"; echo "$? $(lock_busy "$work/live" && echo busy)")"
kill "$live"; wait "$live" 2>/dev/null
# many processes racing for one free lock, and for the machine's slots: never two inside at once, every round
for round in 1 2 3 4 5; do
  racers=()
  for _ in $(seq 1 16); do
    ( while [ ! -e "$work/go$round" ]; do :; done
      until jvm_lock "$work/race$round"; do sleep 0.01; done
      echo in >> "$work/inside$round"; sleep 0.05; echo out >> "$work/inside$round"; jvm_unlock "$work/race$round" ) &
    racers+=($!)
  done
  sleep 0.3; touch "$work/go$round"; wait "${racers[@]}"
done
check "16 racers for one lock, five rounds: each inside alone, one after another" "16 16 16 16 16|in,out" \
  "$(for r in 1 2 3 4 5; do grep -c in "$work/inside$r"; done | paste -sd' ' -)|$(cat "$work"/inside? | paste -sd, - | sed 's/\(in,out,\)*in,out$/in,out/')"
: > "$work/slot-log"
racers=()
for _ in $(seq 1 12); do
  ( while [ ! -e "$work/go-slots" ]; do :; done
    KINOWO_JVM_SLOT_POLL=0.02 jvm_slot_take t 2> /dev/null
    echo "+ $JVM_SLOT" >> "$work/slot-log"; sleep 0.2; echo "- $JVM_SLOT" >> "$work/slot-log"; jvm_slot_release ) &
  racers+=($!)
done
sleep 0.3; touch "$work/go-slots"; wait "${racers[@]}"
check "12 racers for 2 slots: never more than 2 holding at once, never one slot twice" "24 2 ok" \
  "$(wc -l < "$work/slot-log" | tr -d ' ') $(awk '/^\+/ { n++; if (n > m) m = n; if (held[$2]++) bad = 1 } /^-/ { n--; held[$2]-- } END { print m, (bad ? "twice" : "ok") }' "$work/slot-log")"
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
