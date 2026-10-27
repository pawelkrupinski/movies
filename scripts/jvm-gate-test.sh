#!/usr/bin/env bash
# jvm-gate.sh: a JVM start waits while too much memory is paged out (the reading stubbed), then for one of the
# machine's slots; a child rides its parent's slot; a slot (a kernel lock) is free the moment its holder is killed, and
# held by one process at a time however many race for it; neither goes ahead on time alone while memory is short (the
# memory deadline only when set, the slot deadline only when memory is fine), and a riding child waits on neither; a
# tool's JVM is sized for a share of the cores on macOS only; a long-lived JVM's keeper holds the slot handed to it.
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
check "...the deadline named: KINOWO_MEMORY_GATE_MAX_WAIT is the escape hatch" "1" \
  "$(grep -c 'starting anyway (KINOWO_MEMORY_GATE_MAX_WAIT=0)' "$work/err")"
probe 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48 48
KINOWO_MEMORY_GATE_POLL=0.001 KINOWO_MEMORY_GATE_MAX_WAIT=5 memory_gate t 2> "$work/err"
check "...KINOWO_MEMORY_GATE_MAX_WAIT=<s>: goes ahead after that long" "6 1" \
  "$(wc -l < "$work/calls" | tr -d ' ') $(grep -c 'starting anyway (KINOWO_MEMORY_GATE_MAX_WAIT=5)' "$work/err")"
# 2026-10-19: under sustained pressure the 600 s deadline let every queued tool start anyway (load ~100, 29 of 30 GB of
# swap, 0.1 GB free): while memory is short, time alone never opens the gate
# (a probe of shell builtins only: 650 polls of the file-reading one take half a minute)
echo 0 > "$work/count"
# shellcheck disable=SC2016 # the probe's $n and $((...)) expand when the gate evaluates it
KINOWO_MEMORY_GATE_PROBE='read -r n < "'"$work"'/count"; echo $((n + 1)) > "'"$work"'/count"; [ "$n" -lt 650 ] && echo 48 || echo 12' \
  KINOWO_MEMORY_GATE_POLL=0.001 KINOWO_MEMORY_GATE_STATUS_SECONDS=200 memory_gate t 2> "$work/err"
check "no deadline set: waits as long as memory is short (650 polls past the old 600 s), then opens" "651 0 1" \
  "$(cat "$work/count") $(grep -c 'starting anyway' "$work/err") $(grep -c 'open after 650s' "$work/err")"
check "...saying how to go ahead, then a status line every KINOWO_MEMORY_GATE_STATUS_SECONDS: paged, free, waited, biggest" "1 3" \
  "$(grep -c 'KINOWO_MEMORY_GATE_MAX_WAIT=<seconds> goes ahead' "$work/err") $(grep -c 'still waiting after [0-9]*s: 48 GB paged out, 1 GB free; biggest: ' "$work/err")"

probe 48
KINOWO_MEMORY_GATE=0 memory_gate t 2> "$work/err"
check "KINOWO_MEMORY_GATE=0: off, the reading never taken" "0" "$(wc -l < "$work/calls" | tr -d ' ')"

probe ""
memory_gate t 2> "$work/err"; status=$?
check "nothing readable (Linux CI): starts at once" "0|" "$status|$(cat "$work/err")"

# --- the machine's JVM slots
probe_cmd=$KINOWO_MEMORY_GATE_PROBE; unset KINOWO_MEMORY_GATE_PROBE; export KINOWO_MEMORY_GATE=0 KINOWO_IDENTITY_CACHE="$work/cache" KINOWO_JVM_SLOTS=2 KINOWO_JVM_SLOT_POLL=0.05
slots="$work/cache/jvm-slots"
( jvm_slot_take t; echo "$JVM_SLOT $KINOWO_JVM_SLOT_HELD" > "$work/held"; cat "$slots/1.flock" > "$work/pid1"; echo $$ > "$work/me"
  bash -c '. "'"$REPO_ROOT"'/scripts/jvm-gate.sh"; jvm_slot_take child; echo "${JVM_SLOT:-rode}"' > "$work/child"
  lock_busy "$slots/1.flock" && echo held > "$work/during"
  jvm_slot_release; lock_busy "$slots/1.flock" && echo held > "$work/after" || echo free > "$work/after" )
check "a free slot is taken, held by this process, and named to its children" "$slots/1.flock $slots/1.flock yes held" \
  "$(cat "$work/held") $([ "$(cat "$work/pid1")" = "$(cat "$work/me")" ] && echo yes || echo no) $(cat "$work/during")"
check "...a child tool rides it rather than waiting on its own parent" "rode" "$(cat "$work/child")"
# a child riding its parent's slot never waits on memory either: the memory it would wait for is its parent's JVM's
( jvm_slot_take t
  KINOWO_MEMORY_GATE=1 KINOWO_MEMORY_GATE_PROBE='echo 99' KINOWO_MEMORY_GATE_FREE_PROBE='echo 0' KINOWO_MEMORY_GATE_MAX_WAIT=1 \
    bash -c '. "'"$REPO_ROOT"'/scripts/jvm-gate.sh"; memory_gate child; jvm_slot_take child' 2> "$work/child-err"
  jvm_slot_release )
check "...nor on memory, short as it is (no deadlock against its own parent)" "" "$(cat "$work/child-err")"
check "...and it is given back on release" "free" "$(cat "$work/after")"
# a process started with the lock descriptors closed (an sbt server, a kept JVM: jvm-gate.sh) still has the name in its
# environment, but holds no slot: it takes one of its own rather than riding one it does not hold
( jvm_slot_take t
  bash -c '. "'"$REPO_ROOT"'/scripts/jvm-gate.sh"; jvm_slot_take outliving; echo "${JVM_SLOT:-rode}"' 7>&- > "$work/outliving"
  jvm_slot_release )
check "...a child started with the slot's descriptor closed rides nothing: it takes a slot of its own" "$slots/2.flock" \
  "$(cat "$work/outliving")"

spec_lock_holder "$slots/1.flock"; a=$SPEC_LOCK_HOLDER; spec_lock_holder "$slots/2.flock"; b=$SPEC_LOCK_HOLDER
( jvm_slot_take t; echo "${JVM_SLOT##*/}" > "$work/got" ) 2> "$work/err" &
waiter=$!
wait_until "the waiter said every slot is busy" grep -q 'JVM slots busy' "$work/err"
check "every slot busy: waits, saying so with the holders" "yes no" \
  "$(grep -q "all 2 JVM slots busy (KINOWO_JVM_SLOTS; holders: $a,$b)" "$work/err" && echo yes || echo no) $([ -s "$work/got" ] && echo yes || echo no)"
kill -9 "$b"; wait "$b" 2>/dev/null; wait "$waiter"
check "...and a slot whose holder is killed (kill -9: no trap, no clean-up) is free to the waiter at once" "2.flock" "$(cat "$work/got")"
spec_lock_holder "$slots/2.flock"; c=$SPEC_LOCK_HOLDER
( KINOWO_JVM_SLOT_MAX_WAIT=0 jvm_slot_take t; echo "${JVM_SLOT:-none}" > "$work/got" ) 2> "$work/err"
check "past the slot deadline: goes ahead without one, warned" "none 1" "$(cat "$work/got") $(grep -c 'starting anyway' "$work/err")"
# ...but not while memory is short: a slot holder may be hung, yet another JVM is what memory cannot take. Memory reads
# fine at the gate (5 GB paged), then short (99) at the slot deadline: it waits on until the slot frees
readings=(5); for _ in $(seq 1 400); do readings+=(99); done
probe "${readings[@]}"
( KINOWO_MEMORY_GATE=1 KINOWO_MEMORY_GATE_PROBE="$probe_cmd" KINOWO_MEMORY_GATE_FREE_PROBE='echo 0' KINOWO_JVM_SLOT_MAX_WAIT=0 \
    KINOWO_MEMORY_GATE_STATUS_SECONDS=1 jvm_slot_take t; echo "${JVM_SLOT##*/}" > "$work/got" ) 2> "$work/err" &
waiter=$!
wait_until "the waiter said it is still waiting" grep -q 'still waiting' "$work/err"
kill "$c"; wait "$c" 2>/dev/null; wait "$waiter"
check "...but while memory is short it keeps waiting past the slot deadline, saying why, until a slot frees" "2.flock 0 yes" \
  "$(cat "$work/got") $(grep -c 'starting anyway' "$work/err") $(grep -q 'past KINOWO_JVM_SLOT_MAX_WAIT=0, but 99 GB paged out, 0 GB free: still waiting' "$work/err" && echo yes || echo no)"
kill "$a"; wait "$a" 2>/dev/null
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
  for i in $(seq 1 16); do
    ( touch "$work/ready$round.$i"; while [ ! -e "$work/go$round" ]; do :; done
      until jvm_lock "$work/race$round"; do sleep 0.01; done
      echo in >> "$work/inside$round"; sleep 0.05; echo out >> "$work/inside$round"; jvm_unlock "$work/race$round" ) &
    racers+=($!)
  done
  wait_until "every racer for round $round is ready" '[ "$(ls "$work"/ready$round.* 2>/dev/null | wc -l)" -eq 16 ]'
  touch "$work/go$round"; wait "${racers[@]}"
done
check "16 racers for one lock, five rounds: each inside alone, one after another" "16 16 16 16 16|in,out" \
  "$(for r in 1 2 3 4 5; do grep -c in "$work/inside$r"; done | paste -sd' ' -)|$(cat "$work"/inside? | paste -sd, - | sed 's/\(in,out,\)*in,out$/in,out/')"
: > "$work/slot-log"
racers=()
for i in $(seq 1 12); do
  ( touch "$work/ready-slots.$i"; while [ ! -e "$work/go-slots" ]; do :; done
    KINOWO_JVM_SLOT_POLL=0.02 jvm_slot_take t 2> /dev/null
    echo "+ $JVM_SLOT" >> "$work/slot-log"; sleep 0.2; echo "- $JVM_SLOT" >> "$work/slot-log"; jvm_slot_release ) &
  racers+=($!)
done
wait_until "every slot racer is ready" '[ "$(ls "$work"/ready-slots.* 2>/dev/null | wc -l)" -eq 12 ]'
touch "$work/go-slots"; wait "${racers[@]}"
check "12 racers for 2 slots: never more than 2 holding at once, never one slot twice" "24 2 ok" \
  "$(wc -l < "$work/slot-log" | tr -d ' ') $(awk '/^\+/ { n++; if (n > m) m = n; if (held[$2]++) bad = 1 } /^-/ { n--; held[$2]-- } END { print m, (bad ? "twice" : "ok") }' "$work/slot-log")"
( KINOWO_JVM_SLOTS=0 jvm_slot_take t; echo "${JVM_SLOT:-none}" > "$work/got" )
check "KINOWO_JVM_SLOTS=0: no slot taken" "none" "$(cat "$work/got")"

# --- a long-lived JVM's keeper (sbt-warm's kept server, identity-slice's --serve JVM): the slot handed to a process its
# taker starts, held past the taker for the keeper's whole life, and free the moment the keeper ends
export KINOWO_IDENTITY_CACHE="$work/keeper-cache"; slots="$work/keeper-cache/jvm-slots"
( jvm_slot_take t
  jvm_slot_keeper_start "$work" "$work/keeper.log" bash -c 'echo "in $PWD"; { : >&6; } 2>/dev/null && echo "holds 6"; exec sleep 60' 6>> "$work/six"
  echo "$KEEPER_PID ${JVM_SLOT:-handed}" > "$work/keeper" )
read -r keeper handed < "$work/keeper"
wait_until "the keeper's command wrote its log" '[ -s "$work/keeper.log" ]'
check "a keeper holds the slot handed to it after its taker has gone, named in it, run in its directory, no other lock" \
  "held $keeper alive handed|in $work" \
  "$(spec_lock_state "$slots/1.flock") $(cat "$slots/1.flock") $(kill -0 "$keeper" 2>/dev/null && echo alive || echo gone) $handed|$(cat "$work/keeper.log")"
kill "$keeper"; wait_until "the keeper has ended" '! kill -0 "$keeper" 2>/dev/null'
check "...and the slot is free once the keeper has ended" "free" "$(spec_lock_state "$slots/1.flock")"
( jvm_slot_take t; jvm_slot_take_if_free && echo second || echo riding ) > "$work/got"
check "a keeper's poll takes no second slot while it holds one" "riding" "$(cat "$work/got")"
spec_lock_holder "$slots/1.flock"; a=$SPEC_LOCK_HOLDER; spec_lock_holder "$slots/2.flock"; b=$SPEC_LOCK_HOLDER
( jvm_slot_take_if_free && echo took || echo none; kill "$b"; wait "$b" 2>/dev/null
  for _ in $(seq 1 200); do jvm_slot_take_if_free && { echo "took ${JVM_SLOT##*/}"; break; }; sleep 0.05; done ) > "$work/got"
check "...and one started without a slot takes the first that frees" "none|took 2.flock" "$(paste -sd'|' "$work/got")"
( KINOWO_JVM_SLOTS=0 jvm_slot_take_if_free && echo took || echo none ) > "$work/got"
check "...none with KINOWO_JVM_SLOTS=0" "none" "$(cat "$work/got")"
kill "$a" 2>/dev/null

# --- THE GOVERNOR (~/bin/agentctl gov): a stub that records its call and takes the lock on the descriptor it was given
cat > "$work/agentctl" <<'STUB'
#!/usr/bin/env bash
# def cmd_gov: the line an agentctl with the governor has
printf '%s\n' "$*" >> "$GOV_CALLS"
case " $* " in *" --try "*) [ -n "${GOV_STUB_FULL:-}" ] && exit 1 ;; esac
python3 -I -c 'import fcntl; fcntl.flock(7, fcntl.LOCK_EX | fcntl.LOCK_NB)'
STUB
chmod +x "$work/agentctl"
export GOV_CALLS="$work/gov-calls"
governed() { KINOWO_GOVERNOR=1 KINOWO_GOVERNOR_BIN="$work/agentctl" KINOWO_JVM_GATE_OS=Darwin AGENTCTL_GOV_DIR="$work/gov" \
  KINOWO_JVM_SLOTS=1 KINOWO_IDENTITY_CACHE="$work/gov-slots" "$@"; }
: > "$GOV_CALLS"
( export KINOWO_MEMORY_GATE=1 KINOWO_MEMORY_GATE_PROBE='echo 48' KINOWO_MEMORY_GATE_FREE_PROBE='echo 1'
  unset KINOWO_GOV_CLASS
  governed jvm_slot_take sbt-warm 2> "$work/err"
  echo "$(spec_lock_state "$JVM_SLOT") $([ "$KINOWO_JVM_SLOT_HELD" = "$JVM_SLOT" ] && echo named) ${JVM_SLOT%/*}" ) > "$work/got"
check "with the governor a JVM takes its ticket on descriptor 7, held by the shell, named for its children" \
  "held named $work/gov" "$(cat "$work/got")"
check "...admitted as an agent's, its label and expected GB passed; the memory gate leaves memory to it" \
  "1 1 0" "$(grep -c -- '^gov admit --fd 7 --ticket .* --class agent --mem 5 --label sbt-warm --pid ' "$GOV_CALLS") \
$(wc -l < "$GOV_CALLS" | tr -d ' ') $(grep -c 'memory gate' "$work/err")"
check "...the slots untouched" "0" "$(find "$work/gov-slots" -name '*.flock' 2>/dev/null | wc -l | tr -d ' ')"
: > "$GOV_CALLS"
( governed jvm_slot_take t; ( governed jvm_slot_take child; echo "child $(jvm_slot_riding && echo rides)" ) ) > "$work/got"
check "...a child of a ticket's holder rides it: no second ticket" "child rides 1" "$(cat "$work/got") $(wc -l < "$GOV_CALLS" | tr -d ' ')"
: > "$GOV_CALLS"
( unset KINOWO_GOV_CLASS
  governed jvm_slot_take identity-measure-local; jvm_slot_release
  KINOWO_GOV_CLASS=check governed jvm_slot_take sbt-warm; jvm_slot_release
  mkdir -p "$work/movies-wt-red-2059" && cd "$work/movies-wt-red-2059" && governed jvm_slot_take sbt-warm ) 2> /dev/null
check "...classes: identity-measure* a measurement, KINOWO_GOV_CLASS (a check's hook) as set, a red fixer's checkout red" \
  "measure check red" "$(sed -n 's/.* --class \([a-z]*\) .*/\1/p' "$GOV_CALLS" | paste -sd' ' -)"
: > "$GOV_CALLS"
( GOV_STUB_FULL=1 governed jvm_slot_take_if_free && echo took || echo "none $({ : >&7; } 2>/dev/null && echo open || echo closed)"
  ) > "$work/got"
check "...a keeper's poll is one look (--try): not admitted, it holds nothing" "none closed 1" \
  "$(cat "$work/got") $(grep -c -- '--label keeper .*--try' "$GOV_CALLS")"
: > "$GOV_CALLS"
( KINOWO_GOVERNOR=0 KINOWO_GOVERNOR_BIN="$work/agentctl" KINOWO_JVM_GATE_OS=Darwin KINOWO_JVM_SLOTS=1 \
    KINOWO_IDENTITY_CACHE="$work/gov-slots" jvm_slot_take t; echo "${JVM_SLOT##*/}"
  KINOWO_GOVERNOR_BIN="$work/agentctl" KINOWO_JVM_GATE_OS=Linux governor_bin || echo linux-none ) > "$work/got"
printf '#!/bin/sh\nexit 2\n' > "$work/old-agentctl"; chmod +x "$work/old-agentctl"
KINOWO_GOVERNOR_BIN="$work/old-agentctl" KINOWO_JVM_GATE_OS=Darwin governor_bin || echo old-none >> "$work/got"
check "...KINOWO_GOVERNOR=0 keeps the slots; on Linux (CI) there is no governor; nor with an agentctl without one" "1.flock|linux-none|old-none|0" \
  "$(paste -sd'|' "$work/got")|$(wc -l < "$GOV_CALLS" | tr -d ' ')"

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
