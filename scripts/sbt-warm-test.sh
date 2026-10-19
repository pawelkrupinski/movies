#!/usr/bin/env bash
# scripts/sbt-warm: a fresh checkout is seeded from the newest local build of a main commit (never a branch's, never
# its own), the commands reach this checkout's server, a removed checkout's server is stopped, a live server holds one JVM
# slot through its keeper, and an idle one is stopped by it, never under a running command. sbt is a stub.
# Run: bash scripts/sbt-warm-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"
. "$REPO_ROOT/scripts/scratch-zinc.sh"

scratch="$(mktemp -d)"; scratch="$(cd "$scratch" && pwd -P)"
# Every fake server this spec starts, stopped however it exits. (It once ran `kill "${sleeper:-0}"`: `kill 0`, the WHOLE
# process group — the caller's shell and every spec beside it — whenever it ended before $sleeper was set.)
fakes=()
trap 'kill ${fakes[@]+"${fakes[@]}"} 2>/dev/null; rm -rf "$scratch"' EXIT
export KINOWO_SBT_WARM_STATE="$scratch/state" KINOWO_IDENTITY_CACHE="$scratch/identity"
# the seeds this spec checks are its own scratch builds: a caller's seed (fail-before exports its built checkout) is none
unset KINOWO_SBT_WARM_SEED
# this checkout's gate, not local main's; a server's keeper polling fast, done 2 s after its server is gone
export KINOWO_JVM_GATE_FOLLOW_MAIN=0 KINOWO_SBT_WARM_KEEPER_POLL=0.2 KINOWO_SBT_WARM_KEEPER_GRACE_SECONDS=2
mkdir -p "$scratch/bin"
# the stub is sbt --client: it runs the commands in the server project/target/active.json names (as sbt does while that
# server lives), starting one in the current checkout when there is none, and logs which sources that server sees
cat > "$scratch/bin/sbt" <<STUB
#!/usr/bin/env bash
printf "%s|%s\\n" "\$(pwd -P)" "\$*" >> "$scratch/sbt-calls"
# ...and which of the lock descriptors (scripts/jvm-gate.sh) it inherited: the server it starts must hold none
printf "%s|%s|%s\\n" "\${SBT_OPTS:-}" "\${KINOWO_JVM_SLOT_HELD:-}" "\$(for fd in 5 6 7 8 9; do { : >&\$fd; } 2>/dev/null && printf 'fd%s ' \$fd; done)" >> "$scratch/sbt-env"
active=project/target/active.json
[ -f "\$active" ] || { mkdir -p project/target; echo "{\\"uri\\":\\"local://\$(pwd -P)/sock\\"}" > "\$active"; }
server=\$(sed -n 's|.*"local://\\(.*\\)/sock".*|\\1|p' "\$active")
printf "%s|%s\\n" "\$server" "\$(cat "\$server/worker/src/s" 2>/dev/null)" >> "$scratch/sbt-sources"
# a slow command: whether the server named was still running as it ended
if [ -n "\${STUB_SLOW_SERVER:-}" ]; then
  sleep 3; kill -0 "\$STUB_SLOW_SERVER" 2>/dev/null && echo running > "$scratch/slow" || echo stopped > "$scratch/slow"
fi
# a long command: its call's last-used mark aged past the idle timeout mid-run, then whether it is fresh again
if [ -n "\${STUB_AGE_USED:-}" ]; then
  touch -t 202001010000 "\$STUB_AGE_USED"; sleep 3
  [ -n "\$(find "\$STUB_AGE_USED" -mmin +1)" ] && echo idle > "$scratch/busy" || echo busy > "$scratch/busy"
fi
STUB
chmod +x "$scratch/bin/sbt"
export PATH="$scratch/bin:$PATH"

# fake_server's pid once it carries the server's command line (the background shell has exec'd)
await_server() { for _ in $(seq 1 100); do grep -q -- --detach-stdio <<<"$(ps -o args= -p "$1" 2>/dev/null)" && return; sleep 0.1; done; }

origin="$scratch/origin.git"; repo="$scratch/repo"
scratch_repo "$origin" --bare
scratch_repo "$repo"
mkdir -p "$repo/worker/src"; echo a > "$repo/a"; echo s > "$repo/worker/src/s"; scratch_git "$repo" add -A; scratch_git "$repo" commit -qm one
scratch_git "$repo" remote add origin "$origin"
echo b >> "$repo/a"; scratch_git "$repo" commit -qam two
scratch_git "$repo" push -q origin HEAD:main; scratch_git "$repo" fetch -q origin
# built: an older main commit (worktree), the newest main commit (an identity ref tree), a branch ahead of main
build() { mkdir -p "$1/worker/target/scala-3.9.0/test-classes" "$1/worker/target/scala-3.9.0/zinc" "$1/project/target"
  echo "$2" > "$1/worker/target/scala-3.9.0/zinc/from"
  echo "{\"uri\":\"local://$1/sock\"}" > "$1/project/target/active.json"; }  # its server's socket, as sbt leaves it
scratch_git "$repo" worktree add -q --detach "$scratch/old" HEAD~1; build "$scratch/old" old
mkdir -p "$KINOWO_IDENTITY_CACHE/refs"; build "$KINOWO_IDENTITY_CACHE/refs/$(scratch_git "$repo" rev-parse HEAD)" newest
scratch_git "$repo" worktree add -q -b ahead "$scratch/ahead" HEAD
echo c >> "$scratch/ahead/a"; scratch_git "$scratch/ahead" commit -qam three; build "$scratch/ahead" branch
scratch_git "$repo" worktree add -q -b fresh "$scratch/fresh" origin/main

(cd "$scratch/fresh" && "$REPO_ROOT/scripts/sbt-warm" "worker/testOnly X" "web/compile" >/dev/null 2>&1)
check "a fresh checkout is seeded from the newest build of a main commit" "newest" \
  "$(cat "$scratch/fresh/worker/target/scala-3.9.0/zinc/from" 2>/dev/null)"
check "...its project build too" "yes" "$([ -d "$scratch/fresh/project/target" ] && echo yes || echo no)"
check "the commands go to this checkout's server, as one command line" "$scratch/fresh|--client worker/testOnly X; web/compile" \
  "$(tail -1 "$scratch/sbt-calls")"
check "...and the checkout is kept in the registry" "$scratch/fresh" "$(cat "$KINOWO_SBT_WARM_STATE"/*.root 2>/dev/null)"
# 2026-10-14: the seed copied project/target/active.json, so sbt --client ran in the seed source's live server, against
# that checkout's sources — an edit here was never compiled
echo edited > "$scratch/fresh/worker/src/s"
(cd "$scratch/fresh" && "$REPO_ROOT/scripts/sbt-warm" "worker/test" >/dev/null 2>&1)
check "a seeded checkout's commands run in its own server, against its own edited sources" "$scratch/fresh|edited" \
  "$(tail -1 "$scratch/sbt-sources")"
out=$(cd "$scratch/fresh" && "$REPO_ROOT/scripts/sbt-warm" "worker/test" 2>&1 >/dev/null)
check "each call names the checkout and server it uses" "yes" \
  "$(grep -q "checkout $scratch/fresh, server " <<<"$out" && echo yes || echo "no: $out")"
echo stale > "$scratch/fresh/worker/target/scala-3.9.0/zinc/from"
(cd "$scratch/fresh" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
check "a checkout with compiled output is never seeded over" "stale" "$(cat "$scratch/fresh/worker/target/scala-3.9.0/zinc/from")"
# 2026-10-19: e2e has test sources only, so its Zinc state is test-zinc alone, never zinc: counted unbuilt, every call
# deleted and re-cloned its target from an older build — 12-16 s a call, under a live server's feet
mkdir -p "$scratch/fresh/e2e/src" "$scratch/fresh/e2e/target/scala-3.9.0/test-zinc"
echo mine > "$scratch/fresh/e2e/target/scala-3.9.0/test-zinc/from"
mkdir -p "$KINOWO_IDENTITY_CACHE/refs/$(scratch_git "$repo" rev-parse HEAD)/e2e/target/scala-3.9.0/test-zinc"
(cd "$scratch/fresh" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
check "a test-only module with Zinc state is never seeded over" "mine" \
  "$(cat "$scratch/fresh/e2e/target/scala-3.9.0/test-zinc/from" 2>/dev/null)"

scratch_git "$repo" worktree add -q -b other "$scratch/other" origin/main
(cd "$scratch/other" && KINOWO_SBT_WARM_SEED="$scratch/old" "$REPO_ROOT/scripts/sbt-warm" --seed >/dev/null 2>&1)
# 2026-10-18, GitHub suspended: local main ran two days ahead of origin/main, and every fresh checkout was seeded from an
# origin/main ancestor's build, recompiling those days of commits — the branch's own build is the nearest it contains
scratch_git "$repo" worktree add -q -b past "$scratch/past" ahead
echo d >> "$scratch/past/a"; scratch_git "$scratch/past" commit -qam four
(cd "$scratch/past" && "$REPO_ROOT/scripts/sbt-warm" --seed >/dev/null 2>&1)
check "a checkout ahead of origin/main is seeded from the nearest build its HEAD contains" "branch" \
  "$(cat "$scratch/past/worker/target/scala-3.9.0/zinc/from" 2>/dev/null)"
check "KINOWO_SBT_WARM_SEED names the source instead" "old" "$(cat "$scratch/other/worker/target/scala-3.9.0/zinc/from" 2>/dev/null)"
# 2026-10-19: IntelliJ's JPS build wrote ~400 fixtures classes into the root's worker classes/, no Zinc analysis listing
# them; every checkout seeded from it carried them, and one shadowed fixtures-classes' current class (NoClassDefFoundError
# on a class renamed away). A seed keeps only the products its source's analysis lists.
mkdir -p "$scratch/polluted"; scratch_zinc_build "$scratch/polluted" worker tools/Kept.class
echo orphan > "$scratch/polluted/worker/target/scala-3.9.0/classes/tools/Orphan.class"
echo orphan > "$scratch/polluted/worker/target/scala-3.9.0/classes/tools/Orphan.tasty"
echo resource > "$scratch/polluted/worker/target/scala-3.9.0/classes/logback.xml"
scratch_git "$repo" worktree add -q -b clean "$scratch/clean" origin/main
(cd "$scratch/clean" && KINOWO_SBT_WARM_SEED="$scratch/polluted" "$REPO_ROOT/scripts/sbt-warm" --seed >/dev/null 2>&1)
check "a seed drops the class files its source's analysis never produced, and keeps its products and resources" \
  "Kept.class Kept.tasty logback.xml" \
  "$(cd "$scratch/clean/worker/target/scala-3.9.0/classes" 2>/dev/null && echo tools/* logback.xml | sed 's#tools/##g')"

# a module new since the checkout was seeded (identity/, 2026-10-14) is seeded on its own; the modules built stay
mkdir -p "$KINOWO_IDENTITY_CACHE/refs/$(scratch_git "$repo" rev-parse HEAD)/identity/target/scala-3.9.0/zinc"
echo newest > "$KINOWO_IDENTITY_CACHE/refs/$(scratch_git "$repo" rev-parse HEAD)/identity/target/scala-3.9.0/zinc/from"
mkdir -p "$scratch/other/identity/src"
(cd "$scratch/other" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
check "a module the checkout has not built yet is seeded, the built ones kept" "newest old" \
  "$(cat "$scratch/other/identity/target/scala-3.9.0/zinc/from" 2>/dev/null) $(cat "$scratch/other/worker/target/scala-3.9.0/zinc/from")"

# the build changed under the kept server (a rebase onto a new module): it reloads before running the commands
(cd "$scratch/other" && exec -a "java -jar /fake/sbt-launch.jar --detach-stdio" sleep 300) &
server=$!; fakes+=("$server")
for _ in $(seq 1 100); do pgrep -f 'fake/sbt-launch.jar --detach' >/dev/null && break; sleep 0.1; done
echo 'lazy val x = 1' > "$scratch/other/build.sbt"
(cd "$scratch/other" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
check "a changed build is reloaded first" "--client reload; compile" "$(tail -1 "$scratch/sbt-calls" | cut -d'|' -f2)"
(cd "$scratch/other" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
check "...once" "--client compile" "$(tail -1 "$scratch/sbt-calls" | cut -d'|' -f2)"
kill "$server" 2>/dev/null; wait "$server" 2>/dev/null

# a kept server reads the environment it started with: a call passing other KINOWO_* settings restarts it with them
(cd "$scratch/other" && exec -a "java -jar /fake/sbt-launch.jar --detach-stdio" sleep 300) &
server=$!; fakes+=("$server")
for _ in $(seq 1 100); do pgrep -f 'fake/sbt-launch.jar --detach' >/dev/null && break; sleep 0.1; done
(cd "$scratch/other" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
check "a call with the server's own settings keeps it" "running" "$(kill -0 "$server" 2>/dev/null && echo running || echo stopped)"
out=$(cd "$scratch/other" && KINOWO_IDENTITY_CORPUS_DIR=/corpora "$REPO_ROOT/scripts/sbt-warm" compile 2>&1 >/dev/null)
sleep 0.3
check "a call passing other KINOWO_* settings restarts the server with them" "stopped yes" \
  "$(kill -0 "$server" 2>/dev/null && echo running || echo stopped) $(grep -q "settings differ" <<<"$out" && echo yes || echo "no: $out")"
kill "$server" 2>/dev/null; wait "$server" 2>/dev/null
check "...and records when, why and by whom in the stops log" "1" \
  "$(grep -cE "^[0-9-]{10} [0-9:]{8} server pid=$server checkout=$scratch/other why=\"this call's KINOWO_.* settings differ.*\" by=[0-9]+:.*sbt-warm compile" "$KINOWO_SBT_WARM_STATE/stops.log" 2>/dev/null)"
# ...but never under another call's running command: it waits for that command to end (a restart killed the server under
# the agent's own test run, "sbt server disconnected", whenever a second call passed other settings)
(cd "$scratch/other" && exec -a "java -jar /fake/sbt-launch.jar --detach-stdio" sleep 300) &
server=$!; fakes+=("$server"); await_server "$server"
(cd "$scratch/other" && KINOWO_IDENTITY_CORPUS_DIR=/corpora "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)  # its settings, kept
rm -f "$scratch/slow"
(cd "$scratch/other" && KINOWO_IDENTITY_CORPUS_DIR=/corpora STUB_SLOW_SERVER=$server "$REPO_ROOT/scripts/sbt-warm" test >/dev/null 2>&1) &
slow=$!
sleep 1
out=$(cd "$scratch/other" && "$REPO_ROOT/scripts/sbt-warm" compile 2>&1 >/dev/null)
wait "$slow"
check "a settings restart waits for another call's running command to end" "running yes" \
  "$(cat "$scratch/slow" 2>/dev/null) $(grep -q "waiting for it to end" <<<"$out" && echo yes || echo "no: $out")"
kill "$server" 2>/dev/null; wait "$server" 2>/dev/null
(cd "$scratch/other" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)  # back to the default settings

# 2026-10-15: a seed ran every call (a module that never gets Zinc state) and removed active.json under the LIVE server,
# so each call started a new one — 69 leaked servers. A live server's active.json survives a seeding call...
fake_server() { (cd "$1" && exec -a "java -jar /fake/sbt-launch.jar --detach-stdio" sleep 300) & fakes+=("$!"); }
mkdir -p "$scratch/other/e2e/src"
fake_server "$scratch/other"; server=$!
for _ in $(seq 1 100); do pgrep -f 'fake/sbt-launch.jar --detach' >/dev/null && break; sleep 0.1; done
echo '{"uri":"local://elsewhere/sock"}' > "$scratch/other/project/target/active.json"
(cd "$scratch/other" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
check "a seeding call leaves the live server's active.json alone" "elsewhere" \
  "$(sed -n 's|.*local://\(.*\)/sock.*|\1|p' "$scratch/other/project/target/active.json")"
# ...and several servers in one checkout are stopped, so a call starts exactly one
fake_server "$scratch/other"; second=$!
sleep 0.5
(cd "$scratch/other" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
sleep 0.3
check "several servers in one checkout are all stopped" "stopped stopped" \
  "$(kill -0 "$server" 2>/dev/null && echo running || echo stopped) $(kill -0 "$second" 2>/dev/null && echo running || echo stopped)"
kill "$server" "$second" 2>/dev/null


# two calls finding no server: one starts it, the other waits for it (the stub's call is slow and logs its span)
scratch_git "$repo" worktree add -q -b locked "$scratch/locked" origin/main
cp -R "$scratch/fresh/worker/target" "$scratch/locked/worker/target" 2>/dev/null
cp "$scratch/bin/sbt" "$scratch/sbt.orig"
cat > "$scratch/bin/sbt" <<STUB
#!/usr/bin/env bash
echo "start \$(pwd -P)" >> "$scratch/spans"; sleep 1; echo "end \$(pwd -P)" >> "$scratch/spans"
STUB
(cd "$scratch/locked" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1) &
first=$!
sleep 0.3
(cd "$scratch/locked" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1) &
second=$!
wait "$first" "$second"
check "a second call without a server waits for the first's start instead of overlapping it" "start end start end" \
  "$(sed 's| .*||' "$scratch/spans" | tr '\n' ' ' | sed 's/ $//')"
# a call killed while it held the start lock (kill -9: no trap ran) leaves it free at once
locked_key=$(grep -lx "$scratch/locked" "$KINOWO_SBT_WARM_STATE"/*.root); locked_key=${locked_key%.root}
spec_lock_holder "$locked_key.flock"; dead=$SPEC_LOCK_HOLDER; kill -9 "$dead"; wait "$dead" 2>/dev/null
check "a lock left by a killed call does not block the next" "ok" \
  "$(cd "$scratch/locked" && KINOWO_SBT_WARM_LOCK_SECONDS=0 "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1 && echo ok)"
check "...and the lock is released after the call" "free" "$(spec_lock_state "$locked_key.flock")"
cp "$scratch/sbt.orig" "$scratch/bin/sbt"

# a removed checkout's server is stopped by the next call from anywhere
fresh_key=$(grep -lx "$scratch/fresh" "$KINOWO_SBT_WARM_STATE"/*.root); fresh_key=${fresh_key%.root}
fake_server "$scratch/fresh"; server=$!; await_server "$server"
echo "$server" > "$fresh_key.pid"
rm -rf "$scratch/fresh"; scratch_git "$repo" worktree prune
(cd "$scratch/other" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
sleep 0.2
check "a removed checkout's server is stopped, and forgotten" "stopped 0" \
  "$(kill -0 "$server" 2>/dev/null && echo running || echo stopped) $(grep -l "$scratch/fresh" "$KINOWO_SBT_WARM_STATE"/*.root 2>/dev/null | wc -l | tr -d ' ')"
kill "$server" 2>/dev/null

# ...but a pid file outlives its process, and the pid comes back as SOMEONE ELSE'S: only an sbt server is stopped
echo "$scratch/gone" > "$KINOWO_SBT_WARM_STATE/gone.root"
sleep 300 & bystander=$!; fakes+=("$bystander")
echo "$bystander" > "$KINOWO_SBT_WARM_STATE/gone.pid"
(cd "$scratch/other" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
check "a removed checkout's stale pid, now another process, is not killed" "running" \
  "$(kill -0 "$bystander" 2>/dev/null && echo running || echo stopped)"
kill "$bystander" 2>/dev/null

# --stop and the idle reap stop THIS checkout's server, and only when it has one: `sbt --client shutdown` goes to the
# server active.json names (a seed's source: another worktree's live run), or starts a server just to stop it
scratch_git "$repo" worktree add -q -b idle "$scratch/idle" origin/main
fake_server "$scratch/old"; theirs=$!; await_server "$theirs"
(cd "$scratch/idle" && "$REPO_ROOT/scripts/sbt-warm" --seed >/dev/null 2>&1)
check "a seed leaves no active.json naming the source's server" "absent" \
  "$([ -e "$scratch/idle/project/target/active.json" ] && echo present || echo absent)"
echo "{\"uri\":\"local://$scratch/old/sock\"}" > "$scratch/idle/project/target/active.json"
: > "$scratch/sbt-calls"
(cd "$scratch/idle" && "$REPO_ROOT/scripts/sbt-warm" --stop >/dev/null 2>&1)
idle_key=$(printf '%s' "$scratch/idle" | shasum | cut -c1-12)
echo "$scratch/idle" > "$KINOWO_SBT_WARM_STATE/$idle_key.root"; touch -t 202001010000 "$KINOWO_SBT_WARM_STATE/$idle_key.used"
# --status says when each kept server was last used -- on Linux too, where `stat -f` (BSD's format flag) is GNU's
# "file system" and fails: a `stat` refusing -f stands in for GNU's.
mkdir -p "$scratch/gnu" && printf '#!/bin/sh\ncase "$1" in -f*) exit 1 ;; esac\nexec /usr/bin/stat "$@"\n' > "$scratch/gnu/stat" && chmod +x "$scratch/gnu/stat"
check "--status says when a kept server was last used, whichever stat the machine has" "1" \
  "$(cd "$scratch/idle" && PATH="$scratch/gnu:$PATH" "$REPO_ROOT/scripts/sbt-warm" --status 2>/dev/null | grep -c "kept: $scratch/idle (.*, last used 2020-01-01 00:00)")"
(cd "$scratch/idle" && "$REPO_ROOT/scripts/sbt-warm" --reap >/dev/null 2>&1)
check "--stop and --reap with no server of this checkout ask sbt nothing, and leave another's server running" "0 running" \
  "$(grep -c shutdown "$scratch/sbt-calls") $(kill -0 "$theirs" 2>/dev/null && echo running || echo stopped)"
fake_server "$scratch/idle"; mine=$!; await_server "$mine"
(cd "$scratch/idle" && "$REPO_ROOT/scripts/sbt-warm" --stop >/dev/null 2>&1)
sleep 0.3
check "--stop stops this checkout's own server, and only it" "stopped running" \
  "$(kill -0 "$mine" 2>/dev/null && echo running || echo stopped) $(kill -0 "$theirs" 2>/dev/null && echo running || echo stopped)"
kill "$mine" "$theirs" 2>/dev/null
# The root checkout (not a linked worktree) is where every tool call starts: a bare --stop there is refused, so an
# agent's clean-up run before its `cd` leaves the root's server alone; --stop --root stops it on purpose.
fake_server "$repo"; roots=$!; await_server "$roots"
check "--stop in the root checkout is refused, leaving its server running" "2 running" \
  "$(cd "$repo" && "$REPO_ROOT/scripts/sbt-warm" --stop >/dev/null 2>&1; echo $?) $(kill -0 "$roots" 2>/dev/null && echo running || echo stopped)"
(cd "$repo" && "$REPO_ROOT/scripts/sbt-warm" --stop --root >/dev/null 2>&1)
sleep 0.3
check "...and --stop --root stops it" "stopped" "$(kill -0 "$roots" 2>/dev/null && echo running || echo stopped)"
kill "$roots" 2>/dev/null

# 2026-10-19: the last-used mark was touched only as a call started and ended, so the watchdog (and --reap) stopped the
# server ten idle minutes into any longer command -- `testUnit` ended in "sbt server disconnected" mid-e2e
busy_key=$(printf '%s' "$scratch/slotted" | shasum | cut -c1-12)
scratch_git "$repo" worktree add -q -b slotted "$scratch/slotted" origin/main
(cd "$scratch/slotted" && KINOWO_SBT_WARM_HEARTBEAT_SECONDS=1 STUB_AGE_USED="$KINOWO_SBT_WARM_STATE/$busy_key.used" \
  "$REPO_ROOT/scripts/sbt-warm" testUnit >/dev/null 2>&1)
check "a running command keeps its server marked as in use, however long it runs" "busy" "$(cat "$scratch/busy" 2>/dev/null)"
# ...and so does a call waiting at the memory gate or for a JVM slot (up to 30 minutes) before its command starts: the
# gate's first probe ages the mark, as ten minutes of waiting would, and its last one says whether it is fresh again
used="$KINOWO_SBT_WARM_STATE/$busy_key.used" aged="$scratch/aged-once"
# (once the last call's keeper has ended: while it lives the call takes no slot, so it never reaches the gate)
for _ in $(seq 1 50); do [ "$(spec_lock_state "$KINOWO_SBT_WARM_STATE/$busy_key.keep.flock")" = held ] || break; sleep 0.1; done
(cd "$scratch/slotted" && KINOWO_SBT_WARM_HEARTBEAT_SECONDS=1 KINOWO_MEMORY_GATE=1 KINOWO_MEMORY_GATE_FREE_PROBE='echo 0' \
  KINOWO_MEMORY_GATE_POLL=1 KINOWO_MEMORY_GATE_MAX_WAIT=3 KINOWO_MEMORY_GATE_PROBE="if [ -e '$aged' ]; then
      [ -n \"\$(find '$used' -mmin +1)\" ] && echo idle > '$scratch/waited' || echo busy > '$scratch/waited'
    else touch '$aged' && touch -t 202001010000 '$used'; fi; echo 99" \
  "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
check "...and a call waiting at the gate before its command starts" "busy" "$(cat "$scratch/waited" 2>/dev/null)"

scratch_git "$repo" worktree add -q -b gated "$scratch/gated" origin/main
out=$(cd "$scratch/gated" && KINOWO_MEMORY_GATE=1 KINOWO_MEMORY_GATE_PROBE='echo 99' KINOWO_MEMORY_GATE_FREE_PROBE='echo 0' KINOWO_MEMORY_GATE_MAX_WAIT=0 \
  "$REPO_ROOT/scripts/sbt-warm" compile 2>&1 >/dev/null)
check "a new server waits on the memory gate while the Mac is paging (deadline 0: warned, then started)" "yes" \
  "$(grep -q '\[sbt-warm\] memory gate: still 99 GB paged out' <<< "$out" && echo yes || echo no)"

: > "$scratch/sbt-env"
(cd "$scratch/slotted" && KINOWO_JVM_SLOTS=4 KINOWO_JVM_CPUS=4 KINOWO_JVM_GATE_OS=Darwin "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
check "a command's server is sized for KINOWO_JVM_CPUS cores (macOS)" " -XX:ActiveProcessorCount=4" \
  "$(tail -1 "$scratch/sbt-env" | cut -d'|' -f1)"
check "...sbt (and the server it starts, outliving the call) inheriting no lock's descriptor and no slot's name" "|" \
  "$(tail -1 "$scratch/sbt-env" | cut -d'|' -f2,3)"

: > "$scratch/sbt-env"
(cd "$scratch/slotted" && KINOWO_JVM_GATE_OS=Darwin "$REPO_ROOT/scripts/sbt-warm" --serve "web/PageTest/runMain tools.FixtureServerMain port" >/dev/null 2>&1)
check "a --serve command runs the command it serves, not the flag" "--client web/PageTest/runMain tools.FixtureServerMain port" \
  "$(tail -1 "$scratch/sbt-calls" | cut -d'|' -f2)"

# 2026-10-19: kept servers started with the slot's descriptor closed held no slot, yet each grew to its 4 GB heap: ten
# worktrees' idle servers were ~40 GB the slots never counted (load ~100, 29 of 30 GB of swap). One JVM, one slot: the
# server's keeper holds it for the server's life, and the commands run in the server take none.
scratch_git "$repo" worktree add -q -b counted "$scratch/counted" origin/main
counted_key="$KINOWO_SBT_WARM_STATE/$(printf '%s' "$scratch/counted" | shasum | cut -c1-12)"
one_slot="$KINOWO_IDENTITY_CACHE/jvm-slots/1.flock"
fake_server "$scratch/counted"; server=$!; await_server "$server"
for _ in $(seq 1 50); do [ "$(spec_lock_state "$one_slot")" = free ] && break; sleep 0.1; done  # earlier keepers done
echo "2026-10-19 15:14:01 REAP pid=$server ppid=1 rule=hog age=1h42m" > "$scratch/jvm-reaper.log"
(cd "$scratch/counted" && KINOWO_JVM_SLOTS=1 KINOWO_JVM_REAPER_LOG="$scratch/jvm-reaper.log" "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
keeper=$(cat "$counted_key.keep.flock" 2>/dev/null)
check "a live server holds a JVM slot after its call has ended: its keeper's" "held ${keeper:-none} alive" \
  "$(spec_lock_state "$one_slot") $(cat "$one_slot") $(kill -0 "${keeper:-0}" 2>/dev/null && echo alive || echo gone)"
out=$(cd "$scratch/counted" && KINOWO_JVM_SLOTS=1 KINOWO_JVM_SLOT_MAX_WAIT=0 KINOWO_MEMORY_GATE=0 "$REPO_ROOT/scripts/sbt-warm" compile 2>&1 >/dev/null)
check "...a command on the warm server takes no second slot for the same JVM (with the only slot held, it never waits)" "yes no" \
  "$(grep -q 'this command takes none' <<<"$out" && echo yes || echo "no: $out") $(grep -q 'slots busy\|slot still busy' <<<"$out" && echo yes || echo no)"
kill "$server"; wait "$server" 2>/dev/null
for _ in $(seq 1 50); do kill -0 "${keeper:-0}" 2>/dev/null || break; sleep 0.1; done
check "...and once the server is gone, its keeper ends and the slot is free" "gone free" \
  "$(kill -0 "${keeper:-0}" 2>/dev/null && echo alive || echo gone) $(spec_lock_state "$one_slot")"
# 2026-10-19: two kept servers vanished between calls, nothing logged anywhere sbt-warm wrote (the JVM reaper had killed
# one). A server its keeper saw end without sbt-warm stopping it is a "vanished" line in the stops log, naming what
# else logged its end.
check "...a server ended by something else is logged as vanished, with the JVM reaper's kill of it" "1" \
  "$(grep -c "vanished pid=$server checkout=$scratch/counted why=\"ended without sbt-warm stopping it; .*REAP pid=$server .*rule=hog" "$KINOWO_SBT_WARM_STATE/stops.log" 2>/dev/null)"

# IDLE EXIT: the keeper stops a server idle past KINOWO_SBT_WARM_IDLE_MINUTES, freeing its heap and slot -- but never
# under a running command, even one whose last-used mark has aged past the timeout (the heartbeat slowed here to 100 s)
scratch_git "$repo" worktree add -q -b idler "$scratch/idler" origin/main
idler_key="$KINOWO_SBT_WARM_STATE/$(printf '%s' "$scratch/idler" | shasum | cut -c1-12)"
fake_server "$scratch/idler"; server=$!; await_server "$server"
rm -f "$scratch/busy"
(cd "$scratch/idler" && KINOWO_SBT_WARM_HEARTBEAT_SECONDS=100 STUB_AGE_USED="$idler_key.used" "$REPO_ROOT/scripts/sbt-warm" testUnit >/dev/null 2>&1)
keeper=$(cat "$idler_key.keep.flock" 2>/dev/null)
check "a server whose last-used mark aged past the idle timeout under a running command is not stopped" "idle running" \
  "$(cat "$scratch/busy" 2>/dev/null) $(kill -0 "$server" 2>/dev/null && echo running || echo stopped)"
touch -t 202001010000 "$idler_key.used"
for _ in $(seq 1 50); do kill -0 "$server" 2>/dev/null || break; sleep 0.1; done
wait "$server" 2>/dev/null
for _ in $(seq 1 50); do kill -0 "${keeper:-0}" 2>/dev/null || break; sleep 0.1; done
check "...an idle one is: stopped by its keeper, which ends, its slot free" "stopped gone" \
  "$(kill -0 "$server" 2>/dev/null && echo running || echo stopped) $(kill -0 "${keeper:-0}" 2>/dev/null && echo alive || echo gone)"
: > "$scratch/sbt-calls"
(cd "$scratch/idler" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
check "...and the next call goes ahead with a fresh server and keeper" "1 alive" \
  "$(grep -c "$scratch/idler|--client compile" "$scratch/sbt-calls") $(kill -0 "$(cat "$idler_key.keep.flock" 2>/dev/null || echo 0)" 2>/dev/null && echo alive || echo gone)"

# 2026-10-19: --stop stopped the server but not its keeper, and deleted the lock file naming it. The next call, finding
# no keeper, waited for a JVM slot the old keepers held -- and while it waited it held the checkout's busy lock, the very
# "no call running" the old keepers waited for before ending: a deadlock, broken only by killing them by pid. Its own
# slots here (one), the old keeper given a long grace: a keeper mid-grace is what the next call meets.
scratch_git "$repo" worktree add -q -b restopped "$scratch/restopped" origin/main
restopped_key="$KINOWO_SBT_WARM_STATE/$(printf '%s' "$scratch/restopped" | shasum | cut -c1-12)"
own_slot="$scratch/own-slots/jvm-slots/1.flock"
# the keepers whose working directory is <checkout>, as "<count> <pids>" -- less a keeper's own subshells (each $(...) of
# its poll is a fork carrying its command line: counted, they made one keeper two)
keepers_of() { pgrep -f 'sbt-warm --keep' | while read -r p; do
    ps -o args= -p "$(ps -o ppid= -p "$p" 2>/dev/null | tr -d ' ')" 2>/dev/null | grep -q -- 'sbt-warm --keep' && continue
    [ "$(lsof -a -p "$p" -d cwd -Fn 2>/dev/null | sed -n 's/^n//p')" = "$1" ] && echo "$p"; done | tr '\n' ' ' | awk '{ print NF, $0 } END { if (NR == 0) print 0 }' | sed 's/ *$//'; }
fake_server "$scratch/restopped"; server=$!; await_server "$server"
(cd "$scratch/restopped" && KINOWO_IDENTITY_CACHE="$scratch/own-slots" KINOWO_JVM_SLOTS=1 KINOWO_SBT_WARM_KEEPER_GRACE_SECONDS=60 \
  "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
old_keeper=$(cat "$restopped_key.keep.flock" 2>/dev/null)
(cd "$scratch/restopped" && "$REPO_ROOT/scripts/sbt-warm" --stop >/dev/null 2>&1)
check "--stop stops the checkout's keeper with its server, its JVM slot free" "stopped gone free" \
  "$(kill -0 "$server" 2>/dev/null && echo running || echo stopped) $(kill -0 "${old_keeper:-0}" 2>/dev/null && echo alive || echo gone) $(spec_lock_state "$own_slot")"
out=$(cd "$scratch/restopped" && KINOWO_IDENTITY_CACHE="$scratch/own-slots" KINOWO_JVM_SLOTS=1 KINOWO_JVM_SLOT_MAX_WAIT=4 KINOWO_MEMORY_GATE=0 \
  "$REPO_ROOT/scripts/sbt-warm" compile 2>&1 >/dev/null)
check "...so the next call takes the slot at once, never waiting on a keeper it keeps alive" "no" \
  "$(grep -q 'slots busy\|slot still busy' <<<"$out" && echo "yes: $out" || echo no)"
new_keeper=$(cat "$restopped_key.keep.flock" 2>/dev/null)
fake_server "$scratch/restopped"; server=$!; await_server "$server"
sleep 0.5
check "...and the server it keeps has ONE keeper, the new one" "1 $new_keeper" "$(keepers_of "$scratch/restopped")"
# the idle reap stops a checkout the same way: its keeper too
(cd "$scratch/restopped" && KINOWO_SBT_WARM_IDLE_MINUTES=0 "$REPO_ROOT/scripts/sbt-warm" --reap >/dev/null 2>&1)
check "--reap stops an idle server's keeper with it" "stopped 0" \
  "$(kill -0 "$server" 2>/dev/null && echo running || echo stopped) $(keepers_of "$scratch/restopped")"
kill "$server" 2>/dev/null

spec_summary
