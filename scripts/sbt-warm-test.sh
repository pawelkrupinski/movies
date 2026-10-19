#!/usr/bin/env bash
# scripts/sbt-warm: a fresh checkout is seeded from the newest local build of a main commit (never a branch's, never
# its own), the commands reach this checkout's server, and a removed checkout's server is stopped. sbt is a stub.
# Run: bash scripts/sbt-warm-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

scratch="$(mktemp -d)"; scratch="$(cd "$scratch" && pwd -P)"
# Every fake server this spec starts, stopped however it exits. (It once ran `kill "${sleeper:-0}"`: `kill 0`, the WHOLE
# process group — the caller's shell and every spec beside it — whenever it ended before $sleeper was set.)
fakes=()
trap 'kill ${fakes[@]+"${fakes[@]}"} 2>/dev/null; rm -rf "$scratch"' EXIT
export KINOWO_SBT_WARM_STATE="$scratch/state" KINOWO_IDENTITY_CACHE="$scratch/identity"
mkdir -p "$scratch/bin"
# the stub is sbt --client: it runs the commands in the server project/target/active.json names (as sbt does while that
# server lives), starting one in the current checkout when there is none, and logs which sources that server sees
cat > "$scratch/bin/sbt" <<STUB
#!/usr/bin/env bash
printf "%s|%s\\n" "\$(pwd -P)" "\$*" >> "$scratch/sbt-calls"
# ...and which of the lock descriptors (scripts/jvm-gate.sh) it inherited: the server it starts must hold none
printf "%s|%s|%s\\n" "\${SBT_OPTS:-}" "\${KINOWO_JVM_SLOT_HELD:-}" "\$(for fd in 6 7 8; do { : >&\$fd; } 2>/dev/null && printf 'fd%s ' \$fd; done)" >> "$scratch/sbt-env"
active=project/target/active.json
[ -f "\$active" ] || { mkdir -p project/target; echo "{\\"uri\\":\\"local://\$(pwd -P)/sock\\"}" > "\$active"; }
server=\$(sed -n 's|.*"local://\\(.*\\)/sock".*|\\1|p' "\$active")
printf "%s|%s\\n" "\$server" "\$(cat "\$server/worker/src/s" 2>/dev/null)" >> "$scratch/sbt-sources"
STUB
chmod +x "$scratch/bin/sbt"
export PATH="$scratch/bin:$PATH"

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

# 2026-10-19: IntelliJ's own build (JPS) wrote the fixtures config's classes into the root checkout's worker MAIN classes,
# hours after sbt's last compile there; every seed copied them, and on the test classpath they shadowed the real fixtures
# (a stale UnmatchedClusters named a renamed wiring object: NoClassDefFoundError, a unit run hung 50 min). Only what the
# source's Zinc wrote — no newer than its analysis — is seeded; a resource and the analysis itself are kept.
stray="$scratch/stray"; scratch_git "$repo" worktree add -q --detach "$stray" HEAD~1; build "$stray" stray
target="$stray/worker/target/scala-3.9.0"
mkdir -p "$target/classes/tools" "$target/test-zinc"; echo stray > "$target/test-zinc/from"
for f in classes/tools/Owned.class classes/tools/Owned.tasty classes/logback.xml test-classes/T.class; do
  echo x > "$target/$f"; touch -t 202610181900 "$target/$f"; done
touch -t 202610181908 "$target/zinc/from" "$target/test-zinc/from"
for f in classes/tools/Stray.class classes/tools/Stray.tasty classes/logback.xml test-classes/Late.class; do
  echo x > "$target/$f"; touch -t 202610182316 "$target/$f"; done
scratch_git "$repo" worktree add -q -b seeded-from-stray "$scratch/seeded" origin/main
(cd "$scratch/seeded" && KINOWO_SBT_WARM_SEED="$stray" "$REPO_ROOT/scripts/sbt-warm" --seed >/dev/null 2>&1)
check "a seed drops the classes its source's Zinc did not write, keeping the ones it did and every resource" \
  "Owned.class Owned.tasty T.class logback.xml" \
  "$(cd "$scratch/seeded/worker/target/scala-3.9.0" && find classes test-classes -type f -exec basename {} \; | LC_ALL=C sort | tr '\n' ' ' | sed 's/ $//')"
check "...and leaves the source's own build as it was" "Owned.class Owned.tasty Stray.class Stray.tasty" \
  "$(find "$target/classes/tools" -type f -exec basename {} \; | LC_ALL=C sort | tr '\n' ' ' | sed 's/ $//')"
# both built: left, they would be seed sources for the checks below
scratch_git "$repo" worktree remove --force "$scratch/seeded"; scratch_git "$repo" worktree remove --force "$stray"

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

# fake_server's pid once it carries the server's command line (the background shell has exec'd)
await_server() { for _ in $(seq 1 100); do grep -q -- --detach-stdio <<<"$(ps -o args= -p "$1" 2>/dev/null)" && return; sleep 0.1; done; }

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

scratch_git "$repo" worktree add -q -b gated "$scratch/gated" origin/main
out=$(cd "$scratch/gated" && KINOWO_MEMORY_GATE=1 KINOWO_MEMORY_GATE_PROBE='echo 99' KINOWO_MEMORY_GATE_FREE_PROBE='echo 0' KINOWO_MEMORY_GATE_MAX_WAIT=0 \
  "$REPO_ROOT/scripts/sbt-warm" compile 2>&1 >/dev/null)
check "a new server waits on the memory gate while the Mac is paging (deadline 0: warned, then started)" "yes" \
  "$(grep -q '\[sbt-warm\] memory gate: still 99 GB paged out' <<< "$out" && echo yes || echo no)"

scratch_git "$repo" worktree add -q -b slotted "$scratch/slotted" origin/main
: > "$scratch/sbt-env"
(cd "$scratch/slotted" && KINOWO_JVM_SLOTS=4 KINOWO_JVM_CPUS=4 KINOWO_JVM_GATE_OS=Darwin "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
check "a command holds one of the machine's JVM slots, its server sized for KINOWO_JVM_CPUS cores (macOS)" \
  " -XX:ActiveProcessorCount=4|$KINOWO_IDENTITY_CACHE/jvm-slots/1.flock" "$(tail -1 "$scratch/sbt-env" | cut -d'|' -f1,2)"
check "...sbt (and the server it starts, outliving the call) inheriting neither the slot's nor the start lock's descriptor" "" \
  "$(tail -1 "$scratch/sbt-env" | cut -d'|' -f3)"
check "...and gives the slot back when it ends" "free" "$(spec_lock_state "$KINOWO_IDENTITY_CACHE/jvm-slots/1.flock")"

: > "$scratch/sbt-env"
(cd "$scratch/slotted" && KINOWO_JVM_SLOTS=4 KINOWO_JVM_GATE_OS=Darwin "$REPO_ROOT/scripts/sbt-warm" --serve "web/PageTest/runMain tools.FixtureServerMain port" >/dev/null 2>&1)
check "a --serve command (a server running until stopped) holds no JVM slot for its life" "" \
  "$(tail -1 "$scratch/sbt-env" | cut -d'|' -f2)"
check "...and runs the command it serves, not the flag" "--client web/PageTest/runMain tools.FixtureServerMain port" \
  "$(tail -1 "$scratch/sbt-calls" | cut -d'|' -f2)"

spec_summary
