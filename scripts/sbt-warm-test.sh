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
  "$(printf '%s' "$out" | grep -q "checkout $scratch/fresh, server " && echo yes || echo "no: $out")"
echo stale > "$scratch/fresh/worker/target/scala-3.9.0/zinc/from"
(cd "$scratch/fresh" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
check "a checkout with compiled output is never seeded over" "stale" "$(cat "$scratch/fresh/worker/target/scala-3.9.0/zinc/from")"

scratch_git "$repo" worktree add -q -b other "$scratch/other" origin/main
(cd "$scratch/other" && KINOWO_SBT_WARM_SEED="$scratch/old" "$REPO_ROOT/scripts/sbt-warm" --seed >/dev/null 2>&1)
check "KINOWO_SBT_WARM_SEED names the source instead" "old" "$(cat "$scratch/other/worker/target/scala-3.9.0/zinc/from" 2>/dev/null)"

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
await_server() { for _ in $(seq 1 100); do ps -o args= -p "$1" 2>/dev/null | grep -q -- --detach-stdio && return; sleep 0.1; done; }

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
(cd "$scratch/idle" && "$REPO_ROOT/scripts/sbt-warm" --reap >/dev/null 2>&1)
check "--stop and --reap with no server of this checkout ask sbt nothing, and leave another's server running" "0 running" \
  "$(grep -c shutdown "$scratch/sbt-calls") $(kill -0 "$theirs" 2>/dev/null && echo running || echo stopped)"
fake_server "$scratch/idle"; mine=$!; await_server "$mine"
(cd "$scratch/idle" && "$REPO_ROOT/scripts/sbt-warm" --stop >/dev/null 2>&1)
sleep 0.3
check "--stop stops this checkout's own server, and only it" "stopped running" \
  "$(kill -0 "$mine" 2>/dev/null && echo running || echo stopped) $(kill -0 "$theirs" 2>/dev/null && echo running || echo stopped)"
kill "$mine" "$theirs" 2>/dev/null

spec_summary
