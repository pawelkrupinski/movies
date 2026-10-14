#!/usr/bin/env bash
# scripts/sbt-warm: a fresh checkout is seeded from the newest local build of a main commit (never a branch's, never
# its own), the commands reach this checkout's server, and a removed checkout's server is stopped. sbt is a stub.
# Run: bash scripts/sbt-warm-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"

scratch="$(mktemp -d)"; scratch="$(cd "$scratch" && pwd -P)"
trap 'kill "${sleeper:-0}" 2>/dev/null; rm -rf "$scratch"' EXIT
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
server=$!
for _ in $(seq 1 100); do pgrep -f 'fake/sbt-launch.jar --detach' >/dev/null && break; sleep 0.1; done
echo 'lazy val x = 1' > "$scratch/other/build.sbt"
(cd "$scratch/other" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
check "a changed build is reloaded first" "--client reload; compile" "$(tail -1 "$scratch/sbt-calls" | cut -d'|' -f2)"
(cd "$scratch/other" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
check "...once" "--client compile" "$(tail -1 "$scratch/sbt-calls" | cut -d'|' -f2)"
kill "$server" 2>/dev/null; wait "$server" 2>/dev/null

# a removed checkout's server is stopped by the next call from anywhere
sleep 300 & sleeper=$!
for f in "$KINOWO_SBT_WARM_STATE"/*.root; do echo "$sleeper" > "${f%.root}.pid"; done
rm -rf "$scratch/fresh"; scratch_git "$repo" worktree prune
(cd "$scratch/other" && "$REPO_ROOT/scripts/sbt-warm" compile >/dev/null 2>&1)
sleep 0.2
check "a removed checkout's server is stopped, and forgotten" "stopped 0" \
  "$(kill -0 "$sleeper" 2>/dev/null && echo running || echo stopped) $(grep -l "$scratch/fresh" "$KINOWO_SBT_WARM_STATE"/*.root 2>/dev/null | wc -l | tr -d ' ')"

spec_summary
