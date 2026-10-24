#!/usr/bin/env bash
# sbt-sources-key.sh: one source set, one hash, whichever job computes it -- so a convergence leg can name the exact
# entry Main's e2e job saved for the same commit. The hash follows a source the set names and ignores one it does not.
set -euo pipefail

script="$(cd "$(dirname "$0")" && pwd)/sbt-sources-key.sh"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
fails=0
check() { if [ "$2" = "$3" ]; then echo "ok   $1"; else echo "FAIL $1: expected '$3', got '$2'"; fails=$((fails + 1)); fi; }

. "$(cd "$(dirname "$0")/../.." && pwd)/scripts/scratch-git.sh"
scratch_repo "$work/repo"
mkdir -p "$work/repo/common/src/main/scala" "$work/repo/worker/src/fixtures/scala" "$work/repo/web/src/main/resources"
echo 'object A' > "$work/repo/common/src/main/scala/A.scala"
echo 'object F' > "$work/repo/worker/src/fixtures/scala/F.scala"
echo 'a' > "$work/repo/web/src/main/resources/a.css"
echo 'x' > "$work/repo/README"
echo 'lazy val root = project' > "$work/repo/build.sbt"
scratch_git "$work/repo" add -A
key() { (cd "$work/repo" && "$script" "$1"); }

e2e=$(key e2e) convergence=$(key convergence)
check "a hash, of sha256's length" "${#e2e}" 64
check "the same tree hashes the same" "$(key e2e)" "$e2e"
check "two sets are two keys" "$([ "$e2e" != "$convergence" ] && echo distinct)" distinct

echo y > "$work/repo/README" && scratch_git "$work/repo" add README
check "a file neither set names moves neither key" "$(key e2e) $(key convergence)" "$e2e $convergence"

echo 'object G' > "$work/repo/worker/src/fixtures/scala/F.scala" && scratch_git "$work/repo" add -A
check "a fixtures source moves both keys: the e2e job compiles worker's fixtures too" \
  "$([ "$(key convergence)" != "$convergence" ] && [ "$(key e2e)" != "$e2e" ] && echo moved)" moved
e2e=$(key e2e) convergence=$(key convergence)

echo 'b' > "$work/repo/web/src/main/resources/a.css" && scratch_git "$work/repo" add -A
check "a web resource moves the e2e key" "$([ "$(key e2e)" != "$e2e" ] && echo moved)" moved
check "...and not the convergence key, which does not name it" "$(key convergence)" "$convergence"

echo 'object B' > "$work/repo/common/src/main/scala/A.scala" && scratch_git "$work/repo" add -A
e2e2=$(key e2e)
check "a main source moves both keys" "$([ "$e2e2" != "$e2e" ] && [ "$(key convergence)" != "$convergence" ] && echo moved)" moved

code=0; (cd "$work/repo" && "$script" nope > /dev/null 2>&1) || code=$?
check "an unknown set is an error, never an empty key" "$code" 2

[ "$fails" = 0 ] || { echo "$fails failure(s)"; exit 1; }
