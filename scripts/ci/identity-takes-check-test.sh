#!/usr/bin/env bash
# identity-takes-check.sh's choice of BASE, against a stub `gh` and a throwaway repository: the newest
# earlier commit whose own check passed (so a red check stays red until fixed or accepted), however far
# back, across pages of history; a check that has only ever failed diffs from the base its first run
# took, never the head's parent; the parent only when the check never ran; a failed history read stops
# the check (exit 2) rather than falling back to the parent; and a range touching no identity code
# passes without resolving anything.
set -euo pipefail

script="$(cd "$(dirname "$0")" && pwd)/identity-takes-check.sh"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
fails=0
check() { if [ "$2" = "$3" ]; then echo "ok   $1"; else echo "FAIL $1: expected '$3', got '$2'"; fails=$((fails + 1)); fi; }

# origin holds c1 <- c2 <- c3, none touching identity code; the job checks out c3 shallowly, as actions/checkout does.
. "$(cd "$(dirname "$0")/../.." && pwd)/scripts/scratch-git.sh"
scratch_repo "$work/origin.git" --bare
scratch_repo "$work/src"
for n in 1 2 3; do
  echo "$n" > "$work/src/README"
  scratch_git "$work/src" add README
  scratch_git "$work/src" commit -q -m "c$n"
done
scratch_git "$work/src" push -q "$work/origin.git" HEAD:refs/heads/main
c1=$(scratch_git "$work/src" rev-parse HEAD~2) c2=$(scratch_git "$work/src" rev-parse HEAD~1) c3=$(scratch_git "$work/src" rev-parse HEAD)
mkdir -p "$work/job"
scratch_git "$work/job" clone -q --depth=1 --branch main "file://$work/origin.git" .

# gh: GraphQL history pages of the commits before c3 -- c2, then (PAGED: on a second page) c1, each with its parent and
# its `identity takes (pl)` state from $work/status-<sha> (absent: none); HISTORY_FAILS fails the read.
c0=0000000000000000000000000000000000000000
mkdir -p "$work/bin"
cat > "$work/bin/gh" <<EOF
#!/usr/bin/env bash
[ -n "\${HISTORY_FAILS:-}" ] && { echo "HTTP 502" >&2; exit 1; }
state() { cat "$work/status-\$1" 2>/dev/null || echo -; }
case "\$*" in
  *"cursor=$c2 0"*) echo "$c1 \$(state $c1) $c0" ;;
  *cursor=*) echo "not a valid cursor" >&2; exit 1 ;; # GitHub's history cursor is "<sha> <offset>", space and all
  *) echo "$c2 \$(state $c2) $c1"
     if [ -n "\${PAGED:-}" ]; then echo "NEXT $c2 0"; else echo "$c1 \$(state $c1) $c0"; fi ;;
esac
EOF
chmod +x "$work/bin/gh"

run() { rm -rf "$work/out"; (cd "$work/job" && PATH="$work/bin:$PATH" GITHUB_REPOSITORY=o/r GITHUB_SHA=$c3 RUNNER_TEMP=$work \
  "$script" pl "$work/out" > "$work/log" 2>&1); }

echo SUCCESS > "$work/status-$c1"
run || true; check "the newest green commit is the base, past a red one" "$(cat "$work/out/base.txt")" "$c1"
check "a range with no identity code passes unresolved" "$(grep -c 'no identity code changed' "$work/out/verdict.txt")" 1

echo FAILURE > "$work/status-$c2"
PAGED=1 run || true; check "a green commit on a later page of history, past a red one" "$(cat "$work/out/base.txt")" "$c1"

rm "$work/status-$c1"
run || true; check "a check that only ever failed diffs from its first run's base, never the head's parent" "$(cat "$work/out/base.txt")" "$c1"

rm "$work/status-$c2"
run || true; check "a check that never ran: the parent" "$(cat "$work/out/base.txt")" "$c2"

code=0; HISTORY_FAILS=1 run || code=$?
check "a failed history read stops the check" "$code" 2

[ "$fails" = 0 ] || { echo "$fails failure(s)"; exit 1; }
