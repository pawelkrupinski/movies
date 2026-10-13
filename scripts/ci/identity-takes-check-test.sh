#!/usr/bin/env bash
# identity-takes-check.sh's choice of BASE, against a stub `gh` and a throwaway repository: the newest
# earlier commit whose own check passed (so a red check stays red until fixed or accepted), else the
# parent; a failed status read stops the check (exit 2) rather than falling back to the parent; and a
# range touching no identity code passes without resolving anything.
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

# gh: the commits API lists c3 c2 c1; statuses come from $work/status-<sha> (absent: none); STATUS_FAILS fails that read.
mkdir -p "$work/bin"
cat > "$work/bin/gh" <<EOF
#!/usr/bin/env bash
case "\$2" in
  *"commits?sha="*) printf '%s\n' "$c2" "$c1" ;;
  */statuses*) sha=\${2#*commits/}; sha=\${sha%%/*}
               [ "\${STATUS_FAILS:-}" = "\$sha" ] && { echo "HTTP 502" >&2; exit 1; }
               cat "$work/status-\$sha" 2>/dev/null || true ;;
  *"commits/$c3") echo "$c2" ;;
esac
EOF
chmod +x "$work/bin/gh"

run() { (cd "$work/job" && PATH="$work/bin:$PATH" GITHUB_REPOSITORY=o/r GITHUB_SHA=$c3 RUNNER_TEMP=$work \
  "$script" pl "$work/out" > "$work/log" 2>&1); }

echo success > "$work/status-$c1"
run; check "the newest green commit is the base, past a red one" "$(cat "$work/out/base.txt")" "$c1"
check "a range with no identity code passes unresolved" "$(grep -c 'no identity code changed' "$work/out/verdict.txt")" 1

rm "$work/status-$c1"
run; check "no green commit: the parent" "$(cat "$work/out/base.txt")" "$c2"

code=0; STATUS_FAILS=$c2 run || code=$?
check "a failed status read stops the check" "$code" 2

[ "$fails" = 0 ] || { echo "$fails failure(s)"; exit 1; }
