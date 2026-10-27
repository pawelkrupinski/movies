#!/usr/bin/env bash
# scripts/ci/sbt-guard/sbt against a stand-in sbt: it passes arguments, output and status through, and runs a batch once
# more (after `staleTastyDrop`) only when it failed on a stale TASTy read that the build's own retry did not take.
# Run: bash scripts/ci/sbt-guard-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m sbt-guard\n'

work="$(cd "$(mktemp -d)" && pwd -P)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"

# The stand-in: each call is logged; $FAKE_SBT_SCRIPT names, per call number, what it prints and how it exits.
cat > "$work/bin/sbt" <<'EOF'
#!/usr/bin/env bash
n=$(( $(cat "$FAKE_SBT_DIR/calls" 2>/dev/null || echo 0) + 1 ))
echo "$n" > "$FAKE_SBT_DIR/calls"
echo "$*" >> "$FAKE_SBT_DIR/args"
case "$(sed -n "${n}p" "$FAKE_SBT_DIR/script")" in
  ok)      echo "out $*"; echo "err $*" >&2; exit 0 ;;
  fail)    echo "[error] Compilation failed"; exit 1 ;;
  stale)   echo "[warn] Ignoring analyses of \$anonfun due to error in reading TASTy."; exit 1 ;;
  retried) echo "[warn] Ignoring analyses of x due to error in reading TASTy."
           echo "[warn] [stale TASTy] running the batch again: compile"; exit 1 ;;
  *)       exit 9 ;;
esac
EOF
chmod +x "$work/bin/sbt"
export FAKE_SBT_DIR="$work"

guard() {
  rm -f "$work/calls" "$work/args"
  printf '%s\n' "$@" > "$work/script"
  PATH="$REPO_ROOT/scripts/ci/sbt-guard:$work/bin:$PATH" sbt -batch 'set x := 1' compile > "$work/stdout" 2> "$work/stderr"
  echo $? > "$work/status"
}
calls() { cat "$work/calls" 2>/dev/null || echo 0; }

guard ok
check "a passing run: one call, status 0" "1 0" "$(calls) $(cat "$work/status")"
check "its arguments reach sbt unchanged" "-batch set x := 1 compile" "$(cat "$work/args")"
check "its stdout stays stdout" "out -batch set x := 1 compile" "$(cat "$work/stdout")"
check "its stderr stays stderr" "err -batch set x := 1 compile" "$(cat "$work/stderr")"

guard fail
check "an ordinary failure: one call, its status" "1 1" "$(calls) $(cat "$work/status")"

guard stale ok ok
check "a stale read the build did not retry: drop, then the batch again, passing" "3 0" "$(calls) $(cat "$work/status")"
check "the drop is staleTastyDrop, the rerun the same arguments" \
  "-batch staleTastyDrop|-batch set x := 1 compile" "$(sed -n '2,3p' "$work/args" | paste -sd'|' -)"

guard stale ok fail
check "a rerun that fails again keeps its failure" "3 1" "$(calls) $(cat "$work/status")"

guard retried
check "a stale read the build already retried: left failed, no second retry" "1 1" "$(calls) $(cat "$work/status")"

rm -f "$work/calls" "$work/args"
printf '%s\n' stale ok ok > "$work/script"
PATH="$REPO_ROOT/scripts/ci/sbt-guard:$work/bin:$PATH" sbt --client 'worker/testOnly X' > /dev/null 2>&1
check "a --client run drops through its server, then runs again" \
  "--client worker/testOnly X|--client staleTastyDrop|--client worker/testOnly X" "$(paste -sd'|' - < "$work/args")"

rm -f "$work/calls"
PATH="$REPO_ROOT/scripts/ci/sbt-guard:/usr/bin:/bin" sbt compile > /dev/null 2>&1
check "no real sbt behind it: 127" "127" "$?"

spec_summary
