#!/usr/bin/env bash
# hook-beside.sh: <check> runs beside the share's other checks, at once, each told its part; either failing fails the
# job; with nothing to run beside, the command runs as it is.
# Run: bash scripts/ci/hook-beside-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m hook-beside.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
# A fake hook: says which part it was given, and exits $RC_ONLY (the beside run) or $RC_REST (the rest's). Each part
# waits for the other's mark before it ends: only two runs AT ONCE get past it -- one after another, the first waits out
# its bound and says so. Proven so, never by elapsed wall time (shell-spec.sh's HUNG STUBS).
cat > "$work/hook" <<'HOOK'
#!/usr/bin/env bash
part="rest"; [ -n "${PREPUSH_ONLY:-}" ] && part="only"
echo "$part ONLY=${PREPUSH_ONLY:-} ELSEWHERE=${PREPUSH_ELSEWHERE:-} args=$*"
touch "$MARKS/$part"
other=rest; [ "$part" = rest ] && other=only
if [ -n "${WAIT_FOR_OTHER:-}" ]; then
    for _ in $(seq 1 200); do [ -e "$MARKS/$other" ] && break; sleep 0.05; done
    [ -e "$MARKS/$other" ] || echo "$part: NEVER SAW $other"
fi
[ "$part" = only ] && exit "${RC_ONLY:-0}"
exit "${RC_REST:-0}"
HOOK
chmod +x "$work/hook"
run() { # [env...]: hook-beside over the fake hook; its output to $work/out, its exit printed
    rm -rf "$work/marks"; mkdir -p "$work/marks"
    env MARKS="$work/marks" TMPDIR="$work/tmp" "$@" bash "$REPO_ROOT/scripts/ci/hook-beside.sh" scripttests -- \
        "$work/hook" --checks-only base..HEAD > "$work/out" 2>&1
    echo $?
}

code="$(run CHECKS="scripttests sbt actionlint" PREPUSH_ELSEWHERE="oldrefs identity" WAIT_FOR_OTHER=1)"
check "scripttests and the rest's other checks run at once, each told its part" "0 0 1 1" \
  "$code $(grep -c 'NEVER SAW' "$work/out") $(grep -c '^only ONLY=scripttests ' "$work/out") $(grep -c '^rest ONLY= ELSEWHERE=oldrefs identity scripttests args=--checks-only base..HEAD$' "$work/out")"
check "...the beside run's output printed whole after the rest's, never interleaved" "rest only" \
  "$(grep -oE '^(rest|only) ' "$work/out" | tr -d ' ' | tr '\n' ' ' | sed 's/ $//')"
check "...and kept in the job's temp dir, where its failure steps read the logs" "1" \
  "$(grep -c '^only ONLY=scripttests' "$work/tmp/scripttests-beside.log")"

check "a beside run's failure fails the job" "1" "$(run CHECKS="scripttests sbt" RC_ONLY=1)"
check "...as does the rest's" "1" "$(run CHECKS="scripttests sbt" RC_REST=1)"
check "...a failure outranks the other's stall signal" "1" "$(run CHECKS="scripttests sbt" RC_REST=75 RC_ONLY=1)"
check "...and a stall alone is the stall signal" "75" "$(run CHECKS="scripttests sbt" RC_ONLY=75)"

code="$(run CHECKS="sbt actionlint" PREPUSH_ELSEWHERE="oldrefs identity")"
check "no scripttests in the share: the hook runs once, as it is" "0 1 0" \
  "$code $(grep -c '^rest ONLY= ELSEWHERE=oldrefs identity args=' "$work/out") $(grep -c '^only' "$work/out")"
code="$(run CHECKS="scripttests" RC_REST=3)"
check "scripttests alone: nothing to run beside it, the hook once, its exit the job's" "3 1 0" \
  "$code $(grep -c '^rest ' "$work/out") $(grep -c '^only' "$work/out")"

spec_summary
