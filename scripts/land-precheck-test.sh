#!/usr/bin/env bash
# scripts/land-precheck over a scratch repository: the rebase dry run names its conflicts, the hook's commit-rule and
# linter checks run alone (PREPUSH_ONLY) over the range, the registry's lints for the pushed paths run in one sbt-warm
# call, and the verdict is 0 (pass), 1 (a check refused) or 2 (could not check: agentctl queues the change anyway).
# Run: bash scripts/land-precheck-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
. "$REPO_ROOT/scripts/scratch-git.sh"
unset PREPUSH_SKIP PREPUSH_ONLY PREPUSH_ELSEWHERE
testroot="$(mktemp -d)"
trap 'rm -rf "$testroot"' EXIT

# The scratch repository: the precheck itself, a stub hook (its plan modes answer, its checks record what they were
# asked and fail on $HOOK_FAIL) and a stub sbt-warm (records its command, fails on $SBT_FAIL with $SBT_OUT).
origin="$testroot/origin" repo="$testroot/repo" log="$testroot/log"
scratch_repo "$origin" --bare
scratch_repo "$repo"
mkdir -p "$repo/scripts/hooks"
cp "$REPO_ROOT/scripts/land-precheck" "$REPO_ROOT/scripts/tool-conventions.sh" "$repo/scripts/"
cat > "$repo/scripts/hooks/pre-push" <<STUB
#!/usr/bin/env bash
case "\$1" in
    --lint-plan) grep -q '\.scala\$' && printf 'worker:tools.ALintSpec\ncommon:tools.BLintSpec\n'; exit 0 ;;
esac
echo "hook \$* only=\$PREPUSH_ONLY reuse=\${PREPUSH_NO_REUSE:-}" >> "$log"
[ -z "\${HOOK_FAIL:-}" ] || { echo "driftguards: abc123: Drift-Slice missing" >&2; exit 1; }
STUB
cat > "$repo/scripts/sbt-warm" <<STUB
#!/usr/bin/env bash
echo "sbt-warm \$*" >> "$log"
[ -z "\${SBT_OUT:-}" ] || echo "\$SBT_OUT"
[ -z "\${SBT_FAIL:-}" ]
STUB
chmod +x "$repo/scripts/hooks/pre-push" "$repo/scripts/sbt-warm" "$repo/scripts/land-precheck"
printf 'one\n' > "$repo/notes.md"
scratch_git "$repo" add -A; scratch_git "$repo" commit -qm base
scratch_git "$repo" remote add origin "$origin"; scratch_git "$repo" push -q origin HEAD:main
scratch_git "$repo" fetch -q origin
scratch_git "$repo" checkout -qb change
mkdir -p "$repo/worker"; printf 'object A\n' > "$repo/worker/A.scala"
scratch_git "$repo" add -A; scratch_git "$repo" commit -qm "a scala change"

precheck() { (cd "$repo" && scripts/land-precheck > "$testroot/out" 2>&1); echo $?; }

printf '\033[36m▸\033[0m a clean change passes, every step named and timed\n'
: > "$log"
check "exit 0" "0" "$(precheck)"
check "each step reports PASS" "3" "$(grep -cE '^precheck (rebase|rules|lints) PASS \([0-9]+ s\)' "$testroot/out")"
check "the hook runs once, alone on the commit-rule and linter checks, over origin/main..HEAD, never reusing a pass" \
  "1 yes yes" "$(grep -c '^hook' "$log") $(grep -q -- '--checks-only [0-9a-f]*\.\.[0-9a-f]* only=allowlists driftguards newchecks judgements shellcheck actionlint oldrefs reuse=1' "$log" && echo yes || echo no) $(grep -q 'trailers\|redgate' "$log" && echo no || echo yes)"
check "the selected lints run in one sbt-warm call, a testOnly per module" "sbt-warm common/Test/testOnly tools.BLintSpec; worker/Test/testOnly tools.ALintSpec" \
  "$(grep '^sbt-warm' "$log")"

printf '\033[36m▸\033[0m a refusal\n'
: > "$log"
check "a failing commit rule: exit 1, named" "1 1" "$(HOOK_FAIL=1 precheck) $(grep -c 'precheck rules FAIL' "$testroot/out")"
check "...with the hook's own line" "1" "$(grep -c 'Drift-Slice missing' "$testroot/out")"
check "a failing lint: exit 1" "1 1" "$(SBT_FAIL=1 SBT_OUT='[info] *** FAILED *** x' precheck) $(grep -c 'precheck lints FAIL' "$testroot/out")"
check "sbt that could not run (no test failed): exit 2, could not check" "2 1" \
  "$(SBT_FAIL=1 SBT_OUT='sbt server did not start within 90 seconds' precheck) $(grep -c 'precheck lints UNCHECKED' "$testroot/out")"

printf '\033[36m▸\033[0m the rebase dry run\n'
scratch_git "$repo" checkout -q -b other origin/main
printf 'two\n' > "$repo/notes.md"; scratch_git "$repo" commit -qam "main's notes"; scratch_git "$repo" push -q origin HEAD:main
scratch_git "$repo" checkout -q change
printf 'three\n' > "$repo/notes.md"; scratch_git "$repo" commit -qam "the change's notes"
scratch_git "$repo" fetch -q origin
before="$(scratch_git "$repo" rev-parse HEAD)"
check "a conflict with origin/main: exit 1, the file named" "1 1" "$(precheck) $(grep -c 'conflicts with origin/main in: notes.md' "$testroot/out")"
check "...and the branch is left as it was" "$before $(scratch_git "$repo" rev-parse --abbrev-ref HEAD)" \
  "$(scratch_git "$repo" rev-parse HEAD) $(scratch_git "$repo" rev-parse --abbrev-ref HEAD)"
check "...with nothing to do on the tree" "" "$(scratch_git "$repo" status --porcelain)"
spec_summary
