#!/usr/bin/env bash
# The action-run lint (shellcheck-action-runs.sh) against hand-written composite actions: a finding is reported at its
# line in the action file, expressions and single-line runs are read as actionlint reads them.
# Run: bash scripts/ci/shellcheck-action-runs-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m shellcheck-action-runs.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
lint="$REPO_ROOT/scripts/ci/shellcheck-action-runs.sh"

mkdir -p "$work/clean" "$work/dirty"
cat > "$work/clean/action.yml" <<'YML'
name: clean
runs:
    using: composite
    steps:
        - name: Block with an expression in a condition
          shell: bash
          run: |
              set -euo pipefail
              if [ "${{ inputs.flag }}" = "true" ]; then
                  echo "on: ${{ inputs.code }}"
              fi
        - shell: bash
          run: '"$GITHUB_WORKSPACE"/scripts/x.sh ''quoted'' "${{ inputs.code }}"'
        - uses: actions/checkout@v4
          with:
              ref: main
YML
cat > "$work/dirty/action.yml" <<'YML'
name: dirty
runs:
    using: composite
    steps:
        - name: Fine
          shell: bash
          run: echo fine
        - name: A bare `done` after a command, which shellcheck warns of (SC1010)
          shell: bash
          run: |
              echo start

              echo "$RUNNER_TEMP"
              echo done
        - name: After
          shell: bash
          run: echo after
YML

out=$(bash "$lint" "$work/clean/action.yml" 2>&1); status=$?
check "a clean action passes" "0|" "$status|$out"
out=$(bash "$lint" "$work/dirty/action.yml" 2>&1); status=$?
check "a warning fails the lint" "1" "$status"
check "…reported at its line in the action file" "$work/dirty/action.yml:14: warning:" "$(printf '%s\n' "$out" | grep -o '^[^ ]* warning:' | head -1)"
check "…and only the block that has it" "1" "$(printf '%s\n' "$out" | grep -c 'warning')"
out=$(bash "$lint" "$work/clean/action.yml" "$work/dirty/action.yml" 2>&1); status=$?
check "several actions are linted together" "1|1" "$status|$(printf '%s\n' "$out" | grep -c "dirty/action.yml:14:")"
check "the repository's own composite actions are clean" "0" "$(bash "$lint" >/dev/null 2>&1; echo $?)"

spec_summary
