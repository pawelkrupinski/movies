#!/usr/bin/env bash
# scripts/ci/dispatch-takes.sh against a stub `gh`: convergence's preflight dispatches Identity takes on its own ref with
# the takes-meaningful inputs it was given and its run id as parent-run, passes no empty input, and fails when GitHub
# refuses it.
# Run: bash scripts/ci/dispatch-takes-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m dispatch-takes.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin"
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$WORK/calls"
[ -z "${FAIL:-}" ] || { echo "$FAIL" >&2; exit 1; }
STUB
chmod +x "$work/bin/gh"
export PATH="$work/bin:$PATH" WORK="$work" DISPATCH_RETRY_SECONDS=0
run() { : > "$work/calls"; env "$@" "$REPO_ROOT/scripts/ci/dispatch-takes.sh" main 42 > "$work/out" 2>&1; echo $?; }

check "a dispatch naming nothing hands only parent-run" "0" "$(run)"
check "...on the convergence run's ref" "workflow run identity-takes.yml --ref main -f parent-run=42" "$(cat "$work/calls")"
check "the takes inputs are forwarded, empty ones left out" "0" \
  "$(run INPUT_TAKES_BASE=abc INPUT_TAKES_SKIP=shadow INPUT_DRIFT_TWICE=true INPUT_REUSE_RUN=7 INPUT_JVM_OPTIONS= INPUT_RECORDING=99)"
check "...each as identity-takes.yml names it" \
  "workflow run identity-takes.yml --ref main -f parent-run=42 -f takes-base=abc -f recording=99 -f takes-skip=shadow -f drift-twice=true -f reuse-run=7" \
  "$(cat "$work/calls")"
check "a refusal fails the step" "1" "$(run FAIL='HTTP 403: Resource not accessible')"
check "...saying the run has no takes run beside it" "true" "$(grep -q 'no takes run' "$work/out" && echo true || echo false)"

# every forwarded input is one both workflows declare
for name in $(sed -n 's/^FORWARDED="\(.*\)"$/\1/p' "$REPO_ROOT/scripts/ci/dispatch-takes.sh"); do
  for wf in identity-model-convergence.yml identity-takes.yml; do
    check "$wf declares the forwarded input $name" "true" \
      "$(grep -Eq "^            $name:\$" "$REPO_ROOT/.github/workflows/$wf" && echo true || echo false)"
  done
done
spec_summary
