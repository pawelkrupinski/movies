#!/usr/bin/env bash
# gh-optional.sh against a stub `gh` that answers with STUB_EXIT and STUB_ERR on stderr.
# Run: bash scripts/ci/gh-optional-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m gh-optional.sh\n'

stub_dir="$(mktemp -d)"
trap 'rm -rf "$stub_dir"' EXIT
cat > "$stub_dir/gh" <<'STUB'
#!/usr/bin/env bash
echo "gh stdout noise"
[ -n "${STUB_ERR:-}" ] && echo "$STUB_ERR" >&2
exit "${STUB_EXIT:-0}"
STUB
chmod +x "$stub_dir/gh"

# optional <exit> <stderr>  -> prints "<verdict on stdout>|<exit status>"
optional() {
  local out status
  out=$(STUB_EXIT="$1" STUB_ERR="$2" PATH="$stub_dir:$PATH" bash "$REPO_ROOT/scripts/ci/gh-optional.sh" release download t 2>/dev/null)
  status=$?
  echo "$out|$status"
}

check "a download reads as present, with gh's stdout kept off the verdict" "present|0" "$(optional 0 "")"
check "no matching release asset reads as absent" "absent|0" "$(optional 1 "no assets match the file pattern")"
check "a missing release reads as absent" "absent|0" "$(optional 1 "release not found")"
check "a run without the artifact reads as absent" "absent|0" "$(optional 1 "no artifact matches any of the names or patterns provided")"
check "an expired artifact reads as absent" "absent|0" "$(optional 1 "no valid artifacts found to download")"
check "a 404 run reads as absent" "absent|0" "$(optional 1 "HTTP 404: Not Found (https://api.github.com/repos/o/r/actions/runs/1)")"
check "a refused token is a FAILED read, not absent" "|1" "$(optional 1 "HTTP 403: Resource not accessible by integration")"
check "a 5xx is a FAILED read, not absent" "|1" "$(optional 1 "HTTP 502: Bad Gateway")"
check "a network failure is a FAILED read, not absent" "|1" "$(optional 1 "dial tcp: lookup api.github.com: no such host")"

# The assignment pattern the call sites use stops a `set -e` script on a failed read.
caller=$(STUB_EXIT=1 STUB_ERR="HTTP 500" PATH="$stub_dir:$PATH" bash -e -c \
  "present=\$(bash '$REPO_ROOT/scripts/ci/gh-optional.sh' release download t); echo reached" 2>/dev/null)
check "a set -e caller stops at a failed read instead of carrying on as if absent" "" "$caller"

spec_summary
