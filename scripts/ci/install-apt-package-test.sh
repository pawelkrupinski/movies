#!/usr/bin/env bash
# install-apt-package.sh against a stub `sudo` + `apt-get` that hang or fail on cue.
# Run: bash scripts/ci/install-apt-package-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m install-apt-package.sh\n'

stub_dir="$(mktemp -d)"
trap 'rm -rf "$stub_dir"' EXIT
# `sudo` just runs its command; `apt-get` logs each call and, while STUB_HANG_CALLS > calls
# so far, hangs (a stalled mirror) — or fails outright when STUB_APT=broken.
cat > "$stub_dir/sudo" <<'STUB'
#!/usr/bin/env bash
exec "$@"
STUB
cat > "$stub_dir/apt-get" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$STUB_LOG"
calls=$(wc -l < "$STUB_LOG")
[ "${STUB_APT:-}" = broken ] && exit 100
[ "$calls" -le "${STUB_HANG_CALLS:-0}" ] && sleep 30
exit 0
STUB
chmod +x "$stub_dir/sudo" "$stub_dir/apt-get"
export STUB_LOG="$stub_dir/log"

# install <hang calls> [broken] -> "<exit status> <seconds>"
install() {
  : > "$STUB_LOG"
  local started=$SECONDS
  STUB_HANG_CALLS="$1" STUB_APT="${2:-}" PATH="$stub_dir:$PATH" APT_TIMEOUT_SECONDS=1 APT_RETRY_PAUSE_SECONDS=0 \
    bash "$REPO_ROOT/scripts/ci/install-apt-package.sh" socat > "$stub_dir/out" 2>&1
  echo "$? $((SECONDS - started))"
}

check "a healthy mirror installs at once" "0" "$(install 0 | cut -d' ' -f1)"
check "...with one update and one install" "2" "$(wc -l < "$STUB_LOG" | tr -d ' ')"
result="$(install 1)"
check "a stalled first update is cut off and retried, then installs" "0" "${result%% *}"
check "...well inside the stall it would have waited out" "true" "$([ "${result##* }" -lt 10 ] && echo true || echo false)"
check "a mirror that never answers gives up after the bounded attempts" "1" "$(install 99 | cut -d' ' -f1)"
check "...having tried three times" "3" "$(wc -l < "$STUB_LOG" | tr -d ' ')"
check "a failing apt-get fails the install" "1" "$(install 0 broken | cut -d' ' -f1)"

spec_summary
