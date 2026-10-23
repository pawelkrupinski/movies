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
# so far, hangs (a stalled mirror: it records its pid in $STUB_HUNG and blocks until killed, so the
# install gets past it only by its own bound) — or fails outright when STUB_APT=broken.
cat > "$stub_dir/sudo" <<'STUB'
#!/usr/bin/env bash
exec "$@"
STUB
cat > "$stub_dir/apt-get" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$STUB_LOG"
calls=$(wc -l < "$STUB_LOG")
[ "${STUB_APT:-}" = broken ] && exit 100
[ "$calls" -le "${STUB_HANG_CALLS:-0}" ] && { echo $$ >> "$STUB_HUNG"; exec sleep 86400; }
exit 0
STUB
# `dpkg-query`: whether the package is installed already -- STUB_INSTALLED says it is, otherwise it is unknown.
cat > "$stub_dir/dpkg-query" <<'STUB'
#!/usr/bin/env bash
[ -n "${STUB_INSTALLED:-}" ] && { echo "install ok installed"; exit 0; }
echo "dpkg-query: no packages found matching ${*: -1}" >&2; exit 1
STUB
chmod +x "$stub_dir/sudo" "$stub_dir/apt-get" "$stub_dir/dpkg-query"
export STUB_LOG="$stub_dir/log" STUB_HUNG="$stub_dir/hung"

# install <hang calls> [broken] -> its exit status (a stall it does not cut off: shell-spec.sh's spec_bounded's 124)
install() {
  : > "$STUB_LOG"; : > "$STUB_HUNG"
  STUB_HANG_CALLS="$1" STUB_APT="${2:-}" PATH="$stub_dir:$PATH" APT_TIMEOUT_SECONDS=1 APT_RETRY_PAUSE_SECONDS=0 \
    spec_bounded bash "$REPO_ROOT/scripts/ci/install-apt-package.sh" socat > "$stub_dir/out" 2>&1
  echo "$?"
}
failed_attempts() { grep -c '^\[apt\] installing socat failed or ran past 1s' "$stub_dir/out"; }

check "a healthy mirror installs at once" "0" "$(install 0)"
check "...with one update and one install" "2" "$(wc -l < "$STUB_LOG" | tr -d ' ')"
# A stall is proven cut off by what follows it, never by elapsed wall time (shell-spec.sh's HUNG STUBS): the stub
# never answers, so the install gets on only through its bound, leaving no apt-get running.
check "a stalled first update is cut off and retried, then installs" "0" "$(install 1)"
check "...after one failed attempt, leaving no apt-get running" "1 0" "$(failed_attempts) $(spec_still_running "$STUB_HUNG")"
check "a mirror that never answers gives up after the bounded attempts" "1" "$(install 99)"
check "...having tried three times, leaving no apt-get running" "3 0" "$(failed_attempts) $(spec_still_running "$STUB_HUNG")"
check "a failing apt-get fails the install" "1" "$(install 0 broken)"
check "a package installed already (the Forgejo job image's socat) is not installed again" "0" "$(STUB_INSTALLED=1 install 0 broken)"
check "...no apt-get run at all" "0" "$(wc -l < "$STUB_LOG" | tr -d ' ')"

spec_summary
