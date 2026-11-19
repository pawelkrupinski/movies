#!/usr/bin/env bash
# ubuntu-security-via-mirror.sh against a sources directory of its own (APT_SOURCES_DIR, APT_SOURCES_LIST), no sudo.
# Run: bash scripts/ci/ubuntu-security-via-mirror-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
# shellcheck source=scripts/shell-spec.sh
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m ubuntu-security-via-mirror.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

route() {  # route: run the script over $work/d and $work/sources.list; echoes its exit status
  APT_SOURCES_DIR="$work/d" APT_SOURCES_LIST="$work/sources.list" APT_SOURCES_SUDO="" \
    bash "$REPO_ROOT/scripts/ci/ubuntu-security-via-mirror.sh" > "$work/out" 2>&1
  echo $?
}
fresh() { rm -rf "$work/d" "$work/sources.list"; mkdir -p "$work/d"; }
count() { grep -o "$1" "$2" 2>/dev/null | wc -l | tr -d ' '; }  # count <pattern> <file>

# Main #2680: the 26.04 runner image lists security.ubuntu.com (http and https) beside its EC2 archive mirror, apt took
# the security host's copy of new -updates packages, and security.ubuntu.com refused connections and answered 503 for
# the ~10 min of all three Playwright install attempts. The mirror carries every pocket, -security included.
fresh
cat > "$work/d/ubuntu.sources" <<'SOURCES'
Types: deb
URIs: http://us-east-2.ec2.archive.ubuntu.com/ubuntu/
Suites: resolute resolute-updates resolute-backports
Components: main universe restricted multiverse
Signed-By: /usr/share/keyrings/ubuntu-archive-keyring.gpg

Types: deb
URIs: http://security.ubuntu.com/ubuntu/
Suites: resolute-security
Components: main universe restricted multiverse
Signed-By: /usr/share/keyrings/ubuntu-archive-keyring.gpg
SOURCES
printf 'deb https://security.ubuntu.com/ubuntu resolute-updates main\n' > "$work/d/extra.list"
check "security.ubuntu.com is routed to the runner's archive mirror" "0" "$(route)"
check "...in a deb822 stanza" "0|2" \
  "$(count 'security.ubuntu.com' "$work/d/ubuntu.sources")|$(count 'us-east-2.ec2.archive.ubuntu.com' "$work/d/ubuntu.sources")"
check "...keeping that stanza's suite" "1" "$(count 'Suites: resolute-security' "$work/d/ubuntu.sources")"
check "...and in a one-line .list" "deb http://us-east-2.ec2.archive.ubuntu.com/ubuntu resolute-updates main" \
  "$(cat "$work/d/extra.list")"

fresh
printf 'deb http://archive.ubuntu.com/ubuntu resolute main\ndeb http://security.ubuntu.com/ubuntu resolute-security main\n' \
  > "$work/sources.list"
check "the main sources.list is routed too" "0|0" "$(route)|$(count 'security.ubuntu.com' "$work/sources.list")"

# No archive mirror to route to: leave the sources as they are, never leave apt with none.
fresh
printf 'deb http://security.ubuntu.com/ubuntu resolute-security main\n' > "$work/d/only.list"
check "with no archive mirror configured, nothing changes" "0|deb http://security.ubuntu.com/ubuntu resolute-security main" \
  "$(route)|$(cat "$work/d/only.list")"

fresh
printf 'deb http://archive.ubuntu.com/ubuntu resolute main\n' > "$work/d/plain.list"
check "no security.ubuntu.com entry changes nothing" "0|deb http://archive.ubuntu.com/ubuntu resolute main" \
  "$(route)|$(cat "$work/d/plain.list")"

spec_summary
