#!/usr/bin/env bash
# drop-microsoft-apt-sources.sh against a sources directory of its own (APT_SOURCES_DIR, APT_SOURCES_LIST), no sudo.
# Run: bash scripts/ci/drop-microsoft-apt-sources-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
# shellcheck source=scripts/shell-spec.sh
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m drop-microsoft-apt-sources.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT

ubuntu_stanza='Types: deb
URIs: http://azure.archive.ubuntu.com/ubuntu/
Suites: resolute resolute-updates
Components: main universe
Signed-By: /usr/share/keyrings/ubuntu-archive-keyring.gpg'
microsoft_stanza='Types: deb
URIs: https://packages.microsoft.com/ubuntu/26.04/prod
Suites: resolute
Components: main'

drop() {  # drop: run the script over $work/d and $work/sources.list; echoes its exit status
  APT_SOURCES_DIR="$work/d" APT_SOURCES_LIST="$work/sources.list" APT_SOURCES_SUDO="" \
    bash "$REPO_ROOT/scripts/ci/drop-microsoft-apt-sources.sh" > "$work/out" 2>&1
  echo $?
}
fresh() { rm -rf "$work/d" "$work/sources.list"; mkdir -p "$work/d"; }
count() { grep -o "$1" "$2" 2>/dev/null | wc -l | tr -d ' '; }  # count <pattern> <file>: 0 for a file that is gone

# Runner image 20261101.64 (Main #2149): apt-get update fetched nothing -- two bare "Reading package lists..." and no
# Hit/Get line -- and every Playwright dependency was "Unable to locate". A sources file holding Ubuntu's archive beside a
# Microsoft stanza was deleted whole, and Ubuntu's archive with it.
fresh
printf '%s\n\n%s\n' "$ubuntu_stanza" "$microsoft_stanza" > "$work/d/ubuntu.sources"
check "a file holding Ubuntu's archive and Microsoft's keeps Ubuntu's" "0" "$(drop)"
check "...Ubuntu's stanza" "1" "$(count 'archive.ubuntu.com' "$work/d/ubuntu.sources")"
check "...and not Microsoft's" "0" "$(count 'packages.microsoft.com' "$work/d/ubuntu.sources")"

fresh
printf '%s\n' "$ubuntu_stanza" > "$work/d/ubuntu.sources"
echo 'deb [arch=amd64] https://packages.microsoft.com/ubuntu/26.04/prod resolute main' > "$work/d/microsoft-prod.list"
echo 'deb https://packages.microsoft.com/repos/azure-cli/ resolute main  # azure-cli' > "$work/d/azure-cli.list"
check "a Microsoft-only file is removed" "0|absent|absent" \
  "$(drop)|$([ -e "$work/d/microsoft-prod.list" ] && echo present || echo absent)|$([ -e "$work/d/azure-cli.list" ] && echo present || echo absent)"
check "...and Ubuntu's file is left as it was" "$ubuntu_stanza" "$(cat "$work/d/ubuntu.sources")"

fresh
printf 'deb http://archive.ubuntu.com/ubuntu resolute main\ndeb https://packages.microsoft.com/ubuntu/26.04/prod resolute main\n' \
  > "$work/d/mixed.list"
check "a one-line-per-source file loses only Microsoft's line" "0|deb http://archive.ubuntu.com/ubuntu resolute main" \
  "$(drop)|$(cat "$work/d/mixed.list")"

fresh
echo 'deb http://archive.ubuntu.com/ubuntu resolute main' > "$work/sources.list"
check "no Microsoft source at all changes nothing" "0|deb http://archive.ubuntu.com/ubuntu resolute main" \
  "$(drop)|$(cat "$work/sources.list")"

# No source left -- the image shipped none, or dropping Microsoft's took the last -- would only show as every package
# "Unable to locate" in a later install: it fails here, by name.
fresh
echo 'deb https://packages.microsoft.com/ubuntu/26.04/prod resolute main' > "$work/d/microsoft-prod.list"
check "no apt source left fails" "1" "$(drop)"
check "...naming it" "1" "$(count '::error::no apt source is left' "$work/out")"

fresh
check "an empty sources directory and no sources.list fail the same way" "1" "$(drop)"

spec_summary
