#!/usr/bin/env bash
# install-playwright.sh against a stub `sudo`/`npx` and a throwaway apt sources directory.
# Run: bash scripts/ci/install-playwright-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m install-playwright.sh\n'

stub_dir="$(mktemp -d)"
trap 'rm -rf "$stub_dir"' EXIT
printf '#!/usr/bin/env bash\n"$@"\n' > "$stub_dir/sudo"
printf '#!/usr/bin/env bash\nprintf "%%s\\n" "$*" >> "$STUB_LOG"\n' > "$stub_dir/npx"
chmod +x "$stub_dir/sudo" "$stub_dir/npx"
export STUB_LOG="$stub_dir/log"

sources="$stub_dir/sources.list.d"
mkdir -p "$sources"
echo 'deb [arch=amd64] https://packages.microsoft.com/ubuntu/24.04/prod noble main' > "$sources/microsoft-prod.list"
printf 'Types: deb\nURIs: https://packages.microsoft.com/repos/azure-cli/\n' > "$sources/azure-cli.sources"
echo 'deb http://ppa.launchpadcontent.net/git-core/ppa/ubuntu noble main' > "$sources/git-core.list"

PATH="$stub_dir:$PATH" APT_SOURCES_DIR="$sources" \
  bash "$REPO_ROOT/scripts/ci/install-playwright.sh" chromium webkit > "$stub_dir/out" 2>&1
status=$?

check "the install succeeds" "0" "$status"
check "a packages.microsoft.com .list source is dropped before apt runs" "no" \
  "$([ -e "$sources/microsoft-prod.list" ] && echo yes || echo no)"
check "...and a deb822 .sources one too" "no" "$([ -e "$sources/azure-cli.sources" ] && echo yes || echo no)"
check "every other source is kept" "yes" "$([ -e "$sources/git-core.list" ] && echo yes || echo no)"
check "the browsers reach playwright install --with-deps" "playwright install --with-deps chromium webkit" \
  "$(cat "$STUB_LOG")"

spec_summary
