#!/usr/bin/env bash
# install-hooks.sh: points core.hooksPath at scripts/hooks and makes the hooks git runs executable -- and only those:
# a data file or a test library in the directory stays as it was. Runs in a scratch repository.
# Run: bash scripts/install-hooks-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m install-hooks\n'
work="$(cd "$(mktemp -d)" && pwd -P)"
trap 'rm -rf "$work"' EXIT
git -C "$work" init -q
mkdir -p "$work/scripts/hooks"
cp "$REPO_ROOT/scripts/install-hooks.sh" "$REPO_ROOT/scripts/hooks-path.sh" "$work/scripts/"
for f in pre-push pre-commit post-commit local-tests.txt lint-registry.tsv pre-push-test-lib.sh; do : > "$work/scripts/hooks/$f"; done
(cd "$work" && bash scripts/install-hooks.sh > /dev/null 2>&1); rc=$?
executable() { [ -x "$work/scripts/hooks/$1" ] && echo yes || echo no; }
check "install-hooks succeeds and sets core.hooksPath" "0 scripts/hooks" "$rc $(git -C "$work" config core.hooksPath)"
check "...the hooks git runs become executable" "yes yes yes" "$(executable pre-push) $(executable pre-commit) $(executable post-commit)"
check "...a data file or a test library does not" "no no no" "$(executable local-tests.txt) $(executable lint-registry.tsv) $(executable pre-push-test-lib.sh)"

spec_summary
