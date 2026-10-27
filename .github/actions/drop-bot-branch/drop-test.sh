#!/usr/bin/env bash
# drop.sh against a stub `gh` whose ref lookup answers $STUB_REF (ok | 404 | 500).
# Run: bash .github/actions/drop-bot-branch/drop-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m drop-bot-branch/drop.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
cat > "$work/gh" <<'STUB'
#!/usr/bin/env bash
if [ "$2" = "-X" ]; then echo "$3 $4" >> "$(dirname "$0")/deleted"; exit 0; fi
case "$STUB_REF" in
  ok)  exit 0 ;;
  404) echo "gh: Not Found (HTTP 404)" >&2; exit 1 ;;
  *)   echo "gh: Server Error (HTTP 500)" >&2; exit 1 ;;
esac
STUB
chmod +x "$work/gh"
# On Forgejo the same answers come from git: `ls-remote --exit-code` exits 0 found, 2 absent, 128 a failed read.
cat > "$work/git" <<'STUB'
#!/usr/bin/env bash
if [ "$1" = push ]; then echo "$*" >> "$(dirname "$0")/deleted"; exit 0; fi
case "$STUB_REF" in ok) exit 0 ;; 404) exit 2 ;; *) exit 128 ;; esac
STUB
chmod +x "$work/git"

drop() {  # drop <ok|404|500> [server url]
  rm -f "$work/deleted"
  STUB_REF="$1" GITHUB_SERVER_URL="${2:-https://github.com}" GH_REPO=o/r BRANCH=bot/x PATH="$work:$PATH" \
    bash "$REPO_ROOT/.github/actions/drop-bot-branch/drop.sh" > /dev/null 2>&1
  echo $?
}

check "an existing branch is deleted" "0" "$(drop ok)"
check "...by its ref" "DELETE repos/o/r/git/refs/heads/bot/x" "$(cat "$work/deleted" 2>/dev/null)"
check "a 404 is no branch yet" "0" "$(drop 404)"
check "...and deletes nothing" "" "$(cat "$work/deleted" 2>/dev/null)"
check "any other failure fails the step rather than reading as no branch" "1" "$(drop 500)"
check "on Forgejo an existing branch is deleted through git" "0|push origin --delete bot/x" \
  "$(drop ok http://forgejo:3000)|$(cat "$work/deleted" 2>/dev/null)"
check "...a missing one is no branch yet" "0|" "$(drop 404 http://forgejo:3000)|$(cat "$work/deleted" 2>/dev/null)"
check "...and a failed read fails the step" "1|" "$(drop 500 http://forgejo:3000)|$(cat "$work/deleted" 2>/dev/null)"

spec_summary
