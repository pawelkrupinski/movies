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

drop() {
  rm -f "$work/deleted"
  STUB_REF="$1" GH_REPO=o/r BRANCH=bot/x PATH="$work:$PATH" \
    bash "$REPO_ROOT/.github/actions/drop-bot-branch/drop.sh" > /dev/null 2>&1
  echo $?
}

check "an existing branch is deleted" "0" "$(drop ok)"
check "...by its ref" "DELETE repos/o/r/git/refs/heads/bot/x" "$(cat "$work/deleted" 2>/dev/null)"
check "a 404 is no branch yet" "0" "$(drop 404)"
check "...and deletes nothing" "" "$(cat "$work/deleted" 2>/dev/null)"
check "any other failure fails the step rather than reading as no branch" "1" "$(drop 500)"

spec_summary
