#!/usr/bin/env bash
# release-store-prune.sh against a stand-in `gh` keeping the release in a directory: `--json assets --jq` answered
# through jq over it (createdAt: the files' modification order), `delete-asset` removing the file.
# Run: bash scripts/ci/release-store-prune-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m release-store-prune.sh\n'

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/store"
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
[ "$1" = release ] || exit 1
case $2 in
  view) [ -n "${STUB_LIST_FAILS:-}" ] && { echo "HTTP 502" >&2; exit 1; }
        filter=; while [ $# -gt 0 ]; do [ "$1" = --jq ] && filter=$2; shift; done
        # shellcheck disable=SC2012 # the names are the stand-in's own, no odd characters
        ls -tr "$STORE" | jq -R . | jq -s '{assets: [to_entries[] | {name: .value, createdAt: ("2026-01-01T" + (.key | tostring | (("0" * (6 - length)) + .)))}]}' \
          | jq -r "$filter" ;;
  delete-asset) rm -f "$STORE/$4" ;;
  *) exit 1 ;;
esac
STUB
chmod +x "$work/bin/gh"
export STORE="$work/store" GH_RELEASE_RETRY_DELAY=0
fill() { # <prefix> <count>: that many assets, each newer than the one before
  local i
  for i in $(seq 1 "$2"); do touch -t "2026010100$(printf %02d "$(( $(find "$STORE" -type f | wc -l) / 60 ))").$(printf %02d "$(( $(find "$STORE" -type f | wc -l) % 60 ))")" "$STORE/$1$i.gz"; done
}
prune() { PATH="$work/bin:$PATH" bash "$REPO_ROOT/scripts/ci/release-store-prune.sh" "$@"; }
count() { find "$STORE" -name "$1*" | wc -l | tr -d ' '; }

# THE TAKES STORE (identity-takes, ~725 assets on 2026-11-15): kinds and countries each under their own count, the
# store as a whole over its budget -- the oldest go, whatever their kind, until one more fits.
fill takes-decisions-pl- 6; fill takes-shape-uk- 6; fill takes-trace-de- 6
prune identity-takes takes-trace-de- 24 10
check "a store over its total keeps the newest, one short of the total (room for the result about to be stored)" 9 "$(find "$STORE" -type f | wc -l | tr -d ' ')"
check "...the oldest gone first, never the newest" "0 6" "$(count takes-decisions-pl-1.gz) $(count takes-trace-de-)"

# A kind and country over its own count, the store under its total: only that prefix's oldest go.
rm -f "$STORE"/*; fill takes-decisions-pl- 5; fill takes-shape-pl- 3
prune identity-takes takes-decisions-pl- 3 900
check "a prefix over its count keeps its newest, one short of the count" "2 3" "$(count takes-decisions-pl-) $(count takes-shape-pl-)"
check "...its newest kept" 1 "$(count takes-decisions-pl-5.gz)"

# Under both: nothing goes. A listing that fails: nothing goes, and the store still succeeds (best effort).
rm -f "$STORE"/*; fill drift-green-us- 4
check "a store under both keeps all" "0 4" "$(prune identity-drift drift-green-us- 48 900; echo $?) $(count drift-green-us-)"
check "a listing that fails prunes nothing and fails nobody" "0 4" "$(STUB_LIST_FAILS=1 prune identity-drift drift-green-us- 1 1; echo $?) $(count drift-green-us-)"
check "a call missing an argument is refused" 2 "$(prune identity-drift drift- 1 2> /dev/null; echo $?)"

spec_summary
