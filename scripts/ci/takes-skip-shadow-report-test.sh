#!/usr/bin/env bash
# takes-skip-shadow-report.sh against a stub `gh`: READY only with >= 20 SKIPs and none disagreeing; a read that fails is
# exit 2, never a tally.
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"
printf '\033[36m▸\033[0m takes-skip-shadow-report.sh\n'
stub=$(mktemp -d)
trap 'rm -rf "$stub"' EXIT
# STUB_COMMITS commits on page 1, none after; each with STUB_STATUS's description (DISAGREE on the commit STUB_BAD names);
# STUB_GH=fail fails every read.
cat > "$stub/gh" <<'STUB'
#!/usr/bin/env bash
[ "${STUB_GH:-}" = fail ] && exit 1
case "$*" in
  *"commits?sha="*"page=1"*) for i in $(seq 1 "${STUB_COMMITS:-0}"); do printf 'c%039d\n' "$i"; done ;;
  *"commits?sha="*) ;;
  *statuses*) sha=${2#repos/o/r/commits/}; sha=${sha%%/*}
              d=${STUB_STATUS:-"shadow: gate SKIP, takes unmoved -- agree"}
              [ "$sha" = "${STUB_BAD:-}" ] && d="shadow: gate SKIP, takes moved -- DISAGREE"
              printf 'takes skip gate (pl)\tsuccess\t%s\n' "$d" ;;
esac
STUB
chmod +x "$stub/gh"
tally() { PATH="$stub:$PATH" GH_REPO=o/r bash "$REPO_ROOT/scripts/ci/takes-skip-shadow-report.sh" main 100 > "$stub/out" 2>&1; echo $?; }
check "20 SKIPs, none disagreeing: ready" 0 "$(STUB_COMMITS=20 tally)"
check "19: not yet" 1 "$(STUB_COMMITS=19 tally)"
check "one disagreeing among 30: never ready" 1 "$(STUB_COMMITS=30 STUB_BAD=$(printf 'c%039d' 7) tally)"
check "...counted as such" 1 "$(grep -c 'gate SKIP 30 (disagreeing 1)' "$stub/out")"
check "RUNs alone: not ready" 1 "$(STUB_COMMITS=30 STUB_STATUS='shadow: gate RUN, takes moved -- agree' tally)"
check "a read that fails: no tally" 2 "$(STUB_COMMITS=30 STUB_GH=fail tally)"
check "no repository named: no tally" 2 "$(PATH="$stub:$PATH" GH_REPO='' bash "$REPO_ROOT/scripts/ci/takes-skip-shadow-report.sh" > /dev/null 2>&1; echo $?)"
spec_summary
