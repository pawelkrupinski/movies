#!/usr/bin/env bash
# scripts/identity-slice-refresh: a case is re-emitted with every listing its expected.tsv names, its slice files land in
# its own directory whatever name identity-slice pinned it under, its hand expected.tsv survives, and a failed re-emit
# leaves the case as it was. identity-slice is a stub.
# Run: bash scripts/identity-slice-refresh-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m identity-slice-refresh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
cases="$work/identity-slices"
mkdir -p "$cases"

# the stub records its arguments, writes fresh slice files under PIN_AS (default: a new name) and says where;
# EMIT_FAILS makes it fail
cat > "$work/identity-slice" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" > "$CASES/../calls"
[ -z "${EMIT_FAILS:-}" ] || { echo "the recording lacks it" >&2; exit 3; }
dir="$CASES/${PIN_AS:-uk-new-name-0000}"
mkdir -p "$dir"
for f in slice.json.gz rows.json.gz http.jsonl.gz filed.json.gz; do echo fresh > "$dir/$f"; done
printf 'venue\trawTitle\tfilm\tlabel\nX\tY\ttmdb:1\tfrom the judgements\n' > "$dir/expected.tsv"
echo "--emit-case: 2 listing(s) pinned in $dir (IdentitySliceCasesSpec replays it):"
STUB
chmod +x "$work/identity-slice"

make_case() {
  rm -rf "${cases:?}/$1"; mkdir -p "$cases/$1"
  for f in slice.json.gz rows.json.gz http.jsonl.gz; do echo stale > "$cases/$1/$f"; done
  printf 'venue\trawTitle\tfilm\tlabel\nFilmhouse Newlyn\tFallen Angels By Noël Coward\tnot:tmdb:7\tby hand\nRegent Christchurch\tFallen Angels\ttmdb:9\tjudged\n' \
    > "$cases/$1/expected.tsv"
}
refresh() { CASES="$cases" IDENTITY_SLICE="$work/identity-slice" "$REPO_ROOT/scripts/identity-slice-refresh" "$@" > /dev/null 2>&1; echo $?; }

make_case uk-fallen-angels-1111
check "a case re-pinned under another name succeeds" "0" "$(refresh --recording 38713274617 "$cases/uk-fallen-angels-1111")"
check "...asking its country with every listing its expected.tsv names, on the recording given" \
  "uk --recording 38713274617 --key Filmhouse Newlyn|Fallen Angels By Noël Coward --key Regent Christchurch|Fallen Angels --emit-case" \
  "$(cat "$work/calls")"
check "...its slice files replaced by the fresh ones" "fresh" "$(cat "$cases/uk-fallen-angels-1111/slice.json.gz")"
check "...the filed answers carried too" "fresh" "$(cat "$cases/uk-fallen-angels-1111/filed.json.gz" 2>/dev/null)"
check "...its hand expectation kept" "not:tmdb:7" "$(sed -n 2p "$cases/uk-fallen-angels-1111/expected.tsv" | cut -f3)"
check "...and no copy left under the other name" "false" "$([ -e "$cases/uk-new-name-0000" ] && echo true || echo false)"

make_case uk-in-place-2222
check "a case re-pinned in place succeeds" "0" "$(PIN_AS=uk-in-place-2222 refresh "$cases/uk-in-place-2222")"
check "...its hand expectation restored over the judgements'" "not:tmdb:7" "$(sed -n 2p "$cases/uk-in-place-2222/expected.tsv" | cut -f3)"
check "...its slice fresh" "fresh" "$(cat "$cases/uk-in-place-2222/slice.json.gz")"

make_case uk-unrecorded-3333
check "a re-emit that fails is a failure" "1" "$(EMIT_FAILS=1 refresh "$cases/uk-unrecorded-3333")"
check "...leaving the case as it was" "stale not:tmdb:7" \
  "$(cat "$cases/uk-unrecorded-3333/slice.json.gz") $(sed -n 2p "$cases/uk-unrecorded-3333/expected.tsv" | cut -f3)"

check "a directory without expected.tsv is no case" "1" "$(refresh "$work")"
check "no case at all is a usage error" "2" "$(refresh)"

spec_summary
