#!/usr/bin/env bash
# scripts/identity-slice-refresh: a case is re-emitted with every listing its expected.tsv names (once each, a last line
# without its newline too), into a scratch root; its slice files land in its own directory, its hand expected.tsv
# survives, and no other case is touched. A re-emit that fails -- even after writing -- or that pins the listings as
# several slices leaves the case as it was. identity-slice is a stub.
# Run: bash scripts/identity-slice-refresh-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m identity-slice-refresh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
cases="$work/identity-slices"
mkdir -p "$cases"

# The stub records its arguments and pins under the emit root it is handed (KINOWO_IDENTITY_SLICE_EMIT_ROOT, as
# scripts/identity-slice reads it), saying where. PIN_AS names the directory (default "uk-new-name-0000"; it may name an
# existing case: the stub must not reach it); TWO_SLICES pins a second one; FAIL_AFTER_WRITE writes, then fails.
cat > "$work/identity-slice" <<'STUB'
#!/usr/bin/env bash
printf '%s\n' "$*" > "$CALLS"
root=${KINOWO_IDENTITY_SLICE_EMIT_ROOT:?the refresh must hand identity-slice an emit root}
pin() {
  dir="$root/$1"; mkdir -p "$dir"
  for f in slice.json.gz rows.json.gz http.jsonl.gz filed.json.gz; do echo fresh > "$dir/$f"; done
  printf 'venue\trawTitle\tfilm\tlabel\nX\tY\ttmdb:1\tfrom the judgements\n' > "$dir/expected.tsv"
  echo "--emit-case: 1 listing(s) pinned in $dir (IdentitySliceCasesSpec replays it):"
}
pin "${PIN_AS:-uk-new-name-0000}"
[ -z "${TWO_SLICES:-}" ] || pin uk-second-cluster-9999
[ -z "${FAIL_AFTER_WRITE:-}" ] || { echo "SLICE INCOMPLETE" >&2; exit 3; }
STUB
chmod +x "$work/identity-slice"

make_case() {
  rm -rf "${cases:?}/$1"; mkdir -p "$cases/$1"
  for f in slice.json.gz rows.json.gz http.jsonl.gz; do echo stale > "$cases/$1/$f"; done
  # the same listing twice (a `not:` case's rows), the last line without its newline
  printf 'venue\trawTitle\tfilm\tlabel\nFilmhouse Newlyn\tFallen Angels By Noël Coward\tnot:tmdb:7\tby hand\nFilmhouse Newlyn\tFallen Angels By Noël Coward\tnot:tmdb:8\tby hand\nRegent Christchurch\tFallen Angels\ttmdb:9\tjudged' \
    > "$cases/$1/expected.tsv"
}
untouched() { echo "$(cat "$cases/$1/slice.json.gz") $(sed -n 2p "$cases/$1/expected.tsv" | cut -f3)"; }
refresh() { CALLS="$work/calls" IDENTITY_SLICE="$work/identity-slice" "$REPO_ROOT/scripts/identity-slice-refresh" "$@" > /dev/null 2>&1; echo $?; }

make_case uk-fallen-angels-1111
make_case uk-new-name-0000 # another case the emitted directory's name matches
check "a case re-pins" "0" "$(refresh --recording 38713274617 "$cases/uk-fallen-angels-1111")"
check "...asking its country with each listing its expected.tsv names once, the unterminated last line too" \
  "uk --recording 38713274617 --key Filmhouse Newlyn|Fallen Angels By Noël Coward --key Regent Christchurch|Fallen Angels --emit-case" \
  "$(cat "$work/calls")"
check "...its slice files replaced by the fresh ones" "fresh" "$(cat "$cases/uk-fallen-angels-1111/slice.json.gz")"
check "...the filed answers carried too" "fresh" "$(cat "$cases/uk-fallen-angels-1111/filed.json.gz" 2>/dev/null)"
check "...its hand expectation kept" "not:tmdb:7" "$(sed -n 2p "$cases/uk-fallen-angels-1111/expected.tsv" | cut -f3)"
check "...and the case of the emitted directory's name untouched" "stale not:tmdb:7" "$(untouched uk-new-name-0000)"
check "...nor any directory added" "2" "$(find "$cases" -mindepth 1 -maxdepth 1 -type d | wc -l | tr -d ' ')"

make_case uk-unrecorded-3333
check "a re-emit that writes, then fails, is a failure" "1" "$(FAIL_AFTER_WRITE=1 refresh "$cases/uk-unrecorded-3333")"
check "...leaving the case as it was" "stale not:tmdb:7" "$(untouched uk-unrecorded-3333)"

make_case uk-two-clusters-4444
check "listings pinned as two slices are a failure" "1" "$(TWO_SLICES=1 refresh "$cases/uk-two-clusters-4444")"
check "...leaving the case as it was" "stale not:tmdb:7" "$(untouched uk-two-clusters-4444)"
check "...and adding no case" "false" "$([ -e "$cases/uk-second-cluster-9999" ] && echo true || echo false)"

mkdir -p "$cases/uk-empty-5555"; printf 'venue\trawTitle\tfilm\tlabel\n' > "$cases/uk-empty-5555/expected.tsv"
check "an expected.tsv naming no listing is a failure" "1" "$(refresh "$cases/uk-empty-5555")"
check "a directory without expected.tsv is no case" "1" "$(refresh "$work")"
check "no case at all is a usage error" "2" "$(refresh)"

spec_summary
