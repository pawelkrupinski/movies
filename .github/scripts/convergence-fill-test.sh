#!/usr/bin/env bash
# convergence-fill.sh against a stub `gh` whose release holds a pair's fills and refetch lists.
# Run: bash .github/scripts/convergence-fill-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m convergence-fill.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/release"
fill="$REPO_ROOT/.github/scripts/convergence-fill.sh"

# A fill as a fill row packs one: the page it fetched, under the tree's own paths.
mkdir -p "$work/fetched/test/resources/fixtures/enrichment-us/www.flicks.us/movie"
printf 'toy story 5\n' > "$work/fetched/test/resources/fixtures/enrichment-us/www.flicks.us/movie/toy-story-5"
FIXTURE_RELEASE_TAG=t bash "$fill" pack "$work/fetched" "$work/release/fill-us-100-201.tar.zst" > "$work/out-pack"
check "a fill that fetched something is packed" "true" "$([ -s "$work/release/fill-us-100-201.tar.zst" ] && echo true || echo false)"
mkdir -p "$work/empty"
FIXTURE_RELEASE_TAG=t bash "$fill" pack "$work/empty" "$work/none.tar.zst" > /dev/null
check "a fill that fetched nothing packs no archive, so nothing is published" "false" "$([ -e "$work/none.tar.zst" ] && echo true || echo false)"

# Assets of the release: two fills and two lists for pair 100, others for another pair and country.
cp "$work/release/fill-us-100-201.tar.zst" "$work/release/fill-us-100-35.tar.zst"
printf 'k\tGET\thttps://old\n' > "$work/release/refetch-us-100-35.tsv"
printf 'a\tGET\thttps://www.flicks.us/movie/a/\nb\tGET\thttps://www.flicks.us/movie/b/\n' > "$work/release/refetch-us-100-201.tsv"
: > "$work/release/fill-us-99-300.tar.zst"
: > "$work/release/fill-uk-100-301.tar.zst"
: > "$work/release/enrichment-us-100.tar.zst"

cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
release="$(dirname "$0")/../release"
if [ "$1 $2" = "release view" ]; then
  [ -z "${STUB_UNREACHABLE:-}" ] || exit 1
  ls "$release"; exit 0
fi
if [ "$1 $2" = "release download" ]; then
  while [ "$#" -gt 0 ]; do
    case "$1" in --pattern) pattern="$2" ;; --dir) dir="$2" ;; --output) output="$2" ;; esac; shift
  done
  [ -f "$release/$pattern" ] || exit 1
  if [ -n "${output:-}" ]; then cp "$release/$pattern" "$output"; else cp "$release/$pattern" "$dir/"; fi
  exit 0
fi
exit 1
STUB
chmod +x "$work/bin/gh"
run() { PATH="$work/bin:$PATH" FIXTURE_RELEASE_TAG=convergence-fixtures bash "$fill" "$@"; }

check "a pair's fills are its own country's and corpus's, oldest run first" \
  "fill-us-100-35.tar.zst fill-us-100-201.tar.zst" "$(run fills us 100)"
check "a release that cannot be listed gives no fills, not a failed leg" "0:" \
  "$(out=$(STUB_UNREACHABLE=1 run fills us 100 2>/dev/null); echo "$?:$out")"
# Every leg's setup asks, under `bash -e`, before any fill exists (run 37608599525 failed every leg here).
check "a pair with no fills yet gives none, and succeeds" "0:" "$(out=$(run fills es 100); echo "$?:$out")"

run unpack "$work/stage" fill-us-100-201.tar.zst fill-us-100-gone.tar.zst > "$work/out-unpack" 2>&1
check "each fill is laid over the staged tree, under the tree's own paths" "toy story 5" \
  "$(cat "$work/stage/test/resources/fixtures/enrichment-us/www.flicks.us/movie/toy-story-5" 2>/dev/null)"
check "...and a fill that cannot be restored is a warning, not a failure" "true" \
  "$(grep -q '::warning::fill fill-us-100-gone.tar.zst' "$work/out-unpack" && echo true || echo false)"

check "the NEWEST refetch list of the pair is the one read" "2" "$(run gaps us 100 "$work/gaps.tsv" 2>/dev/null)"
check "...written where it was asked for" "true" "$(grep -q 'movie/b/' "$work/gaps.tsv" && echo true || echo false)"
check "a pair with no list has no gaps" "0" "$(run gaps es 100 "$work/none.tsv" 2>/dev/null)"

spec_summary
