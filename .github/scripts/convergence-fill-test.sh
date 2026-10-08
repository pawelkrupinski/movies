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
printf '# 2 fetchable gap(s)\na\tGET\thttps://www.flicks.us/movie/a/\nb\tGET\thttps://www.flicks.us/movie/b/\n' > "$work/release/refetch-us-100-201.tsv"
: > "$work/release/fill-us-99-300.tar.zst"
: > "$work/release/fill-uk-100-301.tar.zst"
: > "$work/release/enrichment-us-100.tar.zst"

cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
release="$(dirname "$0")/../release"
if [ "$1 $2" = "release view" ]; then
  echo x >> "$release/../views"
  [ -z "${STUB_UNREACHABLE:-}" ] || exit 1
  # A GitHub 5xx on the first listing only.
  if [ -n "${STUB_FAILS_ONCE:-}" ] && [ ! -e "$release/../failed-once" ]; then : > "$release/../failed-once"; exit 1; fi
  ls "$release"; exit 0
fi
if [ "$1 $2" = "release download" ]; then
  while [ "$#" -gt 0 ]; do
    case "$1" in --pattern) pattern="$2" ;; --dir) dir="$2" ;; --output) output="$2" ;; esac; shift
  done
  echo "$pattern" >> "$release/../downloads"
  if [ -n "${STUB_DOWNLOAD_FAILS_ONCE:-}" ] && [ ! -e "$release/../failed-download-once" ]; then : > "$release/../failed-download-once"; exit 1; fi
  [ -f "$release/$pattern" ] || exit 1
  if [ -n "${output:-}" ]; then cp "$release/$pattern" "$output"; else cp "$release/$pattern" "$dir/"; fi
  exit 0
fi
exit 1
STUB
chmod +x "$work/bin/gh"
run() { PATH="$work/bin:$PATH" FIXTURE_RELEASE_TAG=convergence-fixtures CONVERGENCE_FILL_LIST_BACKOFF=0 bash "$fill" "$@"; }
views() { wc -l < "$work/views" 2>/dev/null | tr -d ' '; }

check "a pair's fills are its own country's and corpus's, oldest run first" \
  "fill-us-100-35.tar.zst fill-us-100-201.tar.zst" "$(run fills us 100)"

# A leg's convergence row publishes its pre-suite fill under its run attempt and row; a later leg lays every one over
# the tree. A re-run of a failed job is a new attempt, so it never meets its first attempt's name ("already exists").
: > "$work/release/fill-us-100-201-convergence.tar.zst"
: > "$work/release/fill-us-100-201-2-convergence.tar.zst"
: > "$work/release/fill-us-100-201-1-convergence.tar.zst"
: > "$work/release/fill-us-100-35-sample.tar.zst"
: > "$work/release/fill-us-100-201-Bogus.tar.zst"
check "a row's fill is one of the pair's, by run id, then attempt, then name" \
  "fill-us-100-35-sample.tar.zst fill-us-100-35.tar.zst fill-us-100-201-convergence.tar.zst fill-us-100-201.tar.zst fill-us-100-201-1-convergence.tar.zst fill-us-100-201-2-convergence.tar.zst" \
  "$(run fills us 100)"
rm "$work/release/fill-us-100-201-convergence.tar.zst" "$work/release/fill-us-100-201-2-convergence.tar.zst" \
  "$work/release/fill-us-100-201-1-convergence.tar.zst" "$work/release/fill-us-100-35-sample.tar.zst" "$work/release/fill-us-100-201-Bogus.tar.zst"
# A listing that FAILED is not an empty release: read as "no fills", the leg would decide its verdict on —
# and pin into its bisect's pair — a tree without the fills its pair has.
check "a release that cannot be listed fails the fills, loudly" "1:" \
  "$(out=$(STUB_UNREACHABLE=1 run fills us 100 2>/dev/null); echo "$?:$out")"
check "...saying which release it could not list" "true" \
  "$(err=$(STUB_UNREACHABLE=1 run fills us 100 2>&1 >/dev/null); grep -q '::error::could not list release convergence-fixtures' <<< "$err" && echo true || echo false)"
rm -f "$work/views"
STUB_UNREACHABLE=1 run fills us 100 > /dev/null 2>&1
check "...but only after asking three times" "3" "$(views)"
check "...and the gaps too, rather than reporting none" "1" \
  "$(STUB_UNREACHABLE=1 run gaps us 100 "$work/unreachable.tsv" > /dev/null 2>&1; echo "$?")"
# One GitHub 5xx is not a release that cannot be listed: a single failed listing must not fail the leg.
rm -f "$work/views" "$work/failed-once"
check "a listing that fails once and then answers gives the pair's fills" "0:fill-us-100-35.tar.zst fill-us-100-201.tar.zst" \
  "$(out=$(STUB_FAILS_ONCE=1 run fills us 100 2>/dev/null); echo "$?:$out")"
check "...having asked twice" "2" "$(views)"
# Every leg's setup asks, under `bash -e`, before any fill exists (run 37608599525 failed every leg here).
check "a pair with no fills yet gives none, and succeeds" "0:" "$(out=$(run fills es 100); echo "$?:$out")"

check "each fill is laid over the staged tree" "0" "$(run unpack "$work/stage" fill-us-100-201.tar.zst > "$work/out-unpack" 2>&1; echo "$?")"
check "...under the tree's own paths" "toy story 5" \
  "$(cat "$work/stage/test/resources/fixtures/enrichment-us/www.flicks.us/movie/toy-story-5" 2>/dev/null)"
# The pair names every fill: a leg that replayed without one would decide its verdict on — and hand its bisect — a
# tree its pair does not describe.
rm -f "$work/downloads"
check "a fill that cannot be restored fails the unpack, rather than being replayed without" "1" \
  "$(run unpack "$work/stage2" fill-us-100-gone.tar.zst fill-us-100-201.tar.zst > "$work/out-gone" 2>&1; echo "$?")"
check "...saying which, loudly" "true" \
  "$(grep -q '::error::fill fill-us-100-gone.tar.zst could not be restored' "$work/out-gone" && echo true || echo false)"
check "...having asked three times" "3" "$(grep -c '^fill-us-100-gone.tar.zst$' "$work/downloads")"
check "...and still laid the ones it could" "true" \
  "$([ -f "$work/stage2/test/resources/fixtures/enrichment-us/www.flicks.us/movie/toy-story-5" ] && echo true || echo false)"
# One failed download is not a fill that cannot be restored.
rm -f "$work/failed-download-once"
check "a fill whose download fails once and then answers is laid over" "0" \
  "$(STUB_DOWNLOAD_FAILS_ONCE=1 run unpack "$work/stage3" fill-us-100-201.tar.zst > /dev/null 2>&1; echo "$?")"

check "the NEWEST refetch list of the pair is the one read" "2" "$(run gaps us 100 "$work/gaps.tsv" 2>/dev/null)"
check "...written where it was asked for" "true" "$(grep -q 'movie/b/' "$work/gaps.tsv" && echo true || echo false)"
check "a pair with no list has no gaps" "0" "$(run gaps es 100 "$work/none.tsv" 2>/dev/null)"
# A re-run's list is a later attempt of the same run, and newer than its first attempt's.
printf '# 1\nc\tGET\thttps://www.flicks.us/movie/c/\n' > "$work/release/refetch-us-100-201-2.tsv"
check "a re-run's list is newer than its first attempt's" "true" \
  "$(run gaps us 100 "$work/rerun.tsv" > /dev/null 2>&1; grep -q 'movie/c/' "$work/rerun.tsv" && echo true || echo false)"
rm "$work/release/refetch-us-100-201-2.tsv"

# What an origin refused a fill is skipped by the next fills — the newest few lists' worth — so a gap it refuses for
# good is not asked through paid egress on every run.
printf '# 1 refused gap(s)\nb\tGET\thttps://www.flicks.us/movie/b/\n' > "$work/release/refused-us-100-200-1.tsv"
check "a gap an origin refused lately is not fetched again" "1" "$(run gaps us 100 "$work/skipped.tsv" 2>/dev/null)"
check "...the others are" "true:false" \
  "$(grep -q 'movie/a/' "$work/skipped.tsv" && echo true || echo false):$(grep -q 'movie/b/' "$work/skipped.tsv" && echo true || echo false)"
for run in 202 203 204 205; do printf '# 0 refused gap(s)\n' > "$work/release/refused-us-100-$run-1.tsv"; done
check "...but asked again once newer fills have published lists without it" "2" "$(run gaps us 100 "$work/again.tsv" 2>/dev/null)"
rm "$work/release"/refused-us-100-*.tsv

spec_summary
