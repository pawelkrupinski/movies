#!/usr/bin/env bash
# convergence-publish.sh against a stub `gh` whose release is a directory.
# Run: bash .github/scripts/convergence-publish-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m convergence-publish.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
publish="$REPO_ROOT/.github/scripts/convergence-publish.sh"
mkdir -p "$work/bin" "$work/leg"

# The release is $work/release (absent until created); an asset's createdAt is in $work/created/<name>,
# else the upload's moment, which sorts after every seeded one. Every write is logged to $work/calls.
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
root="$(dirname "$0")/.."; release="$root/release"
[ "$1" = release ] || exit 1
verb="$2"; shift 2; tag="$1"; shift
case "$verb" in
  view)
    [ -d "$release" ] || { echo "release not found" >&2; exit 1; }
    if [ "${1:-}" = --json ]; then
      for f in "$release"/*; do
        [ -e "$f" ] || continue
        name=$(basename "$f")
        created=$(cat "$root/created/$name" 2>/dev/null || echo 2026-10-15T12:00:00Z)
        printf '{"name":"%s","createdAt":"%s"}\n' "$name" "$created"
      done | jq -s '{assets: .}' | jq -r "$4"
    fi ;;
  create) echo "create $tag" >> "$root/calls"; mkdir -p "$release" ;;
  upload)
    files=(); for a in "$@"; do case "$a" in --*) ;; *) files+=("$a") ;; esac; done
    echo "upload $tag $*" >> "$root/calls"
    [ -z "${STUB_UPLOAD_FAILS:-}" ] || { echo "HTTP 422" >&2; exit 1; }
    for f in "${files[@]}"; do cp "$f" "$release/"; done ;;
  delete-asset) echo "delete $1" >> "$root/calls"; rm -f "$release/$1" ;;
  download)
    while [ "$#" -gt 0 ]; do case "$1" in --pattern) pattern=$2; shift ;; --dir) dir=$2; shift ;; esac; shift; done
    [ -f "$release/$pattern" ] || { echo "no asset matches $pattern" >&2; exit 1; }
    cp "$release/$pattern" "$dir/" ;;
  *) exit 1 ;;
esac
STUB
chmod +x "$work/bin/gh"
run() { (cd "$work/leg" && env PATH="$work/bin:$PATH" FIXTURE_RELEASE_TAG=convergence-fixtures GITHUB_ENV="$work/github-env" \
          GH_RELEASE_RETRY_DELAY=0 "$@" > "$work/out" 2>&1); echo $?; }
calls() { grep -c "^$1" "$work/calls" 2>/dev/null || echo 0; }
assets() { (cd "$work/release" && ls | tr '\n' ' ' | sed 's/ $//'); }

# --- tree ---------------------------------------------------------------------------------------
check "a leg that packed nothing publishes nothing" "0 0" "$(run bash "$publish" tree pl true leg.log) $(calls upload)"

mkdir -p "$work/leg/enrichment-upload"
echo tree > "$work/leg/enrichment-upload/enrichment-pl.tar.zst"
echo "[pl] boot complete" > "$work/leg/leg.log"
check "an unfinished boot publishes the working tree" "0" \
  "$(run env KINOWO_CONVERGENCE_CORPUS_RUN=100 bash "$publish" tree pl false leg.log)"
check "creating the release it did not find" "1" "$(calls create)"
check "and pins nothing" "enrichment-pl.tar.zst|" "$(assets)|$(cat "$work/github-env" 2>/dev/null)"

: > "$work/release/enrichment-pl.tar.gz"
check "a finished boot over a corpus run publishes the pinned copy too" "0" \
  "$(run env KINOWO_CONVERGENCE_CORPUS_RUN=100 bash "$publish" tree pl true leg.log)"
check "in the one upload call, beside the working tree" \
  "upload convergence-fixtures enrichment-upload/enrichment-pl.tar.zst enrichment-upload/enrichment-pl-100.tar.zst --clobber" \
  "$(grep '^upload' "$work/calls" | tail -1)"
check "naming it to the pin step" "KINOWO_PINNED_TREE=enrichment-pl-100.tar.zst" "$(cat "$work/github-env")"
check "retiring the gzip working tree, and creating no second release" "enrichment-pl-100.tar.zst enrichment-pl.tar.zst 1" \
  "$(assets) $(calls create)"
: > "$work/github-env"
echo "[uk] boot complete" > "$work/leg/leg.log"
run env KINOWO_CONVERGENCE_CORPUS_RUN=101 bash "$publish" tree pl true leg.log > /dev/null
check "another country's boot line pins nothing" "" "$(cat "$work/github-env")"
check "a failed upload fails the step" "1" \
  "$(run env STUB_UPLOAD_FAILS=1 GH_RELEASE_ATTEMPTS=1 bash "$publish" tree pl false leg.log)"

# --- pin ----------------------------------------------------------------------------------------
check "no pinned tree leaves the pair as it was" "0 absent" \
  "$(run bash "$publish" pin pl) $([ -e "$work/release/hermetic-pl.txt" ] && echo present || echo absent)"

rm -f "$work/release"/*
mkdir -p "$work/created"
for n in 1 2 3 4 5 6 7; do
  for asset in "enrichment-pl-$n.tar.zst" "fill-pl-$n-900.tar.zst" "refetch-pl-$n-900.tsv" "refused-pl-$n-900-2-takes.tsv"; do
    : > "$work/release/$asset"; echo "2026-10-0${n}T00:00:00Z" > "$work/created/$asset"
  done
done
: > "$work/release/enrichment-pl-0.tar.gz"; echo 2026-09-30T00:00:00Z > "$work/created/enrichment-pl-0.tar.gz"
: > "$work/release/enrichment-uk-1.tar.zst"; : > "$work/release/fill-uk-1-900.tar.zst"
: > "$work/release/enrichment-pl.tar.zst"
check "a pinned tree is pinned" "0" "$(run env KINOWO_PINNED_TREE=enrichment-pl-7.tar.zst KINOWO_CONVERGENCE_CORPUS_RUN=7 \
  KINOWO_CONVERGENCE_CORPUS_RECORDED_AT=2026-10-07T00:00:00Z bash "$publish" pin pl)"
check "as the marker naming the pair" "7 2026-10-07T00:00:00Z enrichment-pl-7.tar.zst" "$(cat "$work/release/hermetic-pl.txt")"
check "keeping the newest five pairs of either compressor, and only their fills and lists" \
  "enrichment-pl-3.tar.zst enrichment-pl-4.tar.zst enrichment-pl-5.tar.zst enrichment-pl-6.tar.zst enrichment-pl-7.tar.zst enrichment-pl.tar.zst enrichment-uk-1.tar.zst fill-pl-3-900.tar.zst fill-pl-4-900.tar.zst fill-pl-5-900.tar.zst fill-pl-6-900.tar.zst fill-pl-7-900.tar.zst fill-uk-1-900.tar.zst hermetic-pl.txt refetch-pl-3-900.tsv refetch-pl-4-900.tsv refetch-pl-5-900.tsv refetch-pl-6-900.tsv refetch-pl-7-900.tsv refused-pl-3-900-2-takes.tsv refused-pl-4-900-2-takes.tsv refused-pl-5-900-2-takes.tsv refused-pl-6-900-2-takes.tsv refused-pl-7-900-2-takes.tsv" \
  "$(assets)"

# A country's first pin, its release holding no fill, refetch or refused list at all: the prune's grep finds nothing,
# which under pipefail failed the step after the pin was written (record run 161, Spain).
rm -f "$work/release"/*
: > "$work/release/enrichment-es-161.tar.zst"
check "a first pin, with no fills to prune, passes" "0" "$(run env KINOWO_PINNED_TREE=enrichment-es-161.tar.zst \
  KINOWO_CONVERGENCE_CORPUS_RUN=161 KINOWO_CONVERGENCE_CORPUS_RECORDED_AT=2026-10-20T08:29:15Z bash "$publish" pin es)"

# --- prod-latest --------------------------------------------------------------------------------
check "no pinned tree leaves prod-latest as it was" "0 absent" \
  "$(run bash "$publish" latest pl) $([ -e "$work/release/prod-latest-pl.txt" ] && echo present || echo absent)"
rm -f "$work/release"/* "$work/created"/*
for n in 1 2 3 4 5 6 7 8; do
  : > "$work/release/enrichment-pl-$n.tar.zst"; echo "2026-10-0${n}T00:00:00Z" > "$work/created/enrichment-pl-$n.tar.zst"
done
echo "2 2026-10-02T00:00:00Z enrichment-pl-2.tar.zst" > "$work/release/hermetic-pl.txt"
check "a complete recording is marked prod-latest" "0" "$(run env KINOWO_PINNED_TREE=enrichment-pl-8.tar.zst \
  KINOWO_CONVERGENCE_CORPUS_RUN=8 KINOWO_CONVERGENCE_CORPUS_RECORDED_AT=2026-10-08T00:00:00Z KINOWO_KEPT_RECORDINGS=1 \
  bash "$publish" latest pl)"
check "naming it in the hermetic marker's format" "8 2026-10-08T00:00:00Z enrichment-pl-8.tar.zst" \
  "$(cat "$work/release/prod-latest-pl.txt")"
check "leaving the hermetic pair as it was" "2 2026-10-02T00:00:00Z enrichment-pl-2.tar.zst" "$(cat "$work/release/hermetic-pl.txt")"
check "pruning to the newest five, never the hermetic pair or a kept landing recording" \
  "enrichment-pl-1.tar.zst enrichment-pl-2.tar.zst enrichment-pl-4.tar.zst enrichment-pl-5.tar.zst enrichment-pl-6.tar.zst enrichment-pl-7.tar.zst enrichment-pl-8.tar.zst hermetic-pl.txt prod-latest-pl.txt" \
  "$(assets)"

 # --- dead ---------------------------------------------------------------------------------------
# Run 41351315090 found the release at GitHub's 1000-asset cap, 101 of them identity overlays published by a mode
# since removed: an overlay is only laid over its own recording's tree, so one whose tree is gone is read by nothing.
rm -f "$work/release"/* "$work/calls"
for asset in enrichment-uk-573.tar.zst identity-overlay-uk-573.tar.gz identity-overlay-uk-36691290304.tar.gz \
             identity-overlay-us-36691290304.tar.gz fill-uk-573-9-1-convergence.tar.zst \
             takes-decisions-uk-0123abcd.gz takes-full-uk.txt takes-final-us-0123abcd.gz; do : > "$work/release/$asset"; done
check "dead assets are pruned" "0" "$(run env TAKES_RELEASE_TAG=identity-takes bash "$publish" dead uk)"
check "...an overlay whose tree is gone, and the takes results stored on their own release since, only the country's own" \
  "enrichment-uk-573.tar.zst fill-uk-573-9-1-convergence.tar.zst identity-overlay-uk-573.tar.gz identity-overlay-us-36691290304.tar.gz takes-final-us-0123abcd.gz" \
  "$(assets)"
rm -f "$work/calls"
check "a release with nothing dead deletes nothing" "0 0" "$(run env TAKES_RELEASE_TAG=identity-takes bash "$publish" dead uk) $(calls delete)"
rm -f "$work/calls"
check "a takes store still on the fixtures' release is the store, never pruned" "0 delete identity-overlay-us-36691290304.tar.gz" \
  "$(run env TAKES_RELEASE_TAG=convergence-fixtures bash "$publish" dead us) $(paste -sd' ' "$work/calls")"

check "an unknown command is refused" "64" "$(run bash "$publish" publish pl)"

spec_summary
