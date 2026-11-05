#!/usr/bin/env bash
# identity-slice-coverage against a stand-in sbt (IDENTITY_SLICE_COVERAGE_SBT):
#   - every slice covered passes (0), its tally shown;
#   - a slice lacking answers fails (1), naming the slice, each request and the --repin line, the checkout's paths short;
#   - a coverage that never ran (a compile failure) is no verdict (2), sbt's log shown;
#   - --repin reads which recordings the lacking slices need, finds them in the shared cache, and re-pins from there;
#   - a slice given on the command line is the one replayed, made absolute; none given: both fixture roots.
# Run: bash scripts/identity-slice-coverage-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m identity-slice-coverage\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
# sbt as `worker/Test/runMain scripts.IdentitySliceCoverage ... --report FILE [--needs FILE] [--repin ...]` leaves it
cat > "$work/sbt" <<'EOF'
#!/usr/bin/env bash
cmd=$1
echo "$cmd" >> "$FAKE_LOG"
report=$(grep -oE -- '--report [^ ]+' <<< "$cmd" | cut -d' ' -f2)
needs=$(grep -oE -- '--needs [^ ]+' <<< "$cmd" | cut -d' ' -f2)
echo "[info] compiling 3 Scala sources"
case ${FAKE_MODE:-covered} in
  broken) echo "[error] IdentitySliceCoverage.scala:1: not found"; exit 1 ;;
  covered) echo "identity-slice-coverage: 3 slice(s) replayed in 9.1s; 0 lack answers or could not be decided (0 request(s) lacking)" > "$report"; exit 0 ;;
esac
if grep -q -- '--repin' <<< "$cmd"; then
  { echo "identity-slice-coverage: 3 slice(s) replayed in 9.1s; 1 lack answers or could not be decided (1 request(s) lacking)"
    echo "  $FAKE_REPO/test/resources/fixtures/identity-slices/uk-halstead-nutcracker (recording 573): lacks 1 answer(s)"
    echo "  $FAKE_REPO/test/resources/fixtures/identity-slices/uk-halstead-nutcracker: re-pinned from 573, 1 request(s) were lacking"
    echo "identity-slice-coverage: 1 slice(s) re-pinned; commit their http.jsonl.gz"; } > "$report"
  exit 0
fi
[ -z "$needs" ] || printf '573\tuk\n' > "$needs"
{ echo "identity-slice-coverage: 3 slice(s) replayed in 9.1s; 1 lack answers or could not be decided (1 request(s) lacking)"
  echo "  $FAKE_REPO/test/resources/fixtures/identity-slices/uk-halstead-nutcracker (recording 573): lacks 1 answer(s)"
  echo "      GET https://api.themoviedb.org/3/search/movie?query=NATIONAL%20THEATRE%20LIVE"
  echo "SLICE ANSWERS MISSING: the slices above ask requests their fixtures do not hold. Re-pin them all from their recordings: scripts/identity-slice-coverage --repin"; } > "$report"
exit 1
EOF
chmod +x "$work/sbt"
cache="$work/cache"
mkdir -p "$cache/recordings/573/pair/test/resources/fixtures/corpus" "$cache/recordings/573/pair/test/resources/fixtures/enrichment-uk"
touch "$cache/recordings/573/pair/test/resources/fixtures/corpus/cinema-scrapes-uk.json.gz"
export FAKE_LOG="$work/calls" FAKE_REPO="$REPO_ROOT" IDENTITY_SLICE_COVERAGE_SBT="$work/sbt" KINOWO_IDENTITY_CACHE="$cache"

run() { : > "$FAKE_LOG"; bash "$REPO_ROOT/scripts/identity-slice-coverage" "$@" > "$work/out" 2>&1; echo $?; }

check "every slice covered passes" "0" "$(run)"
check "...its tally shown" "1" "$(grep -c '^identity-slice-coverage: 3 slice(s) replayed' "$work/out")"
check "...both fixture roots replayed, absolute" "1" \
  "$(grep -c -- "--slices $REPO_ROOT/test/resources/fixtures/identity-slices --slices $REPO_ROOT/test/resources/fixtures/drift-slices --report " "$FAKE_LOG")"
check "a slice lacking answers fails" "1" "$(FAKE_MODE=lacking run)"
check "...naming the slice, short" "1" "$(grep -c '^  test/resources/fixtures/identity-slices/uk-halstead-nutcracker (recording 573): lacks 1 answer(s)$' "$work/out")"
check "...each request" "1" "$(grep -c 'GET https://api.themoviedb.org/3/search/movie?query=NATIONAL%20THEATRE%20LIVE' "$work/out")"
check "...and the --repin line" "1" "$(grep -c 'scripts/identity-slice-coverage --repin$' "$work/out")"
check "...never sbt's own lines" "0" "$(grep -c '^\[info\]' "$work/out")"
check "a coverage that never ran is no verdict" "2" "$(FAKE_MODE=broken run)"
check "...sbt's log shown" "1" "$(grep -c 'not found' "$work/out")"
check "--repin re-pins from the slices' own recordings" "0" "$(FAKE_MODE=lacking run --repin)"
check "...asking first which recordings they need" "1" "$(head -1 "$FAKE_LOG" | grep -c -- '--needs ')"
check "...then re-pinning from the shared cache" "1" "$(sed -n 2p "$FAKE_LOG" | grep -c -- "--repin --recordings $cache/recordings")"
check "...said" "1" "$(grep -c 're-pinned from 573' "$work/out")"
check "--repin with nothing lacking re-pins nothing" "0 1" "$(run --repin) $(grep -c 'nothing to re-pin' "$work/out")"
check "a slice named is the one replayed, absolute" "1" \
  "$(run test/resources/fixtures/drift-slices/de/x/ > /dev/null; grep -c -- "--slices $PWD/test/resources/fixtures/drift-slices/de/x --report" "$FAKE_LOG")"
check "an unknown option is a usage error" "2" "$(run --bogus)"

spec_summary
