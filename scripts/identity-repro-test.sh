#!/usr/bin/env bash
# scripts/identity-repro: a production listing to a verdict — found in the recording's corpus (case and accents aside)
# and sliced, or why not — over a fabricated recording in a scratch cache and a stub identity-slice whose output is set.
# Run: bash scripts/identity-repro-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m identity-repro\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
fixtures="$work/cache/recordings/42/pair/test/resources/fixtures"
mkdir -p "$fixtures/corpus" "$fixtures/enrichment-pl"
cat <<'JSON' | gzip > "$fixtures/corpus/cinema-scrapes-pl.json.gz"
[{"_id":"Cinema City Arkadia","scrapedAt":"2026-10-23T00:10:00Z","films":[{"movie":{"title":"Świąteczna zabawa z Peppą"}},{"movie":{"title":"Rose"}}]},
 {"_id":"Cinema City Bonarka","scrapedAt":"2026-10-23T00:11:00Z","films":[{"movie":{"title":"Świąteczna zabawa z Peppą"}}]},
 {"_id":"Kino Muranów","scrapedAt":"2026-10-23T00:12:00Z","films":[{"movie":{"title":"Rose"}},{"movie":{"title":"Rosemary's Baby"}}]}]
JSON
echo '{"recordedAt": "2026-10-23T00:30:00Z"}' > "$fixtures/corpus/prod-coverage-pl.json"
cat > "$work/slice" <<STUB
#!/usr/bin/env bash
printf '%s\n' "\$@" > "$work/args"
printf '%b' "\${SLICE_OUT:-}"
exit "\${SLICE_EXIT:-0}"
STUB
chmod +x "$work/slice"

repro() {
  KINOWO_IDENTITY_CACHE="$work/cache" IDENTITY_REPRO_SLICE="$work/slice" \
    "$REPO_ROOT/scripts/identity-repro" "$@" --recording 42 --no-mirror > "$work/out" 2> "$work/err"
  echo $?
}
out() { cat "$work/out"; }

check "a listing the corpus holds is sliced by its exact key on that recording" \
  "0|pl --key Cinema City Arkadia|Świąteczna zabawa z Peppą --recording 42 --emit-case" \
  "$(repro pl arkadia 'swiateczna ZABAWA')|$(tr '\n' ' ' < "$work/args" | sed 's/ $//')"
check "...and is REPRODUCIBLE when the slice asked nothing it lacked" "REPRODUCIBLE" "$(out | cut -d: -f1)"
check "--no-emit pins no case" "0|pl --key Cinema City Arkadia|Rose --recording 42" \
  "$(repro pl arkadia rose --no-emit)|$(tr '\n' ' ' < "$work/args" | sed 's/ $//')"
check "several matching titles are AMBIGUOUS, nothing sliced" "2|AMBIGUOUS" "$(rm -f "$work/args"; repro pl muranow Ros)|$(out | cut -d: -f1)$( [ -e "$work/args" ] && echo ' sliced')"
check "a venue the recording lacks is NOT-IN-RECORDING" "3|NOT-IN-RECORDING" "$(repro pl 'Kino Lalka' Rose)|$(out | cut -d: -f1)"
check "...and so is a title its venue does not list, naming the venue's scrape" "3|yes" \
  "$(repro pl Bonarka Rose)|$(out | grep -q 'Cinema City Bonarka scraped 2026-10-23T00:11:00Z' && echo yes)"
check "a slice the judgements do not judge NEEDS-JUDGEMENT" "4|NEEDS-JUDGEMENT" \
  "$(SLICE_OUT='--emit-case REFUSED for x: judge them first\n' repro pl arkadia zabawa)|$(out | cut -d: -f1)"
check "a slice the recording left questions unanswered NEEDS-ANSWERS, naming them" "5|NEEDS-ANSWERS|yes" \
  "$(SLICE_OUT='UNANSWERED by the recording (read as unknown): tmdb 1778515\n' repro pl arkadia zabawa)|$(out | cut -d: -f1)|$(out | grep -q 'tmdb 1778515' && echo yes)"
check "a slice taking what prod serves REPRODUCES it, its gaps named" "0|REPRODUCIBLE|yes" \
  "$(SLICE_OUT='-- prod'"'"'s stored state (mirror):\nUNANSWERED by the recording (read as unknown): imdb x\n' repro pl arkadia zabawa)|$(out | cut -d: -f1)|$(out | grep -q 'imdb x' && echo yes)"
check "prod taking otherwise is named beside the verdict" "yes" \
  "$(SLICE_OUT='-- prod'"'"'s stored state (mirror):\nPROD TAKES OTHERWISE for 1 of 1: Cinema City Arkadia | Rose: slice none, prod tmdb:7\n' repro pl arkadia zabawa >/dev/null; out | grep -q 'slice none, prod tmdb:7' && echo yes)"
check "a failed slice FAILS" "1|FAILED" "$(SLICE_EXIT=1 repro pl arkadia zabawa)|$(out | cut -d: -f1)"
check "an unknown country is refused" "2" "$(repro xx arkadia zabawa)"

spec_summary
