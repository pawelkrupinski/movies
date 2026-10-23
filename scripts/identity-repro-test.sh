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

# --wait: a "Record scrape fixtures" run newer than prod-latest's, in flight, is waited for (its id and ETA said, then
# agentctl wait's backoff), before reproducing on prod-latest. A stub gh answers from $RUNS (applying --jq as gh does),
# prod-latest names recording 42, and run view says in progress for $PENDING reads, then $CONCL; `sleep` logs its delays.
mkdir -p "$work/bin"
cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
jq_filter="" dir="" prev=""
for a in "$@"; do [ "$prev" = --jq ] && jq_filter=$a; [ "$prev" = --dir ] && dir=$a; prev=$a; done
case "$1 $2" in
  "release download") echo "42 2026-10-23T00:30:00Z" > "$dir/prod-latest-pl.txt" ;;
  "run list") jq -r "$jq_filter" <<< "${RUNS:-[]}" ;;
  "run view") n=$(( $(cat "$WORK/views" 2>/dev/null || echo 0) + 1 )); echo "$n" > "$WORK/views"
              if [ "$n" -le "${PENDING:-0}" ]; then st='{"status":"in_progress","conclusion":""}'
              else st="{\"status\":\"completed\",\"conclusion\":\"${CONCL:-success}\"}"; fi
              jq -r "$jq_filter" <<< "$st" ;;
  *) exit 1 ;;
esac
STUB
printf '#!/bin/sh\necho "$1" >> "$WORK/slept"\n' > "$work/bin/sleep"
chmod +x "$work/bin/gh" "$work/bin/sleep"
rec() { printf '{"databaseId":%s,"status":"%s","conclusion":"%s","headBranch":"main","createdAt":"2026-10-23T00:00:00Z","updatedAt":"2026-10-23T%s:00Z"}' "$@"; }
waitrepro() {
  rm -f "$work/views" "$work/slept"
  WORK="$work" KINOWO_GH="$work/bin/gh" PATH="$work/bin:$PATH" KINOWO_IDENTITY_CACHE="$work/cache" IDENTITY_REPRO_SLICE="$work/slice" \
    "$REPO_ROOT/scripts/identity-repro" pl arkadia zabawa --no-mirror --wait "$@" > "$work/out" 2> "$work/err"
  echo $?
}
check "--wait with a newer recording in flight says its run and ETA, then waits with the backoff" "0|yes|yes|60 90" \
  "$(RUNS="[$(rec 50 in_progress '' 00:00),$(rec 41 completed success 01:00),$(rec 40 completed failure 01:20)]" PENDING=2 waitrepro)|$(grep -q 'recording run 50 in flight (newer than prod-latest.s 42): ETA ~80 min (median of 2 recent runs)' "$work/err" && echo yes)|$(grep -q 'recording run 50 finished' "$work/err" && echo yes)|$(paste -sd' ' "$work/slept")"
check "...then reproduces on prod-latest" "REPRODUCIBLE|pl --key Cinema City Arkadia|Świąteczna zabawa z Peppą --recording 42 --emit-case" \
  "$(out | cut -d: -f1)|$(tr '\n' ' ' < "$work/args" | sed 's/ $//')"
check "a recording in flight that fails is said, and prod-latest used" "0|yes" \
  "$(RUNS="[$(rec 50 in_progress '' 00:00)]" CONCL=failure waitrepro)|$(grep -q 'recording run 50 ended failure: reproducing on prod-latest.s 42' "$work/err" && echo yes)"
check "nothing newer than prod-latest in flight: said, no wait" "0|yes|" \
  "$(RUNS="[$(rec 41 in_progress '' 00:00)]" waitrepro)|$(grep -q 'no recording newer than prod-latest.s (run 42) in flight' "$work/err" && echo yes)|$(cat "$work/slept" 2>/dev/null)"
check "--wait with a named recording is refused" "2" "$(waitrepro --recording 42)"

spec_summary
