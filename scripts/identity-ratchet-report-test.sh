#!/usr/bin/env bash
# identity-ratchet-report against a stub `gh`: a run's identity-ratchet artifact, a PR naming its run, and an older run
# whose findings only its pull-request job's (ANSI-coloured) log holds.
# Run: bash scripts/identity-ratchet-report-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m identity-ratchet-report\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/bin" "$work/artifacts/111111"
tab=$'\t'
{
  echo "status${tab}country${tab}venue${tab}rawTitle${tab}film${tab}title${tab}basis${tab}ids${tab}was${tab}explanation"
  echo "UNJUDGED${tab}de${tab}CineStar Augsburg${tab}Herr der Ringe (ext.)${tab}tmdb:1434460${tab}Herr der Ringe (2025)${tab}Filled${tab}tmdb:1434460${tab}${tab}filled by venues.current ¶ guards passed: bill.several"
  echo "WRONG${tab}pl${tab}Janosik${tab}Binti${tab}tmdb:855156${tab}Binti (2021)${tab}Agreed${tab}tmdb:855156${tab}${tab}"
  echo "RIGHT${tab}uk${tab}Act One${tab}Carmen${tab}tmdb:1${tab}Carmen (2026)${tab}Broadcast${tab}tmdb:1${tab}${tab}"
} > "$work/artifacts/111111/ratchet-takes.tsv"
esc=$'\033'
{
  printf 'pull-request\tRun the ratchet\t2026-10-13T11:32:05.97Z %s[0m[%s[0minfo%s[0m] %s[31m  WRONG takes:%s[0m\n' "$esc" "$esc" "$esc" "$esc" "$esc"
  printf 'pull-request\tRun the ratchet\t2026-10-13T11:32:05.97Z ^[[0m[info] ^[[31m  pl\tJanosik\tedukacja MH: Binti\ttmdb:855156\tBinti (2021)^[[0m\n'
  printf 'pull-request\tRun the ratchet\t2026-10-13T11:32:05.97Z [info]   List(Take("pl", ...))\n'
  printf 'pull-request\tRun the ratchet\t2026-10-13T11:32:05.97Z [info]   UNJUDGED takes:\n'
  printf 'pull-request\tRun the ratchet\t2026-10-13T11:32:05.97Z [info]   uk\tAct One\tTHE COMIC STRIP\ttmdb:396598\tThe Comic Strip (1981)\tFilled\ttmdb:396598,imdb:tt0478854\n'
  printf 'pull-request\tRun the ratchet\t2026-10-13T11:32:05.97Z [info] - should keep every right take *** FAILED ***\n'
  printf 'pull-request\tRun the ratchet\t2026-10-13T11:32:05.97Z [info]   right takes LOST:\n'
  printf 'pull-request\tRun the ratchet\t2026-10-13T11:32:05.97Z [info]   (de,Brotfabrik,Jeder Acker, jede Fabrik,wikidata:Q141180912)\n'
  printf 'pull-request\tRun the ratchet\t2026-10-13T11:32:05.97Z [info]   HashSet(("uk", ...))\n'
} > "$work/old.log"

cat > "$work/bin/gh" <<'STUB'
#!/usr/bin/env bash
echo "$*" >> "$STUB/calls"
case "$1 $2" in
  "run download") run=$3; while [ "$#" -gt 0 ]; do case "$1" in --dir) dir="$2" ;; esac; shift; done
                  [ -z "${GH_DOWN:-}" ] || { echo "HTTP 502: Bad Gateway" >&2; exit 1; }
                  [ -z "${GH_HANGS:-}" ] || { echo $$ >> "$GH_HANGS"; exec sleep 86400; } # never answers: blocks until killed
                  [ -d "$STUB/artifacts/$run" ] || { echo "no artifact matches any of the names or patterns provided" >&2; exit 1; }
                  cp "$STUB/artifacts/$run/"* "$dir/" ;;
  "pr view")      echo '{"body":"The weekly recapture (identity-capture.yml, run 111111).\n\nTo land: judge"}' ;;
  "run view")     if [[ " $* " == *" --log "* ]]; then cat "$STUB/old.log"
                  else echo '{"jobs":[{"name":"capture (de)","databaseId":1},{"name":"pull-request","databaseId":42}]}'; fi ;;
  *) exit 1 ;;
esac
STUB
chmod +x "$work/bin/gh"

report() { STUB="$work" KINOWO_IDENTITY_CACHE="$work/cache" PATH="$work/bin:$PATH" spec_bounded python3 -I "$REPO_ROOT/scripts/identity-ratchet-report" "$@" > "$work/out" 2>&1; echo $?; }

check "a run's artifact is printed" "0" "$(report 111111)"
check "...counting every status" "1" "$(grep -c '^findings: WRONG 1, UNJUDGED 1, RIGHT 1$' "$work/out")"
check "...listing the default statuses, not RIGHT" "0" "$(grep -c 'Carmen' "$work/out")"
check "...each take on one line" "1" "$(grep -c 'CineStar Augsburg .*tmdb:1434460 .*Filled' "$work/out")"
report 111111 --explain > /dev/null
check "--explain prints the explanation under its take" "1" "$(grep -c '^        guards passed: bill.several$' "$work/out")"
report 111111 --status ALL --tsv > /dev/null
check "--tsv --status ALL prints every finding as the artifact's TSV" "4" "$(wc -l < "$work/out" | tr -d ' ')"
rm "$work/calls"
report 111111 > /dev/null
check "a second read is the cached download's" "0" "$(grep -c 'run download' "$work/calls" 2>/dev/null || echo 0)"
check "a PR is read through the run its body names" "0" "$(report 230 --status ALL)"
check "...the same findings" "1" "$(grep -c 'Herr der Ringe' "$work/out")"

check "a run without the artifact is read from its pull-request job's log" "0" "$(report 222222 --status ALL --tsv)"
check "...its WRONG take, colours stripped" "1" "$(grep -c "^WRONG${tab}pl${tab}Janosik${tab}edukacja MH: Binti${tab}tmdb:855156${tab}Binti (2021)" "$work/out")"
check "...its UNJUDGED take with basis and ids" "1" "$(grep -c "^UNJUDGED${tab}uk${tab}Act One${tab}THE COMIC STRIP${tab}tmdb:396598${tab}The Comic Strip (1981)${tab}Filled${tab}tmdb:396598,imdb:tt0478854" "$work/out")"
check "...its LOST take, a comma in the title kept" "1" "$(grep -c "^LOST${tab}de${tab}Brotfabrik${tab}Jeder Acker, jede Fabrik${tab}${tab}${tab}${tab}${tab}wikidata:Q141180912" "$work/out")"
check "...and nothing from the Scala dumps" "4" "$(grep -vc '^\[identity-ratchet-report\]' "$work/out")"
rm -f "$work/calls"
check "a download that fails is not read as an absent artifact" "1 0" "$(GH_DOWN=1 report 333333) $(grep -c 'run view' "$work/calls")"
check "...it says it could not download, and keeps no findings" "1 no" \
  "$(grep -c "could not download run 333333's identity-ratchet artifact: HTTP 502" "$work/out") \
$([ -e "$work/cache/ratchet/333333/ratchet-takes.tsv" ] && echo yes || echo no)"
# The stub never answers: the report gets past it only by its timeout cutting gh off -- said, and no gh left running
# prove that, never elapsed wall time (shell-spec.sh's HUNG STUBS).
: > "$work/hung"
check "a gh that hangs is cut off, and fails" "1" "$(GH_HANGS="$work/hung" KINOWO_IDENTITY_DOWNLOAD_TIMEOUT=1 report 444444)"
check "...saying it timed out, leaving no gh running" "1 0" "$(grep -c 'timed out after 1s' "$work/out") $(spec_still_running "$work/hung")"
check "--help prints the usage" "0" "$(report --help)"
check "...naming the sources" "1" "$(grep -c 'run id | PR number' "$work/out")"
check "an unknown source is a usage error" "2" "$(report nonsense)"

spec_summary
