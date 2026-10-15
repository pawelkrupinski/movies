#!/usr/bin/env bash
# identity-capture-report.sh over a scratch capture: the countries' capture logs, the ratchet's findings TSV and
# its sbt log, as the identity-capture workflow's publish job holds them.
# Run: bash scripts/ci/identity-capture-report-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

printf '\033[36m▸\033[0m identity-capture-report.sh\n'

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
report="$REPO_ROOT/scripts/ci/identity-capture-report.sh"

mkdir -p "$work/captures/identity-capture-pl"
printf '[identity-capture] pl: CAPTURE 120 clusters\nnoise\n  captured 120 listings\n' \
  > "$work/captures/identity-capture-pl/identity-capture-pl.txt"
printf 'status\tcountry\tvenue\trawTitle\tfilm\ttitle\tbasis\tids\twas\texplanation\n' > "$work/takes.tsv"
printf 'UNJUDGED\tpl\tkino-x\tDiuna 2\ttmdb:1\tDune 2\ttitle\t\t\twhy\n' >> "$work/takes.tsv"
printf 'RIGHT\tpl\tkino-x\tAlien\ttmdb:2\tAlien\ttitle\t\t\t\n' >> "$work/takes.tsv"
printf '[info] - pl *** FAILED ***\n[info] the fixture lacks answers this resolve asks\n[info] compiling\n' > "$work/ratchet.log"

run() { (cd "$work" && OPEN_PR="$1" GREEN="$2" BRANCH=chore/identity-capture GITHUB_RUN_ID=42 \
  bash "$report" captures takes.tsv ratchet.log); }

red=$(run false false)
# shellcheck disable=SC2016 # the backticks are the report's Markdown, matched literally
check "a weekly recapture names its run and branch" "true" \
  "$(grep -q 'weekly recapture.*run 42), on `chore/identity-capture`' <<< "$red" && echo true || echo false)"
check "a red ratchet asks for judgements, not a PR" "true" "$(grep -q 'The ratchet is red, so no PR yet' <<< "$red" && echo true || echo false)"
check "each country's capture lines are listed, its noise is not" "[identity-capture] pl: CAPTURE 120 clusters|  captured 120 listings" \
  "$(sed -n '/### Per country/,/### The ratchet/p' <<< "$red" | grep -E 'identity-capture|captured' | paste -sd'|' -)"
check "the findings come from the TSV, through scripts/identity-ratchet-report" "true" \
  "$(grep -q 'findings: UNJUDGED 1, RIGHT 1' <<< "$red" && grep -q 'Diuna 2' <<< "$red" && echo true || echo false)"
check "a right take is counted, not listed" "false" "$(grep -q '  pl  kino-x  *Alien' <<< "$red" && echo true || echo false)"
check "the sbt log's failures, which no TSV carries, are kept" "true" \
  "$(grep -q '\- pl \*\*\* FAILED \*\*\*' <<< "$red" && grep -q 'lacks answers' <<< "$red" && echo true || echo false)"
check "the rest of the sbt log is not" "false" "$(grep -q compiling <<< "$red" && echo true || echo false)"
check "the run's untruncated findings command is named" "true" \
  "$(grep -q 'scripts/identity-ratchet-report 42 --explain' <<< "$red" && echo true || echo false)"

green=$(run true true)
check "a judged recapture is named as the re-check" "true" "$(grep -q 'The judged recapture.*re-checked' <<< "$green" && echo true || echo false)"
check "a green ratchet says it is land-ready" "true" "$(grep -q 'The ratchet is green' <<< "$green" && echo true || echo false)"

rm -rf "$work/captures"
recheck=$(run true true)
check "no captures (a re-check) lists no countries" "false" "$(grep -q 'Per country' <<< "$recheck" && echo true || echo false)"
rm -f "$work/takes.tsv"
none=$(run false false)
check "no findings TSV says so rather than failing" "true" \
  "$(grep -q 'the ratchet wrote no findings' <<< "$none" && echo true || echo false)"

spec_summary
