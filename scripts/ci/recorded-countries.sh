#!/usr/bin/env bash
# WHICH COUNTRIES A RECORDING CAPTURED COMPLETE: the codes whose tree `Record scrape fixtures` run <run> published as
# its pinned copy, `enrichment-<code>-<run>.tar.*` on the $FIXTURE_RELEASE_TAG release -- which convergence-publish.sh
# uploads only when that country's boot completed. Printed comma-separated, in <codes>' order; empty when none.
#
#   recorded-countries.sh <run> [<codes>]     # <codes> default pl,de,uk,es,us; an empty <codes> is the default too
#
# WHY. Capture and verify are separate verdicts ("Capture, then verify"): a country the capture left
# incomplete is the capture's red, so the verify run judges only the countries this names, and the recorder dispatches
# no verify when it names none. Exit 1 (saying why) when the release cannot be read: an unread release is no evidence
# that nothing was captured.
# Tested by scripts/ci/recorded-countries-test.sh against a stub `gh`.
set -uo pipefail

run="${1:?usage: recorded-countries.sh <run> [<codes>]}"
codes="${2:-}"
[ -n "$codes" ] || codes="pl,de,uk,es,us"
tag="${FIXTURE_RELEASE_TAG:-convergence-fixtures}"

assets="$(gh release view "$tag" --json assets --jq '.assets[].name' 2>&1)" || {
    echo "could not list release $tag's assets: $(head -c 200 <<< "$assets")" >&2
    exit 1
}

found=()
IFS=, read -r -a list <<< "$codes"
for code in "${list[@]}"; do
    [ -n "$code" ] || continue
    grep -qE "^enrichment-$code-$run\\.tar\\.(gz|zst)$" <<< "$assets" && found+=("$code")
done
(IFS=,; echo "${found[*]:-}")
