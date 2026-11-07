#!/usr/bin/env bash
# recording-held.sh <run>: does the repository hold "Record scrape fixtures" recording <run> where
# scripts/identity-fetch-recording.sh looks for its corpus? First the ARCHIVE release (RECORDING_ARCHIVE_TAG, default
# identity-recordings: scrapes-<cc>-<run>.tar.*, where the Forgejo-era recordings 573 and 528 live -- numbers no run
# of this forge has), then the run's own scrape-fixtures-<cc> artifacts.
#
# Exit 0 held (or unreadable: a listing that fails is no evidence, warned and passed), 1 the run holds no corpus
# (expired or deleted), 3 neither the archive nor this forge holds it. Every refusal is a ::error:: line.
# Tested by scripts/ci/recording-held-test.sh.
set -uo pipefail
run=${1:?usage: recording-held.sh <run>}
archive=${RECORDING_ARCHIVE_TAG:-identity-recordings}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

if gh release view "$archive" -R "${GH_REPO:?GH_REPO names the repository}" --json assets --jq '.assets[].name' \
        >"$work/assets" 2>"$work/err"; then
    grep -Eq "^scrapes-[a-z]+-$run\.tar\.(zst|gz)$" "$work/assets" && exit 0
else
    echo "::warning::could not list release $archive ($(head -c 200 "$work/err")) -- reading run $run's artifacts"
fi

if gh api "repos/$GH_REPO/actions/runs/$run/artifacts?per_page=100" \
        --jq '.artifacts[] | select(.expired != true) | .name' >"$work/names" 2>"$work/err"; then
    grep -q '^scrape-fixtures-' "$work/names" && exit 0
    echo "::error::recording $run holds no scrape-fixtures-* corpus (expired or deleted)"
    exit 1
elif grep -q 'HTTP 404' "$work/err"; then
    echo "::error::neither the archive release $archive nor this forge holds recording $run"
    exit 3
fi
echo "::warning::could not list recording $run's artifacts ($(head -c 200 "$work/err")) -- not checked"
exit 0
