#!/usr/bin/env bash
#
# THE PAIR EVERY LEG WILL REPLAY EXISTS, checked in seconds before any leg takes a runner (identity-model-convergence.yml's
# `preflight`). Each of these used to fail every leg of a run separately, minutes into its setup, after the JDK, sbt
# and the build cache: 2026-10-20..22, 50 takes rows on a recording this forge never held (38713274617, "the pinned pair
# has expired"), 30 rows on a pinned tree the release no longer kept ("it keeps the newest five") and 15 on a country
# with no pin -- ~5 of the 13.4 failed job-hours those two days.
#
#   convergence-preflight.sh <codes, comma-separated> [recording run]
#
# With a recording (the workflow's `recording` input, identity-measure --recording): that run's scrape corpus
# (`scrape-fixtures-<code>`) and its tree (`enrichment-<code>-<run>.tar.*` in $FIXTURE_RELEASE_TAG), per country.
# Without: each country's pin (`hermetic-<code>.txt`, convergence-setup.sh's `pinned`), the tree it names, and the
# corpus of the run it names. Fills are not checked (convergence-fill.sh lists only what the release holds).
#
# ONLY AN ANSWERED "NOT THERE" FAILS: a gh read that fails otherwise is said as a warning and passes, exactly as
# convergence-setup.sh tells a failed read from an expired pair (a throttled storage is no evidence of a missing pair).
# Reads GH_TOKEN, GH_REPO (gh's own) and FIXTURE_RELEASE_TAG.
set -uo pipefail

codes=${1:?usage: convergence-preflight.sh <codes> [recording run]}
recording=${2:-}
tag=${FIXTURE_RELEASE_TAG:-convergence-fixtures}
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

problems=0
error() { echo "::error::$*"; problems=$((problems + 1)); }
warn() { echo "::warning::$*"; }

# The release's asset names, one per line; empty file and a warning when the release cannot be read.
assets_known=true
if ! gh release view "$tag" --json assets --jq '.assets[].name' >"$work/assets" 2>"$work/err"; then
    warn "could not read release $tag ($(head -c 200 "$work/err")) -- its pins and trees are not checked here"
    assets_known=false
fi
has_asset() { # <glob>
    local name
    while IFS= read -r name; do
        # shellcheck disable=SC2053 # the pin names its tree as a glob (`enrichment-de-573.tar.*`)
        [[ $name == $1 ]] && return 0
    done <"$work/assets"
    return 1
}

# artifacts <run>: the run's unexpired artifact names into $work/artifacts-<run>. Exit 0 read, 3 the forge answered
# the run does not exist (a github.com-era run on Forgejo), 1 any other failure.
artifacts() {
    [ -f "$work/artifacts-$1" ] && return 0
    [ -f "$work/status-$1" ] && return "$(cat "$work/status-$1")"
    if gh api "repos/${GH_REPO:?GH_REPO names the repository}/actions/runs/$1/artifacts?per_page=100" \
        --jq '.artifacts[] | select(.expired != true) | .name' >"$work/artifacts-$1" 2>"$work/err"; then
        return 0
    fi
    rm -f "$work/artifacts-$1"
    if grep -q 'HTTP 404' "$work/err"; then echo 3 >"$work/status-$1"; else echo 1 >"$work/status-$1"; fi
    return "$(cat "$work/status-$1")"
}

# corpus_present <run> <code> <what the pair is>
corpus_present() {
    artifacts "$1"
    case $? in
        0) grep -qxF "scrape-fixtures-$2" "$work/artifacts-$1" ||
            error "$3: recording run $1 holds no scrape-fixtures-$2 (its artifacts expired or were deleted) -- re-run Record scrape fixtures, or measure a recording this forge holds" ;;
        3) error "$3: recording run $1 is not on this forge (a github.com-era run?) -- measure the pinned pair or a recording this forge holds" ;;
        *) [ -f "$work/warned-$1" ] || warn "could not list recording run $1's artifacts ($(head -c 200 "$work/err")) -- not checked"
           touch "$work/warned-$1" ;;
    esac
}

if [ -z "$recording" ] && [ "$assets_known" = true ]; then
    gh release download "$tag" --pattern 'hermetic-*.txt' --dir "$work/pins" --clobber >/dev/null 2>"$work/err" ||
        { warn "could not download the pins ($(head -c 200 "$work/err")) -- not checked"; assets_known=pins-unread; }
fi

IFS=, read -r -a list <<<"$codes"
for code in "${list[@]}"; do
    [ -n "$code" ] || continue
    if [ -n "$recording" ]; then
        [ "$assets_known" != true ] || has_asset "enrichment-$code-$recording.tar.*" ||
            error "$code: release $tag holds no enrichment-$code-$recording.tar.* (recording $recording's tree; it keeps the newest five recordings)"
        corpus_present "$recording" "$code" "$code"
        continue
    fi
    [ "$assets_known" = true ] || continue
    if ! has_asset "hermetic-$code.txt"; then
        error "No recorded pair pinned for $code (release asset hermetic-$code.txt) -- a hermetic leg replays nothing else. Record one: gh workflow run \"Record scrape fixtures\" --ref main"
        continue
    fi
    read -r corpus _ tree _ <"$work/pins/hermetic-$code.txt"
    if [ -z "${tree:-}" ]; then
        error "hermetic-$code.txt is malformed: '$(head -1 "$work/pins/hermetic-$code.txt")'"
        continue
    fi
    has_asset "$tree" || error "$code: the pinned tree $tree is not in release $tag (it keeps the newest five) -- a hermetic leg replays nothing else"
    corpus_present "$corpus" "$code" "$code's pinned pair"
done

if [ "$problems" -gt 0 ]; then
    echo "$problems problem(s): no leg would replay its pair, so none starts"
    exit 1
fi
echo "every leg's pair is there (${recording:+recording $recording, }countries $codes)"
