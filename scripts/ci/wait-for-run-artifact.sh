#!/usr/bin/env bash
#
# Wait until THIS workflow run has uploaded artifact <name>, produced by another job of the run —
# main.yml's `image-worker` checks for the `stage-worker` dist ci's e2e corpus row stages (since
# 2026-10-22 it `needs: ci`, so the artifact is already there and this returns at once).
#
#   wait-for-run-artifact.sh <artifact name> <producing job name fragment> <timeout seconds>
#
# Exits 0 once the artifact is listed, 1 the moment the producing job finishes without it (failed,
# cancelled or skipped: it will never come) or when the timeout passes. THE TIMEOUT COUNTS FROM THE PRODUCER'S START:
# while every producing job still waits for a runner the waiter waits too, saying so, for at most WAIT_QUEUED_SECONDS
# (default 1800) -- Record scrape fixtures #181 (run 43122318896): the UK order row started at 23:47 and gave up at
# 00:07, "no artifact scrape-fixtures-uk after 1080s", while its convergence row, which uploads it, was still queued
# for a runner; it started at 00:12 and uploaded at 00:14. Needs GH_TOKEN with
# `actions: read`, GITHUB_REPOSITORY and GITHUB_RUN_ID; POLL_SECONDS overrides the 10 s poll -- 60 s on github.com,
# where GITHUB_TOKEN has 1,000 API requests an hour for the whole repository (github-return-plan 2026-10-30, 3.4).
set -euo pipefail

name=${1:?artifact name}
producer=${2:?producing job name fragment}
timeout=${3:?timeout seconds}
if [ "${GITHUB_SERVER_URL:-}" = "https://github.com" ]; then poll=${POLL_SECONDS:-60}; else poll=${POLL_SECONDS:-10}; fi
run="repos/$GITHUB_REPOSITORY/actions/runs/$GITHUB_RUN_ID"

# Capture the listing, then match it: `gh api … | grep -qx` would let grep exit at its first match
# while gh still writes a listing past the pipe buffer (over 100 artifacts, paginated), and the
# SIGPIPE that kills gh turns a match into a failure under pipefail. A failed listing is "not yet".
listed() {
    local names
    names=$(gh api "$run/artifacts?per_page=100" --paginate --jq '.artifacts[].name') || return 1
    grep -qx -- "$name" <<<"$names"
}

queued_limit=${WAIT_QUEUED_SECONDS:-1800}
started=""   # SECONDS when a producing job was first seen running (or none is listed): the timeout counts from it
said=-1      # the last minute the queued wait was said, so a long wait says itself once a minute
while :; do
    if listed; then
        echo "artifact $name is ready after ${SECONDS}s"
        exit 0
    fi
    # A failed listing (a GitHub API 5xx) says nothing about the producer: poll again rather than
    # let `set -e` fail a job that is only waiting.
    # `<status> <conclusion>` of every job whose name holds the fragment (read from the environment, never spliced into
    # the filter); the producer has ended only once each of them has -- a matrix, or a fragment two names share, is
    # still producing while one of its jobs runs.
    ended=$(PRODUCER=$producer gh api "$run/jobs?per_page=100" --paginate \
        --jq '.jobs[] | select(.name | contains(env.PRODUCER)) | "\(.status) \(.conclusion)"') ||
        { echo "::warning::could not list this run's jobs; polling again"; ended=""; }
    # Not one producing job has started (each still queued, waiting or pending): the timeout has not begun.
    if [ -n "$ended" ] && ! grep -qvE '^(queued|waiting|pending|requested) ' <<<"$ended"; then
        if [ "$SECONDS" -ge "$queued_limit" ]; then
            echo "::error::'$producer' still waited for a runner after ${SECONDS}s, so $name never came"
            exit 1
        fi
        if [ $((SECONDS / 60)) != "$said" ]; then said=$((SECONDS / 60)); echo "'$producer' is still queued for a runner after ${SECONDS}s: waiting for it to start"; fi
        sleep "$poll"
        continue
    fi
    [ -n "$started" ] || started=$SECONDS
    if grep -qv '^completed ' <<<"$ended"; then ended=""; fi
    ended=$(sed 's/^completed //' <<<"$ended" | paste -sd, -)
    # A finished producer may have uploaded in the moment since the listing above: look once more.
    if [ -n "$ended" ]; then
        if listed; then
            echo "artifact $name is ready after ${SECONDS}s"
            exit 0
        fi
        echo "::error::'$producer' finished ($ended) without uploading $name"
        exit 1
    fi
    if [ $((SECONDS - started)) -ge "$timeout" ]; then
        echo "::error::no artifact $name after ${timeout}s of '$producer' running"
        exit 1
    fi
    sleep "$poll"
done
