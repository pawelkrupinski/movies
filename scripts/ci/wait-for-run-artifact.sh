#!/usr/bin/env bash
#
# Wait until THIS workflow run has uploaded artifact <name>, from a job that cannot `needs:` its
# producer — main.yml's `image-worker` waits on the `stage-worker` dist ci's e2e staging row stages,
# because `needs: ci` would also make it wait for every other ci row.
#
#   wait-for-run-artifact.sh <artifact name> <producing job name fragment> <timeout seconds>
#
# Exits 0 once the artifact is listed, 1 the moment the producing job finishes without it (failed,
# cancelled or skipped: it will never come) or when the timeout passes. Needs GH_TOKEN with
# `actions: read`, GITHUB_REPOSITORY and GITHUB_RUN_ID; POLL_SECONDS overrides the 10 s poll.
set -euo pipefail

name=${1:?artifact name}
producer=${2:?producing job name fragment}
timeout=${3:?timeout seconds}
poll=${POLL_SECONDS:-10}
run="repos/$GITHUB_REPOSITORY/actions/runs/$GITHUB_RUN_ID"

deadline=$((SECONDS + timeout))
while :; do
    if gh api "$run/artifacts?per_page=100" --jq '.artifacts[].name' | grep -qx "$name"; then
        echo "artifact $name is ready after ${SECONDS}s"
        exit 0
    fi
    ended=$(gh api "$run/jobs?per_page=100" --paginate \
        --jq ".jobs[] | select(.name | contains(\"$producer\")) | select(.status == \"completed\") | .conclusion")
    # A finished producer may have uploaded in the moment since the listing above: look once more.
    if [ -n "$ended" ]; then
        if gh api "$run/artifacts?per_page=100" --jq '.artifacts[].name' | grep -qx "$name"; then
            echo "artifact $name is ready after ${SECONDS}s"
            exit 0
        fi
        echo "::error::'$producer' finished ($ended) without uploading $name"
        exit 1
    fi
    if [ "$SECONDS" -ge "$deadline" ]; then
        echo "::error::no artifact $name after ${timeout}s"
        exit 1
    fi
    sleep "$poll"
done
