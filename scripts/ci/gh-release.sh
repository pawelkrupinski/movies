#!/usr/bin/env bash
# `gh release <args...>`, retried only when GitHub answers with a race rather than a verdict.
#
# The verdicts are the `github-release` rows of test/resources/retry-classification.json;
# scripts/ci/gh-release-test.sh holds this script to that table row by row.
#
# RETRIED (transient): 409 (a concurrent write to the same release/asset), 5xx, and a 403 that
# is GitHub's secondary rate limit. FAILS FAST (permanent): `HTTP 403: Resource not accessible
# by integration` — the token was refused this request. It used to be retried as "transient",
# and on 2026-09-24 (run 36018126285, commit 9eee5d666) four attempts over 40s met the identical
# 403 on `release edit` of 382906859: within a run it never cleared, so retrying only delayed a
# red build. That run was not racing another (the workflow's concurrency group serialises them;
# the previous run's publish ended 11 min earlier), the release was neither draft nor immutable,
# and its token showed `Contents: write` exactly like the next run's, which published fine.
# Everything else (404 / `release not found`, 422) is an answer; retrying it would only delay the
# caller acting on it.
#
# Usage: gh-release.sh <view|edit|create|upload|...> <args...>
#   GH_RELEASE_ATTEMPTS      total attempts (default 4)
#   GH_RELEASE_RETRY_DELAY   first back-off in seconds, doubled per retry (default 5)
# Tested by scripts/ci/gh-release-test.sh against a stub `gh`.
set -uo pipefail

attempts="${GH_RELEASE_ATTEMPTS:-4}"
delay="${GH_RELEASE_RETRY_DELAY:-5}"
err="$(mktemp)"
trap 'rm -f "$err"' EXIT

# transient | permanent, for gh's stderr in "$err".
classify() {
  if grep -Eq 'HTTP (409|5[0-9][0-9])' "$err"; then echo transient
  elif grep -q 'HTTP 403' "$err" && grep -qi 'secondary rate limit' "$err"; then echo transient
  else echo permanent
  fi
}

for ((attempt = 1; ; attempt++)); do
  gh release "$@" 2> "$err"
  status=$?
  cat "$err" >&2
  [ "$status" -eq 0 ] && exit 0
  if grep -q 'Resource not accessible by integration' "$err"; then
    echo "::error::gh release $1 was refused with 403 'Resource not accessible by integration' — a permission verdict, not retried. Check the job's \`permissions:\` (needs contents: write) and whether the release is immutable or a draft owned by another actor." >&2
    exit "$status"
  fi
  if [ "$attempt" -ge "$attempts" ] || [ "$(classify)" != transient ]; then
    exit "$status"
  fi
  echo "::warning::gh release $1 failed transiently (attempt $attempt/$attempts); retrying in ${delay}s" >&2
  sleep "$delay"
  delay=$((delay * 2))
done
