#!/usr/bin/env bash
# `gh release <args...>`, retried when GitHub answers with a race rather than a verdict.
#
# WHY. The rolling `android-latest` release answered `HTTP 403: Resource not accessible by
# integration` twice — 2026-09-04 (a create) and 2026-09-23 (an edit of release 382906859) — on
# runs whose job declares `contents: write`, between runs where the identical step, token and
# grant succeeded. Neither had a concurrent writer: the 09-23 run's duplicate sibling (GitHub
# fired two `push` runs for one PushEvent) was cancelled at t+3s, minutes before any release
# step. A 403 that the next attempt does not reproduce is transient on GitHub's side, so a
# release write that meets one waits and tries again instead of turning a green build red.
#
# Retries ONLY 403, 409 (a concurrent write to the same release/asset) and 5xx. A 404 ("release
# not found", which `gh release view` answers with no HTTP code at all) or a 422 is a real
# answer; retrying it would only delay the caller acting on it.
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

for ((attempt = 1; ; attempt++)); do
  gh release "$@" 2> "$err"
  status=$?
  cat "$err" >&2
  [ "$status" -eq 0 ] && exit 0
  if [ "$attempt" -ge "$attempts" ] || ! grep -Eq 'HTTP (403|409|5[0-9][0-9])' "$err"; then
    exit "$status"
  fi
  echo "::warning::gh release $1 failed transiently (attempt $attempt/$attempts); retrying in ${delay}s" >&2
  sleep "$delay"
  delay=$((delay * 2))
done
