#!/usr/bin/env bash
# `gh <download args...>` for something that may legitimately not exist yet — a marker asset on a
# release, an artifact of a run that expired or never uploaded one.
#
# Prints `present` (downloaded) or `absent` (gh says there is nothing to download) on stdout and
# exits 0. Anything else — an auth refusal, a 5xx, a network failure — is a FAILED read, not an
# absent one: gh's stderr is shown and the script exits non-zero. The call sites used to run
# `gh ... 2>/dev/null` inside an `if`, which read an outage exactly like "no marker yet".
#
# Use it as an assignment, so `set -e` (GitHub's default `bash -eo pipefail`) stops on a failure:
#   present=$(scripts/ci/gh-optional.sh release download "$TAG" --pattern "$marker" --dir d --clobber)
#   if [ "$present" = present ]; then …
# Tested by scripts/ci/gh-optional-test.sh against a stub `gh`.
set -uo pipefail

err="$(mktemp)"
trap 'rm -f "$err"' EXIT

# gh's own stdout goes to stderr, so this script's stdout carries only the verdict.
gh "$@" >&2 2> "$err"
status=$?
if [ "$status" -eq 0 ]; then
  cat "$err" >&2
  echo present
  exit 0
fi
cat "$err" >&2
# What gh prints when the thing asked for is not there: a release asset pattern matching nothing,
# a missing release, a run with no such (or an expired) artifact, a run or release that 404s.
if grep -Eq 'no assets match|release not found|no artifact matches|no valid artifacts found|HTTP 404' "$err"; then
  echo absent
  exit 0
fi
echo "::error::gh $* failed (exit $status) — a failed read, not an absent one" >&2
exit "$status"
