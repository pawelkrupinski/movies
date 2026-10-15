#!/usr/bin/env bash
# `gh <download args...>` for something that may legitimately not exist yet — a marker asset on a
# release, an artifact of a run that expired or never uploaded one.
#
# Prints `present` (downloaded) or `absent` (gh says there is nothing to download) on stdout and
# exits 0. Anything else — an auth refusal, a 5xx, a network failure — is a FAILED read, not an
# absent one: gh's stderr is shown and the script exits non-zero. The call sites used to run
# `gh ... 2>/dev/null` inside an `if`, which read an outage exactly like "no marker yet".
#
# The download is tool-conventions.sh's gh_download — one classifier of absent against failed, one retry: a throttled
# artifact storage (a 5xx, "Egress is over the account limit") is asked again with a jittered backoff before it is a
# failed read. Each attempt is bounded by GH_OPTIONAL_TIMEOUT seconds (default 1800).
#
# Use it as an assignment, so `set -e` (GitHub's default `bash -eo pipefail`) stops on a failure:
#   present=$(scripts/ci/gh-optional.sh release download "$TAG" --pattern "$marker" --dir d --clobber)
#   if [ "$present" = present ]; then …
# Tested by scripts/ci/gh-optional-test.sh against a stub `gh`.
set -uo pipefail

# shellcheck source=scripts/tool-conventions.sh
. "$(dirname "${BASH_SOURCE[0]}")/../tool-conventions.sh" # gh_download

# gh's own stdout goes to stderr, so this script's stdout carries only the verdict.
gh_download "${GH_OPTIONAL_TIMEOUT:-1800}" "$@" >&2
case $? in
  0) echo present; exit 0 ;;
  3) echo absent; exit 0 ;;
esac
echo "::error::gh $* failed — a failed read, not an absent one (gh's error above)" >&2
exit 1
