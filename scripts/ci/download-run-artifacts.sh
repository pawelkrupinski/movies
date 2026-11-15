#!/usr/bin/env bash
# download-run-artifacts.sh <pattern> <dir>: every artifact of this run (GITHUB_RUN_ID, GITHUB_REPOSITORY) whose name
# matches <pattern>, each into <dir>/<artifact name> (`gh run download -p` nests by name, the only match too) -- the
# body of .github/actions/download-artifacts.
#
# Through tool-conventions.sh's gh_download, one classifier for every download: a 5xx, a timeout or a throttled
# artifact storage is asked again with a jittered backoff, each attempt said in the log and bounded by
# DOWNLOAD_ARTIFACTS_TIMEOUT seconds (300), the directory emptied between attempts. A single read used to fail the job
# outright on GitHub's first 5xx. No artifact matching is still a failure ("no artifact matches"), never asked again:
# the rows that should have uploaded them did not.
# Tested by scripts/ci/download-run-artifacts-test.sh against a stub `gh`.
set -uo pipefail
pattern=${1:?usage: download-run-artifacts.sh <pattern> <dir>}
dest=${2:?usage: download-run-artifacts.sh <pattern> <dir>}
# shellcheck source=scripts/tool-conventions.sh
. "$(dirname "${BASH_SOURCE[0]}")/../tool-conventions.sh" # gh_download
mkdir -p "$dest" || exit 1
gh_download "${DOWNLOAD_ARTIFACTS_TIMEOUT:-300}" run download "${GITHUB_RUN_ID:?}" -R "${GITHUB_REPOSITORY:?}" \
  --pattern "$pattern" --dir "$dest"
case $? in
  0) ;;
  3) echo "::error::no artifact of run $GITHUB_RUN_ID matches $pattern"; exit 1 ;;
  *) echo "::error::could not download run $GITHUB_RUN_ID's $pattern artifacts -- a failed read after every attempt (gh's errors above), not a missing artifact"; exit 1 ;;
esac
find "$dest" -mindepth 1 -maxdepth 1 -exec basename {} \; | sort | sed 's/^/downloaded /'
