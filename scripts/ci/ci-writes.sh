#!/usr/bin/env bash
# MAY THIS JOB WRITE OUTSIDE ITS RUNNER? The one shadow switch of the move back to GitHub (github-return-plan
# 2026-10-30, 3.1): while Forgejo is primary, GitHub runs every workflow in SHADOW -- it builds, tests and judges,
# and writes nothing: no image push, no publish or deploy marker, no release or release asset (recordings,
# convergence fixtures, the takes store, the Android pre-releases), no bot branch, PR or issue, no metric push, no
# Grafana annotation, no commit comment, no measure/check/land branch.
#
#   writes allowed  <=>  GITHUB_SERVER_URL != https://github.com  ||  CI_ROLE == primary
#
# So Forgejo (and a laptop: no GITHUB_SERVER_URL) behaves exactly as before, and github.com writes only once the
# owner sets the repository variable CI_ROLE=primary at the cut. UNSET COUNTS AS SHADOW: a workflow that forgets to
# hand its job `CI_ROLE: ${{ vars.CI_ROLE }}` stays read-only on github.com rather than writing twice.
#
# The same expression gates whole jobs and steps in the workflows (WriteGateSpec):
#   if: github.server_url != 'https://github.com' || vars.CI_ROLE == 'primary'
# This file is for writes that happen inside a script or composite action, where `vars` cannot be read.
#
# Usage, as a command:   scripts/ci/ci-writes.sh && push=--push
#        sourced:        . scripts/ci/ci-writes.sh; ci_writes || { ci_writes_skipped "the upload"; exit 0; }
# Exit: 0 writes allowed, 1 shadow. Tested by scripts/ci/ci-writes-test.sh.

ci_writes() {
  [ "${GITHUB_SERVER_URL:-}" != "https://github.com" ] || [ "${CI_ROLE:-}" = primary ]
}

ci_writes_skipped() { # <what was not done>
  echo "::notice title=Shadow run, not written::${*:-a write} skipped: github.com with CI_ROLE=${CI_ROLE:-unset} (only CI_ROLE=primary writes; Forgejo is primary)" >&2
}

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  ci_writes
fi
