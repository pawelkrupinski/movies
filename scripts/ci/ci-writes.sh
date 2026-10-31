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
# One exception, fixture data in the convergence-fixtures release ([[ci_writes_shadow_data]], below).
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

# THE ONE SHADOW EXCEPTION: fixture DATA in movies' own convergence-fixtures release. A github.com convergence leg
# publishes what its fill fetched (fill-*.tar.zst) and the gaps it could not answer or was refused (refetch-*.tsv,
# refused-*.tsv), and prunes its older lists, as Forgejo's legs do. Without them github.com's takes and
# order-independence rows read only the fills copied over from Forgejo, and went red whenever Forgejo had published a
# newer one (convergence 41209071921: "the code moved listings to another film" in pl, de and uk, green on Forgejo).
# They are data a later leg reads, never a deploy, a pin or a pointer: the pins, prod-latest-/hermetic-/corpus-green-
# pointers, enrichment trees, the takes store, recordings, images and every other write stay CI_ROLE=primary only.
# The names carry the run id ($GITHUB_RUN_ID), and the two forges' run ids never meet: github.com's are 11 digits
# (41209071921), Forgejo's 4 (4433) -- a github.com leg's data never overwrites Forgejo's, nor the other way round.
CI_SHADOW_DATA_RELEASE=convergence-fixtures
CI_SHADOW_DATA_ASSET='^(fill-[a-z]{2}-[0-9]+-[0-9]+-[0-9]+(-[a-z]+)?\.tar\.zst|(refetch|refused)-[a-z]{2}-[0-9]+-[0-9]+-[0-9]+(-[a-z]+)?\.tsv)$'

# ci_writes_shadow_data <gh release subcommand> <tag> <args...>: 0 when the call is an upload or delete-asset of such
# data alone, in the convergence-fixtures release (every asset it names matches; flags are skipped).
ci_writes_shadow_data() {
  local cmd=${1:-} tag=${2:-} named=0 arg
  case "$cmd" in upload|delete-asset) ;; *) return 1 ;; esac
  [ "$tag" = "$CI_SHADOW_DATA_RELEASE" ] || return 1
  shift 2
  for arg in "$@"; do
    case "$arg" in -*) continue ;; esac
    [[ "${arg##*/}" =~ $CI_SHADOW_DATA_ASSET ]] || return 1
    named=$((named + 1))
  done
  [ "$named" -gt 0 ]
}

if [ "${BASH_SOURCE[0]}" = "$0" ]; then
  ci_writes
fi
