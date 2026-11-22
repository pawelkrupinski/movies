#!/usr/bin/env bash
# Purpose: Resolve every dependency of the sbt build once (`sbt update` over the root aggregate
# Resolve every dependency of the sbt build once (`sbt update` over the root aggregate: every module, every
# configuration, and the build's own plugins as it loads), so ci.yml's `deps` job can save the dependency cache under
# the build's exact key before the run's other jobs restore it.
# Usage: scripts/ci/sbt-deps-resolve.sh
#
# The retries are the sbt guard's (scripts/ci/sbt-guard/sbt, first on every CI job's PATH): lm-coursier 2.1.13 (sbt
# 1.13.1) re-resolves only a "transient" failure -- "Connection timed out" or an HTTP 5xx, 3 attempts 1 s apart,
# doubling (ResolutionRun.isTransientResolutionError, ResolutionParams.defaultRetry) -- and its file cache retries only
# SSL and access-denied errors, so neither the 429 Maven Central answered a burst of runners with (run 44250628374) nor
# a 404 it gave one runner for a pom it served the rest (Main #2678) was retried, and neither
# `csrConfiguration.withRetry` nor a COURSIER_* variable changes that (the shaded copy reads
# `lmcoursier.internal.shaded.coursier.*` properties). The guard runs a download failure again after each of
# SBT_GUARD_DOWNLOAD_BACKOFF's waits (default "30 60 120": four attempts, ~3.5 min), each keeping what the ones before
# it downloaded.
#
# Then coursier's negative records go (coursier-forget-errors.sh), so the entry the job saves carries none: every job
# restoring it would otherwise take a 404 one runner once got as the answer, without asking the repository again.
# Test: scripts/ci/sbt-deps-resolve-test.sh.
set -euo pipefail

if ! sbt -Dsbt.supershell=false -Dsbt.color=false update; then
  echo "::error::sbt-deps: the build's dependencies did not resolve -- see the download errors above" >&2
  exit 1
fi
"$(dirname "$0")/coursier-forget-errors.sh"
