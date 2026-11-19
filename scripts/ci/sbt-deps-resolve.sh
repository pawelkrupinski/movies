#!/usr/bin/env bash
# Resolve every dependency of the sbt build once (`sbt update` over the root aggregate: every module, every
# configuration, and the build's own plugins as it loads), so ci.yml's `deps` job can save the dependency cache under
# the build's exact key before the run's other jobs restore it.
# Usage: scripts/ci/sbt-deps-resolve.sh
#
# Retries the whole resolve, with a backoff of tens of seconds, because nothing under it retries a 429: lm-coursier
# 2.1.13 (sbt 1.13.1) re-resolves only a "transient" failure -- "Connection timed out" or an HTTP 5xx, 3 attempts 1 s
# apart, doubling (ResolutionRun.isTransientResolutionError, ResolutionParams.defaultRetry) -- and its file cache retries
# only SSL and access-denied errors. So the 429 Maven Central answered a burst of runners with (run 44250628374) failed
# the job at once, and neither `csrConfiguration.withRetry` nor a COURSIER_* variable changes that (the shaded copy reads
# `lmcoursier.internal.shaded.coursier.*` properties). Each attempt keeps what the one before it downloaded.
# Env: SBT_DEPS_BACKOFF (seconds before each retry, space-separated; default "30 60 120": four attempts, ~3.5 min).
# Test: scripts/ci/sbt-deps-resolve-test.sh.
set -euo pipefail

read -r -a waits <<< "${SBT_DEPS_BACKOFF:-30 60 120}"
attempts=$((${#waits[@]} + 1))

attempt=1
while :; do
  echo "sbt-deps: resolving the build's dependencies (attempt $attempt of $attempts)"
  if sbt -Dsbt.supershell=false -Dsbt.color=false update; then
    echo "sbt-deps: resolved on attempt $attempt"
    exit 0
  fi
  if [ "$attempt" -ge "$attempts" ]; then
    echo "::error::sbt-deps: the build's dependencies did not resolve in $attempts attempts -- see the download errors above" >&2
    exit 1
  fi
  wait="${waits[$((attempt - 1))]}"
  echo "sbt-deps: attempt $attempt failed; retrying in ${wait}s"
  sleep "$wait"
  attempt=$((attempt + 1))
done
