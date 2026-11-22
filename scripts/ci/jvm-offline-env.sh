#!/usr/bin/env bash
# Purpose: Environment lines (for GITHUB_ENV) under which no JVM of the job reaches the network, and sbt-guard does not retry a download
# Environment lines (for GITHUB_ENV) under which no JVM of the job reaches the network, and sbt-guard does not retry a
# download: every http(s) connection goes to a proxy on 127.0.0.1's discard port, refused at once. A job whose
# dependencies were all restored from a cache (.github/actions/old-ref-deps) runs under it, so a build that would still
# download fails there, by name, instead of passing while Maven Central answers and failing when it rate-limits
# (run 44807195909's 429s).
#   jvm-offline-env.sh >> "$GITHUB_ENV"
set -euo pipefail
blocked="-Dhttp.proxyHost=127.0.0.1 -Dhttp.proxyPort=9 -Dhttps.proxyHost=127.0.0.1 -Dhttps.proxyPort=9"
echo "JAVA_TOOL_OPTIONS=${JAVA_TOOL_OPTIONS:+$JAVA_TOOL_OPTIONS }$blocked"
echo "SBT_GUARD_DOWNLOAD_BACKOFF="
