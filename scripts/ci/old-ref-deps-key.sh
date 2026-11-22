#!/usr/bin/env bash
# Purpose: The old refs' dependency cache: its key and fallback prefix, as GITHUB_OUTPUT lines (.github/actions/old-ref-deps)
# The old refs' dependency cache: its key and fallback prefix, as GITHUB_OUTPUT lines (.github/actions/old-ref-deps).
#
# WHY: the old-ref compile (main.yml's old-ref-compile row) compiles the identity tools into pinned old refs (scripts/identity-slice-old-ref-test.sh's
# floors), each with ITS OWN build: sbt 1.13.0 where main is on 1.13.1, and the libraries of its day. Main's dependency
# cache holds only main's build, so every check downloaded the floors' sbt and libraries from Maven Central, and a 429
# there failed the check (run 44807195909, 2026-11-20). The floors are fixed commits, so what their builds resolve is
# fixed too: keyed by the floors alone, one entry main saves once serves every check, offline.
#
# Its paths are the dependency cache's own (.github/actions/sbt-deps-key's): the entry is restored OVER main's, so the
# floors' builds find both theirs and what they share with main where sbt looks.
#   old-ref-deps-key.sh >> "$GITHUB_OUTPUT"
set -euo pipefail
here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
prefix="sbt-old-ref-deps-${RUNNER_OS:-$(uname -s)}-"
floors=$(bash "$here/../identity-slice-old-ref-test.sh" --floors)
[ -n "$floors" ] || { echo "old-ref-deps-key: the old-ref test names no floor" >&2; exit 1; }
digest=$(printf '%s\n' "$floors" | { sha256sum 2>/dev/null || shasum -a 256; } | cut -c1-16)
echo "prefix=$prefix"
echo "key=$prefix$digest"
