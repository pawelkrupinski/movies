#!/usr/bin/env bash
# Every JVM a script launches with a bare `java -…` runs AWT headless. sbt's JVMs get the flag from .jvmopts (and the
# forked web Test JVM from build.sbt, both guarded by the TestJvmSpecs); a tool that runs `java` itself bypasses both.
# On macOS such a JVM that touches AWT registers as a foreground app and steals focus from whatever the user is doing:
# every `identity-what-if` resolve (WhatIfResolve) did, pulling the user onto the desktop at random moments.
#
#   scripts/tool-jvm-headless-test.sh
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

# a launch: `java -` at a command or array start, outside comments, in any non-test script
launches() {
  grep -rnE '(^|[(;&|"[:space:]])java -' "$@" 2>/dev/null \
    | grep -vE '^[^:]+-test\.sh:|^[^:]+_test\.py:|^[^:]+:[0-9]+:[[:space:]]*#'
}
missing() { launches "$@" | grep -v -- '-Djava.awt.headless=true' | cut -d: -f1,2; }

echo "[spec] tool-jvm-headless"
scratch="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-headless.XXXXXX")"; trap 'rm -rf "$scratch"' EXIT
printf '#!/bin/sh\n# java -Xmx1g is only a comment\nexec java -Xmx1g -cp x Main\n' > "$scratch/bare.sh"
printf '#!/bin/sh\nCMD=(java -Xmx1g -Djava.awt.headless=true -cp x Main)\n' > "$scratch/ok.sh"
check "the check finds a bare launch and passes a headless one (comments aside)" "$scratch/bare.sh:3" \
  "$(missing "$scratch")"
check "the repository's tool JVMs launch at least once (the check still sees them)" yes \
  "$([ "$(cd "$REPO_ROOT" && launches scripts .github/scripts infra/bin | wc -l)" -ge 3 ] && echo yes || echo no)"
check "every tool JVM a script launches runs AWT headless" "" \
  "$(cd "$REPO_ROOT" && missing scripts .github/scripts infra/bin)"
spec_summary
