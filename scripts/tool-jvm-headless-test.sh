#!/usr/bin/env bash
# Every JVM a script launches with a bare `java -…` runs AWT headless. sbt's JVMs get the flag from .jvmopts (and the
# forked web Test JVM from build.sbt, both guarded by the TestJvmSpecs); a tool that runs `java` itself bypasses both.
# On macOS such a JVM that touches AWT registers as a foreground app and steals focus from whatever the user is doing:
# every `identity-what-if` resolve (WhatIfResolve) did, pulling the user onto the desktop at random moments.
#
#   scripts/tool-jvm-headless-test.sh
# script-test-reads: scripts/* .github/* infra/* android/* ios/*
set -uo pipefail
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
. "$REPO_ROOT/scripts/shell-spec.sh"

# a launch: `java -` at a command or array start, outside comments, in any non-test script -- text files only, never
# an installed dependency tree (an npm package's binaries and vendored scripts are not the repository's)
launches() {
  grep -rnIE --exclude-dir=node_modules '(^|[(&;|"[:space:]])java -' "$@" 2>/dev/null \
    | grep -vE '^[^:]+-test\.sh:|^[^:]+_test\.py:|^[^:]+:[0-9]+:[[:space:]]*#'
}
# a Python tool's launch: an argv list holding `"java"` (or a `java` variable) -- the flag may sit in a constant of
# the file (JVM_FLAGS) or in lib/identity_jvm.JAVA_OPTIONS, so a launch is headless when its file names either; a tuple of names (`("java", "mongod")`) is no
# argv. 2026-11-25, Tool self-tests #551: the shell-to-Python migration moved launches out of the shell grep's sight.
python_launches() {
  python3 -I -B - "$@" <<'PY'
import ast, os, sys
def sources(roots):
    for root in roots:
        for top, dirs, files in os.walk(root):
            dirs[:] = [d for d in dirs if d != "node_modules"]
            for name in sorted(files):
                path = os.path.join(top, name)
                if name.endswith("_test.py"):
                    continue
                try:
                    with open(path, "rb") as f:
                        text = f.read()
                except OSError:
                    continue
                if name.endswith(".py") or (b"\0" not in text and text.startswith(b"#!") and b"python" in text.split(b"\n", 1)[0]):
                    yield path, text
def is_java(node):
    return (isinstance(node, ast.Constant) and node.value == "java") or (isinstance(node, ast.Name) and node.id == "java")
for path, text in sources(sys.argv[1:]):
    try:
        tree = ast.parse(text)
    except (SyntaxError, ValueError):
        continue
    headless = b"-Djava.awt.headless=true" in text or b"identity_jvm.JAVA_OPTIONS" in text
    for node in ast.walk(tree):
        if isinstance(node, ast.List) and any(is_java(e) for e in node.elts):
            print("%s:%d:%s" % (path, node.lineno, "-Djava.awt.headless=true" if headless else "bare"))
PY
}
missing() { { launches "$@"; python_launches "$@"; } | grep -v -- '-Djava.awt.headless=true' | cut -d: -f1,2; }

echo "[spec] tool-jvm-headless"
scratch="$(mktemp -d "${TMPDIR:-/tmp}/kinowo-headless.XXXXXX")"; trap 'rm -rf "$scratch"' EXIT
printf '#!/bin/sh\n# java -Xmx1g is only a comment\nexec java -Xmx1g -cp x Main\n' > "$scratch/bare.sh"
printf '#!/bin/sh\nCMD=(java -Xmx1g -Djava.awt.headless=true -cp x Main)\n' > "$scratch/ok.sh"
check "the check finds a bare launch and passes a headless one (comments aside)" "$scratch/bare.sh:3" \
  "$(missing "$scratch")"
# 2026-11-21, Order independence #99: js-quality-report-test.sh, run earlier in the shuffled order, installs
# scripts/js-quality/node_modules; a bare `grep -r` read its jscpd binary and failed with "Binary file ... matches"
mkdir -p "$scratch/installed/node_modules/pkg/bin"
printf 'x\000exec java -Xmx1g -cp x Main\n' > "$scratch/installed/node_modules/pkg/bin/tool"
printf '#!/bin/sh\nexec java -Xmx1g -cp x Main\n' > "$scratch/installed/node_modules/pkg/vendored.sh"
printf 'import subprocess\n# ["java", "-Xmx1g"] is only a comment\nsubprocess.run(["java", "-Xmx1g", "Main"])\nCOMMS = ("java", "mongod")\n' > "$scratch/bare.py"
printf '#!/usr/bin/env python3\nFLAGS = ["-Djava.awt.headless=true"]\njava = "java"\nARGV = ["gov-run", "--", java, *FLAGS, "Main"]\n' > "$scratch/ok-tool"
check "the check finds a bare Python launch and passes a headless one (comments and name tuples aside)" \
  "$scratch/bare.py:3" "$(missing "$scratch" | grep -v '\.sh:')"
printf 'import identity_jvm\nARGV = ["java", *identity_jvm.JAVA_OPTIONS, "Main"]\n' > "$scratch/shared-options.py"
check "a Python launch spreading lib/identity_jvm.JAVA_OPTIONS (the shared headless flags) is headless" "" \
  "$(missing "$scratch/shared-options.py")"
check "an installed node_modules (binaries, vendored scripts) is never read as a launch" "" \
  "$(missing "$scratch/installed")"
check "the repository's tool JVMs launch at least once (the check still sees them)" yes \
  "$([ "$(cd "$REPO_ROOT" && { launches scripts .github/scripts infra/bin; python_launches scripts .github/scripts infra/bin; } | wc -l)" -ge 3 ] && echo yes || echo no)"
check "every tool JVM a script launches runs AWT headless" "" \
  "$(cd "$REPO_ROOT" && missing scripts .github/scripts infra/bin)"
spec_summary
