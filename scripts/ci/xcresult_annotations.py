#!/usr/bin/env python3
"""Every failed test of an .xcresult bundle as a GitHub annotation that carries its own assertion.

    python3 scripts/ci/xcresult_annotations.py [--level error|warning] <bundle.xcresult>
    python3 scripts/ci/xcresult_annotations.py [--level error|warning] --json <tests.json>

One `::error file=<path>,line=<n>,title=<Class>.<test>::<Class>.<test>: <assertion>` line per failure message
(`::warning` with --level warning), from xcresult_junit.py's reading of `xcrun xcresulttool get test-results tests`.
The message names the test again because a job's log shows only the message, and the first error a failed step
prints is the line its red is known by: with nothing but "XCUITest classes that fail when run alone: <classes>"
there, Order independence #28-#31 -- a dropped tap, a slider drag a step short, a production the simulator could not
reach -- read as one red that had outlived three fixes. Paths are made relative to the checkout (the directory it runs
in), which is what an annotation's file= wants; one outside it keeps its message and drops the file.

A bundle it cannot read (a step killed mid-write) is a warning naming it, never a failure of its own: the caller's
exit status grades the tests."""
import os
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent))
import xcresult_junit  # noqa: E402

LEVELS = ("error", "warning")


def data(text):
    """`text` as a workflow command's message: what would end or split the line, escaped."""
    return text.replace("%", "%25").replace("\r", "%0D").replace("\n", "%0A")


def prop(text):
    """`text` as a workflow command's property value, where `:` and `,` are syntax too."""
    return data(text).replace(":", "%3A").replace(",", "%2C")


def in_checkout(path, checkout):
    """`path` relative to `checkout`, or None when it lies outside it (or there is none)."""
    if not path:
        return None
    root = os.path.realpath(checkout) + os.sep
    real = os.path.realpath(path)
    return real[len(root):] if real.startswith(root) else None


def annotations(tree, level, checkout):
    lines = []
    for suite, case in xcresult_junit.test_cases(tree):
        if case.get("result") != "Failed":
            continue
        test = f"{suite}.{case.get('name', '').removesuffix('()')}"
        for failure in xcresult_junit.message_nodes(case, "Failure Message") or [{"name": "failed"}]:
            location = failure.get("sourceLocation") or {}
            path = in_checkout(location.get("filePath"), checkout)
            where = f"file={prop(path)},line={location['lineNumber']}," if path and "lineNumber" in location else ""
            lines.append(f"::{level} {where}title={prop(test)}::{data(test + ': ' + failure.get('name', ''))}")
    return lines


def main(argv, checkout=None):
    args = argv[1:]
    level = "error"
    if args[:1] == ["--level"]:
        if len(args) < 2 or args[1] not in LEVELS:
            sys.exit(f"usage: xcresult_annotations.py [--level {'|'.join(LEVELS)}] <bundle.xcresult> | --json <file>")
        level, args = args[1], args[2:]
    try:
        tree = xcresult_junit.load_tree([argv[0], *args])
    except (OSError, ValueError, subprocess.CalledProcessError) as e:
        print(f"::warning title=xcresult::could not read {data(' '.join(args))}: {data(str(e))}")
        return 0
    for line in annotations(tree, level, checkout or os.getcwd()):
        print(line)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
