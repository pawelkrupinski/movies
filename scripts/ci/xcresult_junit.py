#!/usr/bin/env python3
"""Turn an .xcresult bundle's test tree into JUnit XML, using only what Xcode ships.

    python3 scripts/ci/xcresult_junit.py build/Kinowo.xcresult > xcuitest.xml

Reads `xcrun xcresulttool get test-results tests` (or, with --json FILE, a saved copy of that
JSON, which is what the spec feeds it). One <testsuite> per XCTestCase class, one <testcase> per
test, its duration, and for a failure every assertion message with the file:line it fired at.

Replaces `brew install xcresultparser` in ios.yml, which spent ~50 s of every run installing a
formula to do this, and whose report filed every test under "Uncategorized"."""
import json
import subprocess
import sys
from xml.sax.saxutils import escape, quoteattr


def load_tree(argv):
    if len(argv) == 3 and argv[1] == "--json":
        with open(argv[2], encoding="utf-8") as f:
            return json.load(f)
    if len(argv) == 2:
        out = subprocess.run(
            ["xcrun", "xcresulttool", "get", "test-results", "tests", "--path", argv[1]],
            check=True, capture_output=True, text=True).stdout
        return json.loads(out)
    sys.exit("usage: xcresult_junit.py <bundle.xcresult> | --json <tests.json>")


def messages(node, kind):
    """Every `kind` message under `node`, however deep (repetitions and arguments nest them)."""
    found = []
    for child in node.get("children", []):
        if child.get("nodeType") == kind:
            loc = child.get("sourceLocation")
            where = f" ({loc['filePath']}:{loc['lineNumber']})" if loc else ""
            found.append(child.get("name", "") + where)
        else:
            found.extend(messages(child, kind))
    return found


def collect(node, suite, cases):
    """(suite, case) pairs for every Test Case under `node`; the suite is its nearest Test Suite."""
    if node.get("nodeType") == "Test Case":
        cases.append((suite, node))
        return
    if node.get("nodeType") == "Test Suite":
        suite = node.get("name", suite)
    for child in node.get("children", []):
        collect(child, suite, cases)


def junit(tree):
    cases = []
    for root in tree.get("testNodes", []):
        collect(root, "Uncategorized", cases)

    suites = {}
    for suite, case in cases:
        suites.setdefault(suite, []).append(case)

    out = ['<?xml version="1.0" encoding="UTF-8"?>']
    total = sum(len(c) for c in suites.values())
    failed = sum(1 for _, c in cases if c.get("result") == "Failed")
    out.append(f'<testsuites name="xcresult" tests="{total}" failures="{failed}">')
    for suite, members in suites.items():
        s_failed = sum(1 for c in members if c.get("result") == "Failed")
        s_skipped = sum(1 for c in members if c.get("result") == "Skipped")
        s_time = sum(c.get("durationInSeconds", 0.0) for c in members)
        out.append(f'  <testsuite name={quoteattr(suite)} tests="{len(members)}" '
                   f'failures="{s_failed}" skipped="{s_skipped}" time="{s_time:.3f}">')
        for c in members:
            name = c.get("name", "").removesuffix("()")
            attrs = (f'classname={quoteattr(suite)} name={quoteattr(name)} '
                     f'time="{c.get("durationInSeconds", 0.0):.3f}"')
            result = c.get("result")
            if result == "Failed":
                text = "\n".join(messages(c, "Failure Message")) or "failed"
                first = text.splitlines()[0]
                out.append(f'    <testcase {attrs}>')
                out.append(f'      <failure message={quoteattr(first)}>{escape(text)}</failure>')
                out.append('    </testcase>')
            elif result == "Skipped":
                why = "\n".join(messages(c, "Skip Message"))
                out.append(f'    <testcase {attrs}><skipped message={quoteattr(why)}/></testcase>')
            else:
                out.append(f'    <testcase {attrs}/>')
        out.append('  </testsuite>')
    out.append('</testsuites>')
    return "\n".join(out) + "\n"


def main(argv):
    sys.stdout.write(junit(load_tree(argv)))


if __name__ == "__main__":
    main(sys.argv)
