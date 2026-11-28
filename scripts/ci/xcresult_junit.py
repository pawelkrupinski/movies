#!/usr/bin/env python3
"""Turn an .xcresult bundle's test tree into JUnit XML, using only what Xcode ships.

    python3 scripts/ci/xcresult_junit.py build/Kinowo.xcresult > xcuitest.xml

Reads `xcrun xcresulttool get test-results tests` (or, with --json FILE, a saved copy of that
JSON, which is what the spec feeds it). One <testsuite> per XCTestCase class, one <testcase> per
test, its duration, and for a failure every assertion message with the file:line it fired at.

Replaces `brew install xcresultparser` in ios.yml, which spent ~50 s of every run installing a
formula to do this, and whose report filed every test under "Uncategorized"."""
import ci_machine
import json
import sys
from xml.sax.saxutils import escape, quoteattr
from typing import Any, Dict, List


def load_tree(argv):
    if len(argv) == 3 and argv[1] == "--json":
        with open(argv[2], encoding="utf-8") as f:
            return json.load(f)
    if len(argv) == 2:
        out = ci_machine.run(
            ["xcrun", "xcresulttool", "get", "test-results", "tests", "--path", argv[1]],
            check=True, capture_output=True).stdout
        return json.loads(out)
    sys.exit("usage: xcresult_junit.py <bundle.xcresult> | --json <tests.json>")


def message_nodes(node, kind):
    """Every `kind` message node under `node`, however deep (repetitions and arguments nest them)."""
    found = []
    for child in node.get("children", []):
        if child.get("nodeType") == kind:
            found.append(child)
        else:
            found.extend(message_nodes(child, kind))
    return found


def messages(node, kind):
    """Every `kind` message under `node`, with the file:line it fired at."""
    found = []
    for message in message_nodes(node, kind):
        loc = message.get("sourceLocation")
        where = f" ({loc['filePath']}:{loc['lineNumber']})" if loc else ""
        found.append(message.get("name", "") + where)
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


def test_cases(tree):
    """(suite, case) for every Test Case in the tree."""
    cases = []
    for root in tree.get("testNodes", []):
        collect(root, "Uncategorized", cases)
    return cases


def _testcase(suite: str, c: Dict[str, Any]) -> List[str]:
    """The `<testcase>` lines of one case: passed, failed (with its message) or skipped."""
    name = c.get("name", "").removesuffix("()")
    attrs = f'classname={quoteattr(suite)} name={quoteattr(name)} time="{c.get("durationInSeconds", 0.0):.3f}"'
    result = c.get("result")
    if result == "Failed":
        text = "\n".join(messages(c, "Failure Message")) or "failed"
        return [f'    <testcase {attrs}>',
                f'      <failure message={quoteattr(text.splitlines()[0])}>{escape(text)}</failure>',
                '    </testcase>']
    if result == "Skipped":
        why = "\n".join(messages(c, "Skip Message"))
        return [f'    <testcase {attrs}><skipped message={quoteattr(why)}/></testcase>']
    return [f'    <testcase {attrs}/>']


def _testsuite(suite: str, members: List[Dict[str, Any]]) -> List[str]:
    """The `<testsuite>` lines of one suite of cases."""
    failed = sum(1 for c in members if c.get("result") == "Failed")
    skipped = sum(1 for c in members if c.get("result") == "Skipped")
    time = sum(c.get("durationInSeconds", 0.0) for c in members)
    out = [f'  <testsuite name={quoteattr(suite)} tests="{len(members)}" failures="{failed}" skipped="{skipped}" time="{time:.3f}">']
    for c in members:
        out += _testcase(suite, c)
    return out + ['  </testsuite>']


def junit(tree):
    cases = test_cases(tree)
    suites = {}
    for suite, case in cases:
        suites.setdefault(suite, []).append(case)
    total = sum(len(c) for c in suites.values())
    failed = sum(1 for _, c in cases if c.get("result") == "Failed")
    out = ['<?xml version="1.0" encoding="UTF-8"?>', f'<testsuites name="xcresult" tests="{total}" failures="{failed}">']
    for suite, members in suites.items():
        out += _testsuite(suite, members)
    out.append('</testsuites>')
    return "\n".join(out) + "\n"


def main(argv):
    sys.stdout.write(junit(load_tree(argv)))


if __name__ == "__main__":
    main(sys.argv)
