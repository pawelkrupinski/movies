#!/usr/bin/env python3
"""DETERMINISTIC or FLAKY, for every test that failed, from JUnit XML.

A test that failed and then passed even once in the same job, on the same commit, is FLAKY: its
result depends on something other than the code. A test that failed every rerun is
DETERMINISTIC. The build fails either way -- a rerun is never a fix -- this only says which kind
of red it is, so a flake is recorded instead of being rerun into green and forgotten.

Usage:
  flake_verdict.py [--by-project] failed  <junit.xml | dir>...
      print the failed tests, one per line: project US class US name, US being \\x1f (a TAB is
      whitespace to `read`, so an empty project would vanish); dirs are searched for *.xml
  flake_verdict.py [--by-project] verdict --label L --original <xml|dir>... --rerun <xml|dir>... [--flaky-out F]
      compare, write a Markdown table to $GITHUB_STEP_SUMMARY (or stdout), append each FLAKY
      test to F as TSV: label <TAB> class <TAB> name <TAB> passes/reruns
  --by-project keys a test by its Playwright project too (JUnit `hostname`).

Tested by scripts/ci/flake_verdict_test.py.
"""
import os
import re
import sys
import xml.etree.ElementTree as ET
from collections import OrderedDict
from pathlib import Path

# Playwright's --repeat-each has no separate id per repeat in JUnit; strip any repeat marker a
# future version adds so every repeat counts toward the one test.
REPEAT_SUFFIX = re.compile(r"\s*\((?:repeat|retry)\s*#?\d+\)$")


def xml_files(paths):
    for p in map(Path, paths):
        if p.is_dir():
            yield from sorted(p.rglob("*.xml"))
        elif p.is_file():
            yield p


# Set by --by-project: Playwright writes the PROJECT into each testsuite's `hostname`, and the
# same test under two projects is two tests. ScalaTest writes the machine's name there instead.
BY_PROJECT = False


def testcases(paths):
    """(key, failed) per testcase; key = (project, class, name). Unreadable files are skipped."""
    for f in xml_files(paths):
        try:
            root = ET.parse(f).getroot()
        except ET.ParseError:
            continue
        suites = [root] if root.tag == "testsuite" else root.iter("testsuite")
        for suite in suites:
            project = suite.get("hostname", "") if BY_PROJECT else ""
            for case in suite.findall("testcase"):
                if case.find("skipped") is not None:
                    continue
                name = REPEAT_SUFFIX.sub("", case.get("name", ""))
                failed = case.find("failure") is not None or case.find("error") is not None
                yield (project, case.get("classname", ""), name), failed


def failed(paths):
    seen = OrderedDict()
    for key, bad in testcases(paths):
        if bad:
            seen[key] = True
    return list(seen)


def main(argv):
    global BY_PROJECT
    if "--by-project" in argv:
        BY_PROJECT = True
        argv = [a for a in argv if a != "--by-project"]
    if len(argv) >= 2 and argv[1] == "failed":
        for suite, cls, name in failed(argv[2:]):
            print(f"{suite}\x1f{cls}\x1f{name}")
        return 0
    if len(argv) >= 2 and argv[1] == "verdict":
        args = {"--original": [], "--rerun": [], "--label": ["tests"], "--flaky-out": [None]}
        current = None
        for a in argv[2:]:
            if a in args:
                current = a
                if a in ("--label", "--flaky-out"):
                    args[a] = []
            elif current:
                args[current].append(a)
        label, flaky_out = args["--label"][0], args["--flaky-out"][0]
        originally = failed(args["--original"])
        runs, fails = {}, {}
        for key, bad in testcases(args["--rerun"]):
            runs[key] = runs.get(key, 0) + 1
            fails[key] = fails.get(key, 0) + (1 if bad else 0)
        lines = [f"### Failed tests: {label}", "", "| Verdict | Test | Passed on rerun |", "|---|---|---|"]
        flaky = []
        for key in originally:
            suite, cls, name = key
            n, bad = runs.get(key, 0), fails.get(key, 0)
            if n == 0:
                verdict = "NOT RERUN"
            elif bad == n:
                verdict = "DETERMINISTIC"
            else:
                verdict = "FLAKY"
                flaky.append((label, f"{suite + ' ' if suite else ''}{cls}", name, f"{n - bad}/{n}"))
            shown = f"{suite + ' › ' if suite else ''}{cls} › {name}".replace("|", "\\|")
            lines.append(f"| **{verdict}** | {shown} | {n - bad}/{n} |")
        if not originally:
            lines.append("| – | no failed test found in the reports | – |")
        lines += ["", "The build fails either way: a rerun is never a fix. FLAKY tests are added to the flaky-test ledger issue."]
        out = "\n".join(lines) + "\n"
        summary = os.environ.get("GITHUB_STEP_SUMMARY")
        if summary:
            with open(summary, "a", encoding="utf-8") as fh:
                fh.write(out)
        print(out)
        if flaky_out and flaky:
            with open(flaky_out, "a", encoding="utf-8") as fh:
                for row in flaky:
                    fh.write("\t".join(v.replace("\t", " ").replace("\n", " ") for v in row) + "\n")
        return 0
    print(__doc__, file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv))
