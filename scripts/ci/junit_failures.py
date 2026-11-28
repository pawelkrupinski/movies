#!/usr/bin/env python3
"""The failed tests in JUnit XML, one per line: class <TAB> name, each once, in report order.

Usage: junit_failures.py <junit.xml | dir>...   (dirs are searched for *.xml)

A testcase with a <failure> or an <error> failed; a skipped one did not run. An unreadable file
(a report cut off by a killed run) is skipped rather than failing the listing. Read by
scripts/ci/stress_races.py. Tested by scripts/ci/junit_failures_test.py.
"""
import sys
import xml.etree.ElementTree as ET
from pathlib import Path
from typing import Iterable, Iterator, List, Tuple


def xml_files(paths):
    for p in map(Path, paths):
        if p.is_dir():
            yield from sorted(p.rglob("*.xml"))
        elif p.is_file():
            yield p


def _roots(paths: Iterable[str]) -> Iterator[ET.Element]:
    """The root element of each readable JUnit file of `paths`."""
    for f in xml_files(paths):
        try:
            yield ET.parse(f).getroot()
        except ET.ParseError:
            continue


def _failed_cases(root: ET.Element) -> Iterator[Tuple[str, str]]:
    """(class, name) of each testcase under `root` with a <failure> or an <error>."""
    for suite in [root] if root.tag == "testsuite" else root.iter("testsuite"):
        for case in suite.findall("testcase"):
            if case.find("failure") is not None or case.find("error") is not None:
                yield case.get("classname", ""), case.get("name", "")


def failed(paths: Iterable[str]) -> List[Tuple[str, str]]:
    return list(dict.fromkeys(case for root in _roots(paths) for case in _failed_cases(root)))


def main(argv):
    if len(argv) < 2:
        print(__doc__, file=sys.stderr)
        return 2
    for cls, name in failed(argv[1:]):
        print(f"{cls}\t{name}".replace("\n", " "))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
