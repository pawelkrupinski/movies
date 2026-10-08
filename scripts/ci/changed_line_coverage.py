#!/usr/bin/env python3
"""Which lines a change added or modified that no test executes: a JaCoCo XML report intersected with
`git diff -U0 <base> HEAD`. A whole-build coverage figure says nothing about the change under review;
this names the changed executable lines the tests never reached, per file, worst first.

  python3 scripts/ci/changed_line_coverage.py <jacoco.xml> <repo> <base> [--show PATH-FRAGMENT] [--paths PATHSPEC ...]

  <jacoco.xml>  a JaCoCo XML report of the test run (e.g. the jacococli `report --xml` of a
                -javaagent:jacocoagent.jar run's .exec, with the modules' classes and sources)
  <base>        the commit the change is measured from (e.g. origin/main)
  --show        also print the uncovered source lines of the files whose path contains it
  --paths       the pathspecs diffed (default: every module's src/main/scala)

Lines JaCoCo does not list (comments, blank lines, declarations) are not executable and never count."""
import argparse
import collections
import re
import subprocess
import sys
import xml.etree.ElementTree as ET

DEFAULT_PATHS = [":(glob)*/src/main/scala/**"]


def changed_lines(diff_text):
    """{path: {line numbers added or modified}} from a `git diff -U0`."""
    changed = collections.defaultdict(set)
    current = None
    for line in diff_text.splitlines():
        if line.startswith("+++ "):
            target = line[4:]
            current = target[2:] if target.startswith("b/") else None
        elif line.startswith("@@") and current:
            m = re.search(r"\+(\d+)(?:,(\d+))?", line)
            start, count = int(m.group(1)), int(m.group(2) or 1)
            changed[current].update(range(start, start + count))
    return changed


def covered_lines(report_xml):
    """{"<package path>/<File>.scala": {line: covered instructions}} from a JaCoCo XML report."""
    cov = {}
    for pkg in ET.fromstring(report_xml).iter("package"):
        for sf in pkg.iter("sourcefile"):
            lines = cov.setdefault(pkg.get("name") + "/" + sf.get("name"), {})
            for ln in sf.iter("line"):
                lines[int(ln.get("nr"))] = int(ln.get("ci"))
    return cov


def intersect(changed, cov):
    """[(uncovered lines, covered count, path)] for each changed file JaCoCo knows, worst first."""
    rows = []
    for path, lines in changed.items():
        m = re.search(r"/src/main/scala/(.*)$", path)
        data = cov.get(m.group(1)) if m else None
        if data is None:
            continue
        executable = [ln for ln in lines if ln in data]
        uncovered = sorted(ln for ln in executable if data[ln] == 0)
        rows.append((uncovered, len(executable) - len(uncovered), path))
    rows.sort(key=lambda r: (-len(r[0]), -r[1], r[2]))
    return rows


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("report")
    parser.add_argument("repo")
    parser.add_argument("base")
    parser.add_argument("--show")
    parser.add_argument("--paths", nargs="+", default=DEFAULT_PATHS)
    args = parser.parse_args(argv)
    diff = subprocess.run(["git", "-C", args.repo, "diff", "-U0", args.base, "HEAD", "--", *args.paths],
                          capture_output=True, text=True, check=True).stdout
    with open(args.report, encoding="utf-8") as f:
        rows = intersect(changed_lines(diff), covered_lines(f.read()))
    covered = sum(r[1] for r in rows)
    uncovered = sum(len(r[0]) for r in rows)
    print(f"changed executable lines: covered={covered} uncovered={uncovered} "
          f"pct={100 * covered / max(1, covered + uncovered):.1f}")
    for lines, cov, path in rows:
        if not lines or (args.show and args.show not in path):
            continue
        print(f"{len(lines):5d} uncov {cov:5d} cov  {path}")
        if args.show:
            with open(f"{args.repo}/{path}", encoding="utf-8") as f:
                source = f.read().splitlines()
            for ln in lines:
                print(f"   {ln:5d}: {source[ln - 1][:170]}")


if __name__ == "__main__":
    main(sys.argv[1:])
