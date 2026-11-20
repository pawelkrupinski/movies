#!/usr/bin/env python3
"""THE DEAD-CODE LIST, ranked (scripts/dead-code): `deadCode`'s rows (project/DeadCode.scala: kind, verdict, name,
module, source) as a markdown report, the surest and largest first.

Ranking: UNREFERENCED before TEST-ONLY (a test-only class may be a test seam kept in production on purpose -- the
in-memory stores the writing-fakes skill puts beside their Mongo twins); within each, classes before methods; then by
size in lines, largest first (a class from its definition line to the next top-level definition, a method from its
`def` to the first line indented no deeper, both read from the source); then by name.

  dead_code.py <rows.tsv> [--root DIR]     the report to stdout (DIR: the repository, default .)
"""

import argparse
import os
import re
import sys

DEFINITION = r"(?:(?:final|sealed|abstract|case|implicit|open|private(?:\[\w+\])?|protected(?:\[\w+\])?|override|inline|transparent|lazy)\s+)*"


def read_rows(path):
    rows = []
    with open(path, encoding="utf-8") as f:
        for line in f:
            if line.strip():
                kind, verdict, name, module, source = line.rstrip("\n").split("\t")
                rows.append({"kind": kind, "verdict": verdict, "name": name, "module": module, "source": source})
    return rows


def indent(line):
    return len(line) - len(line.lstrip(" "))


def span_lines(lines, start):
    """The lines a definition at `start` spans: every following line blank or indented deeper, and a closing line at
    its own indent that only closes (`}`, `)`)."""
    own = indent(lines[start])
    end = start
    for i in range(start + 1, len(lines)):
        text = lines[i]
        if not text.strip():
            continue
        if indent(text) > own:
            end = i
            continue
        if indent(text) == own and text.strip()[0] in "})":
            end = i
        break
    return end - start + 1


def size(row, text):
    """The candidate's lines in its source, 0 when its definition is not found by name."""
    lines = text.split("\n")
    simple = row["name"].split(".")[-1].split("$")[-1]
    if row["kind"] == "class":
        pattern = re.compile(r"^" + DEFINITION + r"(?:class|object|trait|enum)\s+" + re.escape(simple) + r"\b")
    else:
        pattern = re.compile(r"^\s*" + DEFINITION + r"def\s+" + re.escape(simple) + r"\b")
    for i, line in enumerate(lines):
        if pattern.match(line):
            return span_lines(lines, i)
    return 0


def ranked(rows, root):
    texts = {}
    for row in rows:
        path = os.path.join(root, row["source"])
        if path not in texts:
            try:
                with open(path, encoding="utf-8") as f:
                    texts[path] = f.read()
            except OSError:
                texts[path] = ""
        row["lines"] = size(row, texts[path])
    return sorted(rows, key=lambda r: (r["verdict"] != "unreferenced", r["kind"] != "class", -r["lines"], r["name"]))


def report(rows):
    out = []
    unreferenced = [r for r in rows if r["verdict"] == "unreferenced"]
    test_only = [r for r in rows if r["verdict"] == "test-only"]
    out.append(f"# dead-code: {len(unreferenced)} unreferenced, {len(test_only)} test-only")
    out.append("")
    for title, part in (("## Unreferenced (nothing calls it: delete it, with its tests)", unreferenced),
                        ("## Test-only (only specs call it: a test seam kept on purpose, or production code whose caller went)", test_only)):
        out.append(title)
        out.append("")
        if not part:
            out.append("(none)")
        else:
            out.append("| rank | kind | name | lines | source |")
            out.append("|---|---|---|---|---|")
            for i, r in enumerate(part, 1):
                out.append(f"| {i} | {r['kind']} | `{r['name']}` | {r['lines'] or '?'} | {r['source']} |")
        out.append("")
    return out


def main(argv):
    parser = argparse.ArgumentParser(prog="dead_code.py")
    parser.add_argument("rows")
    parser.add_argument("--root", default=".")
    args = parser.parse_args(argv)
    print("\n".join(report(ranked(read_rows(args.rows), args.root))))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
