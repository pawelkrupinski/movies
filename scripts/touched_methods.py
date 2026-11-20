#!/usr/bin/env python3
"""THE METHODS A CHANGE TOUCHES, and what the refactoring tools say about each (scripts/mutation-check,
scripts/coverage-touched).

A change's production lines (every `.scala` under a deployed module's `src/main`, `git diff -U0 <base> <head>`, new
side) are met with the head's method spans (`quality.MethodSpans`: the outermost `def`s, keyed `<path>:<Owner.method>`
as the quality ratchets key them). A method is TOUCHED when a hunk adds or changes a line in it, or deletes lines next
to one of its lines; a touched line in no method (a class body, an import) belongs to no method and is said apart.

  touched_methods.py paths <base> <head>                     the head's changed production sources, one a line
  touched_methods.py touched <base> <head> --spans <tsv>     the touched methods: path, key, first line, last line
  touched_methods.py coverage --touched <tsv> --xml <scoverage.xml>... [--below <percent>]
                                                             each touched method's statement coverage, those below the
                                                             bar (default 80) first; exit 0 (a report, never a gate)
  touched_methods.py mutation --touched <tsv> --report <stryker report.json>...
                                                             each touched method's mutants: killed, survived, no
                                                             coverage, other; every survivor with its line and
                                                             replacement; exit 0 (a report, never a gate)
"""

import argparse
import json
import os
import re
import subprocess
import sys
import xml.etree.ElementTree as ElementTree

# the deployed modules (quality.QualityReport.ProductionModules): what ships
PRODUCTION_MODULES = ("geodata", "base", "http", "common", "identity", "scrapers", "worker", "web")
HUNK = re.compile(r"^@@ -\d+(?:,\d+)? \+(\d+)(?:,(\d+))? @@")


def is_production(path):
    parts = path.split("/")
    return len(parts) > 3 and parts[0] in PRODUCTION_MODULES and parts[1:3] == ["src", "main"] and path.endswith(".scala")


def git(repo, *args):
    env = {k: v for k, v in os.environ.items() if not k.startswith("GIT_")}
    return subprocess.run(["git", "-C", repo, *args], check=True, capture_output=True, text=True, env=env).stdout


def changed_paths(repo, base, head):
    """The production sources the head has and the range changed (added, modified or renamed to)."""
    out = git(repo, "diff", "--name-only", "--diff-filter=AMR", "-M", base, head)
    return [p for p in out.splitlines() if is_production(p)]


def changed_lines(diff_text):
    """{path: set of new-side lines} from `git diff -U0` text: each added or changed line, and for a hunk that only
    deletes, the lines either side of where the deleted ones were."""
    lines, path = {}, None
    for line in diff_text.splitlines():
        if line.startswith("+++ "):
            target = line[4:]
            path = target[2:] if target.startswith("b/") else None
            continue
        match = HUNK.match(line)
        if match and path is not None:
            start, count = int(match.group(1)), int(match.group(2) if match.group(2) is not None else 1)
            touched = range(start, start + count) if count else (start, start + 1)
            lines.setdefault(path, set()).update(touched)
    return lines


def range_lines(repo, base, head):
    paths = changed_paths(repo, base, head)
    if not paths:
        return {}
    return {p: ls for p, ls in changed_lines(git(repo, "diff", "-U0", "-M", base, head, "--", *paths)).items() if is_production(p)}


def read_spans(path):
    spans = []
    with open(path, encoding="utf-8") as f:
        for row in f:
            if row.strip():
                source, key, first, last = row.rstrip("\n").split("\t")
                spans.append((source, key, int(first), int(last)))
    return spans


def touched(spans, lines):
    """The spans a changed line falls in, in source order; and {path: lines in no method}."""
    hit, loose = [], {}
    for path, changed in sorted(lines.items()):
        own = [s for s in spans if s[0] == path]
        hit.extend(s for s in own if any(s[2] <= n <= s[3] for n in changed))
        outside = sorted(n for n in changed if not any(s[2] <= n <= s[3] for s in own))
        if outside:
            loose[path] = outside
    return hit, loose


def method_of(spans, path, line):
    """The touched method holding `line` of `path`, or None."""
    for span in spans:
        if span[0] == path and span[2] <= line <= span[3]:
            return span
    return None


def repo_relative(source, roots):
    """A report's absolute (or module-relative) source path, as a path from the repository root."""
    for root in roots:
        prefix = root.rstrip("/") + "/"
        if source.startswith(prefix):
            return source[len(prefix):]
    return source


def coverage_rows(spans, xml_paths, roots):
    """Each touched method's (statements, invoked) from scoverage XML reports: a statement counts once (several reports
    can carry one module's statements), invoked when any report saw it run."""
    seen = {}
    for xml_path in xml_paths:
        for statement in ElementTree.parse(xml_path).getroot().iter("statement"):
            if statement.get("ignored") == "true":
                continue
            path = repo_relative(statement.get("source", ""), roots)
            line = int(statement.get("line", "0"))
            span = method_of(spans, path, line)
            if span is None:
                continue
            key = (span[1], statement.get("start"), statement.get("end"))
            seen[key] = seen.get(key, False) or int(statement.get("invocation-count", "0")) > 0
    counts = {s[1]: [0, 0] for s in spans}
    for (method, _, _), invoked in seen.items():
        counts[method][0] += 1
        counts[method][1] += 1 if invoked else 0
    return counts


def coverage_report(spans, counts, below):
    """The report's lines: each touched method under the bar (or with no statement on record), least covered first,
    then the rest."""
    def percent(c):
        return 100.0 * c[1] / c[0] if c[0] else None
    rows = [(s, counts.get(s[1], [0, 0])) for s in spans]
    low = sorted((r for r in rows if r[1][0] == 0 or percent(r[1]) < below), key=lambda r: (percent(r[1]) or 0.0, r[0][1]))
    ok = [r for r in rows if r not in low]
    out = [f"# coverage-touched: {len(spans)} touched method(s), {len(low)} below {below:g}% statement coverage"]
    out.append("")
    for title, part in (("## below the bar (write pinning tests first)", low), ("## covered", ok)):
        out.append(title)
        if not part:
            out.append("(none)")
        for span, (total, invoked) in part:
            figure = "no statements on record" if total == 0 else f"{percent((total, invoked)):5.1f}% ({invoked}/{total})"
            out.append(f"{figure:>28}  {span[1]}  (lines {span[2]}-{span[3]})")
        out.append("")
    return out


MUTANT_KINDS = ("Killed", "Survived", "NoCoverage")


def mutation_rows(spans, reports, roots):
    """Each touched method's mutants, and the mutants of touched files outside every touched method (a count)."""
    by_method = {s[1]: {"Killed": 0, "Survived": 0, "NoCoverage": 0, "Other": 0, "survivors": []} for s in spans}
    outside = 0
    for report_path in reports:
        with open(report_path, encoding="utf-8") as f:
            report = json.load(f)
        base = report.get("projectRoot", "")
        for name, entry in report.get("files", {}).items():
            path = repo_relative(os.path.join(base, name) if base and not os.path.isabs(name) else name, roots)
            for mutant in entry.get("mutants", []):
                line = mutant["location"]["start"]["line"]
                span = method_of(spans, path, line)
                if span is None:
                    outside += 1
                    continue
                row = by_method[span[1]]
                status = mutant.get("status", "")
                # a Timeout is a mutant the tests stopped (an endless loop): killed, as Stryker counts it
                kind = "Killed" if status == "Timeout" else status if status in MUTANT_KINDS else "Other"
                row[kind] += 1
                if kind in ("Survived", "NoCoverage"):
                    row["survivors"].append((line, kind, mutant.get("mutatorName", ""), mutant.get("replacement", "")))
    return by_method, outside


def mutation_report(spans, by_method, outside):
    total = {k: sum(r[k] for r in by_method.values()) for k in ("Killed", "Survived", "NoCoverage", "Other")}
    detected = total["Killed"]
    scored = detected + total["Survived"] + total["NoCoverage"]
    score = f"{100.0 * detected / scored:.1f}%" if scored else "n/a"
    out = [f"# mutation-check: {len(spans)} touched method(s), {scored} mutants: {total['Killed']} killed, "
           f"{total['Survived']} survived, {total['NoCoverage']} not covered ({score} killed)"
           + (f"; {total['Other']} other (compile/runtime error, ignored)" if total["Other"] else "")
           + (f"; {outside} more in the touched files outside the touched methods, not reported" if outside else "")]
    out.append("")
    out.append("| method | killed | survived | no coverage |")
    out.append("|---|---|---|---|")
    order = sorted(spans, key=lambda s: (-(by_method[s[1]]["Survived"] + by_method[s[1]]["NoCoverage"]), s[1]))
    for span in order:
        row = by_method[span[1]]
        out.append(f"| `{span[1]}` | {row['Killed']} | {row['Survived']} | {row['NoCoverage']} |")
    survivors = [(span, s) for span in order for s in sorted(by_method[span[1]]["survivors"])]
    out.append("")
    out.append("## surviving mutants (a behaviour change no test caught)" if survivors else "## no surviving mutant")
    for span, (line, kind, mutator, replacement) in survivors:
        shown = replacement.replace("\n", " ")
        if len(shown) > 100:
            shown = shown[:97] + "..."
        out.append(f"- {span[0]}:{line} [{kind}] {mutator}: `{shown}`  ({span[1].split(':', 1)[1]})")
    return out


def main(argv):
    parser = argparse.ArgumentParser(prog="touched_methods.py", description=__doc__.split("\n\n")[0])
    sub = parser.add_subparsers(dest="command", required=True)
    paths = sub.add_parser("paths")
    paths.add_argument("base")
    paths.add_argument("head")
    paths.add_argument("--repo", default=".")
    touch = sub.add_parser("touched")
    touch.add_argument("base")
    touch.add_argument("head")
    touch.add_argument("--spans", required=True)
    touch.add_argument("--repo", default=".")
    cov = sub.add_parser("coverage")
    cov.add_argument("--touched", required=True)
    cov.add_argument("--xml", action="append", default=[])
    cov.add_argument("--below", type=float, default=80.0)
    cov.add_argument("--root", action="append", default=[])
    mut = sub.add_parser("mutation")
    mut.add_argument("--touched", required=True)
    mut.add_argument("--report", action="append", default=[])
    mut.add_argument("--root", action="append", default=[])
    args = parser.parse_args(argv)
    if args.command == "paths":
        print("\n".join(changed_paths(args.repo, args.base, args.head)))
    elif args.command == "touched":
        hit, loose = touched(read_spans(args.spans), range_lines(args.repo, args.base, args.head))
        for span in hit:
            print("\t".join(map(str, span)))
        for path, lines in loose.items():
            print(f"touched_methods: {path}: lines {','.join(map(str, lines))} changed in no method", file=sys.stderr)
    elif args.command == "coverage":
        spans = read_spans(args.touched)
        print("\n".join(coverage_report(spans, coverage_rows(spans, args.xml, args.root), args.below)))
    elif args.command == "mutation":
        spans = read_spans(args.touched)
        print("\n".join(mutation_report(spans, *mutation_rows(spans, args.report, args.root))))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
