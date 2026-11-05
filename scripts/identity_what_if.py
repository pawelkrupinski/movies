#!/usr/bin/env python3
"""THE LOCAL WHAT-IF's two judgements (scripts/identity-what-if runs the resolves): which families a change can reach,
and what its takes did.

SELECT -- which base families the head must re-resolve. The changed code is mapped to the rule ids it can reach, and the
families whose base trace names one of those ids are picked; anything the mapping cannot attribute picks EVERY family,
and says why:

  inert            no change to what the MODEL takes: tests, fixtures, docs, scripts, CI, Markdown, comment-only lines,
                   imports, and the agreement stage (identity/.../identity/agreement/: the final take, which the what-if
                   does not recompute -- it says so)
  attributed       a change inside identity/src/main/scala, walked up its callers (every identity source line naming the
                   changed definition, at the base and at the head) until each path ends in a RULE ROOT
                   (scripts/identity-what-if-roots.tsv: a definition, the rule-id patterns a change to it can move -- its
                   own accept / refusal ids and the ids of the rules it pre-empts); a path that reaches a definition
                   nobody calls without passing a root (the resolver's own machinery, scoring, families) attributes
                   nothing
  everything else  common/, scrapers/, geodata/, worker/src/main (the lookups), resources (weights, title rules), the
                   build: ALL

A family is picked when any member's trace (DecisionTrace.rulesOf, the stage's id) matches a pattern (`*` any run). The
selection is a static reading of the code, so the what-if's `--verify` also resolves every family at the head and
fails when a take outside the selection moved.

DIFF -- each listing's base and head take (the model's: the head's from its re-resolve where selected, the base's
elsewhere), bucketed by the judgements exactly as scripts/identity_measure.py buckets a CI measurement (identity
_takes_diff's pairing and moves, identity_measure.bucket_of), plus prod's served take beside each.

usage: identity_what_if.py select --repo DIR --base SHA --head SHA|. --takes base/takes.tsv --out keys.txt [--roots F]
       identity_what_if.py diff --cc CC --base DIR --head DIR --judgements F [--verify] [--tsv OUT] [--json]
select exits 0 and prints `selected: N of M families (...)` or `selected: ALL (<why>)`.
diff exits 0, or 1 when --verify finds a take the selection missed.
"""

import argparse
import fnmatch
import json
import os
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
sys.path.insert(0, os.path.join(HERE, "ci"))
import identity_measure as measure  # noqa: E402
import identity_takes_diff as takes  # noqa: E402
from identity_judgements import read as read_judgements  # noqa: E402

ROOTS = os.path.join(HERE, "identity-what-if-roots.tsv")
IDENTITY_SOURCES = "identity/src/main/scala/"
WORKER_SOURCES = "worker/src/main/scala/"
AGREEMENT = "identity/src/main/scala/services/identity/agreement/"
CODE_PREFIXES = ("geodata/src/main/", "common/src/main/", "identity/src/main/", "scrapers/src/main/", "worker/src/main/",
                 "testkit/src/main/", "project/", "conf/")
CODE_FILES = ("build.sbt",)
MAX_CLOSURE = 400

DEFINITION = re.compile(r"^(\s*)(?:@\w+(?:\([^)]*\))?\s+)*(?:(?:private|protected)(?:\[\w+\])?\s+|override\s+|final\s+|implicit\s+|"
                        r"lazy\s+|inline\s+|sealed\s+|abstract\s+|case\s+|transparent\s+|opaque\s+)*"
                        r"(def|val|var|object|class|trait|enum|type|given)\s+`?([A-Za-z_][\w$]*)")


# -- SELECT -----------------------------------------------------------------------------------------------------------

def inert_path(path):
    """A file whose change cannot move a model take: not code, a test tree, or the agreement stage (final takes only)."""
    if path.startswith(AGREEMENT):
        return True
    if any(f"/src/{tree}/" in path for tree in ("test", "it", "fixtures", "pagetest", "e2e")):
        return True
    return not (path.startswith(CODE_PREFIXES) or path in CODE_FILES)


def comment_or_blank(line):
    """A line that changes no behaviour by itself: blank, a comment, an import, or closing brackets alone (a definition
    added or removed changes its other lines too)."""
    text = line.strip()
    return not text.strip("{}()[] ") or text.startswith(("*", "/*", "//", "*/", "import ", "package "))


TYPES = ("object", "class", "trait", "enum", "given")


def _indent(line):
    return len(line) - len(line.lstrip())


def holder_at(lines, index):
    """(name, kind) of the MEMBER holding line `index` (0-based) of `lines`: the outermost definition (a def, val, var,
    type) enclosing it under its object, class or trait -- a local def or val inside a member is the member's -- or that
    type itself for a line in its body outside every member; None at the top of a file. Scopes by indentation: a
    definition holds the lines below it up to the next line indented no deeper than it."""
    chain, floor = [], None
    own = DEFINITION.match(lines[index])
    if own:
        chain.append((own.group(3), own.group(2)))
        floor = len(own.group(1))
    else:
        floor = _indent(lines[index])
    for above in range(index - 1, -1, -1):
        line = lines[above]
        if comment_or_blank(line) and not DEFINITION.match(line):
            continue
        indent = _indent(line)
        if indent >= floor:
            continue
        floor = indent
        m = DEFINITION.match(line)
        if m:
            chain.append((m.group(3), m.group(2)))
    members = [c for c in chain if c[1] not in TYPES]
    # the outermost member below the innermost type: the definition the rest of the code names
    if not chain:
        return None
    innermost_type = next((i for i, c in enumerate(chain) if c[1] in TYPES), len(chain))
    inside = [c for c in chain[:innermost_type] if c[1] not in TYPES]
    return inside[-1] if inside else (chain[innermost_type] if innermost_type < len(chain) else members[-1])


def definition_at(lines, index):
    holder = holder_at(lines, index)
    return holder and holder[0]


def hunks(diff_text):
    """[(path, side, line numbers)] of a `git diff -U0`: each changed line's number on the base ('-') or head ('+') side."""
    out, path, old, new = [], None, None, None
    for line in diff_text.splitlines():
        if line.startswith("diff --git "):
            path = line.split(" b/", 1)[1]
        elif line.startswith("@@"):
            m = re.match(r"@@ -(\d+)(?:,\d+)? \+(\d+)(?:,\d+)? @@", line)
            old, new = int(m.group(1)), int(m.group(2))
        elif old is not None and line.startswith("-") and not line.startswith("---"):
            out.append((path, "-", old, line[1:]))
            old += 1
        elif old is not None and line.startswith("+") and not line.startswith("+++"):
            out.append((path, "+", new, line[1:]))
            new += 1
    return out


class Sources:
    """The identity sources at the base and the head, read once: what the caller walk greps."""

    def __init__(self, repo, base, head):
        self.repo, self.sides = repo, {"-": base, "+": head}
        self.cache = {}

    def files(self, side, roots=(IDENTITY_SOURCES,)):
        ref = self.sides[side]
        if ref == ".":
            out = git(self.repo, "ls-files", "--cached", "--others", "--exclude-standard", "--", *roots)
        else:
            out = git(self.repo, "ls-tree", "-r", "--name-only", ref, "--", *roots)
        return [p for p in out.splitlines() if p.endswith(".scala") and not p.startswith(AGREEMENT)]

    def lines(self, side, path):
        key = (side, path)
        if key not in self.cache:
            ref = self.sides[side]
            try:
                if ref == ".":
                    with open(os.path.join(self.repo, path), encoding="utf-8") as f:
                        text = f.read()
                else:
                    text = git(self.repo, "show", f"{ref}:{path}")
            except (OSError, subprocess.CalledProcessError):
                text = ""
            self.cache[key] = text.splitlines()
        return self.cache[key]

    def callers(self, name):
        """{definition holding a line that names `name`} over the identity sources and the worker's (the model's callers),
        at both sides, its own definition lines aside; a worker definition is named `worker:<name>`."""
        word = re.compile(rf"\b{re.escape(name)}\b")
        found = set()
        for side in ("-", "+"):
            for path in self.files(side, (IDENTITY_SOURCES, WORKER_SOURCES)):
                lines = self.lines(side, path)
                for i, line in enumerate(lines):
                    if comment_or_blank(line) or not word.search(line):
                        continue
                    m = DEFINITION.match(line)
                    if m and m.group(3) == name:
                        continue
                    holder = definition_at(lines, i)
                    if holder and holder != name:
                        found.add(holder if path.startswith(IDENTITY_SOURCES) else "worker:" + holder)
        return found


def read_roots(path):
    """{definition: [rule-id patterns]} of the roots file (`#` comments; columns definition, patterns, why)."""
    roots = {}
    with open(path, encoding="utf-8") as f:
        for line in f:
            if not line.strip() or line.startswith("#") or line.startswith("definition\t"):
                continue
            cols = line.rstrip("\n").split("\t")
            roots[cols[0]] = cols[1].split()
    return roots


def attribute(changes, sources, roots):
    """(patterns, None) -- the rule ids the changed lines can move -- or (None, why) when some change attributes to no
    rule. `changes`: (path, side, line number, text) of every changed line."""
    patterns = set()
    changed = {}
    for path, side, number, text in changes:
        if inert_path(path) or comment_or_blank(text):
            continue
        if not path.startswith(IDENTITY_SOURCES):
            return None, f"{path} changed: code outside the identity model's sources (lookups, normalizer, build, resources)"
        lines = sources.lines(side, path)
        holder = holder_at(lines, number - 1) if 0 < number <= len(lines) else None
        if holder is None:
            return None, f"{path}:{number} changed outside any definition"
        if holder[1] in TYPES:
            return None, f"{path}:{number} changed in {holder[1]} {holder[0]}'s own body, outside its members"
        changed.setdefault(holder[0], f"{path}:{number}")
    for name, where in sorted(changed.items()):
        seen, frontier = {name}, [name]
        while frontier:
            current = frontier.pop()
            if current in roots:
                patterns.update(roots[current])
                continue
            callers = sources.callers(current)
            outside = sorted(c for c in callers if c.startswith("worker:"))
            if outside:
                return None, f"{where} ({name}) reaches the worker's {outside[0][7:]} through {current}, past every rule root"
            # nothing in the model or the worker names it: dead in production (a spec's), and this path moves no take
            for caller in callers - seen:
                seen.add(caller)
                frontier.append(caller)
            if len(seen) > MAX_CLOSURE:
                return None, f"{where} ({name}) reaches over {MAX_CLOSURE} definitions: shared resolver code"
    return patterns, None


def matches(rule_ids, patterns):
    return any(fnmatch.fnmatchcase(rule, pattern) for rule in rule_ids for pattern in patterns)


def read_tsv(path):
    with open(path, encoding="utf-8") as f:
        header = f.readline().rstrip("\n").split("\t")
        return [dict(zip(header, line.rstrip("\n").split("\t"))) for line in f if line.strip()]


def select(rows, patterns):
    """The keys of every listing of a family any member of which a pattern names, and the families picked."""
    families = {}
    for r in rows:
        families.setdefault(r["family"], []).append(r)
    picked = [f for f, members in families.items()
              if any(matches(r["rules"].split("\x1f") if r["rules"] else [], patterns) for r in members)]
    return sorted(r["key"] for f in picked for r in families[f]), len(picked), len(families)


def git(repo, *args):
    return subprocess.run(["git", "-C", repo, *args], check=True, capture_output=True, text=True).stdout


def changes_between(repo, base, head):
    if head == ".":
        text = git(repo, "diff", "-U0", "--no-color", "--no-ext-diff", base)
        for path in git(repo, "ls-files", "--others", "--exclude-standard").splitlines():
            if path.endswith(".scala") or not inert_path(path):
                with open(os.path.join(repo, path), encoding="utf-8", errors="replace") as f:
                    text += f"\ndiff --git a/{path} b/{path}\n@@ -0,0 +1 @@\n" + "".join("+" + l for l in f.read().splitlines(True))
        return hunks(text)
    return hunks(git(repo, "diff", "-U0", "--no-color", "--no-ext-diff", base, head))


def run_select(args):
    rows = read_tsv(args.takes)
    changes = changes_between(args.repo, args.base, args.head)
    patterns, why = attribute(changes, Sources(args.repo, args.base, args.head), read_roots(args.roots))
    families = len({r["family"] for r in rows})
    if patterns is None:
        keys, line = [r["key"] for r in rows], f"selected: ALL {families} families ({why})"
    elif not patterns:
        keys, line = [], f"selected: 0 of {families} families (no change reaches the model's take)"
    else:
        keys, picked, _ = select(rows, patterns)
        line = f"selected: {picked} of {families} families, {len(keys)} listings (rules {' '.join(sorted(patterns))})"
    with open(args.out, "w", encoding="utf-8") as f:
        f.writelines(k + "\n" for k in keys)
    print(line)
    return 0


# -- DIFF -------------------------------------------------------------------------------------------------------------

def film_of(take):
    return int(take[5:]) if take.startswith("tmdb:") else None


def as_row(r, take):
    venue, title = r["venue"], r["rawTitle"]
    return {"key": r["key"], "venue": venue, "rawTitle": title, "film": film_of(take), "filmTitle": None,
            "basis": take, "explanation": [r.get("rules", "").replace("\x1f", " ")[:200]], "blocker": None}


def merged_head(base_rows, head_rows):
    """Each listing's head take: its re-resolve's where the head re-resolved it, the base's elsewhere."""
    head = {r["key"]: r for r in head_rows}
    return [head.get(r["key"], r) for r in base_rows], set(head)


def diff(cc, base_rows, head_rows, prod, judgement_rows, whole_rows=None):
    head_taken, reresolved = merged_head(base_rows, head_rows)
    base_by, head_by = {r["key"]: r for r in base_rows}, {r["key"]: r for r in head_taken}
    # A head Pending where the base had a take is a question the head asks that prod's store never held (new query text,
    # a new shape's search): the what-if refuses every live ask, a real resolve would ask and answer it. Not a lost take --
    # the listing reads as the base's, counted apart, so the verdict is of the takes the head can decide.
    awaiting = {k for k, h in head_by.items() if h["take"] == "pending" and base_by[k]["take"] != "pending"}
    resolved_by = head_by
    head_taken = [base_by[r["key"]] if r["key"] in awaiting else r for r in head_taken]
    head_by = {r["key"]: r for r in head_taken}
    buckets = measure.classify(cc, [as_row(r, r["take"]) for r in base_rows], [as_row(r, r["take"]) for r in head_taken],
                               judgement_rows)
    moved_keys = {r["key"] for r in base_rows if r["take"] != head_by[r["key"]]["take"]}
    bucket_of = {}
    for bucket, entries in buckets.items():
        for e in entries:
            bucket_of[(e["venue"], e["rawTitle"])] = bucket
    report = {"country": cc, "listings": len(base_rows), "reresolved": len(reresolved & set(base_by)),
              "moved": len(moved_keys), "awaiting": len(awaiting), "buckets": {b: len(v) for b, v in buckets.items() if v},
              "pending": sum(1 for k in moved_keys if "pending" in (base_by[k]["take"], head_by[k]["take"]))}
    compared = [k for k in base_by if prod.get(k, "missing") != "missing"]
    report["prod"] = {"compared": len(compared), "base agrees": sum(base_by[k]["take"] == prod[k] for k in compared),
                      "head agrees": sum(head_by[k]["take"] == prod[k] for k in compared),
                      "toward prod": sum(base_by[k]["take"] != prod[k] and head_by[k]["take"] == prod[k] for k in compared),
                      "away from prod": sum(base_by[k]["take"] == prod[k] and head_by[k]["take"] != prod[k] for k in compared)}
    rows = []
    for k in sorted(moved_keys, key=lambda k: (base_by[k]["venue"], base_by[k]["rawTitle"])):
        b, h = base_by[k], head_by[k]
        rows.append({"country": cc, "venue": b["venue"], "rawTitle": b["rawTitle"], "base": b["take"], "head": h["take"],
                     "prod": prod.get(k, "missing"), "bucket": bucket_of.get((b["venue"], b["rawTitle"]), "pending" if
                     "pending" in (b["take"], h["take"]) else "unpaired"), "family": b["family"],
                     "headRules": h.get("rules", "").replace("\x1f", " ")})
    report["unsound"] = []
    if whole_rows is not None:
        whole = {r["key"]: r["take"] for r in whole_rows}
        report["unsound"] = [{"venue": base_by[k]["venue"], "rawTitle": base_by[k]["rawTitle"], "what-if": resolved_by[k]["take"],
                              "whole": whole.get(k, "absent"), "selected": k in reresolved}
                             for k in sorted(base_by)
                             if whole.get(k, "absent") != resolved_by[k]["take"] and (k in whole or k in reresolved)]
    report["examples"] = {b: [f"{e['venue']} | {e['rawTitle']}: {e['before']} -> {e['after']}" for e in v[:3]]
                          for b, v in buckets.items() if v}
    return report, rows


TSV_COLUMNS = ("country", "venue", "rawTitle", "base", "head", "prod", "bucket", "family", "headRules")


def render(report):
    out = [f"== {report['country']}: {report['listings']} listings, {report['reresolved']} re-resolved at the head; "
           f"{report['moved']} take(s) moved ({report['pending']} to or from pending)"]
    if report["awaiting"]:
        out.append(f"  {report['awaiting']} listing(s) await an answer the head asks and prod's store does not hold (new query text): "
                   "counted as no move; a CI measure fills them")
    if report["buckets"]:
        out.append("  " + ", ".join(f"{b} {n}" for b, n in report["buckets"].items()))
    for b, examples in report["examples"].items():
        if b in measure.BLOCKING or b in ("cure", "gained-right"):
            out += [f"    {b}: {e}" for e in examples]
    p = report["prod"]
    out.append(f"  vs prod's served take ({p['compared']} listings prod holds): base agrees {p['base agrees']}, head "
               f"{p['head agrees']}; {p['toward prod']} moved toward prod, {p['away from prod']} away")
    blocking = sum(report["buckets"].get(b, 0) for b in measure.BLOCKING)
    out.append(f"  {'CLEAN' if not blocking else 'BLOCKING'}: right-lost {report['buckets'].get('right-lost', 0)}, "
               f"right->wrong {report['buckets'].get('right->wrong', 0)}, new-wrong {report['buckets'].get('new-wrong', 0)}")
    return "\n".join(out)


def run_diff(args):
    base_rows = read_tsv(os.path.join(args.base, "takes.tsv"))
    head_rows = read_tsv(os.path.join(args.head, "takes.tsv"))
    prod = {r["key"]: r["prod"] for r in read_tsv(os.path.join(args.base, "prod.tsv"))}
    whole_path = os.path.join(args.head, "takes-whole.tsv")
    whole = read_tsv(whole_path) if args.verify else None
    if args.verify and whole is None:
        print("--verify: the head wrote no takes-whole.tsv", file=sys.stderr)
        return 2
    report, rows = diff(args.cc, base_rows, head_rows, prod, read_judgements(args.judgements), whole)
    if args.tsv:
        with open(args.tsv, "w", encoding="utf-8") as f:
            f.write("\t".join(TSV_COLUMNS) + "\n")
            f.writelines("\t".join(str(r[c]) for c in TSV_COLUMNS) + "\n" for r in rows)
    print(json.dumps(report) if args.json else render(report))
    if args.verify:
        if report["unsound"]:
            print(f"  VERIFY FAILED: {len(report['unsound'])} listing(s) take otherwise in the whole head resolve:")
            for u in report["unsound"][:20]:
                print(f"    {u['venue']} | {u['rawTitle']}: what-if {u['what-if']}, whole {u['whole']}"
                      f"{' (selected)' if u['selected'] else ' (OUTSIDE the selection)'}")
            return 1
        print("  VERIFY OK: the whole head resolve takes as the what-if on every listing")
    return 0


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = parser.add_subparsers(dest="command", required=True)
    s = sub.add_parser("select")
    s.add_argument("--repo", required=True)
    s.add_argument("--base", required=True)
    s.add_argument("--head", required=True)
    s.add_argument("--takes", required=True)
    s.add_argument("--out", required=True)
    s.add_argument("--roots", default=ROOTS)
    d = sub.add_parser("diff")
    d.add_argument("--cc", required=True)
    d.add_argument("--base", required=True)
    d.add_argument("--head", required=True)
    d.add_argument("--judgements", required=True)
    d.add_argument("--verify", action="store_true")
    d.add_argument("--tsv")
    d.add_argument("--json", action="store_true")
    args = parser.parse_args(argv)
    return run_select(args) if args.command == "select" else run_diff(args)


if __name__ == "__main__":
    sys.exit(main())
