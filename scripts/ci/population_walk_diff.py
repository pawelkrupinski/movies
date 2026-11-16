#!/usr/bin/env python3
"""THE POPULATION WALK, JUDGED AGAINST THE CHANGE'S BASE (the landing check's half of
services.identity.PopulationMonotonicitySpec; convergence-reds root cause 2, fix C).

The spec, run by the pre-push hook's identity check, writes a CANDIDATE file for each committed slice whose flips
(a listing's take going A -> B -> A, none -> A -> none, along an arrival chain) differ from the known rows
(identity-flip-backlog.tsv): `slice <name>`, `head <venue> <raw title> <line>` per flip at the change's
head, `known <venue> <raw title> <note>` per known row. scripts/ci/population-walk-diff walks the same slices with the
base's code (PopulationMonotonicitySpec.main: `<slice> <venue> <raw title> <shape: path>` rows, `# <slice>: ...` lines)
and this judges, listing by listing (venue, raw title):

  - NEW: flips at the head, not known, and not at the base -- the change adds it: REFUSED, the listing, slice and chain
    named;
  - CURED: a known row the base flips and the head does not -- the change cures it: REFUSED until the row goes (the
    known list only shrinks; main's spec would fail on it);
  - PRE-EXISTING: flips at the head and the base alike, not known (main's own), and a known row neither flips (a stale
    row main holds): said, passed -- not this change's;
  - a slice the base could not walk (its walk failed, no row came back): judged on the known rows alone, as the spec
    would -- never passed blind;
  - a slice THE CHANGE BRINGS (`brought`: its data, or its `population` rows of the backlog, differ from the base's):
    judged on the known rows alone too, unwalked at the base. The base walks the HEAD's slice, so a flip there "at the
    base too" says nothing of main: main's spec judges the slice on the known rows alone, and fails on every flip the
    backlog lacks (Main #2515: 97d260d67 brought the Ratzeburg Grinch slice with its drift row only; its four
    A -> B -> A takes passed here as "pre-existing" and failed main). Each such refusal prints the `population` row to
    add (`backlog_row`), the owner's OK for it named in the commit (scripts/ci/commit_rules.py).

Usage: population_walk_diff.py judge <candidates dir> <base rows file> [--brought FILE]   (exit 1: refused)
       population_walk_diff.py slices <candidates dir>                   (the candidate slices, one a line)
       population_walk_diff.py brought <candidates dir> <base>           (the candidate slices the change brings)
       population_walk_diff.py changed-slices <base>                     (every committed slice the change brings)
       population_walk_diff.py judge-walk <walk file>                    (PopulationMonotonicitySpec.main's walk of
                                                                         them, judged on the known rows: the pre-push
                                                                         hook's `flipbacklog` check; exit 1: refused)
       population_walk_diff.py rows [--first-seen D] [--owner O] [--into BACKLOG] < walk
                                     PopulationMonotonicitySpec.main's walk as the backlog's `population` rows, with
                                     --into appended to BACKLOG where no row names the listing in that slice yet (printed:
                                     the rows appended); exit 1, nothing filed: a slice's walk FAILED or BREACHed
                                     the rule contract (no row covers that: main's spec fails on it).
                                     scripts/identity-slice --emit-drift-slice files them

`differential(head, base)` is the shared core: the keys only the head holds, only the base holds, and both hold.
"""

import datetime
import os
import re
import subprocess
import sys

FIXTURES = "test/resources/fixtures"
BACKLOG = f"{FIXTURES}/identity-flip-backlog.tsv"


def differential(head, base):
    """(new, gone, both): the keys of `head` not in `base`, of `base` not in `head`, and in both -- in head's order,
    then base's."""
    bset, hset = set(base), set(head)
    return ([k for k in head if k not in bset], [k for k in base if k not in hset], [k for k in head if k in bset])


def read_candidates(directory):
    """{slice: {"head": {(venue, title): line}, "known": {(venue, title): note}}} of every candidate file."""
    out = {}
    if not os.path.isdir(directory):
        return out
    for name in sorted(os.listdir(directory)):
        if not name.endswith(".tsv"):
            continue
        slice_name, head, known = None, {}, {}
        with open(os.path.join(directory, name), encoding="utf-8") as fh:
            for raw in fh:
                cols = raw.rstrip("\n").split("\t")
                if cols[0] == "slice" and len(cols) >= 2:
                    slice_name = cols[1]
                elif cols[0] == "head" and len(cols) >= 4:
                    head[(cols[1], cols[2])] = cols[3]
                elif cols[0] == "known" and len(cols) >= 4:
                    known[(cols[1], cols[2])] = cols[3]
        if slice_name:
            out[slice_name] = {"head": head, "known": known}
    return out


def read_base(path):
    """({slice: {(venue, title): shape-and-path}} of every slice the base walked, {slice: why} of those it could not)."""
    walked, failed = {}, {}
    if not os.path.exists(path):
        return walked, failed
    with open(path, encoding="utf-8") as fh:
        for raw in fh:
            line = raw.rstrip("\n")
            if line.startswith("# "):
                head, _, rest = line[2:].partition(": ")
                if rest.startswith("FAILED"):
                    failed[head] = rest
                else:
                    walked.setdefault(head, {})
                continue
            cols = line.split("\t")
            if len(cols) >= 4:
                walked.setdefault(cols[0], {})[(cols[1], cols[2])] = cols[3]
    return walked, failed


def country_of(slice_name):
    """A slice's country: drift-slices/<cc>/<name>, identity-slices/<cc>-<name> (PopulationMonotonicitySpec.countryOf)."""
    m = re.match(r"^drift-slices/([a-z]{2})/", slice_name) or re.match(r"^identity-slices/([a-z]{2})-", slice_name)
    if not m:
        raise ValueError(f"no country in slice name {slice_name!r}")
    return m.group(1)


def backlog_row(slice_name, venue, raw_title, note, first_seen, owner="identity"):
    """The backlog's `population` row of a flip the walk finds (its header: kind, country, seed, slice, venue, rawTitle,
    takes, firstSeen, owner, note)."""
    clean = [c.replace("\t", " ").replace("\n", " ") for c in (venue, raw_title, note)]
    return "\t".join(["population", country_of(slice_name), "", slice_name, clean[0], clean[1], "", first_seen, owner,
                      clean[2]])


def population_rows(text):
    """{slice: [(venue, raw title), ...]} of the backlog's `population` rows (by its header)."""
    rows = [ln.split("\t") for ln in (text or "").splitlines() if ln and not ln.startswith("#")]
    if not rows:
        return {}
    head = rows[0]
    out = {}
    for r in rows[1:]:
        row = dict(zip(head, r))
        if row.get("kind") == "population":
            out.setdefault(row.get("slice", ""), []).append((row.get("venue", ""), row.get("rawTitle", "")))
    return {k: sorted(v) for k, v in out.items()}


def _git(cwd, *args):
    return subprocess.run(["git", "-c", "core.quotePath=false", *args], cwd=cwd, capture_output=True, text=True)


def _rows_at(base, cwd):
    """The backlog's `population` rows at `base` and in the working tree."""
    shown = _git(cwd, "show", f"{base}:{BACKLOG}")
    try:
        with open(os.path.join(cwd, BACKLOG), encoding="utf-8") as fh:
            head = population_rows(fh.read())
    except FileNotFoundError:
        head = {}
    return population_rows(shown.stdout if shown.returncode == 0 else ""), head


def brought(candidates, base, cwd="."):
    """The candidate slices the change brings against `base` (the working tree's side): data that differ (a new or
    edited slice, an untracked file in it) or `population` rows of it the backlog added or removed."""
    base_rows, head_rows = _rows_at(base, cwd)
    out = []
    for name in sorted(candidates):
        path = f"{FIXTURES}/{name}"
        changed = _git(cwd, "diff", "--quiet", base, "--", path).returncode != 0
        untracked = bool(_git(cwd, "ls-files", "--others", "--exclude-standard", "--", path).stdout.strip())
        if changed or untracked or base_rows.get(name, []) != head_rows.get(name, []):
            out.append(name)
    return out


SLICE_ROOTS = ("drift-slices", "identity-slices")


def slice_of(path):
    """The slice a fixtures path lies in (drift-slices/<cc>/<name>, identity-slices/<name>), or None."""
    parts = path.split("/")
    if len(parts) >= 3 and parts[0] == "drift-slices":
        return "/".join(parts[:3]) if len(parts) >= 4 else None
    if len(parts) >= 3 and parts[0] == "identity-slices":
        return "/".join(parts[:2])
    return None


def changed_slices(base, cwd="."):
    """Every committed slice the change brings against `base` (the working tree's side), as `brought` reads one: its
    files differ or are untracked, or its `population` rows changed -- the slices on disk only (a deleted one walks no
    more)."""
    roots = [f"{FIXTURES}/{r}" for r in SLICE_ROOTS]
    paths = _git(cwd, "diff", "--name-only", "--no-renames", base, "--", *roots).stdout.splitlines()
    paths += _git(cwd, "ls-files", "--others", "--exclude-standard", "--", *roots).stdout.splitlines()
    names = {slice_of(p[len(FIXTURES) + 1:]) for p in paths if p.startswith(FIXTURES + "/")}
    base_rows, head_rows = _rows_at(base, cwd)
    names |= {n for n in set(base_rows) | set(head_rows) if base_rows.get(n, []) != head_rows.get(n, [])}
    def on_disk(name):
        root = os.path.join(cwd, FIXTURES, name)
        return os.path.isdir(root) and any(files for _, _, files in os.walk(root))
    return sorted(n for n in names if n and on_disk(n))


def struck_listings(cwd="."):
    """(country, venue, raw title) of every struck flip (identity-flips-struck.tsv): they pass in any slice."""
    try:
        with open(os.path.join(cwd, FIXTURES, "identity-flips-struck.tsv"), encoding="utf-8") as fh:
            lines = [ln.rstrip("\n").split("\t") for ln in fh if ln.strip() and not ln.startswith("#")]
    except FileNotFoundError:
        return set()
    return {tuple(c[:3]) for c in lines[1:] if len(c) >= 3}


def judge_walk(lines, cwd="."):
    """(refusals, notes) of PopulationMonotonicitySpec.main's walk of some slices, judged on the working tree's known
    rows alone -- what main's spec does with them: a flip no `population` row names (a struck listing's aside), a known
    row whose listing no longer flips, a rule contract breach and a walk that failed each refuse."""
    _, known_rows = _rows_at("HEAD", cwd)
    struck = struck_listings(cwd)
    walked, failed, breaches = {}, [], []
    for raw in lines:
        line = raw.rstrip("\n")
        if line.startswith("# "):
            name, _, rest = line[2:].partition(": ")
            walked.setdefault(name, {})
            if rest.startswith("FAILED"):
                failed.append(f"the walk of {name} FAILED: {rest}")
            elif rest.startswith("BREACH "):
                breaches.append(f"rule contract breach in {name} (declare the rule's kind in RuleContract, or fix the "
                                f"rule): {rest[len('BREACH '):]}")
            continue
        cols = line.split("\t")
        if len(cols) >= 4:
            walked.setdefault(cols[0], {})[(cols[1], cols[2])] = f"{cols[1]} | {cols[2]}: {cols[3]}"
    refusals, notes = failed + breaches, []
    for name, head in sorted(walked.items()):
        if any(f.startswith(f"the walk of {name} ") for f in failed):
            continue
        cc = country_of(name)
        head = {k: v for k, v in head.items() if (cc, k[0], k[1]) not in struck}
        known = {k: "a known row" for k in known_rows.get(name, [])}
        more, note = strict(name, head, known, "walked at the change's head", "brought")
        refusals += more
        notes.append(f"{note}: {len(head)} flip(s), {len(known)} known row(s)")
    return refusals, notes


def strict(name, head, known, why, label):
    """Judged on the known rows alone, as main's spec judges: (refusals, note)."""
    refusals = []
    for k, line in head.items():
        if k not in known:
            refusals.append(f"NEW flip ({label}): {name}: {line}" if label != "brought" else
                            f"NEW flip in a slice or known rows this change brings: {name}: {line} -- main's spec fails "
                            f"on it: add its row to identity-flip-backlog.tsv (the owner's OK named in the commit) or "
                            f"fix it:\n  {backlog_row(name, k[0], k[1], line, datetime.date.today().isoformat())}")
    for k, note in known.items():
        if k not in head:
            refusals.append(f"CURED known row ({label}) -- remove it from identity-flip-backlog.tsv: "
                            f"{name}: {k[0]} | {k[1]} ({note})")
    return refusals, f"{name}: {why}: judged on the known rows alone"


def judge(candidates, walked, failed, brought_slices=()):
    """(refusals, notes): one line each."""
    refusals, notes = [], []
    for name, c in sorted(candidates.items()):
        head, known = c["head"], c["known"]
        if name in brought_slices or name not in walked:
            why, label = (("the change brings the slice or its known rows (no base to compare: main's spec judges it so)",
                           "brought") if name in brought_slices else
                          (f"the base could not walk it ({failed.get(name, 'no rows came back')})", "base unwalked"))
            more, note = strict(name, head, known, why, label)
            refusals += more
            notes.append(note)
            continue
        base = walked[name]
        fresh = [k for k in head if k not in known]
        new, _, both = differential(fresh, list(base))
        for k in new:
            refusals.append(f"NEW flip the base does not have: {name}: {head[k]}")
        for k in both:
            notes.append(f"pre-existing (the base flips it too, main's): {name}: {head[k]}")
        for k, note in known.items():
            if k in head:
                continue
            if k in base:
                refusals.append(f"CURED known row (the base flips it, this change does not) -- remove it from "
                                f"identity-flip-backlog.tsv: {name}: {k[0]} | {k[1]} ({note})")
            else:
                notes.append(f"stale known row (the base does not flip it either, main's): {name}: {k[0]} | {k[1]}")
    return refusals, notes


def rows(lines, first_seen, owner):
    """(backlog rows, failed slices) of PopulationMonotonicitySpec.main's walk output."""
    out, failed = [], []
    for raw in lines:
        line = raw.rstrip("\n")
        if line.startswith("# "):
            _, _, rest = line[2:].partition(": ")
            if rest.startswith(("FAILED", "BREACH ")):
                failed.append(line)
            continue
        cols = line.split("\t")
        if len(cols) >= 4:
            out.append(backlog_row(cols[0], cols[1], cols[2], cols[3], first_seen, owner))
    return out, failed


def file_rows(backlog, new_rows):
    """Append to `backlog` each row whose (kind, slice, venue, raw title) no row of it holds; the rows appended."""
    def key(row):
        c = row.split("\t")
        return (c[0], c[3], c[4], c[5]) if len(c) >= 6 else None
    with open(backlog, encoding="utf-8") as fh:
        text = fh.read()
    held = {key(ln) for ln in text.splitlines() if ln and not ln.startswith("#")}
    added = []
    for r in new_rows:
        if key(r) not in held:
            held.add(key(r))
            added.append(r)
    if added:
        with open(backlog, "a", encoding="utf-8") as fh:
            fh.write(("" if text.endswith("\n") or not text else "\n") + "".join(f"{r}\n" for r in added))
    return added


def main(argv):
    if len(argv) == 2 and argv[0] == "slices":
        for name in sorted(read_candidates(argv[1])):
            print(name)
        return 0
    if len(argv) == 3 and argv[0] == "brought":
        for name in brought(read_candidates(argv[1]), argv[2]):
            print(name)
        return 0
    if len(argv) == 2 and argv[0] == "changed-slices":
        for name in changed_slices(argv[1]):
            print(name)
        return 0
    if len(argv) == 2 and argv[0] == "judge-walk":
        with open(argv[1], encoding="utf-8") as fh:
            refusals, notes = judge_walk(fh)
        for line in notes + refusals:
            print(f"population walk: {line}")
        print(f"population walk: {len(notes)} slice(s) the change brings, judged on the known rows: "
              f"{'REFUSED, ' + str(len(refusals)) + ' row(s)' if refusals else 'every flip known'}")
        return 1 if refusals else 0
    if argv and argv[0] == "rows":
        opts = dict(zip(argv[1::2], argv[2::2]))
        out, failed = rows(sys.stdin, opts.get("--first-seen", datetime.date.today().isoformat()),
                           opts.get("--owner", "identity"))
        if "--into" in opts and not failed:
            out = file_rows(opts["--into"], out)
        for r in out:
            print(r)
        for f in failed:
            print(f"population walk: {f}", file=sys.stderr)
        return 1 if failed else 0
    brought_file = None
    if len(argv) == 5 and argv[3] == "--brought":
        brought_file, argv = argv[4], argv[:3]
    if len(argv) != 3 or argv[0] != "judge":
        print(__doc__.split("\n\n")[-2], file=sys.stderr)
        return 2
    candidates = read_candidates(argv[1])
    walked, failed = read_base(argv[2])
    listed = set()
    if brought_file:
        with open(brought_file, encoding="utf-8") as fh:
            listed = {ln.strip() for ln in fh if ln.strip()}
    refusals, notes = judge(candidates, walked, failed, listed)
    for n in notes:
        print(f"population walk: {n}")
    for r in refusals:
        print(f"population walk: {r}")
    print(f"population walk: {len(candidates)} slice(s) judged against the base: "
          f"{'REFUSED, ' + str(len(refusals)) + ' row(s)' if refusals else 'nothing new'}")
    return 1 if refusals else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
