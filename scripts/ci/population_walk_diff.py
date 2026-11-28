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
import sys
from typing import Collection, Dict, Iterable, List, Mapping, Optional, Sequence, Set, Tuple, TypeVar

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import tool_git  # noqa: E402

T = TypeVar("T")
FIXTURES = "test/resources/fixtures"
BACKLOG = f"{FIXTURES}/identity-flip-backlog.tsv"

Key = Tuple[str, str]   # (venue, raw title)
Flips = Dict[Key, str]
Candidate = Dict[str, Flips]   # "head" -> flips at the head, "known" -> the backlog's rows


def differential(head: Sequence[T], base: Sequence[T]) -> Tuple[List[T], List[T], List[T]]:
    """(new, gone, both): the keys of `head` not in `base`, of `base` not in `head`, and in both -- in head's order,
    then base's."""
    bset, hset = set(base), set(head)
    return ([k for k in head if k not in bset], [k for k in base if k not in hset], [k for k in head if k in bset])


def _read_candidate_file(path: str) -> Tuple[Optional[str], Flips, Flips]:
    """(slice name or None, head flips, known rows) of one candidate file."""
    slice_name: Optional[str] = None
    head: Dict[Key, str] = {}
    known: Dict[Key, str] = {}
    with open(path, encoding="utf-8") as fh:
        for raw in fh:
            cols = raw.rstrip("\n").split("\t")
            if len(cols) < 2:
                continue
            if cols[0] == "slice":
                slice_name = cols[1]
            elif len(cols) >= 4 and cols[0] in ("head", "known"):
                (head if cols[0] == "head" else known)[(cols[1], cols[2])] = cols[3]
    return slice_name, head, known


def read_candidates(directory: str) -> Dict[str, Candidate]:
    """{slice: {"head": {(venue, title): line}, "known": {(venue, title): note}}} of every candidate file."""
    out: Dict[str, Candidate] = {}
    if not os.path.isdir(directory):
        return out
    for name in sorted(n for n in os.listdir(directory) if n.endswith(".tsv")):
        slice_name, head, known = _read_candidate_file(os.path.join(directory, name))
        if slice_name:
            out[slice_name] = {"head": head, "known": known}
    return out


def read_base(path: str) -> Tuple[Dict[str, Flips], Dict[str, str]]:
    """({slice: {(venue, title): shape-and-path}} of every slice the base walked, {slice: why} of those it could not)."""
    walked: Dict[str, Flips] = {}
    failed: Dict[str, str] = {}
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


def country_of(slice_name: str) -> str:
    """A slice's country: drift-slices/<cc>/<name>, identity-slices/<cc>-<name> (PopulationMonotonicitySpec.countryOf)."""
    m = re.match(r"^drift-slices/([a-z]{2})/", slice_name) or re.match(r"^identity-slices/([a-z]{2})-", slice_name)
    if not m:
        raise ValueError(f"no country in slice name {slice_name!r}")
    return m.group(1)


def backlog_row(slice_name: str, venue: str, raw_title: str, note: str, first_seen: str, owner: str = "identity") -> str:
    """The backlog's `population` row of a flip the walk finds (its header: kind, country, seed, slice, venue, rawTitle,
    takes, firstSeen, owner, note)."""
    clean = [c.replace("\t", " ").replace("\n", " ") for c in (venue, raw_title, note)]
    return "\t".join(["population", country_of(slice_name), "", slice_name, clean[0], clean[1], "", first_seen, owner,
                      clean[2]])


def population_rows(text: Optional[str]) -> Dict[str, List[Key]]:
    """{slice: [(venue, raw title), ...]} of the backlog's `population` rows (by its header)."""
    rows = [ln.split("\t") for ln in (text or "").splitlines() if ln and not ln.startswith("#")]
    if not rows:
        return {}
    head = rows[0]
    out: Dict[str, List[Key]] = {}
    for r in rows[1:]:
        row = dict(zip(head, r))
        if row.get("kind") == "population":
            out.setdefault(row.get("slice", ""), []).append((row.get("venue", ""), row.get("rawTitle", "")))
    return {k: sorted(v) for k, v in out.items()}


def _rows_at(base: str, cwd: str) -> Tuple[Dict[str, List[Key]], Dict[str, List[Key]]]:
    """The backlog's `population` rows at `base` and in the working tree."""
    shown = tool_git.Git.here(cwd).out("show", f"{base}:{BACKLOG}", quiet=True) or ""
    try:
        with open(os.path.join(cwd, BACKLOG), encoding="utf-8") as fh:
            head = population_rows(fh.read())
    except FileNotFoundError:
        head = {}
    return population_rows(shown), head


def brought(candidates: Collection[str], base: str, cwd: str = ".") -> List[str]:
    """The candidate slices the change brings against `base` (the working tree's side): data that differ (a new or
    edited slice, an untracked file in it) or `population` rows of it the backlog added or removed."""
    base_rows, head_rows = _rows_at(base, cwd)
    git = tool_git.Git.here(cwd)
    out: List[str] = []
    for name in sorted(candidates):
        path = f"{FIXTURES}/{name}"
        changed = not git.ok("diff", "--quiet", base, "--", path, quiet=True)
        untracked = any(git.lines("ls-files", "--others", "--exclude-standard", "--", path))
        if changed or untracked or base_rows.get(name, []) != head_rows.get(name, []):
            out.append(name)
    return out


SLICE_ROOTS = ("drift-slices", "identity-slices")


def slice_of(path: str) -> Optional[str]:
    """The slice a fixtures path lies in (drift-slices/<cc>/<name>, identity-slices/<name>), or None."""
    parts = path.split("/")
    if len(parts) >= 3 and parts[0] == "drift-slices":
        return "/".join(parts[:3]) if len(parts) >= 4 else None
    if len(parts) >= 3 and parts[0] == "identity-slices":
        return "/".join(parts[:2])
    return None


def changed_slices(base: str, cwd: str = ".") -> List[str]:
    """Every committed slice the change brings against `base` (the working tree's side), as `brought` reads one: its
    files differ or are untracked, or its `population` rows changed -- the slices on disk only (a deleted one walks no
    more)."""
    roots = [f"{FIXTURES}/{r}" for r in SLICE_ROOTS]
    git = tool_git.Git.here(cwd)
    paths = git.lines("diff", "--name-only", "--no-renames", base, "--", *roots)
    paths += git.lines("ls-files", "--others", "--exclude-standard", "--", *roots)
    names = {slice_of(p[len(FIXTURES) + 1:]) for p in paths if p.startswith(FIXTURES + "/")}
    base_rows, head_rows = _rows_at(base, cwd)
    names |= {n for n in set(base_rows) | set(head_rows) if base_rows.get(n, []) != head_rows.get(n, [])}
    def on_disk(name: str) -> bool:
        root = os.path.join(cwd, FIXTURES, name)
        return os.path.isdir(root) and any(files for _, _, files in os.walk(root))
    return sorted(n for n in names if n and on_disk(n))


def struck_listings(cwd: str = ".") -> Set[Tuple[str, ...]]:
    """(country, venue, raw title) of every struck flip (identity-flips-struck.tsv): they pass in any slice."""
    try:
        with open(os.path.join(cwd, FIXTURES, "identity-flips-struck.tsv"), encoding="utf-8") as fh:
            lines = [ln.rstrip("\n").split("\t") for ln in fh if ln.strip() and not ln.startswith("#")]
    except FileNotFoundError:
        return set()
    return {tuple(c[:3]) for c in lines[1:] if len(c) >= 3}


def _parse_walk(lines: Iterable[str]) -> Tuple[Dict[str, Flips], List[str], List[str]]:
    """(flips by slice, failed-walk refusals, rule-breach refusals) of PopulationMonotonicitySpec.main's walk."""
    walked: Dict[str, Flips] = {}
    failed: List[str] = []
    breaches: List[str] = []
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
    return walked, failed, breaches


def judge_walk(lines: Iterable[str], cwd: str = ".") -> Tuple[List[str], List[str]]:
    """(refusals, notes) of PopulationMonotonicitySpec.main's walk of some slices, judged on the working tree's known
    rows alone -- what main's spec does with them: a flip no `population` row names (a struck listing's aside), a known
    row whose listing no longer flips, a rule contract breach and a walk that failed each refuse."""
    _, known_rows = _rows_at("HEAD", cwd)
    struck = struck_listings(cwd)
    walked, failed, breaches = _parse_walk(lines)
    refusals = failed + breaches
    notes: List[str] = []
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


def strict(name: str, head: Flips, known: Mapping[Key, str], why: str, label: str) -> Tuple[List[str], str]:
    """Judged on the known rows alone, as main's spec judges: (refusals, note)."""
    refusals: List[str] = []
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


def _judge_slice_against_base(name: str, candidate: Candidate, base: Flips) -> Tuple[List[str], List[str]]:
    """(refusals, notes) of a slice the base walked: only what the change adds or cures counts."""
    head, known = candidate["head"], candidate["known"]
    refusals: List[str] = []
    notes: List[str] = []
    new, _, both = differential([k for k in head if k not in known], list(base))
    refusals += [f"NEW flip the base does not have: {name}: {head[k]}" for k in new]
    notes += [f"pre-existing (the base flips it too, main's): {name}: {head[k]}" for k in both]
    for k, note in known.items():
        if k in head:
            continue
        if k in base:
            refusals.append(f"CURED known row (the base flips it, this change does not) -- remove it from "
                            f"identity-flip-backlog.tsv: {name}: {k[0]} | {k[1]} ({note})")
        else:
            notes.append(f"stale known row (the base does not flip it either, main's): {name}: {k[0]} | {k[1]}")
    return refusals, notes


def _unjudgeable(name: str, brought_slices: Collection[str], failed: Dict[str, str]) -> Tuple[str, str]:
    """(why, label) a slice is judged on the known rows alone: the change brings it, or the base could not walk it."""
    if name in brought_slices:
        return "the change brings the slice or its known rows (no base to compare: main's spec judges it so)", "brought"
    return f"the base could not walk it ({failed.get(name, 'no rows came back')})", "base unwalked"


def judge(candidates: Dict[str, Candidate], walked: Dict[str, Flips], failed: Dict[str, str],
          brought_slices: Collection[str] = ()) -> Tuple[List[str], List[str]]:
    """(refusals, notes): one line each."""
    refusals: List[str] = []
    notes: List[str] = []
    for name, c in sorted(candidates.items()):
        if name in brought_slices or name not in walked:
            why, label = _unjudgeable(name, brought_slices, failed)
            more, note = strict(name, c["head"], c["known"], why, label)
            refusals += more
            notes.append(note)
        else:
            more, more_notes = _judge_slice_against_base(name, c, walked[name])
            refusals += more
            notes += more_notes
    return refusals, notes


def rows(lines: Iterable[str], first_seen: str, owner: str) -> Tuple[List[str], List[str]]:
    """(backlog rows, failed slices) of PopulationMonotonicitySpec.main's walk output."""
    out: List[str] = []
    failed: List[str] = []
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


def file_rows(backlog: str, new_rows: Sequence[str]) -> List[str]:
    """Append to `backlog` each row whose (kind, slice, venue, raw title) no row of it holds; the rows appended."""
    def key(row: str) -> Optional[Tuple[str, str, str, str]]:
        c = row.split("\t")
        return (c[0], c[3], c[4], c[5]) if len(c) >= 6 else None
    with open(backlog, encoding="utf-8") as fh:
        text = fh.read()
    held = {key(ln) for ln in text.splitlines() if ln and not ln.startswith("#")}
    added: List[str] = []
    for r in new_rows:
        if key(r) not in held:
            held.add(key(r))
            added.append(r)
    if added:
        with open(backlog, "a", encoding="utf-8") as fh:
            fh.write(("" if text.endswith("\n") or not text else "\n") + "".join(f"{r}\n" for r in added))
    return added


DOCSTRING = __doc__ or ""  # None under python -OO


def _print_names(names: Collection[str]) -> int:
    for name in names:
        print(name)
    return 0


def _slices_command(argv: List[str]) -> int:
    return _print_names(sorted(read_candidates(argv[1])))


def _brought_command(argv: List[str]) -> int:
    return _print_names(brought(read_candidates(argv[1]), argv[2]))


def _changed_slices_command(argv: List[str]) -> int:
    return _print_names(changed_slices(argv[1]))


def _verdict_line(refusals: List[str], clean: str) -> str:
    return f"REFUSED, {len(refusals)} row(s)" if refusals else clean


def _judge_walk_command(argv: List[str]) -> int:
    with open(argv[1], encoding="utf-8") as fh:
        refusals, notes = judge_walk(fh)
    for line in notes + refusals:
        print(f"population walk: {line}")
    print(f"population walk: {len(notes)} slice(s) the change brings, judged on the known rows: "
          f"{_verdict_line(refusals, 'every flip known')}")
    return 1 if refusals else 0


def _rows_command(argv: List[str]) -> int:
    opts = dict(zip(argv[1::2], argv[2::2]))
    out, failed = rows(sys.stdin, opts.get("--first-seen", datetime.date.today().isoformat()), opts.get("--owner", "identity"))
    if "--into" in opts and not failed:
        out = file_rows(opts["--into"], out)
    for r in out:
        print(r)
    for f in failed:
        print(f"population walk: {f}", file=sys.stderr)
    return 1 if failed else 0


def _judge_files(candidates_dir: str, base_rows: str, brought_file: Optional[str]) -> int:
    candidates = read_candidates(candidates_dir)
    walked, failed = read_base(base_rows)
    listed: Set[str] = set()
    if brought_file:
        with open(brought_file, encoding="utf-8") as fh:
            listed = {ln.strip() for ln in fh if ln.strip()}
    refusals, notes = judge(candidates, walked, failed, listed)
    for line in notes + refusals:
        print(f"population walk: {line}")
    print(f"population walk: {len(candidates)} slice(s) judged against the base: {_verdict_line(refusals, 'nothing new')}")
    return 1 if refusals else 0


def _judge_command(argv: List[str]) -> int:
    brought_file = None
    if len(argv) == 5 and argv[3] == "--brought":
        brought_file, argv = argv[4], argv[:3]
    if len(argv) != 3:
        return _usage()
    return _judge_files(argv[1], argv[2], brought_file)


def _usage() -> int:
    print(DOCSTRING.split("\n\n")[-2], file=sys.stderr)
    return 2


# subcommand -> (argument count it takes, or None for any, handler)
COMMANDS = {
    "slices": (2, _slices_command),
    "brought": (3, _brought_command),
    "changed-slices": (2, _changed_slices_command),
    "judge-walk": (2, _judge_walk_command),
    "rows": (None, _rows_command),
}


def main(argv: List[str]) -> int:
    if argv and argv[0] in COMMANDS:
        arity, handler = COMMANDS[argv[0]]
        if arity is None or len(argv) == arity:
            return handler(argv)
    return _judge_command(argv) if argv[:1] == ["judge"] else _usage()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
