"""THE BASELINE RATCHET ENGINE: what the JS/TS and the Python quality ratchets share (js_quality_ratchet.py,
py_quality_ratchet.py; the Scala ratchets' model, scripts/ci/quality_ratchet.py, which with coupling_ratchet.py reads its
baselines through parse_rows too). Each hands in a `Spec` -- its baseline, its kinds under their trailers, which files it
holds -- and gets the one check and the one update:

  - a measured row past its baseline row fails; a key without one past its kind's limit fails (a NEW function, file,
    clone, cycle over a limit); the measure's limit may not rise past the baseline's;
  - a row whose source git renamed (`git diff -M`) keeps its baseline row at the new path, the function or class name
    unchanged, when its value does not rise: a moved file re-keys its rows, and the move adds nothing;
  - only HELD rows count: a row whose files all lie outside the spec's held paths is reported, never gated;
  - the baseline grows (a row added or raised, a limit raised) only in a commit changing it that carries the kind's
    trailer in its final trailer block (trailers.py, THE ATTRIBUTION RULE); a fall needs none, and `update` records it;
  - THE FALL LOCK (the owner, 2026-11-28: "all complexity that you reduce -- add ratchets for it"): a measured row
    BELOW its baseline row (or gone) fails until the baseline records it, the message naming the update command --
    an unrecorded fall is headroom for the code to creep back to the old row (`refused_falls`, which the Scala
    quality and coupling ratchets call too);
  - a STRICT row (one of the spec's strict paths: the Python ratchet's migrated tools) past a limit fails whatever the
    baseline holds or a trailer says: no row admits it;
  - a commit whose final trailer block says `Coupling: split of <X> into <A>[, <B>]` has its own step's Coupling rises
    judged by the spec's `split_check` (the design rules' split credit, scripts/design-rules/splits.py): a proven
    importer's fan-out is admitted and re-frozen, every other rise refused -- the split trailer excuses nothing else.

A row is `kind<TAB>key<TAB>value`, `limit<TAB>kind<TAB>value` a kind's limit; `#` lines are comments."""
from __future__ import annotations

import os
import subprocess
import sys
from dataclasses import dataclass
from typing import Callable, Dict, List, Optional, Protocol, Set, Tuple, TypeVar

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))  # trailers.py beside it, under `python3 -I` too
import trailers  # noqa: E402  (puts scripts/lib on the path)
import tool_git  # noqa: E402

Rows = Dict[Tuple[str, str], float]
Grown = Tuple[str, str, Optional[float], float]   # kind, key, its row before (None: none), its row after
Rows1 = Dict[str, float]
N = TypeVar("N", int, float)


@dataclass(frozen=True)
class Spec:
    title: str                                # "JS/TS QUALITY": what the messages call the measure
    tool: str                                 # "js quality": the lower-case name its own lines start with
    baseline: str                             # the checked-in baseline, relative to the checkout
    header: str                               # the comment block `update` writes first
    hint: str                                 # what to do about a violation
    update_command: str                       # the command that rewrites the baseline
    held_rows: str                            # "production rows past a limit; ...": what the held count counts
    trailers: Dict[str, Tuple[str, ...]]      # trailer -> the kinds it admits
    held: Callable[[str], bool]               # whether one file or directory of a key is held
    strict: Callable[[str], bool] = lambda _: False  # a held part that may have NO row past a limit, trailer or not
    strict_hint: str = ""                     # what to do about a strict row
    # (sha, its `split of` value, its step's Coupling rises) -> the rises its split does not excuse; None: no split rule
    split_check: Optional[Callable[[str, str, List["Grown"]], List[str]]] = None

    @property
    def kinds(self) -> Tuple[str, ...]:
        return tuple(kind for kinds in self.trailers.values() for kind in kinds)


class Renames:
    """Where git's rename detection says a source that is no longer in the tree went: the commit that deleted it,
    diffed against its parent with `-M` (content similarity, git's default 50%), followed hop by hop until a source in
    the tree. A path still in the tree stays itself; one git saw deleted, not renamed, has no target (None)."""

    def __init__(self) -> None:
        self.known: Dict[str, Optional[str]] = {}

    def follow(self, path: str) -> Optional[str]:
        if path not in self.known:
            self.known[path] = self.trace(path)
        return self.known[path]

    @staticmethod
    def trace(path: str) -> Optional[str]:
        seen: Set[str] = set()
        while not os.path.exists(path):
            if path in seen:
                return None
            seen.add(path)
            deleted = tool_git.Git.here().text("log", "-1", "--no-renames", "--diff-filter=D", "--format=%H", "--", path).strip()
            if not deleted:
                return None
            fields = tool_git.Git.here().text("diff", "-M", "--name-status", "-z", f"{deleted}^", deleted).split("\0")
            renames: Dict[str, str] = {fields[i + 1]: fields[i + 2] for i in range(len(fields) - 2) if fields[i].startswith("R")}
            if path not in renames:
                return None
            path = renames[path]
        return path


class Follows(Protocol):
    """What moved_key and moved_from read of a rename source: where a path went (Renames, or one diff between two refs)."""

    def follow(self, path: str) -> Optional[str]: ...


def moved_key(key: str, renames: Follows) -> Optional[str]:
    """`key` with each of its sources (`<path>:<name>`, `<path>`, `<path>|<path>...`) replaced by where git renamed it,
    the function or class name kept; None when a source is gone with no rename."""
    paths, sep, name = key.partition(":")
    followed = [renames.follow(p) for p in paths.split("|")]
    return None if None in followed else "|".join(sorted(p for p in followed if p)) + sep + name


def moved_from(key: str, value: N, before: Dict[str, N], present: Dict[str, N], renames: Follows) -> Optional[str]:
    """The row `key` is a MOVE of: a row of `before` no longer `present` whose sources git renamed to `key`'s, the name
    unchanged, at or above `value` (a moved source re-keys its rows, and the move adds nothing). None for none."""
    return next((old for old, was in sorted(before.items())
                 if old not in present and moved_key(old, renames) == key and was >= value), None)


def parse_rows(text: str, number: Callable[[str], N]) -> Dict[Tuple[str, str], N]:
    """`kind<TAB>key<TAB>value` rows, each value read by `number` (`float` here; the Scala ratchets' `int`), `#` lines
    and blank ones skipped; a malformed line is a ValueError naming it."""
    rows: Dict[Tuple[str, str], N] = {}
    for n, line in enumerate(text.splitlines(), 1):
        if not line.strip() or line.startswith("#"):
            continue
        parts = line.split("\t")
        try:
            if len(parts) != 3:
                raise ValueError
            rows[(parts[0], parts[1])] = number(parts[2])
        except ValueError:
            raise ValueError(f"line {n}: not `kind<TAB>key<TAB>value`: {line!r}") from None
    return rows


def parse(text: str) -> Rows:
    return parse_rows(text, float)


def show(value: float) -> str:
    return str(int(value)) if value == int(value) else f"{value:g}"


def gated(spec: Spec, key: str) -> bool:
    """A row is held when one of its key's files (or directories) is."""
    return any(spec.held(p) for p in key.split("|"))


def of_kind(spec: Spec, rows: Rows, kind: str) -> Dict[str, float]:
    return {key: value for (k, key), value in rows.items() if k == kind and gated(spec, key)}


def limit_of(rows: Rows, kind: str) -> float:
    if ("limit", kind) not in rows:
        raise ValueError(f"no `limit<TAB>{kind}<TAB>N` row")
    return rows[("limit", kind)]


def kind_violations(spec: Spec, current: Rows, baseline: Rows, kind: str, renames: Renames) -> List[str]:
    out: List[str] = []
    limit = limit_of(baseline, kind)
    if limit_of(current, kind) > limit:
        out.append(f"the measure's {kind} limit {show(limit_of(current, kind))} is past the baseline's {show(limit)}")
    base, now = of_kind(spec, baseline, kind), of_kind(spec, current, kind)
    for key, value in sorted(now.items()):
        if key in base:
            if value > base[key]:
                out.append(f"{kind} {key}: {show(value)}, past its baseline {show(base[key])}")
        elif value > limit and not moved_from(key, value, base, now, renames):
            out.append(f"{kind} {key}: {show(value)}, past the limit {show(limit)} (no baseline row)")
    return out


def violations(spec: Spec, current: Rows, baseline: Rows, renames: Renames) -> List[str]:
    return [v for kind in spec.kinds for v in kind_violations(spec, current, baseline, kind, renames)]


def strict_violations(spec: Spec, current: Rows) -> List[str]:
    """The measured rows past a limit in a strict path: refused whatever the baseline says."""
    return [f"{kind} {key}: {show(value)}, past the limit {show(limit_of(current, kind))}"
            for (kind, key), value in sorted(current.items())
            if kind in spec.kinds and any(spec.held(part) and spec.strict(part) for part in key.split("|"))]


def carried(key: str, value: float, before: Rows1, after: Rows1, renames: Renames) -> bool:
    """Whether the row `key` is no addition: a renamed source's row, carried over at or below its old value."""
    return key not in before and moved_from(key, value, before, after, renames) is not None


def grown_rows(spec: Spec, kind: str, old: Rows, new: Rows, renames: Renames) -> List[Grown]:
    """The rows of `kind` added past the old limit or raised from `old` to `new` (a renamed source's carried row aside)."""
    old_limit = old.get(("limit", kind))
    allowed = old_limit if old_limit is not None else limit_of(new, kind)
    before, after = of_kind(spec, old, kind), of_kind(spec, new, kind)
    return [(kind, key, before.get(key), value) for key, value in sorted(after.items())
            if value > before.get(key, allowed) and not carried(key, value, before, after, renames)]


def kind_growth(spec: Spec, kind: str, old: Rows, new: Rows, renames: Renames) -> List[str]:
    out: List[str] = []
    old_limit = old.get(("limit", kind))
    new_limit = limit_of(new, kind)
    if old_limit is not None and new_limit > old_limit:
        out.append(f"limit {kind} raised {show(old_limit)} -> {show(new_limit)}")
    allowed = old_limit if old_limit is not None else new_limit
    for _kind, key, was, value in grown_rows(spec, kind, old, new, renames):
        out.append(f"{kind} {key} raised {show(was) if was is not None else f'<={show(allowed)}'} -> {show(value)}")
    return out


def growth(spec: Spec, kinds: Tuple[str, ...], old: Rows, new: Rows, renames: Renames) -> List[str]:
    """How the baseline grew in `kinds` from `old` to `new`: a limit raised, a row added past the old limit or raised
    (a row a renamed source carried over, at or below its old value, is no addition)."""
    return [g for kind in kinds for g in kind_growth(spec, kind, old, new, renames)]


def falls(spec: Spec, current: Rows, baseline: Rows) -> List[str]:
    out: List[str] = []
    for kind in spec.kinds:
        now = of_kind(spec, current, kind)
        for key, value in sorted(of_kind(spec, baseline, kind).items()):
            if now.get(key, 0) < value:
                out.append(f"{kind} {key} {show(value)} -> {show(now[key]) if key in now else 'within the limit'}")
    return out


def unjustified_growth(spec: Spec, rng: str, baseline: Rows, renames: Renames) -> bool:
    """Whether `rng` grew the baseline in some trailer's kinds with no commit justifying it; says so."""
    old_text = trailers.text_at(rng.split("..")[0], spec.baseline)
    if not old_text:
        return False
    found = False
    for trailer, kinds in spec.trailers.items():
        def rose(a: str, b: str, kinds: Tuple[str, ...] = kinds) -> List[str]:
            return growth(spec, kinds, parse(a), parse(b), renames)
        if not growth(spec, kinds, parse(old_text), baseline, renames):
            continue
        unjustified = trailers.unjustified_rises(rng, spec.baseline, trailer, rose)
        if unjustified:
            found = True
            print(f"THE {spec.title} BASELINE GREW in {rng} ({', '.join(kinds)}) with no `{trailer}: <why>` trailer"
                  " on the commit changing it:")
            print("\n".join(f"  - {u}" for u in unjustified))
            trailers.print_misplaced(rng, trailer, spec.baseline)
    return found


def split_values(sha: str, repo: tool_git.GitRepo) -> List[str]:
    return [v for v in repo.trailer_values(sha, "Coupling") if v.startswith("split of ")]


def commit_split_refusals(spec: Spec, sha: str, value: str, renames: Renames, repo: tool_git.GitRepo) -> List[str]:
    """What one `split of` commit's own baseline step raised that its split does not excuse."""
    assert spec.split_check is not None
    old, new = (parse(trailers.text_at(rev, spec.baseline, repo)) for rev in (f"{sha}^", sha))
    kinds = spec.trailers.get("Coupling", ())
    grown = [g for kind in kinds for g in grown_rows(spec, kind, old, new, renames)] if old else []
    return [f"{sha[:9]}: {why}" for why in spec.split_check(sha, value, grown)] if grown else []


def split_refusals(spec: Spec, rng: str, renames: Renames, repo: tool_git.GitRepo) -> List[str]:
    """`<sha9>: <why>` for every Coupling rise of a `split of` commit of `rng` its split does not excuse."""
    if spec.split_check is None or "Coupling" not in spec.trailers:
        return []
    out: List[str] = []
    for sha in repo.commits(rng, [spec.baseline], reverse=True):
        out += [r for value in split_values(sha, repo)[:1] for r in commit_split_refusals(spec, sha, value, renames, repo)]
    return out


def checkout_repo() -> tool_git.GitRepo:
    """The repository the ratchet runs in (its working directory's)."""
    return tool_git.SubprocessGitRepo.here()


def say_split_refusals(spec: Spec, ranges: List[str], renames: Renames) -> bool:
    """Whether a split commit of the ranges raised what its split does not excuse; says each."""
    repo = checkout_repo()
    found = [r for rng in ranges for r in split_refusals(spec, rng, renames, repo)]
    if found:
        print(f"THE {spec.title} BASELINE GREW in a `Coupling: split of` commit past what the split credit admits:")
        print("\n".join(f"  - {r}" for r in found))
    return len(found) > 0


def ranges_refused(spec: Spec, ranges: List[str], baseline: Rows, renames: Renames) -> bool:
    """Whether a commit of the ranges grew the baseline unjustified, or past its split's credit; says each."""
    unjustified = [rng for rng in ranges if unjustified_growth(spec, rng, baseline, renames)]
    return say_split_refusals(spec, ranges, renames) or bool(unjustified)


def refused_falls(title: str, baseline: str, update_command: str, down: List[str]) -> bool:
    """THE FALL LOCK: whether the measure fell below `baseline` with the fall unrecorded (each said, with the command
    that records it). Every ratchet refuses it: a fall is recorded in the push making it, so the row cannot creep back."""
    if not down:
        return False
    print(f"{title} FELL BELOW {baseline}, UNRECORDED ({len(down)} rows): lock the fall in -- run `{update_command}`"
          " and commit the baseline with the change (a fall needs no trailer):")
    print("\n".join(f"  - {d}" for d in down[:20]))
    if len(down) > 20:
        print(f"  - ... and {len(down) - 20} more")
    return True


def baseline_rows(spec: Spec) -> Optional[Rows]:
    """The checked-in baseline's rows; None when there is none."""
    try:
        with open(spec.baseline) as f:
            return parse(f.read())
    except FileNotFoundError:
        return None


def read_baseline(spec: Spec) -> Optional[Rows]:
    """The checked-in baseline's rows; None, said, when there is none (a checkout that never recorded one)."""
    rows = baseline_rows(spec)
    if rows is None:
        print(f"{spec.tool}: no baseline {spec.baseline} -- record the measure first: {spec.update_command}", file=sys.stderr)
    return rows


def check(spec: Spec, current: Rows, ranges: List[str]) -> bool:
    baseline = read_baseline(spec)
    if baseline is None:
        return False
    ok, renames = True, Renames()
    found = violations(spec, current, baseline, renames)
    if found:
        ok = False
        print(f"{spec.title} GREW past {spec.baseline}:")
        print("\n".join(f"  - {v}" for v in found))
        print(f"  {spec.hint}")
    strict = strict_violations(spec, current)
    if strict:
        ok = False
        print(f"{spec.title} PAST A LIMIT where no row is admitted:")
        print("\n".join(f"  - {v}" for v in strict))
        print(f"  {spec.strict_hint}")
    if ranges_refused(spec, ranges, baseline, renames):
        ok = False
    if refused_falls(spec.title, spec.baseline, spec.update_command, falls(spec, current, baseline)):
        ok = False
    if ok:
        held = sum(len(of_kind(spec, current, kind)) for kind in spec.kinds)
        print(f"{spec.tool}: within {spec.baseline} ({held} {spec.held_rows})")
    return ok


def update(spec: Spec, current: Rows) -> None:
    lines = [spec.header] + [f"limit\t{kind}\t{show(limit_of(current, kind))}" for kind in spec.kinds]
    for kind in spec.kinds:
        lines += [f"{kind}\t{key}\t{show(value)}" for key, value in sorted(of_kind(spec, current, kind).items())]
    os.makedirs(os.path.dirname(spec.baseline) or ".", exist_ok=True)
    with open(spec.baseline, "w") as f:
        f.write("\n".join(lines) + "\n")
    print(f"{spec.tool}: wrote {spec.baseline} -- a rise is committed with its kind's trailer")


def say_rises(tool: str, baseline: str, rises: List[str]) -> bool:
    """Whether `rises` holds any (each said: row, old value, new value). `update` records a rise like a fall, which lands
    only in a commit carrying its kind's trailer, so it exits non-zero on one: the agent sees it now, not at the push."""
    if not rises:
        return False
    print(f"{tool}: RISES RECORDED IN {baseline} ({len(rises)}) -- each needs its kind's trailer in the commit's final"
          " block, or cut the code back and run update again:", file=sys.stderr)
    print("\n".join(f"  - {r}" for r in rises), file=sys.stderr)
    return True


def update_saying_rises(spec: Spec, current: Rows) -> bool:
    """`update`, then whether the baseline it replaced took no rise (False: each said, see say_rises)."""
    old = baseline_rows(spec)
    rises = growth(spec, spec.kinds, old, current, Renames()) if old else []
    update(spec, current)
    return not say_rises(spec.tool, spec.baseline, rises)


def run(spec: Spec, command: str, current_file: str, ranges: List[str]) -> int:
    """`check` (exit 1 on a violation) or `update`; 2 on an error."""
    try:
        with open(current_file) as f:
            current = parse(f.read())
        if command == "update":
            return int(not update_saying_rises(spec, current))
        return 0 if check(spec, current, ranges) else 1
    except (OSError, ValueError, subprocess.CalledProcessError) as e:
        print(f"{spec.tool}: {e}", file=sys.stderr)
        return 2
