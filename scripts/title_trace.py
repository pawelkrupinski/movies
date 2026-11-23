#!/usr/bin/env python3
"""Every step one raw billing goes through, in production order: the ingest tidy, the venue's own title rules, the format
peel (the slot's title), the canonical tier (the slot key), the search strip rule by rule (each rule's name, before and
after), the listing's shape as the identity model reads it (MultiFilmBill, NonMovieEventClassifier, ListingShape, its
pieces, billedBeside), the searches it issues (IdentityMeasures.searchQueries, SearchTitles.candidates), and the
SearchQueryMarkersSpec / CinemaSlotInvariantsSpec findings it would raise. No recording, no network: the code alone.

  scripts/title-trace "<raw title>" --country <cc> [--venue NAME] [--ref A [--ref B]]
  scripts/title-trace --batch FILE [--ref A [--ref B]]
  scripts/title-trace --compiles REF

  --venue   the venue whose rules apply: its display name, or a unique part of it (case and accents aside); a name the
            roster does not hold is traced with no venue rules
  --ref     trace under that ref's code (laid out and compiled once into the cache, as scripts/identity-slice does);
            two refs print both traces, then their diff. Without --ref: this checkout's code.
  --batch   every `country<TAB>venue<TAB>raw title` line of FILE traced in one JVM per ref, each trace headed `### line`;
            with two refs, only the billings whose steps moved (and their count) — a whole corpus checked for a
            title-rule or word-list change
  --compiles REF  lay REF out with this checkout's tool files dropped in and compile it, nothing else: the tool must
            compile into every ref it may be pointed at, back to its own first commit e90d2b498 (2026-10-14): a member
            newer than that is reached by name (services.identity.OlderRefs; scripts/identity-slice-old-ref-test.sh)

One JVM per ref; the compile is cached in $KINOWO_IDENTITY_CACHE (default ~/.cache/kinowo-identity), shared with
scripts/identity-slice. The ref's tree and classpath come from identity-tools.sh through scripts/lib/identity_ref_trees.py
until that library moves. Tested by scripts/title_trace_test.py."""
from __future__ import annotations

import difflib
import os
import shutil
import sys
import tempfile
from dataclasses import dataclass, field
from typing import Dict, List, Mapping, Optional, Sequence, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import identity_ref_trees  # noqa: E402
import tool_cli  # noqa: E402
import tool_process  # noqa: E402

TOOL = "title-trace"
REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def tool_files() -> List[str]:
    """The files dropped into every ref, in the order the cache keys hash them: scripts/title-trace-tool-files.txt."""
    with open(os.path.join(REPO, "scripts", "title-trace-tool-files.txt")) as f:
        return [line.strip() for line in f if line.strip() and not line.startswith("#")]


def usage() -> str:
    return (__doc__ or "").split("\n\nOne JVM per ref")[0] + "\n"


@dataclass
class Options:
    raw: str = ""
    country: str = ""
    batch: str = ""
    refs: List[str] = field(default_factory=lambda: [])
    venue: List[str] = field(default_factory=lambda: [])

    def word(self, argv: List[str], i: int) -> int:
        flag = argv[i]
        if flag in ("--country", "--venue", "--ref"):
            value = tool_cli.value_at(argv, i)
            if flag == "--country":
                self.country = value
            elif flag == "--venue":
                self.venue += ["--venue", value]
            else:
                self.refs.append(value)
            return i + 1
        if flag == "--batch":
            self.batch = absolute_path(flag, tool_cli.value_at(argv, i))
            return i + 1
        if flag.startswith("--"):
            raise tool_cli.Refused(f"unknown argument {flag}", with_usage=True)
        if self.raw:
            raise tool_cli.Refused("one raw title only (quote it)")
        self.raw = flag
        return i

    def trace_args(self) -> List[str]:
        """What TitleTrace is handed; the usage for a call naming no title and country, or no readable batch."""
        if self.batch:
            if self.raw or not os.path.isfile(self.batch):
                raise tool_cli.Refused("", with_usage=True)
            args = ["--batch", self.batch]
        elif self.raw and self.country:
            args = [self.raw, "--country", self.country, *self.venue]
        else:
            raise tool_cli.Refused("", with_usage=True)
        if len(self.refs) > 2:
            raise tool_cli.Refused("at most two --ref")
        return args


def absolute_path(flag: str, value: str) -> str:
    """tool-conventions.sh's path_flag: the value made absolute, its directory there."""
    directory = os.path.dirname(value) or "."
    if not os.path.isdir(directory):
        raise tool_cli.UsageError(f"{flag} {value}: no directory {directory}")
    return os.path.join(os.path.realpath(directory), os.path.basename(value))


class Tracer:
    def __init__(self, env: Mapping[str, str], runner: tool_process.Runner) -> None:
        self.env, self.runner = env, runner
        self.refs = identity_ref_trees.RefTrees(env, runner, TOOL)

    def tree(self, ref: str) -> Optional[str]:
        return self.refs.tree(ref, tool_files())

    def compiles(self, ref: str) -> int:
        tree = self.tree(ref)
        if tree is None or self.refs.classpath(tree) is None:
            return 1
        self.refs.log(f"{ref} compiles with this checkout's tool files")
        return 0

    def trace(self, tree: str, args: Sequence[str], log: Optional[str] = None) -> int:
        """TitleTrace under the tree's code, from the tree, its output this tool's or into `log`."""
        return self.refs.run_java(tree, ["services.identity.TitleTrace", *args], stdout=log)

    def at_refs(self, refs: List[str], args: Sequence[str], batch: bool) -> int:
        work = tempfile.mkdtemp(prefix="title-trace.", dir=self.env.get("TMPDIR") or "/tmp")
        try:
            outputs: List[str] = []
            for i, ref in enumerate(refs):
                tree = self.tree(ref)
                if tree is None:
                    return 1
                outputs.append(os.path.join(work, f"{i}.txt"))
                if self.trace(tree, args, outputs[-1]) != 0:
                    sys.stdout.write(read(outputs[-1]))
                    return 1
            if len(refs) == 1:
                sys.stdout.write(read(outputs[0]))
            elif batch:
                moved_billings(outputs[0], outputs[1], refs[0], refs[1])
            else:
                self.steps_moved(outputs[0], outputs[1], refs[0], refs[1])
            return 0
        finally:
            shutil.rmtree(work, ignore_errors=True)

    def steps_moved(self, a: str, b: str, ref_a: str, ref_b: str) -> None:
        print(f"=== {ref_a}")
        sys.stdout.write(read(a))
        print()
        if read(a) == read(b):
            print(f"=== {ref_b}: every step the same")
            return
        print(f"=== {ref_a} → {ref_b}: the steps that moved")
        diff = self.runner.capture(tool_process.Command(["diff", "-u", a, b], self.env))
        sys.stdout.write("".join(diff.stdout.splitlines(keepends=True)[2:]))


def read(path: str) -> str:
    with open(path, encoding="utf-8", errors="replace") as f:
        return f.read()


def traces(path: str) -> Dict[str, List[str]]:
    out: Dict[str, List[str]] = {}
    key: Optional[str] = None
    with open(path, encoding="utf-8") as f:
        for line in f:
            if line.startswith("### "):
                key = line[4:].rstrip("\n")
                out[key] = []
            elif key is not None:
                out[key].append(line)
    return out


def moved_billings(a_path: str, b_path: str, ref_a: str, ref_b: str) -> None:
    """Two refs over a batch: the billings that moved, each with its two traces' diff, and their count."""
    a, b = traces(a_path), traces(b_path)
    moved = [k for k in a if a[k] != b.get(k)] + [k for k in b if k not in a]
    for k in moved:
        print(f"### {k}")
        sys.stdout.writelines(list(difflib.unified_diff(a.get(k, []), b.get(k, []), n=0))[2:])
    print(f"=== {ref_a} → {ref_b}: {len(moved)} of {len(a)} billings moved")


def parsed(argv: List[str]) -> Optional[Tuple[Options, List[str]]]:
    """The options and what TitleTrace is handed; None when --help printed the usage. Refused or UsageError raised."""
    o = tool_cli.read_options(Options(), argv, usage(), Options.word)
    return None if o is None else (o, o.trace_args())


def refused(argv: List[str]) -> Tuple[Optional[int], Optional[Tuple[Options, List[str]]]]:
    """(the exit, None) for a call that ends here -- the usage asked for or refused -- else (None, the parsed call)."""
    try:
        call = parsed(argv)
    except tool_cli.UsageError as error:
        return tool_cli.usage_failed(TOOL, error), None
    except tool_cli.Refused as refusal:
        return tool_cli.refused(refusal, usage()), None
    return (0, None) if call is None else (None, call)


def main(argv: List[str], env: Mapping[str, str]) -> int:
    if not argv:
        sys.stderr.write(usage())
        return 2
    tracer = Tracer(env, tool_process.SubprocessRunner())
    if argv[0] == "--compiles":
        if len(argv) < 2:
            print(f"{TOOL}: --compiles REF", file=sys.stderr)
            return 1
        return tracer.compiles(argv[1])
    code, call = refused(argv)
    if call is None:
        return code or 0
    o, args = call
    sys.stdout.flush()
    return tracer.at_refs(o.refs, args, bool(o.batch)) if o.refs else tracer.trace(REPO, args)

if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], os.environ))
