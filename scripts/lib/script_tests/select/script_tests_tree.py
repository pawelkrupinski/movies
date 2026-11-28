"""THE TREE IS LEFT AS FOUND (scripts/run-script-tests): the checkout's state (`git status --porcelain --ignored`: what
is new, changed, deleted, or new and ignored) is taken before the run and after it, and each path new after it is put
on the test that made it. Its candidates are the tests running when it was made (its ctime inside a test's
start..end, a second either side); then each candidate runs AGAIN, ALONE, the quickest first, with the leftovers moved
aside, and the one that makes a path again is its polluter. THE CONTROL WINDOW: with no test running, each claimed path
is watched for as long as the slowest candidate's alone run took (2..60 s): made again then, it is no test's (a writer
beside the run, the landing check's sbt writing target/). The leftovers are put back afterwards. macOS's own
metadata (.metadata_never_index, .DS_Store) is no test's."""
from __future__ import annotations

import os
import shutil
from dataclasses import dataclass, field
from typing import Callable, Dict, List, Protocol, Sequence, Set, Tuple

from script_tests.select.script_tests_changed import Git

MACOS = (".metadata_never_index", ".DS_Store")


def tree_state(git: Git) -> List[str]:
    """The checkout's state, a sorted line a path: XY and the path (new and ignored ones too, not their insides)."""
    out = git(["status", "--porcelain=v1", "--ignored", "--untracked-files=normal"])[1]
    lines = [line for line in out.splitlines() if line and line[3:].rstrip("/").rsplit("/", 1)[-1] not in MACOS]
    return sorted(lines, key=lambda line: line.encode())


def ctime(root: str, path: str) -> float:
    """The ctime of `path`, or of its nearest ancestor that exists (a path deleted)."""
    probe = path.rstrip("/")
    while probe and not os.path.lexists(os.path.join(root, probe)):
        probe = os.path.dirname(probe)
    return os.lstat(os.path.join(root, probe or ".")).st_ctime


def present(root: str, path: str) -> bool:
    full = os.path.join(root, path.rstrip("/"))
    return os.path.lexists(full)


class Again(Protocol):
    """One of the run's tests run again, alone, with nothing beside it: the seconds it took."""

    def __call__(self, index: int) -> int: ...


@dataclass
class Leftovers:
    """What a run left, and the verdict on each path: `polluted` (index -> its status lines), `unclaimed`."""
    new: List[str]
    polluted: Dict[int, List[str]] = field(default_factory=lambda: {})
    unclaimed: List[Tuple[str, List[int]]] = field(default_factory=lambda: [])

    def line_of(self, path: str) -> str:
        return next(line for line in self.new if line[3:] == path)

    def unclaim(self, path: str) -> None:
        """`path` no test's: off every test it was put on."""
        kept = {i: [line for line in lines if line[3:] != path] for i, lines in self.polluted.items()}
        self.polluted = {i: lines for i, lines in kept.items() if lines}

    def claimed(self) -> Set[str]:
        return {line[3:] for lines in self.polluted.values() for line in lines}


@dataclass(frozen=True)
class Window:
    start: int
    end: int
    seconds: int


class Attribution:
    """The run's leftovers put on the tests that make them again alone (`again(index)` runs one, returning its
    seconds; `wait(seconds)` sleeps; `now()` the wall clock)."""

    def __init__(self, root: str, scratch: str, again: Again, wait: Callable[[float], None],
                 now: Callable[[], float]) -> None:
        self.root, self.aside, self.again, self.wait, self.now = root, os.path.join(scratch, "aside"), again, wait, now
        self.moved: List[str] = []

    def candidates(self, paths: Sequence[str], windows: Dict[int, Window]) -> Dict[str, List[int]]:
        out: Dict[str, List[int]] = {}
        for path in paths:
            t = ctime(self.root, path)
            out[path] = sorted(i for i, w in windows.items() if w.start - 1 <= t <= w.end + 1)
        return out

    def move_aside(self, paths: Sequence[str], into: str) -> List[str]:
        moved: List[str] = []
        for path in paths:
            if present(self.root, path):
                target = os.path.join(into, path.rstrip("/"))
                os.makedirs(os.path.dirname(target), exist_ok=True)
                shutil.move(os.path.join(self.root, path.rstrip("/")), target)
                moved.append(path)
        return moved

    def restore(self) -> None:
        """Every leftover moved aside put back where nothing took its place."""
        for path in self.moved:
            here = os.path.join(self.root, path.rstrip("/"))
            if not os.path.lexists(here):
                os.makedirs(os.path.dirname(here), exist_ok=True)
                shutil.move(os.path.join(self.aside, path.rstrip("/")), here)
        self.moved = []

    def made_again(self, path: str, movable: Set[str], since: float) -> bool:
        if path in movable:
            return present(self.root, path)
        return ctime(self.root, path) >= since

    def attribute(self, before: Sequence[str], after: Sequence[str], windows: Dict[int, Window]) -> Leftovers:
        old = set(before)
        left = Leftovers([line for line in after if line not in old])
        return self.claim(left, windows) if left.new else left

    def claim(self, left: Leftovers, windows: Dict[int, Window]) -> Leftovers:
        paths = [line[3:] for line in left.new]
        movable = {line[3:] for line in left.new if line[:3] in ("?? ", "!! ")}
        candidates = self.candidates(paths, windows)
        order = sorted({i for c in candidates.values() for i in c}, key=lambda i: (windows[i].seconds, i))
        print(f"\nrun-script-tests: {len(left.new)} path(s) left in the checkout; running each of the {len(order)} "
              "test(s) running when they were made again, alone", flush=True)
        self.moved = self.move_aside([p for p in paths if p in movable], self.aside)
        slowest = self.run_alone(order, paths, movable, left)
        self.control_window(slowest, movable, left)
        self.restore()
        claimed = left.claimed()
        left.unclaimed = [(left.line_of(p), candidates[p]) for p in paths if p not in claimed]
        return left

    def run_alone(self, order: Sequence[int], paths: Sequence[str], movable: Set[str], left: Leftovers) -> int:
        slowest = 0
        for candidate in order:
            since = self.now()
            slowest = max(slowest, self.again(candidate))
            for path in paths:
                if self.made_again(path, movable, since):
                    left.polluted.setdefault(candidate, []).append(left.line_of(path))
            self.move_aside([p for p in paths if p in movable], os.path.join(self.aside, "..", f"again-{candidate}"))
            if len(left.claimed()) >= len(paths):
                break
        return slowest

    def control_window(self, slowest: int, movable: Set[str], left: Leftovers) -> None:
        """Each claimed path watched with no test running: made again, it is no test's."""
        if not left.polluted:
            return
        since = self.now()
        self.wait(min(60, max(2, slowest)))
        for path in sorted(left.claimed()):
            if self.made_again(path, movable, since):
                left.unclaim(path)
