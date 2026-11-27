"""WHAT A RUN OF scripts/run-script-tests SELECTS: its options, and the self-tests they pick -- by tag and filter, and
with --changed (or --paths) what the change reaches (script_tests_changed.py), the floor's scope with --bash, and the
floor's findings (script_tests_floor.py) when they are asked for."""
from __future__ import annotations

import os
from dataclasses import dataclass, field
from typing import List, Optional, Sequence, Tuple

import machine
import script_tests_changed as changes
import script_tests_floor as floor
from script_tests_found import Found, concurrency_heavy, discover, matches_filter


class Refused(Exception):
    """A --changed or --paths the run cannot read: said, exit 2."""


@dataclass
class Options:
    jobs: str = "8"
    stress: str = "0"
    burn: str = ""
    timeout: str = "0"
    stall: str = "300"
    root: str = ""
    tag: str = ""
    changed: str = ""
    paths: str = ""
    bash: str = ""
    floor: str = ""
    list: bool = False
    all: bool = False
    reads: bool = False
    readers: bool = False
    linux: bool = False
    jobs_given: bool = False
    filters: List[str] = field(default_factory=lambda: [])
    roots: List[str] = field(default_factory=lambda: [])  # where the self-tests are
    constructs: str = ""  # the floor's table, scripts/floor-constructs.tsv


def git_in(root: str) -> changes.Git:
    def git(args: Sequence[str]) -> Tuple[int, str]:
        return machine.output(["git", "-c", "core.quotePath=false", *args], cwd=root)
    return git


@dataclass
class Selection:
    """What a run selected, and the floor's findings (FLOOR) when it was asked for them."""
    tests: List[Found]
    findings: List[str] = field(default_factory=lambda: [])


def direct_changes(o: Options, git: changes.Git) -> List[str]:
    if o.paths:
        try:
            with open(o.paths) as f:
                return changes.sorted_unique(f.read().splitlines())
        except OSError:
            raise Refused(f"run-script-tests: --paths {o.paths} cannot be read")
    found = changes.changed_paths(git, o.changed)
    if found is None:
        raise Refused(f"run-script-tests: --changed {o.changed} is no commit")
    return found


def floor_findings(o: Options, git: changes.Git, bash: str, files: Sequence[str]) -> List[str]:
    def parse_by_floor(path: str) -> Tuple[int, str]:
        return machine.errors([bash, "-n", path], cwd=o.root)
    table = floor.constructs(o.constructs)
    return floor.findings(o.root, parse_by_floor, git, o.changed, files, table)


def wanted(o: Options, t: Found) -> bool:
    """Of its tag (--tag), untagged (or any, --all), and matching a filter."""
    tagged = t.tag == o.tag if o.tag else (o.all or t.tag == "-")
    return tagged and matches_filter(t.path, o.filters)


def select(o: Options, bash: str, findings_by: str = "") -> Selection:
    """The tests a run selects: by tag, filter and (--changed) what the change reaches; with `bash`, the floor's
    scope; `findings_by` the floor bash whose findings to make."""
    every = discover(o.root, o.roots)
    found = [t for t in every if wanted(o, t)]
    if not o.changed:
        return Selection(found)
    git = git_in(o.root)
    direct = direct_changes(o, git)
    shown = floor_findings(o, git, findings_by, floor.bash_sources(o.root, direct)) if findings_by else []
    selector = selector_for(o, git, floor.bash_sources(o.root, direct) if bash else direct, [t.path for t in every],
                            bool(bash))
    return Selection([t for t in found if selector.touched(t.path)], shown)


def selector_for(o: Options, git: changes.Git, direct: Sequence[str], tests: Sequence[str],
                 floor_scope: bool) -> changes.Selector:
    """Over what the direct changes reach (what sources or imports them); the derived reads but in the floor's scope."""
    index = changes.Index(changes.script_index(git, o.roots))
    reach = changes.reached(direct, changes.source_users(o.root, index))
    derived = None if floor_scope else changes.Derived(o.root, index, tests)
    return changes.Selector(o.root, tests, reach, floor_scope=floor_scope, readers=o.readers, derived=derived)


def heavy_only(o: Options, chosen: Selection) -> Optional[int]:
    """--stress with no names: the concurrency-heavy ones; an exit status when there are none to run."""
    chosen.tests = [t for t in chosen.tests if concurrency_heavy(os.path.join(o.root, t.path))]
    if chosen.tests or o.list:
        return None
    since = f" by the changes since {o.changed}" if o.changed else ""
    print(f"run-script-tests: no concurrency-heavy script test selected{since} (STRESS)")
    return 0 if o.changed else 1

