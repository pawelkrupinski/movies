"""THE CHECKOUT scripts/identity_impact_check.py proves the identity test-impact selection in: a detached worktree of
this clone, reset between steps (its ignored build output kept warm), its own scripts/sbt-warm server running the
selection (`identityImpact`) and the identity set's tests, whose JUnit reports say which classes failed.

  Checkout(runner, env, repo, tree)
    .prepare(at)                   the tree detached at `at`, clean but for the ignored build output
    .lay_tooling()                 the selection's tooling, as the clone has it, over an older commit (uncommitted)
    .select(base, head, out)       a Selection of base..head (head None: the working tree)
    .area(every)                   {module: classes} the identity set's lines run, of `every`
    .run_classes(log, classes, countries)   the classes that failed (country-scoped with `countries`)

Classes are {module: set of test classes}; flat / minus / within / restricted are their set algebra."""
from __future__ import annotations

import glob
import os
import re
import shutil
import sys
import xml.etree.ElementTree as ET
from dataclasses import dataclass
from typing import Callable, Dict, List, Mapping, Optional, Set, Tuple

import identity_impact  # (scripts/ci and scripts/lib: on the path identity_impact_check.py sets)
import tool_git
import tool_process

Classes = Dict[str, Set[str]]
# identity_impact.py, untyped (scripts/ci is another lane's): the four names this tool reads, typed here
READ_IMPACT: Callable[[str], Tuple[List[str], Classes]] = getattr(identity_impact, "read_impact")
COUNTRIES_OF: Callable[[str, Optional[str], List[str], str], List[str]] = getattr(identity_impact, "countries")
PLAN: Callable[..., List[str]] = getattr(identity_impact, "plan")
COUNTRIES: List[str] = getattr(identity_impact, "COUNTRIES")
TOOLING = ["identity-impact.sbt", "project/TestImpact.scala", "project/SameFileEdges.scala",
           "scripts/ci/identity_impact.py"]
SET_LINE = re.compile(r"^identity\s+sbt\s+(.*)$")


def say(msg: str) -> None:
    print(f"identity-impact-check: {msg}", file=sys.stderr, flush=True)


@dataclass(frozen=True)
class Selection:
    why: List[str]  # the reasons identityImpact selected ALL, or none
    chosen: Classes  # what it selected
    every: Classes  # every test class
    countries: List[str]


class Checkout:
    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str], repo: str, tree: str) -> None:
        self.runner = runner
        self.env = env
        self.repo = repo
        self.tree = tree

    def git_in(self, root: str, *args: str) -> str:
        done = tool_git.Git(self.runner, self.env, root).call(*args, quiet=True)
        if done.code != 0:
            raise RuntimeError(f"git {' '.join(args)}: {done.stderr.strip()[-2000:]}")
        return done.stdout

    def git(self, *args: str) -> str:
        return self.git_in(self.tree, *args)

    def head(self) -> str:
        return self.git_in(self.repo, "rev-parse", "HEAD").strip()

    def prepare(self, at: str) -> None:
        if not os.path.isdir(self.tree):
            os.makedirs(os.path.dirname(self.tree), exist_ok=True)
            self.git_in(self.repo, "worktree", "add", "--detach", self.tree, at)
        self.git("reset", "-q", "--hard")
        self.git("clean", "-qfd")  # untracked only: the ignored build output stays, warm
        self.git("checkout", "-q", "--detach", at)

    def lay_tooling(self) -> None:
        for path in TOOLING:
            os.makedirs(os.path.dirname(os.path.join(self.tree, path)) or self.tree, exist_ok=True)
            shutil.copy(os.path.join(self.repo, path), os.path.join(self.tree, path))

    def sbt(self, log: str, *commands: str) -> int:
        return self.runner.run(tool_process.Command([os.path.join(self.tree, "scripts", "sbt-warm"), *commands],
                                                    self.env, log=log, cwd=self.tree))

    def set_lines(self) -> List[str]:
        """The identity set's sbt lines, as the clone's local-tests.txt defines them."""
        with open(os.path.join(self.repo, "scripts", "hooks", "local-tests.txt")) as fh:
            return [m.group(1) for m in (SET_LINE.match(line.rstrip("\n")) for line in fh) if m]

    def select(self, base: str, head: Optional[str], out: str) -> Selection:
        changed = self.git("diff", "--name-only", "--no-renames", base, *([head] if head else [])).split()
        with open(out + ".changed", "w") as fh:
            fh.write("\n".join(changed) + "\n")
        if self.sbt(out + ".impact.log", f"identityImpact {out}.changed {out}.impact") != 0:
            raise RuntimeError(f"identityImpact failed: {out}.impact.log")
        why, chosen = READ_IMPACT(out + ".impact")
        _, every = READ_IMPACT(out + ".impact.tests")
        return Selection(why, chosen, every, self.countries(base, head, changed))

    def countries(self, base: str, head: Optional[str], changed: List[str]) -> List[str]:
        cwd = os.getcwd()
        try:
            os.chdir(self.tree)  # identity_impact reads the revisions with git in the working directory
            return COUNTRIES_OF(base, head, changed, self.tree)
        finally:
            os.chdir(cwd)

    def area(self, every: Classes) -> Classes:
        out: Classes = {}
        for line in PLAN(self.set_lines(), [], every, None):
            words = line.split()
            if words and words[0].endswith("/testOnly"):
                out.setdefault(words[0].split("/")[0], set()).update(test_names(words[1:]))
        return out

    def run_classes(self, log: str, classes: Classes, countries: Optional[List[str]] = None) -> Set[str]:
        """Run `classes` (country-scoped with `countries`), the reports cleared first: the classes that failed."""
        reports = os.path.join(self.tree, "target", "test-reports", "unit")
        shutil.rmtree(reports, ignore_errors=True)
        lines = PLAN([f"{m}/testOnly *" for m in classes], [], classes, countries)
        if not lines:
            return set()
        rc = self.sbt(log, *lines)
        failed = failed_reports(reports)
        if rc != 0 and not failed:
            failed.add(f"(the run failed with no failing report: {log})")
        return failed


def failed_reports(reports: str) -> Set[str]:
    failed: Set[str] = set()
    for f in glob.glob(os.path.join(reports, "TEST-*.xml")):
        try:
            root = ET.parse(f).getroot()
        except ET.ParseError:
            continue
        if int(root.get("failures", "0")) + int(root.get("errors", "0")) > 0:
            failed.add(root.get("name", ""))
    return failed


def plan_classes(lines: List[str]) -> Classes:
    """{module: classes} the sbt lines name (`<module>/testOnly A B -- -D...`); a whole `<module>/test` as `*`."""
    out: Classes = {}
    for line in lines:
        words = line.split()
        if words and "/" in words[0]:
            module, task = words[0].split("/", 1)
            if task == "test":
                out.setdefault(module, set()).add("*")
            elif task.startswith("testOnly"):
                out.setdefault(module, set()).update(test_names(words[1:]))
    return out


def test_names(words: List[str]) -> Set[str]:
    """The classes of a testOnly line's words: its sbt options (`--`, `-D...`) aside."""
    return {w for w in words if w != "--" and not w.startswith("-D")}


def flat(classes: Classes) -> Set[str]:
    return {c for cs in classes.values() for c in cs}


def minus(a: Classes, b: Classes) -> Classes:
    out = {m: cs - b.get(m, set()) for m, cs in a.items()}
    return {m: cs for m, cs in out.items() if cs}


def within(a: Classes, b: Classes) -> Classes:
    return {m: cs & b.get(m, set()) for m, cs in a.items() if cs & b.get(m, set())}


def restricted(a: Classes, names: Set[str]) -> Classes:
    return {m: cs & names for m, cs in a.items() if cs & names}
