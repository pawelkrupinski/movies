#!/usr/bin/env python3
"""The population walk's base side, for the pre-push hook's identity check

THE POPULATION WALK'S BASE SIDE (the pre-push hook's identity check; scripts/ci/population_walk_diff.py says what is
judged): the candidate slices PopulationMonotonicitySpec wrote to <candidates dir> walked with <base>'s code, then
judged head against base. Nothing to do (and no sbt) when no slice is a candidate -- the usual case: every slice's
flips are the known rows. The base's rows are kept per base commit and slice under
$KINOWO_IDENTITY_CACHE/population-walk/<sha>/ (a slice's data is the head's, so its key holds the slice's tree hash):
a second push on the same base walks nothing again. The base's tree and classpath are identity-tools.sh's (laid out
once, compiled seeded from the nearest compiled tree: only what the change touched recompiles), through
scripts/lib/identity_tooling/ci/identity_ref_trees.py.
A slice THE CHANGE BRINGS (its data or its `population` rows differ from the base's: population_walk_diff.py brought)
is not walked at the base -- the base would walk the head's slice, and main's spec judges it on the known rows alone,
so this judges it so too (Main #2515), each missing row printed ready to add.

  scripts/ci/population-walk-diff <candidates dir> <base ref>     exit 1: a new flip or a cured known row (named)"""
from __future__ import annotations

import os
import subprocess
import sys
from typing import List, Mapping, Optional

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
sys.path.insert(0, os.path.join(ROOT, "scripts", "lib"))
import ci_machine  # noqa: E402
from identity_tooling.ci import identity_ref_trees  # noqa: E402
import tool_git  # noqa: E402
import tool_process  # noqa: E402
from processes import tool_python  # noqa: E402

JUDGE = os.path.join("scripts", "ci", "population_walk_diff.py")
FIXTURES = "test/resources/fixtures"


def say(line: str) -> None:
    print(f"population walk: {line}", file=sys.stderr, flush=True)


def judge(*args: str, stdout: Optional[str] = None) -> int:
    """population_walk_diff.py with `args`, run from the checkout, its stdout into the file `stdout` (None: ours)."""
    sys.stdout.flush()
    command = tool_python.isolated(JUDGE, *args)
    if stdout is None:
        return subprocess.run(command, cwd=ROOT).returncode
    with open(stdout, "w") as sink:
        return subprocess.run(command, cwd=ROOT, stdout=sink).returncode


def lines_of(text: str) -> List[str]:
    return [line for line in text.splitlines() if line]


class BaseWalk:
    def __init__(self, env: Mapping[str, str], directory: str, sha: str, brought: List[str]) -> None:
        self.env, self.dir, self.sha, self.brought = env, directory, sha, brought
        cache = env.get("KINOWO_IDENTITY_CACHE") or os.path.join(os.path.expanduser("~"), ".cache", "kinowo-identity")
        self.cache = os.path.join(cache, "population-walk", sha)
        self.rows = os.path.join(directory, "base-rows.txt")
        self.trees = identity_ref_trees.RefTrees(env, tool_process.SubprocessRunner(), "population-walk")

    def key_of(self, name: str) -> str:
        """A slice's cache file: its name and the tree hash of its data at the head; none for an uncommitted slice (a
        laptop run on edits), which is never cached."""
        tree = tool_git.Git.here(ROOT).line("rev-parse", "--verify", f"HEAD:{FIXTURES}/{name}")
        return os.path.join(self.cache, f"{name.replace('/', '_')}-{tree[:12]}.txt") if tree else ""

    def append_rows(self, text: str) -> None:
        with open(self.rows, "a") as f:
            f.write(text)

    def cached(self, slices: List[str]) -> List[str]:
        """The slices with no kept rows (the kept ones' rows appended, the slices the change brings left out)."""
        todo: List[str] = []
        for name in slices:
            if name in self.brought:
                continue
            kept = self.key_of(name)
            if kept and os.path.exists(kept) and os.path.getsize(kept) > 0:
                with open(kept) as f:
                    self.append_rows(f.read())
            else:
                todo.append(name)
        return todo

    def java_command(self) -> Optional[List[str]]:
        """The base's java and classpath (its tree laid out and compiled), or None, said."""
        tree = self.trees.tree(self.sha, [])
        classpath = self.trees.classpath(tree) if tree is not None else None
        java = self.trees.java() if classpath is not None else None
        if classpath is None or java is None:
            say(f"the base {self.sha[:9]} could not be compiled: judged on the known rows alone")
            return None
        return [*java, "-cp", classpath]

    def walk(self, todo: List[str]) -> None:
        """The head's slices walked with the base's code, run here where the slices' relative paths resolve."""
        java = self.java_command()
        if java is None:
            return
        out, err = os.path.join(self.dir, "base-walk.txt"), os.path.join(self.dir, "base-walk.err")
        argv = [*java, "services.identity.PopulationMonotonicitySpec", *(f"{FIXTURES}/{name}" for name in todo)]
        code = tool_process.SubprocessRunner().run(tool_process.Command(argv, self.env, log=out, stderr_log=err, cwd=ROOT))
        if code != 0:
            with open(err, errors="replace") as f:
                sys.stderr.write("".join(f.readlines()[-20:]))
            say("the base's walk failed: judged on the known rows alone")
            return
        with open(out, errors="replace") as f:
            walked = lines_of(f.read())
        for name in todo:
            self.keep(name, [ln for ln in walked if ln.startswith(f"# {name}: ") or ln.split("\t")[0] == name])

    def keep(self, name: str, part: List[str]) -> None:
        if not part:
            return
        text = "\n".join(part) + "\n"
        self.append_rows(text)
        kept = self.key_of(name)
        # a failed walk is not kept: the next push tries again
        if kept and not any(ln.startswith(f"# {name}: FAILED") for ln in part):
            os.makedirs(self.cache, exist_ok=True)
            with open(kept + ".part", "w") as f:
                f.write(text)
            os.replace(kept + ".part", kept)


def captured_slices(directory: str) -> List[str]:
    done = subprocess.run(tool_python.isolated(JUDGE, "slices", directory), cwd=ROOT, stdout=subprocess.PIPE, text=True)
    return lines_of(done.stdout)


def main(argv: List[str], env: Mapping[str, str]) -> int:
    if len(argv) < 2:
        print(f"population-walk-diff: {len(argv) + 1}: {'candidates dir' if not argv else 'base ref'}", file=sys.stderr)
        return 1
    directory, base_ref = argv[0], argv[1]
    slices = captured_slices(directory)
    if not slices:
        return 0
    sha = tool_git.Git.here(ROOT).line("rev-parse", "--verify", f"{base_ref}^{{commit}}")
    if sha is None:
        say(f"no base commit {base_ref}: judged on the known rows alone")
        return judge("judge", directory, "/dev/null")
    brought_file = os.path.join(directory, "brought.txt")
    if judge("brought", directory, sha, stdout=brought_file) != 0:
        say("the slices the change brings could not be read: judged on the known rows alone")
        return judge("judge", directory, "/dev/null")
    with open(brought_file) as f:
        brought = lines_of(f.read())
    walk = BaseWalk(env, directory, sha, brought)
    os.makedirs(walk.cache, exist_ok=True)
    open(walk.rows, "w").close()
    todo = walk.cached(slices)
    say(f"{len(slices)} candidate slice(s), {len(brought)} the change brings, "
        f"{len(slices) - len(brought) - len(todo)} cached for base {sha[:9]}")
    if todo:
        started = ci_machine.wall()
        walk.walk(todo)
        say(f"base side {len(todo)} slice(s) in {int(ci_machine.wall() - started)} s")
    return judge("judge", directory, walk.rows, "--brought", brought_file)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], ci_machine.environment()))
