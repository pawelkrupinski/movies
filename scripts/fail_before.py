#!/usr/bin/env python3
# Purpose: Proves the commit gate: a test that fails before the change and passes after it
"""Proves the commit gate: a test that FAILS before the change and PASSES after it.

  scripts/fail-before [<commit>] [--files <glob>]... [--ci | --local] -- <test command...>

<commit> (default HEAD) is the change under test. Two throwaway detached worktrees are made:
  BEFORE  <commit>'s parent, with ONLY the commit's test-side files copied over from <commit>
          (so the new test meets the old production code);
  AFTER   <commit> itself.
The command (one argument: a shell line, run through `bash -c`; several: one command, each argument kept whole), run
from the worktree's root, must FAIL in BEFORE and PASS in AFTER.
  FAIL-BEFORE: yes|no     PASS-AFTER: yes|no     (and the tail of each output)
Exit 0 only for yes/yes; 1 otherwise; 2 for a usage or setup error, or a run that timed out (no verdict).

TEST-SIDE FILES (the only ones copied into BEFORE): the source and resource directories of the build's TEST
configurations, *_test.py, *-test.sh, test/resources/**, .claude/**. Which src/ directories are test-side is read
from <commit>'s build.sbt, not listed here: `Test` and every `config(...)` extending it (src/test, src/it,
src/page, or the directory its `scalaSource`/`resourceDirectory` names). A configuration extending `Compile` --
src/fixtures, the replay wirings and fixture-archive code the tests drive -- is code under test and stays at the
parent: copied, a fix living there met its test in BEFORE and proved nothing. --files <glob> (repeatable; a shell
`case` pattern over the repo-relative path, `*` crosses `/`) replaces that default list.

SBT: a command that goes through scripts/sbt-warm runs against each worktree's own kept server (the
worktree's own copy of the script); both servers are stopped afterwards. A cold worktree compiles from
the seed, so expect minutes: the caller's checkout when it is at <commit> and built (KINOWO_SBT_WARM_SEED, unless
set) — AFTER is its very sources and BEFORE differs from them by the commit's production files alone, where the
newest build of an older main commit cost a recompile of every commit since (6-9 min a run, 2026-10-18).

ON CI (the owner, 2026-11-18: heavy local proofs go to ci-runner's measure lane): --ci runs both on ci-runner instead
(scripts/ci-sbt, through identity-run.yml), side by side -- BEFORE as a commit of the parent's tree with the same
test-side files laid over it, AFTER as <commit> -- for a command that is ONE scripts/sbt-warm call (its sbt commands
run there as `sbt -batch "<a>; <b>"`). The proof is as strong as the local one: each side's verdict is that run's
"Run the command" step, the sbt command's own exit (a test or compile failure fails; passing passes), and a run that
gives none -- a setup step failed, cancelled, timed out -- is no verdict (exit 2), never a "fails before". --local
runs here. Neither: KINOWO_FAIL_BEFORE_ROUTE (auto | local | ci), auto by default -- on CI when the command can go
there, the laptop's governor would hold its two runs now and the measure lane is free (scripts/jvm-gate.sh
jvm_route: `agentctl gov route`), else here. The local runs stay local inside (KINOWO_SBT_WARM_ROUTE=local).
KINOWO_CI_SBT names scripts/ci-sbt (the tests' seam).

SAFETY: the caller's worktree is never touched. The worktrees live in $KINOWO_FAIL_BEFORE_DIR (default
~/.cache/kinowo-fail-before), are removed by a trap, and are the only change to the shared .git (its
worktree list). Every GIT_* variable is dropped first, so a hook's exported GIT_DIR cannot redirect git
(see scripts/scratch-git.sh). KINOWO_FAIL_BEFORE_TIMEOUT (seconds, default 3000) bounds each run, its
whole process group (an sbt-forked JVM too): a run past it is ended and gives no verdict."""
from __future__ import annotations

import fnmatch
import glob
import os
import re
import shlex
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass, field
from typing import List, Mapping, Sequence, Set, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import tool_cli  # noqa: E402
import tool_git  # noqa: E402
import tool_process  # noqa: E402
import tool_shell  # noqa: E402

PURPOSE = "Proves the commit gate: a test that fails before the change and passes after it"
USAGE = "usage: scripts/fail-before [<commit>] [--files <glob>]... [--ci | --local] -- <test command...>"
CONFIG = re.compile(r'lazy val ([A-Za-z0-9_]+) *= *config\("([A-Za-z0-9_-]+)"\) *extend\(? *([A-Za-z0-9_]+)')
OVERRIDE = re.compile(r'([A-Za-z0-9_]+) */ *(scalaSource|javaSource|resourceDirectory) *:= *baseDirectory\.value */ *"src" */ *"([^"]+)"')
DEFAULT_PATTERNS = ("*_test.py", "*-test.sh", "test/resources/*", ".claude/*")


class Died(Exception):
    """`fail-before: <message>` on stderr and the exit status: 2 for a usage slip and for a setup that gives no verdict."""

    def __init__(self, message: str, see_help: bool = False) -> None:
        super().__init__(message)
        self.see_help = see_help


@dataclass
class Options:
    commit: str = "HEAD"
    patterns: List[str] = field(default_factory=lambda: [])
    route: str = "auto"
    command: List[str] = field(default_factory=lambda: [])
    help: bool = False


def read_word(o: Options, argv: Sequence[str], i: int) -> int:
    """`o` with the flag or commit at argv[i] read into it; the index of the last word it took."""
    arg = argv[i]
    if arg == "--files":
        if i + 1 >= len(argv):
            raise Died("--files needs a value", see_help=True)
        o.patterns.append(argv[i + 1])
        return i + 1
    if arg in ("--ci", "--local"):
        o.route = arg[2:]
    elif arg in ("-h", "--help"):
        o.help = True
    elif arg.startswith("-"):
        raise Died(f"unknown option {arg}", see_help=True)
    else:
        o.commit = arg
    return i


def parse(argv: Sequence[str], route: str) -> Options:
    o = Options(route=route)
    i = 0
    while i < len(argv) and not o.help:
        if argv[i] == "--":
            o.command = list(argv[i + 1:])
            break
        i = read_word(o, argv, i) + 1
    if o.help:
        return o
    if not o.command:
        raise Died(USAGE, see_help=True)
    if o.route not in ("auto", "local", "ci"):
        raise Died(f"KINOWO_FAIL_BEFORE_ROUTE is auto, local or ci, not {o.route}", see_help=True)
    return o


def test_configurations(build: str) -> Tuple[Set[str], List[str]]:
    """The configurations extending Test, directly or through another (one may extend a configuration declared after
    it: repeat until none joins), and the src/ directory of each (its sbt default, src/<name>)."""
    values, directories, grew = {"Test"}, ["test"], True
    while grew:
        grew = False
        for m in filter(None, map(CONFIG.search, build.splitlines())):
            if m.group(3) in values and m.group(1) not in values:
                values.add(m.group(1))
                directories.append(m.group(2))
                grew = True
    return values, directories


def test_directories(build: str) -> List[str]:
    """The src/ directories of the build's test configurations: `test`, every `config("x") extend Y` whose Y is Test or
    another such configuration, and any directory such a configuration's `X / scalaSource` / `javaSource` /
    `resourceDirectory` names."""
    values, directories = test_configurations(build)
    overrides = filter(None, map(OVERRIDE.search, build.splitlines()))
    return sorted(set(directories + [m.group(3) for m in overrides if m.group(1) in values]))


def sbt_command_of(command: Sequence[str]) -> str:
    """The sbt command line ("a; b") of a test command that is one scripts/sbt-warm call; "" for anything else (a shell
    line of several commands, a pipe, a redirect, an sbt-warm flag)."""
    args = list(command)
    if len(args) == 1:
        lex = shlex.shlex(args[0], posix=True, punctuation_chars=True)
        lex.whitespace_split = True
        try:
            args = list(lex)
        except ValueError:
            return ""
        if any(t and set(t) <= set("();<>&") | {"|"} for t in args):
            return ""
    if len(args) < 2 or os.path.basename(args[0]) != "sbt-warm" or any(a.startswith("--") for a in args[1:]):
        return ""
    return "; ".join(args[1:])


def last_lines(path: str, count: int = 15) -> str:
    with open(path, errors="replace") as f:
        return "".join(f.readlines()[-count:])


def verdict(before_rc: int, after_rc: int, before_log: str, after_log: str) -> int:
    fail_before, pass_after = before_rc != 0, after_rc == 0
    print(f"\n--- BEFORE (exit {before_rc}), last lines ---\n{last_lines(before_log)}", end="")
    print(f"\n--- AFTER (exit {after_rc}), last lines ---\n{last_lines(after_log)}", end="")
    print(f"\nFAIL-BEFORE: {'yes' if fail_before else 'no'}\nPASS-AFTER: {'yes' if pass_after else 'no'}", flush=True)
    return 0 if fail_before and pass_after else 1


class Proof:
    """One proof of one commit: its parent, its test-side files, and the two runs."""

    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str], top: str, sha: str, parent: str,
                 options: Options) -> None:
        self.runner, self.env, self.top, self.sha, self.parent, self.options = runner, env, top, sha, parent, options
        self.git = tool_git.Git(runner, env, top)
        self.command_line = options.command[0] if len(options.command) == 1 else shlex.join(options.command)
        self.test_paths = self.find_test_paths()
        self.sbt_command = sbt_command_of(options.command)

    def find_test_paths(self) -> List[str]:
        build = self.git.out("show", f"{self.sha}:build.sbt", quiet=True) or ""
        patterns = self.options.patterns or (
            [f"*/src/{d}/*" for d in test_directories(build)] + list(DEFAULT_PATTERNS))
        changed = self.git.out("diff-tree", "--no-commit-id", "--name-only", "-r", "--diff-filter=AMCR", self.sha) or ""
        paths = [p for p in changed.splitlines() if p and any(fnmatch.fnmatchcase(p, pat) for pat in patterns)]
        if not paths:
            raise Died(f"{self.sha} changes no test-side file (see --files): nothing to prove")
        return paths

    def short(self, rev: str) -> str:
        return (self.git.out("rev-parse", "--short", rev) or "").strip()

    def route(self) -> str:
        route = self.options.route
        if route == "ci" and not self.sbt_command:
            raise Died("--ci runs a command that is one scripts/sbt-warm call (its sbt commands go to sbt -batch on CI); "
                       f"this is not: {self.command_line}")
        if route != "auto":
            return route
        held = tool_shell.shell_function(self.runner, self.env, "scripts/jvm-gate.sh", "jvm_route", ["testUnit"])
        if self.sbt_command and held.stdout.strip() == "ci":
            print("fail-before: the governor would hold two local runs now and ci-runner's measure lane is free: both run "
                  "there (--local runs here)", file=sys.stderr)
            return "ci"
        return "local"

    def before_commit(self) -> str:
        """BEFORE as a commit (for CI): the parent's tree with the test-side files of the commit laid over it."""
        scratch = tempfile.mkdtemp(prefix="fail-before-index.")
        git = tool_git.Git(self.runner, {**self.env, "GIT_INDEX_FILE": os.path.join(scratch, "index")}, self.top)
        try:
            git.text("read-tree", self.parent)
            for path in self.test_paths:
                mode, _, rest = git.text("ls-tree", self.sha, "--", path).partition(" ")
                git.text("update-index", "--add", "--cacheinfo", f"{mode},{rest.split()[1]},{path}")
            tree = git.text("write-tree").strip()
        except tool_git.Failed as e:
            raise Died("cannot make the BEFORE commit") from e
        finally:
            shutil.rmtree(scratch, ignore_errors=True)
        name = (self.git.out("config", "user.name", quiet=True) or "").strip() or "fail-before"
        email = (self.git.out("config", "user.email", quiet=True) or "").strip() or "fail-before@localhost"
        message = f"fail-before: BEFORE of {self.short(self.sha)} (its parent and its test-side files)"
        made = self.runner.capture(tool_process.Command(
            ["git", "-c", f"user.name={name}", "-c", f"user.email={email}", "-C", self.top, "commit-tree", tree, "-p",
             self.parent, "-m", message], self.env))
        if made.code != 0 or not made.stdout.strip():
            raise Died("cannot make the BEFORE commit")
        return made.stdout.strip()

    def on_ci(self) -> int:
        ci_sbt = self.env.get("KINOWO_CI_SBT") or os.path.join(os.path.dirname(os.path.abspath(__file__)), "ci-sbt")
        logs = tempfile.mkdtemp(prefix="fail-before-logs.")
        try:
            before = self.before_commit()
            print(f"fail-before: {self.sha} ({len(self.test_paths)} test-side file(s) laid onto {self.short(self.parent)}) "
                  f"on CI: sbt {self.sbt_command}", flush=True)
            runs = [self.start_ci(ci_sbt, commit, name, logs) for commit, name in ((before, "before"), (self.sha, "after"))]
            before_rc, after_rc = (proc.wait() for proc in runs)
            for name in ("before", "after"):
                sys.stderr.write(open(os.path.join(logs, f"{name}.err"), errors="replace").read())
                open(os.path.join(logs, f"{name}.log"), "a").close()
            if 2 in (before_rc, after_rc):
                raise Died(f"a CI run gave no verdict (BEFORE exit {before_rc}, AFTER exit {after_rc}: a setup step failed, "
                           "cancelled or timed out) -- no verdict, as a local timeout")
            return verdict(before_rc, after_rc, os.path.join(logs, "before.log"), os.path.join(logs, "after.log"))
        finally:
            shutil.rmtree(logs, ignore_errors=True)

    def start_ci(self, ci_sbt: str, commit: str, name: str, logs: str) -> "subprocess.Popen[bytes]":
        sys.stdout.flush()
        with open(os.path.join(logs, f"{name}.err"), "wb") as err:
            return subprocess.Popen([ci_sbt, "--commit", commit, "--log", os.path.join(logs, f"{name}.log"), "--",
                                     self.sbt_command], cwd=self.top, env=dict(self.env), stderr=err)

    def seed(self) -> str:
        seed = self.env.get("KINOWO_SBT_WARM_SEED", "")
        if not seed and (self.git.out("rev-parse", "HEAD") or "").strip() == self.sha \
                and glob.glob(os.path.join(self.top, "*", "target", "scala-*", "zinc")):
            return self.top
        return seed

    def run_in(self, worktree: str, log: str, seed: str) -> int:
        env = {**self.env, "KINOWO_SBT_WARM_ROUTE": "local"}
        if seed:
            env["KINOWO_SBT_WARM_SEED"] = seed
        timeout = float(self.env.get("KINOWO_FAIL_BEFORE_TIMEOUT", "3000"))
        return self.runner.run(tool_process.Command(["bash", "-c", self.command_line], env, log=log, cwd=worktree,
                                                    timeout=timeout))

    def copy_test_files(self, before: str, after: str) -> None:
        for path in self.test_paths:
            target = os.path.join(before, path)
            os.makedirs(os.path.dirname(target), exist_ok=True)
            with open(target, "wb") as f:
                if subprocess.run(["git", "-C", self.top, "show", f"{self.sha}:{path}"], stdout=f, env=dict(self.env)).returncode:
                    raise Died(f"cannot copy {path}")
            if os.access(os.path.join(after, path), os.X_OK):
                os.chmod(target, os.stat(target).st_mode | 0o111)

    def locally(self) -> int:
        base = self.env.get("KINOWO_FAIL_BEFORE_DIR") or os.path.join(os.path.expanduser("~"), ".cache", "kinowo-fail-before")
        try:
            os.makedirs(base, exist_ok=True)
            work = tempfile.mkdtemp(prefix="run.", dir=base)
        except OSError as e:
            raise Died(f"cannot create {base}") from e
        before, after = os.path.join(work, "before"), os.path.join(work, "after")
        try:
            for worktree, rev, side in ((before, self.parent, "BEFORE"), (after, self.sha, "AFTER")):
                if not self.git.ok("worktree", "add", "-q", "--detach", worktree, rev, quiet=True):
                    raise Died(f"cannot make the {side} worktree")
            self.copy_test_files(before, after)
            return self.prove(work, before, after)
        finally:
            self.clean_up(work, (before, after))

    def prove(self, work: str, before: str, after: str) -> int:
        print(f"fail-before: {self.sha} ({len(self.test_paths)} test-side file(s) copied onto {self.short(self.parent)})",
              flush=True)
        logs = (os.path.join(work, "before.log"), os.path.join(work, "after.log"))
        seed = self.seed()
        before_rc = self.run_in(before, logs[0], seed)
        # a BEFORE run that hung gives no verdict whatever AFTER does: not another timeout's worth of sbt to learn it
        after_rc = before_rc if before_rc == tool_process.TIMED_OUT else self.run_in(after, logs[1], seed)
        if tool_process.TIMED_OUT in (before_rc, after_rc):
            print(f"\n--- BEFORE (exit {before_rc}) ---\n{last_lines(logs[0])}", end="")
            if os.path.exists(logs[1]):  # not run when BEFORE already had no verdict
                print(f"\n--- AFTER (exit {after_rc}) ---\n{last_lines(logs[1])}", end="", flush=True)
            raise Died(f"a run timed out after {self.env.get('KINOWO_FAIL_BEFORE_TIMEOUT', '3000')}s (exit 124): no verdict "
                       "— a hang is not a failure")
        return verdict(before_rc, after_rc, logs[0], logs[1])

    def clean_up(self, work: str, worktrees: Sequence[str]) -> None:
        for worktree in worktrees:
            stop = os.path.join(worktree, "scripts", "sbt-warm")
            if "sbt-warm" in self.command_line and os.access(stop, os.X_OK):
                subprocess.run([stop, "--stop"], cwd=worktree, env=dict(self.env), capture_output=True)
            self.git.ok("worktree", "remove", "--force", worktree, quiet=True)
        self.git.ok("worktree", "prune", quiet=True)
        shutil.rmtree(work, ignore_errors=True)

    def run(self) -> int:
        return self.on_ci() if self.route() == "ci" else self.locally()


def prove(argv: Sequence[str], environ: Mapping[str, str]) -> int:
    env = {k: v for k, v in environ.items() if not k.startswith("GIT_")}
    options = parse(argv, env.get("KINOWO_FAIL_BEFORE_ROUTE", "auto"))
    if options.help:
        print(f"Purpose: {PURPOSE}\n\n{__doc__}")
        return 0
    runner = tool_git.shared_runner()
    git = tool_git.Git(runner, env, ".")
    top = git.out("rev-parse", "--show-toplevel", quiet=True)
    if top is None:
        raise Died("not inside a git checkout")
    git = tool_git.Git(runner, env, top.strip())
    sha = git.out("rev-parse", "--verify", "-q", f"{options.commit}^{{commit}}", quiet=True)
    if sha is None:
        raise Died(f"no such commit: {options.commit}")
    parent = git.out("rev-parse", "--verify", "-q", f"{sha.strip()}^", quiet=True)
    if parent is None:
        raise Died(f"{options.commit} has no parent")
    return Proof(runner, env, top.strip(), sha.strip(), parent.strip(), options).run()


def main(argv: Sequence[str], environ: Mapping[str, str]) -> int:
    tool_cli.raise_on_signals()
    try:
        return prove(argv, environ)
    except Died as e:
        print(f"fail-before: {e}", file=sys.stderr)
        if e.see_help:
            print("fail-before: see fail-before --help", file=sys.stderr)
        return 2
    except tool_cli.Interrupted as e:
        return 128 + e.signum


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], os.environ))
