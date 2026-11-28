"""The touched-code tree: what scripts/mutation-check and scripts/coverage-touched share
THE TOUCHED-CODE TREE: what scripts/mutation-check and scripts/coverage-touched share -- a throwaway detached worktree
at the range's head, its kept sbt server, the methods the range touches and the specs that reach them.

  main(tool, argv, usage, flags, measure, check)
       the tool's whole run: its command line read (a range <base>..<head>, --out DIR, and `flags` with their
       defaults; -h/--help prints `usage`; `check` may refuse it; a usage error exits 2), the tree opened, `measure(tree, line)` run in it and
       the tree closed again -- also when an INT or TERM ends the run (exit 130) or a step fails (exit 1, its line
       `<tool>: <why>` on stderr).
  TouchedTree.open (by main)
       resolves the range in `repo` (this checkout; $KINOWO_TOUCHED_REPO names another: the tests' seam) -- base, head:
       full shas --, `tools` being the checkout these scripts are in, makes `out` (--out's directory, else a temporary
       one) and the worktree `tree` at the head under $KINOWO_TOUCHED_TREE_DIR (default ~/.cache/kinowo-touched-tree),
       removed -- its sbt server stopped first -- by `close`. The caller's checkout is never touched: the instrumented
       or mutated compile goes to the worktree's own target/. Its first compile is seeded from the caller's build
       (KINOWO_SBT_WARM_SEED, as scripts/fail-before seeds). The caller's tooling measures it: its
       project/refactor-tools.sbt (the plugins), refactor-tools.sbt (`touchedSpecs`) and the project/ sources that task
       reads are laid over the worktree's build, and its scripts drive it -- so an older range is measured as the
       current tools measure (a head whose project/TestImpact.scala predates `TestImpact.Unit` is out of reach).
  tree.sbt(log, commands)
       the commands in the worktree's kept sbt server (the caller's scripts/sbt-warm, run there; $KINOWO_TOUCHED_SBT
       names another: the tests' seam), output to <log>; its exit status
  tree.governed(job, cores, mem, log, commands)
       the same under a scripts/gov-run ticket of its own (`python3 touched_tree.py sbt <dir> <log> <command>...`
       inside it, which execs the sbt)
  tree.methods()
       out/paths.txt (the head's changed production sources) and out/touched.tsv (the touched methods:
       scripts/touched_methods.py over quality.MethodSpans, run in the CALLER's kept server over the worktree's
       sources: the quality module reads them as text); False when the range touches no production method
  tree.specs()
       out/specs.txt: `<module> <spec class>` for every spec that reaches a touched method (`touchedSpecs`,
       project/TouchedSpecs.scala: a spec of the method's source's classes, or of a production class that depends on
       it and uses the method's name -- two steps by name, not the test-impact selection's every transitive dependent)

The sbt server of the worktree holds a JVM slot for its life (stopped at exit); every sbt command takes its own
governor ticket (scripts/sbt-warm), and the callers run the heavy step under a scripts/gov-run ticket of its own.
"""
from __future__ import annotations

import os
import re
import shutil
import signal
import sys
import tempfile
import time
from dataclasses import dataclass, field
from types import FrameType
from typing import Callable, Dict, List, NoReturn, Optional, Sequence, Tuple

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.realpath(__file__))))  # run as a script: scripts/lib is not there
from cli import tool_cli  # noqa: E402
from processes import tool_python  # noqa: E402
from tool_process import Command, Runner, SubprocessRunner, capture_loudly  # noqa: E402

TOOLS = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
# what the caller's tooling lays over the worktree's build: the plugins, the touchedSpecs task and what it reads
OVERLAY = ("project/refactor-tools.sbt", "refactor-tools.sbt", "project/AnalysisUnits.scala",
           "project/TouchedSpecs.scala", "project/UsedNamesAccess.scala")
# an sbt step runs in a session of its own under this bound, so an INT or TERM to the tool ends it (tool_process)
SBT_BOUND = 12 * 3600.0
STOP_BOUND = 300.0


class Failed(Exception):
    """A step that failed: exit 1, `<tool>: <message>` on stderr."""


@dataclass
class CommandLine:
    flags: Dict[str, str]
    range: str = ""

    @property
    def keep(self) -> str:
        return self.flags["--out"]


def read_command_line(argv: List[str], usage: str, defaults: Dict[str, str]) -> Optional[CommandLine]:
    """The range, --out and the tool's `defaults` flags; None when --help printed `usage`."""
    line = tool_cli.read_options(CommandLine({**defaults, "--out": ""}), argv, usage, _word)
    if line is not None and not line.range:
        raise tool_cli.UsageError("a range <base>..<head>")
    return line


def _word(line: CommandLine, argv: List[str], i: int) -> int:
    word = argv[i]
    if word in line.flags:
        line.flags[word] = tool_cli.value_at(argv, i)
        return i + 1
    if word.startswith("-"):
        raise tool_cli.UsageError(f"unknown flag {word}")
    if line.range:
        raise tool_cli.UsageError(f"one range, not '{line.range}' and '{word}'")
    line.range = word
    return i


def split_range(text: str) -> Tuple[str, str]:
    base, dots, head = text.partition("..")
    if not dots or not base or not head:
        raise tool_cli.UsageError(f"a range <base>..<head>, not '{text}'")
    return base, head


def _interrupted(_signum: int, _frame: Optional[FrameType]) -> NoReturn:
    raise SystemExit(130)


def main(tool: str, argv: List[str], usage: str, defaults: Dict[str, str],
         measure: Callable[["TouchedTree", CommandLine], int],
         check: Callable[[CommandLine], object] = lambda line: line) -> int:
    for sig in (signal.SIGINT, signal.SIGTERM):
        signal.signal(sig, _interrupted)
    runner = SubprocessRunner()  # after the handlers: it passes a signal on to them once its command has ended
    try:
        line = read_command_line(argv, usage, defaults)
        if line is None:
            return 0
        check(line)
        tree = TouchedTree(tool, runner, dict(os.environ))
        try:
            tree.open(line.range, line.keep)
            return measure(tree, line)
        finally:
            tree.close()
    except tool_cli.UsageError as error:
        return tool_cli.usage_failed(tool, error)
    except Failed as failure:
        print(f"{tool}: {failure}", file=sys.stderr)
        return 1


@dataclass
class TouchedTree:
    tool: str
    runner: Runner
    env: Dict[str, str]
    tools: str = TOOLS
    repo: str = ""
    base: str = ""
    head: str = ""
    out: str = ""
    tree: str = ""
    out_temporary: bool = False
    started: float = field(default_factory=time.monotonic)

    def open(self, range_text: str, keep: str) -> None:
        base, head = split_range(range_text)
        self.repo = self.env.get("KINOWO_TOUCHED_REPO") or self.tools
        self.base, self.head = self.commit(base), self.commit(head)
        self.out = self._out(keep)
        directory = self.env.get("KINOWO_TOUCHED_TREE_DIR") or os.path.expanduser("~/.cache/kinowo-touched-tree")
        self._made(lambda: os.makedirs(directory, exist_ok=True), directory)
        tree = self._made(lambda: tempfile.mkdtemp(prefix=f"{self.tool}.", dir=directory), "a worktree directory")
        os.rmdir(tree)
        self.tree = tree
        if self.git("worktree", "add", "-q", "--detach", tree, self.head) is None:
            raise Failed(f"cannot make a worktree at {self.head}")
        self._overlay()
        # the caller's build is the nearest seed when it is at the head; sbt-warm's own choice (the newest main build)
        if not self.env.get("KINOWO_SBT_WARM_SEED") and self.git("rev-parse", "HEAD") == self.head:
            self.env["KINOWO_SBT_WARM_SEED"] = self.repo
        self.started = time.monotonic()

    def commit(self, ref: str) -> str:
        sha = self.git("rev-parse", "--verify", "-q", f"{ref}^{{commit}}")
        if sha is None:
            raise tool_cli.UsageError(f"no commit {ref}")
        return sha

    def _out(self, keep: str) -> str:
        if keep:
            self._made(lambda: os.makedirs(keep, exist_ok=True), keep)
            return os.path.abspath(keep)
        self.out_temporary = True
        temporary = self.env.get("TMPDIR") or "/tmp"
        return self._made(lambda: tempfile.mkdtemp(prefix=f"{self.tool}.", dir=temporary), "a work directory")

    @staticmethod
    def _made(make: Callable[[], object], what: str) -> str:
        try:
            return str(make())
        except OSError:
            raise Failed(f"cannot create {what}") from None

    def _overlay(self) -> None:
        for relative in OVERLAY:
            try:
                os.makedirs(os.path.dirname(os.path.join(self.tree, relative)), exist_ok=True)
                shutil.copy(os.path.join(self.tools, relative), os.path.join(self.tree, relative))
            except OSError:
                raise Failed(f"cannot lay {relative} over the worktree") from None

    def close(self) -> None:
        if self.tree and os.path.isdir(self.tree):
            if not self.env.get("KINOWO_TOUCHED_SBT"):
                self.runner.run(Command([self.script("sbt-warm"), "--stop"], self.env, log=os.devnull,
                                        cwd=self.tree, closed_stdin=True, timeout=STOP_BOUND))
            self.git("worktree", "remove", "--force", self.tree)
            self.git("worktree", "prune")
        if self.out_temporary and self.out:
            shutil.rmtree(self.out, ignore_errors=True)

    def git(self, *args: str) -> Optional[str]:
        """git in the repository: its stdout stripped, None when it failed."""
        done = self.runner.capture(Command(["git", "-C", self.repo, *args], self.env))
        return done.stdout.strip() if done.code == 0 else None

    def script(self, name: str) -> str:
        return os.path.join(self.tools, "scripts", name)

    def at(self, name: str) -> str:
        """A file of the work directory."""
        return os.path.join(self.out, name)

    # ── sbt ─────────────────────────────────────────────────────────────────────────────────────────────────────

    def sbt(self, log: str, commands: Sequence[str], directory: Optional[str] = None) -> int:
        return run_sbt(self.runner, self.env, directory or self.tree, log, commands)

    def governed(self, job: str, cores: int, mem: int, log: str, commands: Sequence[str]) -> int:
        """`sbt(log, commands)` under a governor ticket of its own (scripts/gov-run --job `job`)."""
        argv = [self.script("gov-run"), "--job", job, "--cores", str(cores), "--mem", str(mem), "--",
                sys.executable, "-B", os.path.abspath(__file__), "sbt", self.tree, log, *commands]
        return self.runner.run(Command(argv, self.env, timeout=SBT_BOUND))

    def fail_with_log(self, log: str, lines: int, message: str) -> NoReturn:
        """The log's last `lines` lines on stderr, then Failed(message)."""
        try:
            with open(log, errors="replace") as f:
                sys.stderr.write("".join(f.readlines()[-lines:]))
        except OSError:
            pass
        raise Failed(message)

    # ── the touched methods and the specs reaching them ───────────────────────────────────────────────────────

    def touched_methods(self, why: str, *args: str) -> str:
        """scripts/touched_methods.py with `args`: its stdout (its stderr passed on); Failed(why) when it failed."""
        done = capture_loudly(self.runner, Command(tool_python.isolated(self.script("touched_methods.py"), *args), self.env))
        if done.code != 0:
            sys.stdout.write(done.stdout)
            raise Failed(why)
        return done.stdout

    def methods(self) -> bool:
        paths = self.touched_methods("cannot read the range's changed sources", "paths", self.base, self.head,
                                     "--repo", self.repo)
        _write(self.at("paths.txt"), paths)
        if not paths:
            return False
        command = f"quality/runMain quality.MethodSpans --root {self.tree} --out {self.at('spans.tsv')} " + \
            " ".join(paths.split())
        if self.sbt(self.at("spans.log"), [command], self.tools) != 0:
            self.fail_with_log(self.at("spans.log"), 20, f"quality.MethodSpans failed (log: {self.at('spans.log')})")
        touched = self.touched_methods("cannot meet the range's hunks with the method spans", "touched", self.base,
                                       self.head, "--spans", self.at("spans.tsv"), "--repo", self.repo)
        _write(self.at("touched.tsv"), touched)
        return bool(touched)

    def specs(self) -> None:
        log = self.at("specs.log")
        if self.sbt(log, [f"touchedSpecs {self.at('touched.tsv')} {self.at('specs.txt')}"]) != 0:
            self.fail_with_log(log, 20, f"the reaching specs' selection failed (log: {log})")

    def touched_rows(self) -> List[List[str]]:
        return [row.split("\t") for row in _lines(self.at("touched.tsv"))]

    def modules(self) -> List[str]:
        """The modules of the touched methods, sorted."""
        return sorted({row[0].split("/")[0] for row in self.touched_rows()})

    def reaching_specs(self) -> List[Tuple[str, str]]:
        """(module, spec class) of every spec reaching a touched method, in specs.txt's order."""
        return [(words[0], words[1] if len(words) > 1 else "") for words in map(str.split, _lines(self.at("specs.txt")))]

    def finish(self, report: str, notes: Sequence[str]) -> int:
        """The report, the notes and the run's time printed, and kept as report.md."""
        text = report + "".join(f"{note}\n" for note in notes) + \
            f"{self.tool}: {int(time.monotonic() - self.started)} s\n"
        sys.stdout.write(text)
        sys.stdout.flush()
        _write(self.at("report.md"), text)
        return 0


def run_sbt(runner: Runner, env: Dict[str, str], directory: str, log: str, commands: Sequence[str]) -> int:
    """`commands` in the kept sbt server of `directory` (the caller's scripts/sbt-warm, or $KINOWO_TOUCHED_SBT)."""
    return runner.run(Command([sbt_of(env), *commands], sbt_env(env), log=log, cwd=directory, timeout=SBT_BOUND))


def exec_sbt(directory: str, log: str, commands: Sequence[str]) -> NoReturn:
    """run_sbt's command in place of this process (inside a governor ticket: `touched_tree.py sbt ...`)."""
    sink = os.open(log, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o644)
    os.dup2(sink, 1)
    os.dup2(sink, 2)
    os.chdir(directory)
    env = sbt_env(dict(os.environ))
    os.execve(sbt_of(env), [sbt_of(env), *commands], env)


def sbt_of(env: Dict[str, str]) -> str:
    return env.get("KINOWO_TOUCHED_SBT") or os.path.join(TOOLS, "scripts", "sbt-warm")


def sbt_env(env: Dict[str, str]) -> Dict[str, str]:
    return {**env, "KINOWO_SBT_WARM_ROUTE": "local"}


def quoted(items: Sequence[str]) -> str:
    """`"a", "b"`: a Scala Seq's elements."""
    return ", ".join(f'"{item}"' for item in items)


def is_number(text: str, pattern: str = r"[0-9]+") -> bool:
    return re.fullmatch(pattern, text) is not None


def _lines(path: str) -> List[str]:
    with open(path) as f:
        return [line.rstrip("\n") for line in f if line.strip()]


def _write(path: str, text: str) -> None:
    with open(path, "w") as f:
        f.write(text)


if __name__ == "__main__":
    # inside a governor ticket: python3 touched_tree.py sbt <dir> <log> <command>...
    if len(sys.argv) < 5 or sys.argv[1] != "sbt":
        sys.exit("usage: touched_tree.py sbt <dir> <log> <command>...")
    exec_sbt(sys.argv[2], sys.argv[3], sys.argv[4:])
