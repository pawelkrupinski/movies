"""THE SCALAFIX TREE: what scripts/scalafix-move and scripts/scalafix-organize share -- a worktree KEPT between runs
(detached, at the caller's HEAD) whose build compiles with SemanticDB on, so a second rewrite recompiles only what
changed since the first, and the caller's own checkout never pays the full recompile that turning SemanticDB on costs
(every module's scalacOptions change).

  ScalafixTree.opened(tool)
       refuses a caller's checkout with uncommitted changes to tracked files (the rewrite is of HEAD, and its patch is
       laid back over the checkout); `repo` (this checkout; $KINOWO_SCALAFIX_REPO names another: the tests' seam),
       `head` (its HEAD), `out` (a work directory, removed by `close`) and `tree`: $KINOWO_SCALAFIX_TREE, default
       ~/.cache/kinowo-scalafix/<checkout's name>, made the first time (seeded from the caller's build, as
       scripts/fail-before seeds: KINOWO_SBT_WARM_SEED), else moved to `head` (`checkout --detach -f`, untracked files
       cleaned, ignored ones such as target/ kept)
  .sbt(log, commands)
       the commands in the tree's kept sbt server, under a governor ticket (scripts/gov-run --job scalafix), SemanticDB
       on first (`set ThisBuild / semanticdbEnabled := true`) and sbt-scalafix's cache off (it skips a source it once
       rewrote even after the tree is moved back to HEAD; $KINOWO_SCALAFIX_SBT names another sbt: the tests' seam); its
       output into `log`, its exit status returned
  .apply()
       the tree's changes against `head` as one patch (<out>/rewrite.patch, renames kept), laid over the caller's
       checkout and its index (`git apply --index`); False when there was nothing to lay
  commands(arguments)
       one `<module>/<config>/scalafix <arguments>` command a configuration: every module's main, test, integration,
       page and fixtures sources (the configurations refactor-tools.sbt gives scalafix besides Compile and Test)

A step that fails raises `Failed` (its message what the tool says before exiting 1)."""
from __future__ import annotations

import os
import shutil
import signal
import tempfile
from types import FrameType
from typing import Dict, List, Mapping, Optional, Sequence

import tool_git
import tool_process

TOOLS = os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))
# DEPENDENTS FIRST: sbt-scalafix compiles each configuration before rewriting it, so a configuration is rewritten only
# after everything that depends on it -- a rewritten dependency (the moved class's own source) would leave a later
# dependent uncompilable against it mid-run.
CONFIGURATIONS = (
    "quality/Test", "quality/Compile", "e2e/Test", "web/PageTest", "web/IntegrationTest", "web/Test", "web/Compile",
    "worker/IntegrationTest", "worker/Test", "worker/Fixtures", "worker/Compile", "repochecks/Test",
    "repochecks/Fixtures", "scrapers/Test", "scrapers/Fixtures", "scrapers/Compile", "itkit/Compile", "testkit/Test",
    "testkit/Compile", "identity/Test", "identity/Compile", "common/Test", "common/Compile", "http/Test",
    "http/Fixtures", "http/Compile", "base/Test", "base/Compile", "geodata/Test", "geodata/Compile")
SEMANTICDB = ("set ThisBuild / semanticdbEnabled := true", "set ThisBuild / scalafixCaching := false")
# the governor's ticket is taken by gov-run, whose waits are said on the terminal; only sbt's output goes to the log
INTO_LOG = 'log=$1; shift; exec "$@" > "$log" 2>&1'


class Failed(Exception):
    """A step that failed: its message is the tool's last word, exit 1."""


def commands(arguments: str) -> List[str]:
    return [f"{c}/scalafix {arguments}" for c in CONFIGURATIONS]


def log_lines(path: str) -> List[str]:
    """The lines of a log (each with its newline); none when it was never written."""
    try:
        with open(path, errors="replace") as f:
            return f.readlines()
    except OSError:
        return []


def ended(signum: int, _frame: Optional[FrameType]) -> None:
    raise SystemExit(130)


class ScalafixTree:
    def __init__(self, runner: tool_process.Runner, env: Dict[str, str], repo: str, head: str, out: str,
                 tree: str) -> None:
        self.runner = runner
        self.env = env
        self.repo = repo
        self.head = head
        self.out = out
        self.tree = tree
        self.git_repo = tool_git.Git(runner, env, repo)
        self.git_tree = tool_git.Git(runner, env, tree)

    @classmethod
    def opened(cls, tool: str, runner: tool_process.Runner, environ: Mapping[str, str]) -> "ScalafixTree":
        env = dict(environ)
        repo = env.get("KINOWO_SCALAFIX_REPO") or TOOLS
        git = tool_git.Git(runner, env, repo)
        dirty = (git.out("status", "--porcelain", "--untracked-files=no") or "").rstrip("\n")
        if dirty:
            raise Failed(f"the checkout has uncommitted changes (commit or stash them first: the rewrite is of HEAD):"
                         f"\n{dirty}")
        head = (git.out("rev-parse", "HEAD") or "").strip()
        tree = env.get("KINOWO_SCALAFIX_TREE") or os.path.join(os.path.expanduser("~"), ".cache", "kinowo-scalafix",
                                                               os.path.basename(repo))
        signal.signal(signal.SIGINT, ended)
        signal.signal(signal.SIGTERM, ended)
        out = tempfile.mkdtemp(prefix=f"{tool}.", dir=env.get("TMPDIR") or "/tmp")
        opened = cls(runner, env, repo, head, out, tree)
        try:
            opened.moved_or_made()
        except BaseException:
            opened.close()
            raise
        return opened

    def moved_or_made(self) -> None:
        if self.git_tree.ok("rev-parse", "--git-dir", quiet=True):
            if not self.git_tree.ok("checkout", "-q", "--detach", "-f", self.head):
                raise Failed(f"cannot move the scalafix tree {self.tree} to {self.head}")
            if not self.git_tree.ok("clean", "-fdq"):
                raise Failed(f"cannot clean the scalafix tree {self.tree}")
            return
        parent = os.path.dirname(self.tree)
        try:
            os.makedirs(parent, exist_ok=True)
        except OSError:
            raise Failed(f"cannot create {parent}") from None
        if not self.git_repo.ok("worktree", "add", "-q", "--detach", self.tree, self.head, quiet=True):
            raise Failed(f"cannot make the scalafix tree {self.tree}")
        self.env.setdefault("KINOWO_SBT_WARM_SEED", self.repo)

    def close(self) -> None:
        shutil.rmtree(self.out, ignore_errors=True)

    def path(self, name: str) -> str:
        return os.path.join(self.out, name)

    def sbt(self, log: str, sbt_commands: Sequence[str]) -> int:
        sbt = self.env.get("KINOWO_SCALAFIX_SBT") or os.path.join(TOOLS, "scripts", "sbt-warm")
        argv = [os.path.join(TOOLS, "scripts", "gov-run"), "--job", "scalafix", "--cores", "4", "--mem", "4", "--",
                "/bin/sh", "-c", INTO_LOG, "scalafix-sbt", log, sbt, *SEMANTICDB, *sbt_commands]
        env = {**self.env, "KINOWO_SBT_WARM_ROUTE": "local"}
        return self.runner.run(tool_process.Command(argv, env, cwd=self.tree))

    def discard(self) -> None:
        """The tree's changes to tracked files dropped (`checkout -f` to `head`)."""
        self.git_tree.ok("checkout", "-q", "-f", self.head)

    def reset_hard(self) -> None:
        """The tree's index and changes dropped (`reset --hard` to `head`): a staged move undone too."""
        self.git_tree.ok("reset", "-q", "--hard", self.head)

    def apply(self) -> bool:
        if not self.git_tree.ok("add", "-A"):
            raise Failed("cannot stage the scalafix tree's changes")
        patch = self.path("rewrite.patch")
        if not self.git_tree.ok("diff", "--cached", "--binary", "-M", f"--output={patch}", self.head):
            raise Failed("cannot diff the scalafix tree")
        self.git_tree.ok("reset", "-q")
        if os.path.getsize(patch) == 0:
            return False
        if not self.git_repo.ok("apply", "--index", patch):
            raise Failed(f"the rewrite does not apply to the checkout (patch: {patch})")
        return True

    def staged_stat(self) -> str:
        """The last line of the checkout's `git diff --cached --stat`."""
        lines = (self.git_repo.out("diff", "--cached", "--stat") or "").splitlines()
        return lines[-1] if lines else ""
