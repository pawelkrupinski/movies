"""THE BLACK-BOX TEST HARNESS for the repository's command-line tools, in Python: a tool is driven through its command
line in a subprocess -- arguments, environment, stdin, a scratch working tree -- with FAKE binaries (gh, git, sbt,
curl...) first on its PATH, and the test asserts on its stdout, stderr, exit status, the files it wrote and the calls
the fakes logged. Nothing about the tool's language is assumed: ONE test file runs unchanged against a shell tool and
against its Python rewrite at the same command line, which is the shell-to-Python migration's equivalence proof
(/Users/pawel/Documents/shell-to-python-migration.md, section 5). The shell counterpart is scripts/shell-spec.sh.

    class MyToolTest(cli_harness.ToolCase):
        TOOL = cli_harness.tool_path("scripts/ci/my_tool.py", "scripts/ci/my-tool.sh")

        def test_it_says_hello(self):
            self.fakes.script("gh", 'echo "$*" >&2; echo answered')     # a shell body, its argv as "$@"
            self.fakes.program("sbt", "out(' '.join(argv))\\nexit(3)")  # a Python body
            done = self.run_tool("hello", env={"GH_TOKEN": "x"})
            self.assertEqual(0, done.code, done.output)
            self.assertEqual([["api", "user"]], self.fakes.calls("gh"))

What it isolates, as shell-spec.sh and scratch-git.sh do for the shell tests: no GIT_* variable and no user or system
git config reach the tool (a fixed identity is supplied, unsigned commits), the governor and the JVM gate are off, no
GITHUB_* variable of a CI step running the test leaks into the tool, and every path the tool and its fakes write lies
under the case's own scratch directory, removed at its end. A run is bounded (`HANG_BOUND`, the tool's whole process
group ended past it, exit 124): a tool that hangs fails its case instead of wedging the run."""
from __future__ import annotations

import glob
import json
import os
import shlex
import shutil
import signal
import subprocess
import sys
import tempfile
import time
import unittest
from dataclasses import dataclass
from typing import Callable, Dict, List, Mapping, Optional, Sequence, Tuple

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))
HANG_BOUND = float(os.environ.get("SPEC_HANG_BOUND", "600"))
WAIT_BOUND = float(os.environ.get("SPEC_WAIT_BOUND", "120"))
# What a test's tool never inherits from the run around it (run-script-tests drops the same for its tests)
DROPPED_PREFIXES = ("GIT_", "GITHUB_", "RUNNER_", "KINOWO_GOV_", "PREPUSH_", "AGENTCTL_")
ISOLATED = {
    "GIT_CONFIG_NOSYSTEM": "1", "GIT_CONFIG_GLOBAL": os.devnull, "GIT_CONFIG_COUNT": "8",
    "GIT_CONFIG_KEY_0": "user.name", "GIT_CONFIG_VALUE_0": "Spec",
    "GIT_CONFIG_KEY_1": "user.email", "GIT_CONFIG_VALUE_1": "spec@example.test",
    "GIT_CONFIG_KEY_2": "commit.gpgsign", "GIT_CONFIG_VALUE_2": "false",
    "GIT_CONFIG_KEY_3": "tag.gpgsign", "GIT_CONFIG_VALUE_3": "false",
    "GIT_CONFIG_KEY_4": "init.defaultBranch", "GIT_CONFIG_VALUE_4": "main",
    "GIT_CONFIG_KEY_5": "safe.directory", "GIT_CONFIG_VALUE_5": "*",
    # no detached `git maintenance run --auto` after a commit: it takes objects/maintenance.lock after the case ended
    "GIT_CONFIG_KEY_6": "maintenance.auto", "GIT_CONFIG_VALUE_6": "false",
    "GIT_CONFIG_KEY_7": "gc.auto", "GIT_CONFIG_VALUE_7": "0",
    # the JVM gate and the governor are the real Mac's: off in every case (shell-spec.sh)
    "KINOWO_MEMORY_GATE": "0", "KINOWO_JVM_SLOTS": "0", "KINOWO_JVM_CPUS": "0", "KINOWO_GOVERNOR": "0",
    "GH_DOWNLOAD_PUBLIC": "0", "KINOWO_JVM_GATE_FOLLOW_MAIN": "0", "PYTHONDONTWRITEBYTECODE": "1",
}


def tool_path(python: str, shell: Optional[str] = None) -> str:
    """The tool under test, absolute: its Python version once it exists, else its shell one -- so a test written
    before the migration runs unchanged after it. CLI_TOOL_UNDER_TEST names another copy outright (a frozen shell
    copy, a workflow's inline block written out as a script, a mutant)."""
    if os.environ.get("CLI_TOOL_UNDER_TEST"):
        return os.path.abspath(os.environ["CLI_TOOL_UNDER_TEST"])
    for candidate in (python, shell):
        if candidate and os.path.exists(os.path.join(ROOT, candidate)):
            return os.path.join(ROOT, candidate)
    return os.path.join(ROOT, python)


def isolated_env(base: Mapping[str, str], extra: Optional[Mapping[str, Optional[str]]] = None) -> Dict[str, str]:
    """`base` without what a case must not inherit, plus the isolation above, plus `extra` (a None value unsets)."""
    env = {k: v for k, v in base.items() if not k.startswith(DROPPED_PREFIXES)}
    env.update(ISOLATED)
    for key, value in (extra or {}).items():
        if value is None:
            env.pop(key, None)
        else:
            env[key] = value
    return env


@dataclass(frozen=True)
class Done:
    code: int
    stdout: str
    stderr: str

    @property
    def output(self) -> str:
        """stdout then stderr: what a failing assertion shows."""
        return self.stdout + self.stderr


PRELUDE = """#!{python} -B
import json, os, sys
argv = sys.argv[1:]
env = os.environ
with open({calls!r}, "a") as _log:
    _log.write(json.dumps({{"name": {name!r}, "argv": argv, "cwd": os.getcwd()}}) + "\\n")
def out(text, end="\\n"):
    sys.stdout.write(text + end)
def err(text, end="\\n"):
    sys.stderr.write(text + end)
"""
SHELL_PRELUDE = """#!{shell}
printf '%s\\n' "$(python3 -c 'import json, os, sys; print(json.dumps({{"name": sys.argv[1], "argv": sys.argv[2:], "cwd": os.getcwd()}}))' {name} "$@")" >> {calls}
"""


class Fakes:
    """Fake binaries in a directory put first on the tool's PATH, each logging its call (argv, cwd) to one file."""

    def __init__(self, directory: str) -> None:
        self.bin = os.path.join(directory, "bin")
        self.log = os.path.join(directory, "calls.jsonl")
        os.makedirs(self.bin, exist_ok=True)
        open(self.log, "a").close()

    def _write(self, name: str, text: str) -> str:
        path = os.path.join(self.bin, name)
        with open(path, "w") as f:
            f.write(text)
        os.chmod(path, 0o755)
        return path

    def program(self, name: str, body: str) -> str:
        """A fake whose behaviour is Python: `argv`, `env`, `out(text)`, `err(text)` and `exit(code)` in scope."""
        return self._write(name, PRELUDE.format(python=sys.executable, calls=self.log, name=name) + body + "\n")

    def script(self, name: str, body: str, shell: str = "/bin/sh") -> str:
        """A fake whose behaviour is a shell body (POSIX sh, or `shell`: "/usr/bin/env bash"), its arguments as "$@"."""
        prelude = SHELL_PRELUDE.format(shell=shell, name=shlex.quote(name), calls=shlex.quote(self.log))
        return self._write(name, prelude + body + "\n")

    def calls(self, name: Optional[str] = None) -> List[List[str]]:
        """The argv of each call (of `name`'s, or every fake's), in call order."""
        with open(self.log) as f:
            entries = [json.loads(line) for line in f if line.strip()]
        return [e["argv"] for e in entries if name is None or e["name"] == name]

    def clear(self) -> None:
        open(self.log, "w").close()


def run_bounded(argv: Sequence[str], *, env: Mapping[str, str], cwd: str, stdin: Optional[str],
                bound: float = HANG_BOUND) -> Done:
    """`argv` in a process group of its own; past `bound` seconds the whole group is ended, exit 124 (`bounded`)."""
    proc = subprocess.Popen(list(argv), env=dict(env), cwd=cwd, text=True, start_new_session=True,
                            stdin=subprocess.PIPE if stdin is not None else subprocess.DEVNULL,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    try:
        out, err = proc.communicate(stdin, timeout=bound)
        return Done(proc.returncode, out, err)
    except subprocess.TimeoutExpired:
        os.killpg(proc.pid, signal.SIGKILL)
        out, err = proc.communicate()
        return Done(124, out, err + f"\n(cli_harness: ended past its {bound:g} s bound)\n")


def still_running(pid_file: str, grace: float = 10.0) -> int:
    """How many of the pids `pid_file` lists are alive after up to `grace` seconds each; each is then killed, so a
    failing case strands nothing (shell-spec.sh's spec_still_running)."""
    if not os.path.exists(pid_file):
        return 0
    left = 0
    with open(pid_file) as f:
        pids = [int(line) for line in f if line.strip()]
    for pid in pids:
        if wait_until(lambda: not alive(pid), grace):
            continue
        left += 1
        try:
            os.kill(pid, signal.SIGKILL)
        except OSError:
            pass
    return left


def alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except OSError:
        return False
    return True


def wait_until(condition: Callable[[], bool], bound: float = WAIT_BOUND) -> bool:
    """Whether `condition` came to hold within `bound` seconds, polled with a growing pause (never a guessed sleep)."""
    deadline, pause = time.monotonic() + bound, 0.01
    while not condition():
        if time.monotonic() > deadline:
            return False
        time.sleep(pause)
        pause = min(pause * 2, 0.5)
    return True


# A metered ticket's busy tree, sized so that its reading tells measured CPU from a width however loaded the runner:
# three children each burn BUSY_CPU_SECONDS / 3 of CPU, then the tree idles IDLE_SECONDS. The CPU the meter must
# report is at least BUSY_CPU_SECONDS (CPU time does not stretch under load) and below the ticket's wall, while a
# width-1 reading IS the wall. A cpu / wall ratio does stretch: 0.2 on a starved macOS runner (Tool self-tests #634).
BUSY_CPU_SECONDS = 3
IDLE_SECONDS = 3.0


def burn_then_idle() -> None:
    burn = f"import time; t = time.process_time()\nwhile time.process_time() - t < {BUSY_CPU_SECONDS / 3}: pass"
    for worker in [subprocess.Popen([sys.executable, "-c", burn]) for _ in range(3)]:
        worker.wait()
    time.sleep(IDLE_SECONDS)


def usage_seconds(usage: Sequence[str]) -> Tuple[int, int]:
    """A `gov usage` call's (--cpu, --wall) seconds."""
    return int(usage[usage.index("--cpu") + 1]), int(usage[usage.index("--wall") + 1])


def scratch_git(repo: str, *args: str) -> str:
    """git in a scratch repository, isolated as a tool's is (scratch-git.sh's scratch_git), its stdout stripped: for
    what a case class shares (a repository made once in setUpClass) as for ToolCase.git."""
    if (os.path.realpath(repo) + os.sep).startswith(ROOT + os.sep):
        raise AssertionError(f"refusing {repo}: it is inside this checkout, not a scratch repository")
    done = subprocess.run(["git", "-C", repo, *args], env=isolated_env(os.environ), check=True,
                          capture_output=True, text=True)
    return done.stdout.strip()


class ToolCase(unittest.TestCase):
    """A case with a scratch directory (`self.work`), fakes first on the PATH (`self.fakes`) and `run_tool`."""
    TOOL = ""

    def setUp(self) -> None:
        self._dir = tempfile.TemporaryDirectory(prefix="cli-case-")
        self.work = os.path.realpath(self._dir.name)
        self.fakes = Fakes(os.path.join(self.work, ".fakes"))
        # the FIRST cleanup, so the last to run: unittest runs tearDown before any cleanup, and a subclass's cleanups
        # end the processes that write here (SbtWarmCase's keepers: ENOTEMPTY on state/, Tool self-tests #634)
        self.addCleanup(self._dir.cleanup)

    def path(self, *parts: str) -> str:
        return os.path.join(self.work, *parts)

    def write(self, relative: str, text: str) -> str:
        """`text` into `relative` under the scratch directory (parents made); its absolute path."""
        full = self.path(relative)
        os.makedirs(os.path.dirname(full), exist_ok=True)
        with open(full, "w") as f:
            f.write(text)
        return full

    def read(self, relative: str) -> str:
        with open(self.path(relative)) as f:
            return f.read()

    def env(self, extra: Optional[Mapping[str, Optional[str]]] = None) -> Dict[str, str]:
        env = isolated_env(os.environ, extra)
        env["PATH"] = self.fakes.bin + os.pathsep + env.get("PATH", "")
        env.setdefault("TMPDIR", self.work)
        return env

    def run_tool(self, *args: str, env: Optional[Mapping[str, Optional[str]]] = None, cwd: Optional[str] = None,
                 stdin: Optional[str] = None, tool: Optional[str] = None, bound: float = HANG_BOUND) -> Done:
        """The tool (TOOL, or `tool`) with `args`, as its callers run it: executed by path, from `cwd` (the scratch
        directory by default), stdin closed unless given."""
        return run_bounded([tool or self.TOOL, *args], env=self.env(env), cwd=cwd or self.work, stdin=stdin,
                           bound=bound)

    def git(self, repo: str, *args: str) -> str:
        """git in a scratch repository, isolated as the tool's is (scratch-git.sh's scratch_git)."""
        return scratch_git(repo, *args)

    def repo(self, relative: str, *init_args: str) -> str:
        full = self.path(relative)
        os.makedirs(full, exist_ok=True)
        self.git(full, "init", "-q", *init_args)
        return full

    def commit_tree(self, repo: str, message: str) -> str:
        """Every change in the scratch repository committed: the commit's sha."""
        self.git(repo, "add", "-A")
        self.git(repo, "commit", "-q", "-m", message)
        return self.git(repo, "rev-parse", "HEAD")

    def copy_scripts(self, relative: str) -> None:
        """This checkout's scripts/*.py and *.sh, and scripts/ci's and scripts/lib's, into <relative>/scripts/: what a
        tool copied into a scratch repository runs from there."""
        scripts = os.path.join(ROOT, "scripts")
        for sub in ("", "ci", "lib"):
            os.makedirs(self.path(relative, "scripts", sub), exist_ok=True)
            for f in glob.glob(os.path.join(scripts, sub, "*.py")) + glob.glob(os.path.join(scripts, sub, "*.sh")):
                shutil.copy2(f, self.path(relative, "scripts", sub))

    def hold_lock(self, lock: str) -> "subprocess.Popen[bytes]":
        """Another process holding `lock`'s kernel lock, as scripts/jvm-gate.sh's jvm_lock takes it (shell-spec.sh's
        spec_lock_holder) -- another agent's tool mid-download, as a case sees it -- until the case ends."""
        os.makedirs(os.path.dirname(lock), exist_ok=True)
        holder = subprocess.Popen(["bash", "-c", '. "$1"; jvm_lock "$2" || exit 1; exec sleep 600', "_",
                                   os.path.join(ROOT, "scripts/jvm-gate.sh"), lock], env=isolated_env(os.environ),
                                  stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, start_new_session=True)
        self.addCleanup(lambda: (holder.kill(), holder.wait()))

        def held() -> bool:
            try:
                with open(lock) as f:
                    return f.read().strip() == str(holder.pid)
            except OSError:
                return False
        if not wait_until(held):
            raise AssertionError(f"the lock holder never took {lock}")
        return holder

    def origin_and_checkout(self) -> Tuple[str, str]:
        """A bare `origin.git` and a `repo` whose `origin` it is, both on main: the pair a tool pushing to origin
        runs in."""
        origin = self.repo("origin.git", "--bare", "-b", "main")
        checkout = self.repo("repo", "-b", "main")
        self.git(checkout, "remote", "add", "origin", origin)
        return origin, checkout
