"""What every part of identity-slice (scripts/identity_slice_cli.py) shares: the options, the environment it runs in and
hands on, the injected runner, its log (`[identity-slice] ...`), the files it drops into every ref (TOOL_FILES:
scripts/identity-slice-tool-files.txt), and the calls it makes of the shell library it shares with the other identity
tools (scripts/identity-tools.sh, through scripts/lib/tool_shell.py: a ref laid out, a tree compiled, a recording or a
capture fetched, a JVM run under a JVM slot) and of git."""
from __future__ import annotations

import dataclasses
import os
import sys
import time
from typing import Callable, Dict, List, Optional, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
import tool_process  # noqa: E402
import tool_shell  # noqa: E402
from identity_slice_cli_options import TOOL, Options  # noqa: E402

REPO = os.path.dirname(HERE)
IDENTITY_TOOLS = "scripts/identity-tools.sh"
LABEL = "identity-slice"  # what the JVM gate and the governor name this tool's JVMs by


def read_tool_files(repo: str) -> List[str]:
    """The files dropped into every ref, in the order the cache keys hash them."""
    with open(os.path.join(repo, "scripts/identity-slice-tool-files.txt")) as f:
        return [line.rstrip("\n") for line in f if line.rstrip("\n") and not line.startswith("#")]


TOOL_FILES = read_tool_files(REPO)


class Exit(BaseException):
    """The tool ends here with this status, its work directory removed."""

    def __init__(self, code: int) -> None:
        super().__init__(code)
        self.code = code


def say(line: str) -> None:
    """A line on stderr, as `echo ... >&2`."""
    print(line, file=sys.stderr, flush=True)


def cat(path: str) -> None:
    """The file onto stdout, as `cat`."""
    sys.stdout.flush()
    with open(path, "rb") as f:
        sys.stdout.buffer.write(f.read())
    sys.stdout.buffer.flush()


class Context:
    def __init__(self, o: Options, env: Dict[str, str], runner: tool_process.Runner,
                 clock: Callable[[], float] = time.time, sleep: Callable[[float], None] = time.sleep) -> None:
        self.o, self.runner, self.repo, self.clock, self.sleep = o, runner, REPO, clock, sleep
        self.env = dict(env, IDENTITY_FILLS=o.fills) if o.fills else env
        self.cache = env.get("KINOWO_IDENTITY_CACHE") or os.path.join(env.get("HOME", ""), ".cache", "kinowo-identity")
        self.work = ""
        self.recording = ""
        self.mirror_args: List[str] = []

    @staticmethod
    def log(line: str) -> None:
        say(f"[{TOOL}] {line}")

    def knob(self, name: str, default: str) -> str:
        return self.env.get(name) or default

    def command(self, function: str, args: Sequence[str], library: str = IDENTITY_TOOLS) -> tool_process.Command:
        return tool_shell.shell_command(self.env, library, function, args, prelude=f"IDENTITY_TOOL={TOOL}")

    def shell(self, function: str, args: Sequence[str], library: str = IDENTITY_TOOLS) -> tool_process.Captured:
        """`$(function args)`: its stdout the answer, its stderr this tool's as it comes."""
        return self.runner.capture(dataclasses.replace(self.command(function, args, library), stderr_through=True))

    def shell_run(self, function: str, args: Sequence[str]) -> int:
        return self.runner.run(self.command(function, args))

    def capture(self, argv: Sequence[str], timeout: Optional[float] = None) -> tool_process.Captured:
        return self.runner.capture(tool_process.Command(argv, self.env, timeout=timeout, stderr_through=True))

    def run(self, argv: Sequence[str], stdout: str = "-") -> int:
        """The command, its stdout into the file `stdout` (`-`: this tool's)."""
        if stdout != "-":
            argv = ["sh", "-c", 'out=$1; shift; exec "$@" > "$out"', "sh", stdout, *argv]
        return self.runner.run(tool_process.Command(argv, self.env))

    def to_stderr(self, argv: Sequence[str]) -> int:
        """The command, its stdout onto this tool's stderr (`>&2`)."""
        return self.runner.run(tool_process.Command(["sh", "-c", 'exec "$@" >&2', "sh", *argv], self.env))

    def tail(self, n: int, *paths: str) -> None:
        """`tail -n <n> <paths...> >&2`: each file's last lines, headed by its name when there are two."""
        self.to_stderr(["tail", "-n", str(n), *paths])

    def java(self, tree: str, stdout: str, stderr: str, args: Sequence[str]) -> int:
        """IDENTITY_JAVA in <tree> under a JVM slot of its own (identity_java_slotted_to): `-` this tool's stream."""
        return self.shell_run("identity_java_slotted_to", [LABEL, tree, stdout, stderr, *args])

    def git(self, *args: str) -> tool_process.Captured:
        return self.capture(["git", "-C", self.repo, *args])

    def need_ref(self, ref: str) -> bool:
        """Whether the clone holds <ref>: one its history does not reach -- a shallow clone's -- is said by name, never
        decided as some other commit's code."""
        if self.runner.run(tool_process.Command(["sh", "-c", 'exec git -C "$1" rev-parse --verify -q "$2^{commit}" '
                                                 '> /dev/null', "sh", self.repo, ref], self.env)) == 0:
            return True
        shallow = self.git("rev-parse", "--is-shallow-repository").stdout.strip() == "true"
        say(f"no commit {ref} in this SHALLOW clone: git fetch --unshallow" if shallow else f"no commit {ref}")
        return False

    def tree_of(self, ref: str) -> str:
        """This checkout for `.`, else the ref's laid-out tree with this checkout's tool files dropped in; exit 1."""
        if ref == ".":
            return self.repo
        if not self.need_ref(ref):
            raise Exit(1)
        done = self.shell("identity_tree", [ref, *TOOL_FILES])
        if done.code != 0:
            raise Exit(1)
        return done.stdout.rstrip("\n")

    def classpath(self, tree: str) -> Optional[str]:
        """The tree's worker test classpath, compiled once (identity_classpath); None when it does not compile."""
        done = self.shell("identity_classpath", [tree])
        return done.stdout.rstrip("\n") if done.code == 0 else None

    def answer(self, function: str, *args: str) -> str:
        """`x=$(function args)` under errexit: its answer, or the tool ends with its status."""
        done = self.shell(function, args)
        if done.code != 0:
            raise Exit(done.code)
        return done.stdout.rstrip("\n")

    def recording_of(self, run: str, cc: str) -> str:
        return self.answer("identity_recording", run, cc)

    def path(self, name: str) -> str:
        """A file of this call's work directory."""
        return os.path.join(self.work, name)
