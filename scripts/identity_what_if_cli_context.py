"""What every part of identity-what-if (scripts/identity_what_if_cli.py) shares: the options, the environment it runs
in and hands on, the injected runner, clock and sleeper, its log (`[identity-what-if +Ns] ...`, the seconds since it
started) and its HARNESS ERROR, the files it keys its sides by (TOOL_FILES), and the calls it makes of the shell
library it still shares with the other identity tools (scripts/identity-tools.sh, through scripts/lib/tool_shell.py),
of git and of scripts/identity_what_if.py."""
from __future__ import annotations

import hashlib
import os
import sys
from typing import Callable, Dict, List, Optional, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
import tool_process  # noqa: E402
import tool_python  # noqa: E402
import tool_shell  # noqa: E402
from identity_kept_jvms import alive as alive  # noqa: E402 (identity_what_if_cli_mirror's, _sides')
from identity_what_if_cli_options import Options  # noqa: E402

TOOL = "identity-what-if"
REPO = os.path.dirname(HERE)
IDENTITY_TOOLS = "scripts/identity-tools.sh"
# the files the kept sides are keyed by, dropped into every ref's tree (identity-tools.sh's identity_tree): this
# checkout's, so a ref from before the tool works
TOOL_FILES = (
    "worker/src/test/scala/services/identity/OlderRefs.scala", "worker/src/test/scala/services/identity/WhatIfResolve.scala",
    "worker/src/test/scala/services/identity/WhatIfAnswers.scala", "worker/src/test/scala/scripts/StageAnswers.scala",
    "worker/src/fixtures/scala/tools/MemoLookups.scala", "worker/src/fixtures/scala/tools/OlderRefTypes.scala",
    "worker/src/fixtures/scala/services/identity/vocabulary/OlderRefsPackage.scala",
    "worker/src/fixtures/scala/services/identity/measures/OlderRefsPackage.scala",
    "worker/src/fixtures/scala/tools/KeptJvm.scala", "worker/src/test/scala/services/identity/MirrorWorld.scala",
    "worker/src/fixtures/scala/services/identityadapters/OlderRefsPackage.scala")
# the what-if's own code: with the tool files, the tooling hash a published verdict is keyed by
OWN_FILES = (
    "scripts/identity-what-if", "scripts/identity_what_if.py", IDENTITY_TOOLS, "scripts/identity_what_if_cli.py",
    "scripts/identity_what_if_cli_bisect.py", "scripts/identity_what_if_cli_context.py",
    "scripts/identity_what_if_cli_country.py", "scripts/identity_what_if_cli_flow.py",
    "scripts/identity_what_if_cli_mirror.py", "scripts/identity_what_if_cli_options.py",
    "scripts/identity_what_if_cli_resolve.py", "scripts/identity_what_if_cli_sides.py",
    "scripts/identity_what_if_cli_verdict.py")


class Exit(BaseException):
    """The tool ends here with this status, its clean-up (the head's compile waited for, the fences lingering) run."""

    def __init__(self, code: int) -> None:
        super().__init__(code)
        self.code = code


def tools_hash(repo: str, files: Sequence[str]) -> str:
    """identity-tools.sh's identity_tools_hash: each file's name and contents (nothing for a missing one), 12 hex."""
    digest = hashlib.sha1()
    for name in files:
        digest.update(name.encode() + b"\n")
        try:
            with open(os.path.join(repo, name), "rb") as f:
                digest.update(f.read())
        except OSError:
            pass
    return digest.hexdigest()[:12]


def lines_of(path: str) -> List[str]:
    """The file's lines as `read` and `cut` see them: split at newlines only (a key's \\x1e is no line end)."""
    with open(path, newline="") as f:
        text = f.read()
    lines = text.split("\n")
    return lines[:-1] if lines and lines[-1] == "" else lines


def write(path: str, text: str) -> None:
    with open(path, "w", newline="") as f:
        f.write(text)


class Context:
    def __init__(self, o: Options, env: Dict[str, str], runner: tool_process.Runner, clock: Callable[[], float],
                 sleep: Callable[[float], None]) -> None:
        self.o, self.env, self.runner, self.clock, self.sleep = o, env, runner, clock, sleep
        self.repo = REPO
        self.cache = env.get("KINOWO_IDENTITY_CACHE") or os.path.join(env.get("HOME", ""), ".cache", "kinowo-identity")
        self.started = self.seconds()
        self.pid = os.getpid()
        self.cc = ""  # the country being resolved, as the shell tool's `$cc` named it in its messages

    def seconds(self) -> int:
        """bash's SECONDS: whole seconds of the wall clock."""
        return int(self.clock())

    def elapsed(self) -> int:
        return self.seconds() - self.started

    def log(self, line: str) -> None:
        print(f"[{TOOL} +{self.elapsed()}s] {line}", file=sys.stderr, flush=True)

    def harness_error(self, line: str) -> None:
        self.log(f"HARNESS ERROR: {line}")
        print(f"HARNESS ERROR: {line}", file=sys.stderr, flush=True)

    def knob(self, name: str, default: str) -> str:
        return self.env.get(name) or default

    def prelude(self) -> str:
        """Sourced identity-tools.sh logging as this tool does: its name and its seconds."""
        return (f'IDENTITY_TOOL={TOOL}; SECONDS={self.elapsed()}; '
                f'identity_log() {{ echo "[{TOOL} +${{SECONDS}}s] $*" >&2; }}')

    def shell(self, function: str, args: Sequence[str]) -> tool_process.Captured:
        """An identity-tools.sh function: its stdout the answer, its stderr passed on."""
        return tool_shell.shell_function(self.runner, self.env, IDENTITY_TOOLS, function, args, prelude=self.prelude())

    def shell_logged(self, function: str, args: Sequence[str], log: str) -> int:
        """An identity-tools.sh function, its stdout and stderr into `log`."""
        return self.runner.run(self.shell_command(function, args, log))

    def shell_command(self, function: str, args: Sequence[str], log: Optional[str] = None) -> tool_process.Command:
        return tool_shell.shell_command(self.env, IDENTITY_TOOLS, function, args, prelude=self.prelude(), log=log)

    def capture(self, argv: Sequence[str], cwd: Optional[str] = None) -> tool_process.Captured:
        """`$(command)`: its stdout, its stderr passed on."""
        return tool_process.capture_loudly(self.runner, tool_process.Command(argv, self.env, cwd=cwd))

    def git(self, *args: str) -> tool_process.Captured:
        return self.capture(["git", "-C", self.repo, *args])

    def commit_of(self, ref: str) -> Optional[str]:
        done = self.git("rev-parse", "--verify", f"{ref}^{{commit}}")
        return done.stdout.strip() if done.code == 0 else None

    def clean_tree(self) -> bool:
        """No uncommitted edit to a tracked file."""
        return self.git("status", "--porcelain", "--untracked-files=no").stdout.strip() == ""

    def what_if_py(self, args: Sequence[str]) -> List[str]:
        return tool_python.isolated(os.path.join(self.repo, "scripts", "identity_what_if.py"), *args)
