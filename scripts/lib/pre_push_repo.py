"""THE PRE-PUSH HOOK'S RUN, its two ends (scripts/lib/pre_push_run.py composes them): `Repo`, the checkout the run
reads -- its git, its commands, where they run and with what environment -- and `Say`, the hook's own lines
(`▸ pre-push ...` on stdout, `! pre-push ...` on stderr, as the shell hook printed them: agentctl land reads them),
`Terminal` writing them to the hook's two streams."""
from __future__ import annotations

from dataclasses import dataclass
from typing import IO, List, MutableMapping, Optional, Protocol, Sequence, Tuple

import machine
from tool_files import Files

LOG = "\033[36m▸ pre-push\033[0m "
WARN = "\033[33m! pre-push\033[0m "
ALARM = "\033[31m! pre-push\033[0m "


class Say(Protocol):
    """The hook's lines: `log` (`▸ pre-push`, stdout unless `to_err`), `warn` / `alarm` (`! pre-push`, stderr), `raw`
    text, and `flush` before a command writes to the same streams."""

    def log(self, text: str, to_err: bool = False) -> None: ...

    def warn(self, text: str) -> None: ...

    def alarm(self, text: str) -> None: ...

    def raw(self, text: str, to_err: bool = True) -> None: ...

    def flush(self) -> None: ...


@dataclass
class Terminal:
    """The hook's lines on its streams, each written whole and flushed (a check's own output goes between)."""
    out: IO[str]
    err: IO[str]

    def log(self, text: str, to_err: bool = False) -> None:
        self.raw(LOG + text + "\n", to_err)

    def warn(self, text: str) -> None:
        self.raw(WARN + text + "\n", True)

    def alarm(self, text: str) -> None:
        self.raw(ALARM + text + "\n", True)

    def raw(self, text: str, to_err: bool = True) -> None:
        stream = self.err if to_err else self.out
        stream.write(text)
        stream.flush()

    def flush(self) -> None:
        self.out.flush()
        self.err.flush()


@dataclass
class Repo:
    """The checkout the run is in: commands run at `cwd` (its root, then the checked tree) with `env`."""
    cwd: str
    env: MutableMapping[str, str]
    files: Files  # its files, by absolute path or relative to its root

    def git(self, *args: str, text_in: Optional[str] = None) -> Tuple[int, str]:
        return machine.output(["git", *args], text_in, cwd=self.cwd, env=self.env)

    def git_out(self, *args: str) -> str:
        """git's stdout, empty when it fails."""
        code, out = self.git(*args)
        return out if code == 0 else ""

    def lines(self, *args: str) -> List[str]:
        return [line for line in self.git_out(*args).split("\n") if line]

    def output(self, argv: Sequence[str], text_in: Optional[str] = None, timeout: Optional[float] = None,
               quiet: bool = False) -> Tuple[int, str]:
        """(status, stdout) of a command, its stderr this process's (dropped when `quiet`)."""
        return machine.captured(argv, text_in, self.cwd, self.env, timeout, quiet)

    def status(self, argv: Sequence[str], timeout: Optional[float] = None) -> int:
        """A command's status, its stdout and stderr this process's."""
        return machine.status(argv, cwd=self.cwd, env=self.env, timeout=timeout)

    def trailers(self, key: str, revisions: str) -> List[str]:
        """The values of `key` trailers on the commits of `revisions`, one a line."""
        return self.lines("log", f"--format=%(trailers:key={key},valueonly,unfold)", revisions)
