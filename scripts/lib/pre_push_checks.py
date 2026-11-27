"""THE CHECKS, RUN (scripts/hooks/pre-push's header: NO STEP HANGS, THE KNOWN-RED BASELINE). Every check is Python's
(pre_push_registry): `Processes` starts each as `scripts/pre_push.py <hook> --check <name> <state>`, a process of its
own at the checked tree after the run's state as JSON (the changed paths, ranges, shas, temp dir, ...), so the
watchdog's marks and the known-red output capture work for it. Each runs under the stall watchdog
(scripts/stall-watch.sh) and is said as the shell hook said it: `▸ pre-push <c> ok (Ns)`, `! pre-push <c> FAILED
(Ns)`, `pre-push STALLED: <c> ...` (agentctl land parses them)."""
from __future__ import annotations

import json
import os
from dataclasses import dataclass, field
from typing import Dict, List, Mapping, MutableMapping, Optional, Protocol, Sequence, Union

import machine
import pre_push_known_red as known_red
import pre_push_registry as registry
import tool_python
from pre_push_repo import Say, into
from tool_files import Files

Value = Union[str, List[str]]  # a state value: a string, or a list of them
UNKNOWN = 127  # a check no module holds: a command not found, as the shell hook's missing function was


def no_state() -> Dict[str, Value]:
    return {}


def no_names() -> List[str]:
    return []


class Starter(Protocol):
    """What the check loop calls: a check by name, its status."""
    state: Dict[str, Value]

    def start(self, check: str, env: Optional[Mapping[str, str]] = None, output: Optional[str] = None) -> int: ...


@dataclass
class Processes:
    """Each check (`names`: pre_push_registry.CHECKS) a `scripts/pre_push.py <hook> --check <name> <state>` process at
    `cwd` with `env`, after `state` written as JSON to `state_file`."""
    hook: str
    files: Files
    env: MutableMapping[str, str]
    cwd: str
    state_file: str
    names: Sequence[str] = tuple(registry.CHECKS)
    state: Dict[str, Value] = field(default_factory=no_state)

    def start(self, check: str, env: Optional[Mapping[str, str]] = None, output: Optional[str] = None) -> int:
        """The check's status; its stdout and stderr this process's, or both into `output`."""
        if check not in self.names:
            return UNKNOWN
        self.files.replace_text(self.state_file, json.dumps(self.state))
        return self.started(self.argv(check, output), dict(self.env, **(env or {})))

    def argv(self, check: str, output: Optional[str]) -> List[str]:
        script = f"{os.path.dirname(self.hook)}/../pre_push.py"
        argv = tool_python.isolated(script, self.hook, "--check", check, self.state_file)
        return into(output, argv) if output else list(argv)

    def started(self, argv: Sequence[str], env: MutableMapping[str, str]) -> int:
        return machine.status(argv, cwd=self.cwd, env=env)


@dataclass
class Loop:
    """What the check loop needs beside the checks."""
    checks: Starter
    say: Say
    files: Files
    cwd: str
    env: MutableMapping[str, str]
    tmp: str
    stall_watch: str
    stall_seconds: str
    baseline: str = ""         # the known-red baseline's text, "" for none
    base_commit: str = ""
    temp_roots: List[str] = field(default_factory=no_names)
    failed: List[str] = field(default_factory=no_names)
    stalled: List[str] = field(default_factory=no_names)
    known: List[str] = field(default_factory=no_names)


def watchdog(loop: Loop, check: str, stall: str, progress: str) -> Optional["machine.Process[bytes]"]:
    if not loop.files.is_file(loop.stall_watch):
        return None
    return machine.spawn(["bash", loop.stall_watch, "watch", str(machine.own_pid()), loop.stall_seconds, check,
                          stall, progress, loop.tmp], machine.Ran(loop.cwd, loop.env, None, machine.QUIET))


def stop_watchdog(dog: "machine.Process[bytes]") -> None:
    """TERM, again every second while it lives (one TERM sent within microseconds of the fork can be lost), KILL
    after ten."""
    for _ in range(10):
        machine.send(dog.pid, 15)
        if machine.waited(dog, 1.0):
            return
    machine.send(dog.pid, 9)
    machine.waited(dog, None)


def run_one(loop: Loop, check: str) -> None:
    started = machine.monotonic()
    stall, progress = f"{loop.tmp}/stalled.{check}", f"{loop.tmp}/progress.{check}"
    marks = {"KINOWO_PROGRESS_FILE": progress, "KINOWO_STALL_FILE": stall}
    dog = watchdog(loop, check, stall, progress)
    output = f"{loop.tmp}/output.{check}" if loop.baseline else None
    loop.say.flush()
    code = loop.checks.start(check, env=marks, output=output)
    if output:
        loop.say.raw(loop.files.read_text_or(output, ""))
    took = int(machine.monotonic() - started)
    if code == 0:
        loop.say.log(f"{check} ok ({took}s)")
    elif loop.files.read_text_or(stall, ""):
        said = ";".join(loop.files.read_text_or(stall, "").rstrip("\n").split("\n"))
        loop.say.raw(f"pre-push STALLED: {check} (no progress for {loop.stall_seconds}s: {said}; ended after "
                     f"{took}s)\n")
        loop.failed.append(check)
        loop.stalled.append(check)
    else:
        failed(loop, check, took, output)
    if dog is not None:
        stop_watchdog(dog)


def failed(loop: Loop, check: str, took: int, output: Optional[str]) -> None:
    loop.say.warn(f"{check} FAILED ({took}s)")
    text = loop.files.read_text_or(output, "") if output else ""
    if loop.baseline and known_red.known_red(check, text, loop.baseline, loop.temp_roots):
        loop.say.warn(f"KNOWN RED: {check} fails the same way on the base {loop.base_commit[:10]} (the known-red "
                      "baseline): not held against this push")
        loop.known.append(check)
    else:
        loop.failed.append(check)


def run_all(loop: Loop, checks: Sequence[str], skipped: Sequence[str]) -> None:
    for check in checks:
        if check in skipped:
            loop.say.warn(f"{check} SKIPPED (PREPUSH_SKIP)")
            continue
        run_one(loop, check)
