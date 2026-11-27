"""THE CHECKS, RUN (scripts/hooks/pre-push's header: NO STEP HANGS, THE KNOWN-RED BASELINE). Each check is still the
hook's own shell function `check_<name>`: the hook, SOURCED (not run), defines them, and `Shell` calls one in a bash of
its own after the run's state (the changed paths, ranges, shas, temp dir, ...) as shell variables -- what the checks read
as the one shell's globals before. Each runs under the stall watchdog (scripts/stall-watch.sh) and is said as the shell
hook said it: `▸ pre-push <c> ok (Ns)`, `! pre-push <c> FAILED (Ns)`, `pre-push STALLED: <c> ...` (agentctl land
parses them)."""
from __future__ import annotations

import shlex
from dataclasses import dataclass, field
from typing import Dict, List, Mapping, MutableMapping, Optional, Protocol, Sequence, Union

import machine
import pre_push_known_red as known_red
from pre_push_repo import Say
from tool_files import Files

# the hook's state file, then the hook (sourced: it defines the functions), then the function asked, with its arguments
CALL = '. "$1" || exit 1; . "$2" || exit 1; shift 2; "$@"'
CALL_INTO = '. "$1" || exit 1; . "$2" || exit 1; shift 2; "$@" > "$PREPUSH_CHECK_OUTPUT" 2>&1'


Value = Union[str, List[str]]  # a shell variable's: a string, or an array


def shell_value(value: Value) -> str:
    if isinstance(value, list):
        return "(" + " ".join(shlex.quote(v) for v in value) + ")"
    return shlex.quote(value)


def no_state() -> Dict[str, Value]:
    return {}


def no_names() -> List[str]:
    return []


class Functions(Protocol):
    """The hook's checks and gate steps by name: what the run calls (the hook's shell functions today, `Shell`; each
    migration stage moves some into Python behind the same call)."""
    state: Dict[str, Value]

    def call(self, function: str, *args: str, env: Optional[Mapping[str, str]] = None,
             output: Optional[str] = None) -> int: ...


@dataclass
class Shell:
    """The hook's shell functions, each called in a bash of its own at `cwd` with `env`, after `state`."""
    hook: str
    files: Files
    env: MutableMapping[str, str]
    cwd: str
    state_file: str
    state: Dict[str, Value] = field(default_factory=no_state)

    def write_state(self) -> None:
        self.files.replace_text(self.state_file, "".join(
            f"{name}={shell_value(value)}\n" for name, value in self.state.items()))

    def call(self, function: str, *args: str, env: Optional[Mapping[str, str]] = None,
             output: Optional[str] = None) -> int:
        """`function args...`'s status; its stdout and stderr this process's, or both into `output`."""
        self.write_state()
        run_env = dict(self.env, **(env or {}))
        if output:
            run_env["PREPUSH_CHECK_OUTPUT"] = output
        return machine.status(["bash", "-c", CALL_INTO if output else CALL, "pre-push", self.state_file, self.hook,
                               function, *args], cwd=self.cwd, env=run_env)


@dataclass
class Loop:
    """What the check loop needs beside the checks."""
    shell: Functions
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
    code = loop.shell.call(f"check_{check}", env=marks, output=output)
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
