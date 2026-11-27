"""THE MACHINE: the processes, clock and environment the laptop tools touch, in one adapter (the design rules table's
`adapter` row; CLAUDE.md, "Design rules": every other module receives them). scripts/sbt-warm's modules, the JVM gate
(jvm_gate.py, jvm_memory.py, cpu_meter.py) and the stall watchdog (stall_watch.py) read the process table and start,
signal and wait for processes only through it; their tests hand in fakes where they need to.

  environment()                    the process's own environment (a tool's main() hands it on)
  monotonic() / wall() / sleep(s)  the clock
  output(argv) / status(argv)      a command's stdout (and status) / its status, stdout and stderr as asked
  captured(argv)                   a command's status and stdout, its stderr this process's
  errors(argv)                     a command's status and its stderr
  spawn(argv)                      a command started in the background (a Popen)
  send(pid, signal) / alive(pid)   a signal to a process (or its group) / whether it exists
  own_pid() / waited(proc, s)      this process's pid / whether a spawned command ended within s seconds
  replace(argv)                    this process becomes the command (exec)
  spawn_group(argv, env, tty)      a command leading a process group of its own, handed the terminal (tool_bounded)
  piped(producer, consumer, how)   one command's stdout the next one's stdin (`a | b`): the consumer's status
  ps(...), tree_pids, cpu_centis, any_runnable, any_gone, children_flag   the process table, through `ps` on the PATH
  tree_shrank, kept_pids, pid_list, centis                                pure readings of its listings"""
from __future__ import annotations

import os
import signal
import subprocess
import time
from dataclasses import dataclass
from typing import IO, Dict, List, MutableMapping, NoReturn, Optional, Sequence, Tuple, Union

Sink = Union[int, IO[str], IO[bytes], None]
QUIET: Sink = subprocess.DEVNULL  # a Sink dropping what is written to it
Process = subprocess.Popen         # what spawn() starts: `machine.Process[bytes]`


def environment() -> MutableMapping[str, str]:
    return os.environ


def monotonic() -> float:
    return time.monotonic()


def wall() -> float:
    return time.time()


def sleep(seconds: float) -> None:
    time.sleep(seconds)


def local_text(form: str, epoch: float) -> str:
    return time.strftime(form, time.localtime(epoch))


def output(argv: Sequence[str], text_in: Optional[str] = None, cwd: Optional[str] = None,
           env: Optional[MutableMapping[str, str]] = None) -> Tuple[int, str]:
    """(status, stdout) of a command, its stderr dropped; 127 and nothing when it cannot be run."""
    return run(argv, Ran(cwd, env, subprocess.PIPE, subprocess.DEVNULL), text_in)


def captured(argv: Sequence[str], text_in: Optional[str] = None, cwd: Optional[str] = None,
             env: Optional[MutableMapping[str, str]] = None, timeout: Optional[float] = None,
             quiet: bool = False) -> Tuple[int, str]:
    """(status, stdout) of a command, its stderr this process's (a shell's `$(command)`; dropped when `quiet`); 124 past
    `timeout`, 127 and nothing when it cannot be run."""
    return run(argv, Ran(cwd, env, subprocess.PIPE, subprocess.DEVNULL if quiet else None, (), timeout), text_in)


def errors(argv: Sequence[str], cwd: Optional[str] = None, env: Optional[MutableMapping[str, str]] = None,
           out: Sink = subprocess.DEVNULL) -> Tuple[int, str]:
    """(status, stderr) of a command, its stdout dropped (or where `out` says: None, this process's); 127 and nothing
    when it cannot be run."""
    try:
        done = subprocess.run(list(argv), cwd=cwd, env=env, stdin=subprocess.DEVNULL, stdout=out,
                              stderr=subprocess.PIPE, text=True)
    except OSError:
        return 127, ""
    return done.returncode, done.stderr


def status(argv: Sequence[str], cwd: Optional[str] = None, env: Optional[MutableMapping[str, str]] = None,
           out: Sink = None, err: Sink = None, pass_fds: Sequence[int] = (), timeout: Optional[float] = None) -> int:
    """A command's status, its stdin closed, stdout and stderr where asked (this process's own by default); 124 past
    `timeout`, 127 when it cannot be run."""
    return run(argv, Ran(cwd, env, out, err, tuple(pass_fds), timeout))[0]


@dataclass(frozen=True)
class Ran:
    """How a command runs: where, with what environment, its stdout and stderr, the descriptors it keeps, its bound."""
    cwd: Optional[str] = None
    env: Optional[MutableMapping[str, str]] = None
    out: Sink = None
    err: Sink = None
    pass_fds: Tuple[int, ...] = ()
    timeout: Optional[float] = None


def run(argv: Sequence[str], how: Ran, text_in: Optional[str] = None) -> Tuple[int, str]:
    """(status, stdout when piped) of a command: 124 past its bound, 127 when it cannot be run."""
    try:
        done = subprocess.run(list(argv), input=text_in, text=True, cwd=how.cwd, env=how.env, stdout=how.out,
                              stderr=how.err, stdin=None if text_in is not None else subprocess.DEVNULL,
                              pass_fds=how.pass_fds, timeout=how.timeout)
    except subprocess.TimeoutExpired:
        return 124, ""
    except OSError:
        return 127, ""
    return done.returncode, done.stdout or ""


def spawn(argv: Sequence[str], how: Ran = Ran(), own_group: bool = False) -> "subprocess.Popen[bytes]":
    """A command started in the background, its stdin closed; in a process group of its own with `own_group`."""
    return subprocess.Popen(list(argv), cwd=how.cwd, env=how.env, stdin=subprocess.DEVNULL, stdout=how.out,
                            stderr=how.err, pass_fds=how.pass_fds, preexec_fn=os.setpgrp if own_group else None)


def spawn_group(argv: Sequence[str], env: MutableMapping[str, str], tty: Optional[int]) -> "subprocess.Popen[bytes]":
    """A command leading a process group of its own, this process's descriptors (a held ticket on 7 among them) and
    stdin passed on; with `tty` (the terminal it reads, whose foreground this process holds), its group handed that
    foreground before it runs, so one touching the terminal (stty, a prompt) is not stopped."""
    def lead() -> None:
        os.setpgrp()
        if tty is not None:
            signal.signal(signal.SIGTTOU, signal.SIG_IGN)
            try:
                os.tcsetpgrp(tty, os.getpid())
            except OSError:
                pass
            signal.signal(signal.SIGTTOU, signal.SIG_DFL)
    return subprocess.Popen(list(argv), env=dict(env), close_fds=False, preexec_fn=lead)


def piped(producer: Sequence[str], consumer: Sequence[str], how: Ran = Ran()) -> int:
    """`producer | consumer`: the consumer's status, its stdout and stderr where `how` says, the producer's stderr
    this process's; 127 when either cannot be run."""
    try:
        left = subprocess.Popen(list(producer), cwd=how.cwd, env=how.env, stdin=subprocess.DEVNULL,
                                stdout=subprocess.PIPE)
    except OSError:
        return 127
    try:
        right = subprocess.Popen(list(consumer), cwd=how.cwd, env=how.env, stdin=left.stdout, stdout=how.out,
                                 stderr=how.err)
    except OSError:
        left.kill()
        left.wait()
        return 127
    if left.stdout is not None:
        left.stdout.close()
    code = right.wait()
    left.wait()
    return code


def send(pid: int, signum: int, group: bool = False) -> bool:
    """Whether `signum` reached `pid` (its process group with `group`)."""
    try:
        if group:
            os.killpg(pid, signum)
        else:
            os.kill(pid, signum)
    except OSError:
        return False
    return True


def own_pid() -> int:
    return os.getpid()


def waited(proc: "subprocess.Popen[bytes]", seconds: Optional[float]) -> bool:
    """Whether `proc` ended within `seconds` (None: however long it takes)."""
    try:
        proc.wait(seconds)
    except subprocess.TimeoutExpired:
        return False
    return True


def alive(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def replace(argv: Sequence[str]) -> NoReturn:
    os.execvp(argv[0], list(argv))


def ps(*args: str) -> str:
    """ps's stdout, empty when it says nothing or cannot be run."""
    return output(["ps", *args])[1]


def pid_list(pids: Sequence[int]) -> str:
    return ",".join(str(p) for p in pids)


def tree_pids(root: int) -> List[int]:
    """`root` and every descendant of it, breadth first."""
    kids: Dict[int, List[int]] = {}
    for line in ps("-axo", "pid=,ppid=").splitlines():
        fields = line.split()
        if len(fields) == 2 and fields[0].isdigit() and fields[1].isdigit():
            kids.setdefault(int(fields[1]), []).append(int(fields[0]))
    queue = [root]
    for pid in queue:
        queue.extend(kids.get(pid, []))
    return queue


def centis(clock: str) -> int:
    """ps's `time` -- [[dd-]hh:]mm:ss[.cc] -- in centiseconds."""
    days, _, rest = clock.rpartition("-")
    seconds = 0.0
    for part in rest.split(":"):
        seconds = seconds * 60 + float(part)
    return round((seconds + (int(days) * 86400 if days else 0)) * 100)


def children_flag() -> List[str]:
    """ps's flag that adds the CPU of a process's exited (reaped) children to its own: `-S` (BSD, procps), else none."""
    return ["-S"] if output(["ps", "-S", "-o", "time=", "-p", str(os.getpid())])[0] == 0 else []


def cpu_centis(pids: Sequence[int], flag: Sequence[str]) -> int:
    """The CPU the live `pids` have used, summed, with their exited children's where ps can add it."""
    if not pids:
        return 0
    return sum(centis(line.strip()) for line in ps(*flag, "-o", "time=", "-p", pid_list(pids)).splitlines()
               if line.strip())


def any_runnable(pids: Sequence[int]) -> bool:
    """Whether one of the live `pids` is runnable (ps state R): running, or ready and waiting for a CPU."""
    return bool(pids) and any(line.startswith("R") for line in ps("-o", "stat=", "-p", pid_list(pids)).splitlines())


def any_gone(pids: Sequence[int]) -> bool:
    """Whether one of `pids` has ended (gone, or a zombie not yet waited for)."""
    if not pids:
        return False
    states = [line for line in ps("-o", "stat=", "-p", pid_list(pids)).splitlines() if not line.startswith("Z")]
    return len(states) < len(pids)


def tree_shrank(before: Sequence[int], after: Sequence[int]) -> bool:
    """Whether the tree SHRANK between two polls: a pid of `before` is not in `after`, and every pid of `after` was in
    `before` (sbt-warm's client watch counts it as progress: a wind-down; a tree only replacing its processes is none)."""
    return set(after) <= set(before) and bool(set(before) - set(after))


def kept_pids(before: Sequence[int], after: Sequence[int]) -> List[int]:
    """The pids of `after` that were in `before` too."""
    earlier = set(before)
    return [p for p in after if p in earlier]
