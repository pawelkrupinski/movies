#!/usr/bin/env python3
# Purpose: A no-progress bound: a step is ended once it has made no progress for N seconds
"""A NO-PROGRESS bound: a step is ended once it has made no progress for N seconds -- no CPU in its process tree, no
progress mark touched, no log written -- however long it legitimately runs while it does make some. A wall-clock
budget fails a compile under a load of 100; this fails only a step that waits on nothing that moves.

  scripts/stall-watch.sh watch <root pid> <seconds> <label> <stalled file> [<progress file>] [<dir>]
      the watchdog: polls every $KINOWO_STALL_POLL_SECONDS (30) and counts as progress (a) the CPU of <root pid>'s
      descendants (less the watchdog's own, with what their exited children used) growing by >= $KINOWO_STALL_CPU_CENTIS
      (100: one second) in a poll, (b) one of them runnable at the poll (state R: running, or waiting for a CPU), (c)
      <progress file> touched, (d) a file directly in <dir> written (its logs). (b) keeps a busy step alive on a loaded
      machine: under a load of 90 a shell loop forking `date` was granted 5 centiseconds a second, most of them in
      children gone before the poll (2026-10-27: pre-push-test's busy case judged STALLED). A step that waits on nothing
      sleeps. (Processes new since the last poll are no progress: the hook's own helper loops start some every second.)
      <seconds> without any: it appends <label> to
      <stalled file>, prints `STALLED: <label> (no progress for <seconds>s)` on stderr, and ends the descendants (TERM,
      KILL 10 s later). It ends itself when <root pid> does; its caller kills it once the step is done.
      KINOWO_STALL_GROUPS=1: the step runs in process groups of its own, under the root's (run-script-tests: each test
      under tool-conventions.sh's `bounded`): a stall first says what each of those groups was stuck on
      (scripts/stall-report.sh, on stderr), then ends their processes alone, never the root's own group -- whose
      `bounded` then returns the step's status as for any other end.
  . scripts/stall-watch.sh          the helpers the shell tools source: stall_tree_pids <pid>, stall_cpu_centis
                                    [<pid>...], stall_any_runnable [<pid>...], stall_tree_shrank <before> <after>,
                                    stall_any_gone [<pid>...], stall_kept_pids <before> <after>

THE STALL SIGNAL, for any caller (agentctl land among them): a stalled step's tool exits 75 (EX_TEMPFAIL) with a
line `<tool> STALLED: <step> ...` on stderr -- scripts/sbt-warm (`sbt-warm STALLED: <sbt command> ...`) and the
pre-push hook (`pre-push STALLED: <check> ...`). KINOWO_STALL_FILE names a file each appends the stalled step to, and
KINOWO_PROGRESS_FILE one sbt-warm touches while its server makes progress: the work of a detached sbt server is no
CPU of the hook's own process tree, so the hook's watchdog would otherwise see a long compile as a stall.

Why (2026-10-27): a land's pre-push hook sat 2.5 h in `scripts/sbt-warm "common/Test/compile; ..."`, its sbtn client
and the server at 0% CPU, holding the landing lock while five lands queued behind it: the worktree's ONE sbt server was
still running an earlier `web/PageTest/runMain tools.FixtureServerMain` (a serving command, from a page-tests-local
whose shell had died without its clean-up), and sbt runs one command at a time -- the compile queued behind a server
that never ends. Nothing in sbt-warm, the hook or land had a bound.

The watchdog ends at once on TERM, INT or HUP and removes its mark (stall-mark.<pid> in $TMPDIR, the file whose mtime
is the previous poll): a caller ends it the moment a quick step is done, and run-script-tests appends its stderr to the
test's own log, which must stay empty."""
from __future__ import annotations

import os
import signal
import subprocess
import sys
import time
from dataclasses import dataclass
from types import FrameType
from typing import List, Optional, Sequence, Set, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from machine import any_runnable, children_flag, cpu_centis, pid_list, ps, tree_pids  # noqa: E402

USAGE = ("usage: stall-watch.sh watch <root pid> <seconds> <label> <stalled file> [<progress file>] [<dir>]")
REPORT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "stall-report.sh")
KILL_AFTER = 20  # half-second checks between TERM and KILL: ten seconds


def newer_than(path: str, mark_ns: int) -> bool:
    try:
        return os.stat(path).st_mtime_ns > mark_ns
    except OSError:
        return False


def log_written(directory: str, mark_ns: int) -> bool:
    """Whether a file directly in `directory` was written since the mark (a symlink or a subdirectory is none)."""
    try:
        with os.scandir(directory) as entries:
            return any(e.is_file(follow_symlinks=False) and newer_than(e.path, mark_ns) for e in entries)
    except OSError:
        return False


def exists(pid: int) -> bool:
    try:
        os.kill(pid, 0)
    except ProcessLookupError:
        return False
    except PermissionError:
        return True
    return True


def stop(signum: int, frame: Optional[FrameType]) -> None:
    raise SystemExit(0)


@dataclass(frozen=True)
class Watch:
    root: int
    bound: float
    label: str
    stalled: str
    progress: str
    directory: str
    poll: float
    need: int
    groups: bool


def other_groups(root: int, pids: Sequence[int]) -> Tuple[List[int], List[int]]:
    """The groups of `pids` other than the root's own, and the `pids` in them."""
    own = ps("-o", "pgid=", "-p", str(root)).strip()
    groups: Set[int] = set()
    kept: List[int] = []
    for line in ps("-o", "pid=,pgid=", "-p", pid_list(pids)).splitlines():
        fields = line.split()
        if len(fields) == 2 and fields[1] != own:
            kept.append(int(fields[0]))
            groups.add(int(fields[1]))
    return sorted(groups), kept


def end_all(pids: Sequence[int]) -> None:
    """TERM to `pids`, then KILL ten seconds later to those still there."""
    for pid in pids:
        try:
            os.kill(pid, signal.SIGTERM)
        except OSError:
            pass
    left = list(pids)
    for _ in range(KILL_AFTER):
        left = [p for p in left if exists(p)]
        if not left:
            return
        time.sleep(0.5)
    for pid in left:
        try:
            os.kill(pid, signal.SIGKILL)
        except OSError:
            pass


def declare_stalled(w: Watch, quiet: int, pids: List[int]) -> None:
    """Say it, and end the descendants (in groups of their own only, with KINOWO_STALL_GROUPS)."""
    with open(w.stalled, "a") as f:
        f.write(w.label + "\n")
    print(f"STALLED: {w.label} (no progress for {quiet}s: no CPU in its processes, no output, no progress mark)",
          file=sys.stderr, flush=True)
    if w.groups and pids:
        groups, pids = other_groups(w.root, pids)
        for group in groups:
            subprocess.run(["bash", REPORT, str(group)])
    end_all(pids)


def made_progress(w: Watch, pids: Sequence[int], last: int, now: int, mark_ns: int) -> bool:
    """Whether the step moved since the last poll (the first poll, with no baseline yet, counts)."""
    return (last < 0 or now - last >= w.need or any_runnable(pids)
            or bool(w.progress and newer_than(w.progress, mark_ns))
            or bool(w.directory and log_written(w.directory, mark_ns)))


def watch_loop(w: Watch, mark: str) -> None:
    flag = children_flag()
    last, quiet = -1, 0
    while exists(w.root):
        time.sleep(w.poll)
        mine = set(tree_pids(os.getpid()))
        pids = [p for p in tree_pids(w.root)[1:] if p not in mine]
        now = cpu_centis(pids, flag)
        mark_ns = os.stat(mark).st_mtime_ns
        moved = made_progress(w, pids, last, now, mark_ns)
        os.utime(mark, None)
        last = now
        if moved:
            quiet = 0
            continue
        quiet = int(quiet + w.poll + 0.5)
        if quiet >= w.bound:
            declare_stalled(w, quiet, pids)
            return


def watch(w: Watch) -> int:
    for signum in (signal.SIGTERM, signal.SIGINT, signal.SIGHUP):
        signal.signal(signum, stop)
    mark = os.path.join(os.environ.get("TMPDIR", "/tmp"), f"stall-mark.{os.getpid()}")
    try:
        with open(mark, "w"):
            pass
        watch_loop(w, mark)
    except SystemExit:
        pass
    finally:
        try:
            os.unlink(mark)
        except OSError:
            pass
    return 0


def main(argv: Sequence[str]) -> int:
    if list(argv[:1]) != ["watch"]:
        print((__doc__ or "").rstrip("\n"))
        return 2
    args = list(argv[1:])
    if len(args) < 4:
        print(USAGE, file=sys.stderr)
        return 2
    args += [""] * (6 - len(args))
    env = os.environ
    return watch(Watch(int(args[0]), float(args[1]), args[2], args[3], args[4], args[5],
                       float(env.get("KINOWO_STALL_POLL_SECONDS", "30")), int(env.get("KINOWO_STALL_CPU_CENTIS", "100")),
                       env.get("KINOWO_STALL_GROUPS", "") == "1"))


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
