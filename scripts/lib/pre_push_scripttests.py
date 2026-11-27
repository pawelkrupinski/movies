"""THE SCRIPTS' OWN SELF-TESTS (scripts/hooks/pre-push's header: `scripttests`): scripts/run-script-tests over what the
push changed -- natively, with the floor bash 3.2 over the bash the push touches in the same run (`--floor`), and on
Linux (`--linux`, CI's ubuntu in Docker) beside it in the background; opt-in, the concurrency-heavy tests under load
(PREPUSH_STRESS=N). --verify-only's repo-wide readers run alone. A runner from before a flag (the pushed tree's own,
read by grep) is called as it was then. Run as a light check (pre_push_checks.Processes), its lines the shell hook's."""
from __future__ import annotations

import re
from typing import List, Optional, Sequence

import machine
from pre_push_light import Check
from pre_push_repo import into

RUNNER = "scripts/run-script-tests"
SHOWN = 80
PASSED = re.compile(r"^PASS|✓")
FLOOR_FIX = "scripts/floor-bash --install"


def shown_failures(c: Check, log: str) -> None:
    """`grep -v '^PASS\\|✓' log | head -80 >&2`."""
    lines = [line for line in c.log_text(log) if not PASSED.search(line)]
    c.shown(lines[:SHOWN])


def last_line(c: Check, log: str) -> str:
    lines = c.log_text(log)
    return lines[-1] if lines else ""


def supports(c: Check, flag: str) -> bool:
    """Whether the pushed tree's runner takes `flag` (`grep -q -e <flag> scripts/run-script-tests`)."""
    return flag in c.repo.files.read_text_or(RUNNER, "")


def skipped(c: Check, name: str) -> bool:
    return name in c.repo.env.get("PREPUSH_SKIP", "").split()


def floor_bash(c: Check) -> "tuple[bool, str]":
    """(found, path) of the floor bash 3.2 (scripts/floor-bash; PREPUSH_FLOOR_BASH names one); "" for no floor run
    (PREPUSH_SKIP=floor, or a pushed runner from before --bash). No floor bash BLOCKS, as any missing tool does:
    ci-runner's Linux landing checks, whose missing bash 3 once passed in silence, let ed160b94b land a construct 3.2
    cannot parse (Order independence #72)."""
    if not supports(c, "--bash"):
        return True, ""
    if skipped(c, "floor"):
        c.say.warn("scripttests under the floor bash 3.2 SKIPPED (PREPUSH_SKIP=floor)")
        return True, ""
    floor = c.repo.env.get("PREPUSH_FLOOR_BASH", "")
    if not floor and c.holds("-x", "scripts/floor-bash"):
        code, out, err = c.both(["scripts/floor-bash"])
        if code != 0:
            return c.missing("floor", f"bash 3.2 ({err.replace(chr(10), ' ')})", FLOOR_FIX) == 0, ""
        floor = out.strip()
    floor = floor or "/bin/bash"  # a pushed tree from before scripts/floor-bash
    major = bash_major(c, floor) if c.holds("-x", floor) else ""
    if major == "3":
        return True, floor
    found = f"bash {major}" if major else "no bash"
    return c.missing("floor", f"bash 3.2 (the floor; {floor} is {found})",
                     "scripts/floor-bash --install, or PREPUSH_FLOOR_BASH=<a bash 3.2>") == 0, ""


# ── the leaves: each one effect, no decision ──
def background(c: Check, argv: Sequence[str], log: str) -> "machine.Process[bytes]":
    c.say.flush()
    return machine.spawn(into(log, argv), machine.Ran(c.repo.cwd, c.env_with(None)))


def passed(run: "machine.Process[bytes]") -> bool:
    return machine.waited(run, None) and run.returncode == 0


def system(c: Check) -> str:
    return c.repo.env.get("PREPUSH_UNAME") or machine.output(["uname", "-s"])[1].strip()


def bash_major(c: Check, bash: str) -> str:
    """The major version `bash` says, its stderr dropped; "" when it says none."""
    return machine.output([bash, "-c", 'echo "${BASH_VERSINFO[0]}"'], cwd=c.repo.cwd, env=c.repo.env)[1].strip()


def readers(c: Check) -> int:
    """--verify-only's repo-wide self-tests, alone."""
    log = c.tmp("scripttests.log")
    if c.logged([RUNNER, "--changed", c.text("base_sha"), "--readers"], log) == 0:
        c.printed([last_line(c, log)])
        return 0
    shown_failures(c, log)
    return 1


def native(c: Check, floor_args: List[str]) -> int:
    log = c.tmp("scripttests.log")
    if c.logged([RUNNER, "--changed", c.text("base_sha"), *floor_args], log) != 0:
        shown_failures(c, log)
        return 1
    c.printed([last_line(c, log)])
    if c.repo.env.get("PREPUSH_STRESS", "0") == "0":
        c.printed([line for line in c.log_text(log) if line.startswith("run-script-tests: concurrency-heavy")])
    if floor_args:
        floored = sum(1 for line in c.log_text(log) if line.endswith(" (floor)"))
        c.printed([f"under {floor_args[1]}: the floor's scope, in the same run ({floored} test(s))"])
    return 0


def linux(c: Check, run: "machine.Process[bytes]") -> int:
    log = c.tmp("scripttests-linux.log")
    if passed(run):
        c.printed([f"on Linux: {last_line(c, log)}"])
        return 0
    if "--linux needs a running Docker" in c.repo.files.read_text_or(log, ""):
        c.printed(["on Linux: SKIPPED (Docker is not running -- start Rancher/Docker Desktop to run them as CI does)"])
        return 0
    c.shown(["on Linux (scripts/run-script-tests --linux; PREPUSH_SKIP=linux skips it):"])
    shown_failures(c, log)
    return 1


def stress(c: Check, times: str) -> int:
    log = c.tmp("scripttests-stress.log")
    if c.logged([RUNNER, "--changed", c.text("base_sha"), "--stress", times], log) == 0:
        said = ";".join(line for line in c.log_text(log) if re.search(r"^STRESS|no concurrency-heavy", line))
        c.printed([f"stressed (PREPUSH_STRESS={times}): {said}"])
        return 0
    c.shown([f"under stress (PREPUSH_STRESS={times}, scripts/run-script-tests --stress):"])
    shown_failures(c, log)
    return 1


def floor_apart(c: Check, run: "machine.Process[bytes]", floor: str) -> int:
    log = c.tmp("scripttests-floor.log")
    if passed(run):
        c.printed([f"under {floor}: {last_line(c, log)}"])
        return 0
    c.shown([f"under the floor bash {floor} (scripts/floor-bash; PREPUSH_SKIP=floor skips it):"])
    shown_failures(c, log)
    return 1


def scripttests(c: Check) -> int:
    """ONE RUN, ONE POOL (--floor): the floor's scope under 3.2 and the rest natively -- run side by side, a test in
    both ran twice at once, and the contention alone kept a hook change ~+43 s over the landing budget (2026-11-16).
    ALREADY ON LINUX (ci-runner's landing checks): the native run IS the Linux run, so the Docker copy is skipped,
    said."""
    if c.text("scripttests_readers"):
        return readers(c)
    found, floor = floor_bash(c)
    if not found:
        return 1
    floor_args = ["--floor", floor] if floor and supports(c, "--floor)") else []
    apart = floor_run(c, floor) if floor and not floor_args else None
    on_linux = linux_run(c)
    code = native(c, floor_args)
    code |= linux(c, on_linux) if on_linux is not None else 0
    code |= stressed(c) if code == 0 else 0
    return code | (floor_apart(c, apart, floor) if apart is not None else 0)


def stressed(c: Check) -> int:
    """STRESS, opt-in (PREPUSH_STRESS=N): the concurrency-heavy tests the push selects, N runs of each at once."""
    times = c.repo.env.get("PREPUSH_STRESS", "0")
    return stress(c, times) if times != "0" and supports(c, "--stress)") else 0


def floor_run(c: Check, floor: str) -> "machine.Process[bytes]":
    """A runner from before --floor: its own floor run, beside the native one, as before."""
    return background(c, [RUNNER, "--changed", c.text("base_sha"), "--bash", floor], c.tmp("scripttests-floor.log"))


def linux_run(c: Check) -> Optional["machine.Process[bytes]"]:
    """The Docker pass beside the native run; none on Linux (said: the native run is Linux's) or when skipped."""
    if system(c) == "Linux":
        c.printed(["on Linux: this run is Linux's (the Docker pass, run-script-tests --linux, would repeat it)"])
        return None
    if skipped(c, "linux") or not supports(c, "--linux"):
        return None
    return background(c, [RUNNER, "--linux", "--changed", c.text("base_sha")], c.tmp("scripttests-linux.log"))
