"""THE JVM GATE for the Python tools: scripts/jvm-gate.sh's slots, locks and governor tickets (its header says why and
how), the same kernel locks on the same files, so a shell holder and a Python one exclude each other while the shell
tools still source the shell gate. A ticket or slot is held on DESCRIPTOR 7, as there: a shell tool this process runs
with descriptor 7 open rides it (KINOWO_JVM_SLOT_HELD names it), and a Python tool started under a shell holder rides
the shell's. Every collaborator is injected (the environment, the commands it runs, the clock, where it speaks); the
composition root is the tool's main().

  FileLock(path, fd).take() / .give()   jvm-gate.sh's lock_take / lock_give: a kernel lock on `path`, held on `fd`
                                        (any free one when None), the holder's pid written in
  lock_busy(path)                       0 there: whether some process holds `path`'s lock
  Governor                              `agentctl gov` (probe, admit, route, usage): governor_bin, governor_has
  JvmGate                               slot_take, slot_try, take_if_free, riding, release, hand_off, keeper_start,
                                        cpu_take, width (gov_width, with THE GRANTED WIDTH: agentctl's --min-cores),
                                        route, usage, cpu_options -- the shell functions of those names"""
from __future__ import annotations

import fcntl
import json
import os
import random
import subprocess
from dataclasses import dataclass
from typing import Callable, Dict, List, MutableMapping, Optional, Protocol, Sequence, cast

import machine
from jvm_memory import MemoryGate, SlotWait

SLOT_FD = 7


def descriptor_open(fd: int) -> bool:
    try:
        os.fstat(fd)
    except OSError:
        return False
    return True


def flock_now(fd: int, mode: int) -> bool:
    """Whether the open file description on `fd` now holds the lock `mode` (never waiting)."""
    try:
        fcntl.flock(fd, mode | fcntl.LOCK_NB)
    except OSError:
        return False
    return True


def lock_busy(path: str) -> bool:
    """Whether some process holds `path`'s kernel lock (a probe on a description of its own, never kept)."""
    if not os.path.exists(path):
        return False
    try:
        fd = os.open(path, os.O_WRONLY | os.O_APPEND | os.O_CREAT, 0o644)
    except OSError:
        return False
    try:
        return not flock_now(fd, fcntl.LOCK_EX)
    finally:
        os.close(fd)


def write_pid(path: str, pid: Optional[int]) -> None:
    with open(path, "w") as f:
        f.write(f"{pid}\n" if pid is not None else "")


class FileLock:
    """A kernel lock on a file (jvm-gate.sh's THE LOCKS): free the moment every holder of its descriptor has ended."""

    def __init__(self, path: str, fd: Optional[int] = None) -> None:
        self.path, self.want, self.fd = path, fd, -1

    @property
    def held(self) -> bool:
        return self.fd >= 0

    def take(self, pid: int) -> bool:
        """Whether this process now holds the lock (`pid` written in); false at once when another does."""
        try:
            fd = os.open(self.path, os.O_WRONLY | os.O_APPEND | os.O_CREAT, 0o644)
        except OSError:
            return False
        if self.want is not None and fd != self.want:
            os.dup2(fd, self.want)
            os.close(fd)
            fd = self.want
        os.set_inheritable(fd, True)
        if not flock_now(fd, fcntl.LOCK_EX):
            os.close(fd)
            return False
        self.fd = fd
        write_pid(self.path, pid)
        return True

    def give(self) -> None:
        """The lock given back: the pid cleared, the descriptor closed."""
        if self.held:
            write_pid(self.path, None)
            self.close()

    def close(self) -> None:
        if self.held:
            os.close(self.fd)
            self.fd = -1


class Commands(Protocol):
    def status(self, argv: Sequence[str], pass_fds: Sequence[int] = (), quiet: bool = False,
               out_to_err: bool = False) -> int: ...

    def output(self, argv: Sequence[str]) -> Optional[str]: ...


class SubprocessCommands:
    """The real commands: stdout and stderr this process's (stdout to stderr with `out_to_err`), or none (`quiet`)."""

    def status(self, argv: Sequence[str], pass_fds: Sequence[int] = (), quiet: bool = False,
               out_to_err: bool = False) -> int:
        out = subprocess.DEVNULL if quiet else (2 if out_to_err else None)
        return machine.status(argv, out=out, err=subprocess.DEVNULL if quiet else None, pass_fds=pass_fds)

    def output(self, argv: Sequence[str]) -> Optional[str]:
        code, out = machine.output(argv)
        return out if code == 0 else None


@dataclass
class Host:
    """What the gate reads of this process and machine (each a seam for its tests)."""
    env: MutableMapping[str, str]
    pid: int
    system: str
    home: str
    toplevel: Callable[[], str]
    say: Callable[[str], None]


class Governor:
    """`agentctl gov` when it governs here: the Mac, KINOWO_GOVERNOR not 0, an agentctl answering its probe."""

    def __init__(self, host: Host, commands: Commands) -> None:
        self.host, self.commands = host, commands

    def bin(self) -> Optional[str]:
        env = self.host.env
        g = env.get("KINOWO_GOVERNOR_BIN") or os.path.join(self.host.home, "bin/agentctl")
        if env.get("KINOWO_GOVERNOR", "1") == "0" or self.os_name() != "Darwin" or not os.access(g, os.X_OK):
            return None
        if env.get("KINOWO_GOVERNOR_FEATURES", "").startswith(g + "|"):
            return g
        probe = self.commands.output([g, "gov", "probe"]) or ""
        if not probe.startswith("agentctl governor: "):
            self.host.say(f"jvm-gate: {g} answers no `gov probe`: no governor ticket, the JVM slots instead")
            return None
        env["KINOWO_GOVERNOR_FEATURES"] = f"{g}|{probe.strip()[len('agentctl governor: '):]}"
        return g

    def os_name(self) -> str:
        return self.host.env.get("KINOWO_JVM_GATE_OS") or self.host.system

    def has(self, feature: str) -> bool:
        features = self.host.env.get("KINOWO_GOVERNOR_FEATURES", "").partition("|")[2]
        return feature in features.split()

    def admit(self, argv: List[str]) -> bool:
        return self.commands.status(argv, pass_fds=(SLOT_FD,)) == 0

    def route(self, g: str, job: str) -> bool:
        return self.commands.status([g, "gov", "route", "--job", job, "--class", "agent"], out_to_err=True) == 0

    def usage(self, g: str, job: str, wall: int, cpu: int, meter: str) -> None:
        argv = [g, "gov", "usage", "--job", job, "--wall", str(wall), "--cpu", str(cpu)]
        self.commands.status(argv + (["--meter", meter] if meter else []), quiet=True)


def gov_job(env: MutableMapping[str, str], label: str) -> str:
    """The job a ticket for `label` is charged cores for (jvm-gate.sh gov_job)."""
    if env.get("KINOWO_GOV_JOB"):
        return env["KINOWO_GOV_JOB"]
    for prefix, job in (("identity-what-if", "what-if"), ("identity-slice", "slice"), ("identity-measure", "measure"),
                        ("identity", "compile")):
        if label.startswith(prefix):
            return job
    return "keeper" if label in ("sbt-warm", "keeper") else "job"


def granted(ticket: str) -> Optional[str]:
    """The cores the governor granted a ticket (its record, <ticket less .lock>.json), None when it says none."""
    try:
        with open(ticket[: -len(".lock")] + ".json") as f:
            record = json.load(f)
    except (OSError, ValueError):
        return None
    cores: object = cast(Dict[str, object], record).get("cores") if isinstance(record, dict) else None
    return str(cores) if isinstance(cores, (int, float)) else None


def whole(cores: str) -> int:
    """The whole cores, at least 1 ("1.5" -> 1)."""
    head = cores.split(".")[0]
    return int(head) if head.isdigit() and int(head) >= 1 else 1


class JvmGate:
    """One process's JVM slot or governor ticket, on descriptor 7 (jvm-gate.sh's JVM_SLOT)."""

    def __init__(self, host: Host, governor: Governor, memory: MemoryGate, slot_wait: SlotWait, slot_dir: str) -> None:
        self.host, self.governor, self.memory, self.slot_wait, self.slot_dir = host, governor, memory, slot_wait, slot_dir
        self.slot = ""
        self.lock: Optional[FileLock] = None
        self.granted: Optional[str] = None

    @property
    def env(self) -> MutableMapping[str, str]:
        return self.host.env

    def riding(self) -> bool:
        """Whether this process holds a slot, its own or its parent's (descriptor 7 open, its name exported)."""
        return bool(self.env.get("KINOWO_JVM_SLOT_HELD")) and descriptor_open(SLOT_FD)

    def gov_class(self, label: str) -> str:
        if self.env.get("KINOWO_GOV_CLASS"):
            return self.env["KINOWO_GOV_CLASS"]
        if label.startswith("identity-measure"):
            return "measure"
        return "red" if "-wt-red-" in os.path.basename(self.host.toplevel()) else "agent"

    def _held(self, lock: FileLock) -> None:
        self.lock, self.slot = lock, lock.path
        self.env["KINOWO_JVM_SLOT_HELD"] = lock.path

    def charge(self, label: str) -> List[str]:
        """What a ticket for `label` is charged: its job, its cores, and with a governor that grants its minimum."""
        if not self.governor.has("cores"):
            return []
        cores = ["--job", gov_job(self.env, label)]
        cores += ["--cores", self.env["KINOWO_GOV_CORES"]] if self.env.get("KINOWO_GOV_CORES") else []
        if self.env.get("KINOWO_GOV_MIN_CORES") and self.governor.has("grant"):
            cores += ["--min-cores", self.env["KINOWO_GOV_MIN_CORES"]]
        return cores

    def gov_take(self, label: str, try_once: bool = False) -> bool:
        """A governor ticket on descriptor 7: whether it was admitted."""
        g = self.governor.bin()
        if g is None:
            return False
        cores = self.charge(label)
        tickets = self.env.get("AGENTCTL_GOV_DIR") or os.path.join(self.host.home, ".cache/agentctl/gov")
        os.makedirs(tickets, exist_ok=True)
        lock = FileLock(os.path.join(tickets, f"t-{self.host.pid}-{random.randrange(10 ** 9)}.lock"), SLOT_FD)
        fd = os.open(lock.path, os.O_WRONLY | os.O_APPEND | os.O_CREAT, 0o644)
        os.dup2(fd, SLOT_FD)
        os.close(fd)
        lock.fd = SLOT_FD
        argv = [g, "gov", "admit", "--fd", str(SLOT_FD), "--ticket", lock.path, "--class", self.gov_class(label),
                "--mem", self.env.get("KINOWO_GOV_MEM_GB", "5"), "--label", label, "--pid", str(self.host.pid),
                "--cwd", self.env.get("PWD") or os.getcwd(), *cores, *(["--try"] if try_once else [])]
        if self.governor.admit(argv):
            self._held(lock)
            self.granted = granted(lock.path) if "--min-cores" in cores else None
            if self.env.get("KINOWO_GOV_CORES"):
                self.env["AGENTCTL_GOV_WIDTH"] = str(whole(self.granted or self.env["KINOWO_GOV_CORES"]))
            return True
        lock.close()
        os.remove(lock.path)
        return False

    def _ticket(self, label: str, job: str, mem: str, cores: str = "", min_cores: str = "") -> bool:
        keys = ("KINOWO_GOV_JOB", "KINOWO_GOV_MEM_GB", "KINOWO_GOV_CORES", "KINOWO_GOV_MIN_CORES")
        saved = {k: self.env.get(k) for k in keys}
        self.env.update(dict(zip(keys, (job, mem, cores, min_cores))))
        try:
            return self.gov_take(label)
        finally:
            for key, value in saved.items():
                if value is None:
                    self.env.pop(key, None)
                else:
                    self.env[key] = value

    def governs(self) -> bool:
        """Whether a governor governs here (the Mac, KINOWO_GOVERNOR not 0, an agentctl answering its probe)."""
        return self.governor.bin() is not None

    def width(self, label: str, job: str, want: int, mem: str = "1", fixed: bool = False) -> int:
        """THE GOVERNED FAN-OUT's width (jvm-gate.sh gov_width): under a ticket already at most its width, taking none;
        else a ticket charged `want` cores -- with a governor that grants, asked --min-cores 1 (unless `fixed`: a pool
        with a time bound) and as wide as it granted --, held until release(); one wide when the governor fails."""
        want = max(1, want)
        above = self.env.get("AGENTCTL_GOV_WIDTH")
        if above:
            return min(want, whole(above))
        g = self.governor.bin()
        if g is None or self.riding() or self.env.get("KINOWO_GOV_CPU_HELD") or self.slot:
            return want
        if self._ticket(label, job, mem, str(want), "" if fixed else "1"):
            return min(want, whole(self.granted)) if self.granted else want
        self.host.say(f"[{label}] the governor ({g} gov admit) failed: running one wide, without a ticket")
        return 1

    def cpu_take(self, label: str, job: str) -> None:
        """A cores-only ticket for one call into a kept JVM (jvm-gate.sh jvm_cpu_take)."""
        g = self.governor.bin()
        if g is None or not self.governor.has("cores") or self.riding() or self.env.get("KINOWO_GOV_CPU_HELD"):
            return
        if not self.slot and not self._ticket(label, job, "0"):
            self.host.say(f"[{label}] the governor ({g} gov admit) failed: running without a cores ticket")

    def slot_try(self, label: str = "tool") -> bool:
        """One pass over the slots, never waiting (with the governor: one look, its --try)."""
        if self.governor.bin() is not None:
            return self.gov_take(label, try_once=True)
        os.makedirs(self.slot_dir, exist_ok=True)
        for n in range(1, int(self.env.get("KINOWO_JVM_SLOTS", "4")) + 1):
            lock = FileLock(os.path.join(self.slot_dir, f"{n}.flock"), SLOT_FD)
            if lock.take(self.host.pid):
                self._held(lock)
                return True
        return False

    def slot_take(self, label: str = "tool") -> None:
        """The memory gate, then a slot (or the governor's ticket): waiting for one as jvm-gate.sh jvm_slot_take."""
        if self.riding():
            return
        if self.governor.bin() is not None:
            if not self.gov_take(label):
                self.host.say(f"[{label}] the governor ({self.governor.bin()} gov admit) failed: starting without a "
                              "ticket")
            return
        self.memory.wait(label)
        if self.env.get("KINOWO_JVM_SLOTS", "4") != "0":
            self.slot_wait.wait(label, self)

    def take_if_free(self, label: str = "keeper") -> bool:
        """A keeper's poll: whether this process, holding no slot, has just taken a free one (slots on)."""
        return not self.riding() and self.env.get("KINOWO_JVM_SLOTS", "4") != "0" and self.slot_try(label)

    def release(self) -> None:
        if self.lock is not None:
            self.lock.give()
        self._forget()

    def _forget(self) -> None:
        self.lock, self.slot = None, ""
        self.env.pop("KINOWO_JVM_SLOT_HELD", None)

    def hand_off(self, pid: int) -> None:
        """The slot now belongs to `pid` (started with descriptor 7 open): its pid written in, this descriptor closed."""
        if self.lock is not None:
            write_pid(self.lock.path, pid)
            self.lock.close()
        self._forget()

    def keeper_start(self, directory: str, log: str, argv: Sequence[str], held: Sequence[int]) -> int:
        """The KEEPER of a long-lived JVM: `argv` started in `directory`, detached (a process group of its own, nohup,
        stdin closed, output appended to `log`), with descriptor 7 and `held` open, and this process's slot handed off
        to it. Its pid."""
        keep = tuple(fd for fd in (*held, SLOT_FD) if descriptor_open(fd))
        with open(log, "a") as out:
            proc = machine.spawn(["nohup", *argv], machine.Ran(directory, None, out, subprocess.STDOUT, keep),
                                 own_group=True)
        self.hand_off(proc.pid)
        return proc.pid

    def cpu_options(self) -> str:
        cpus = self.env.get("KINOWO_JVM_CPUS", "4")
        if cpus == "0" or self.governor.os_name() != "Darwin":
            return ""
        return f"-XX:ActiveProcessorCount={cpus}"

    def route(self, job: str) -> str:
        """`ci` when the governor would hold `job` now and ci-runner's measure lane is free, else `local`."""
        g = self.governor.bin()
        if g is None or not self.governor.has("cores") or self.riding() or self.env.get("KINOWO_GOV_CPU_HELD"):
            return "local"
        if self.gov_class(job) != "agent":
            return "local"
        return "ci" if self.governor.route(g, job) else "local"

    def usage(self, job: str, wall: int, cpu: int, meter: str = "") -> None:
        g = self.governor.bin()
        if g is not None and self.governor.has("cores"):
            self.governor.usage(g, job, wall, cpu, meter)
