"""THE MEMORY GATE and the wait for a JVM slot, for the Python tools (scripts/jvm-gate.sh's memory_gate and
jvm_slot_take loop; its header says why): a tool waits while memory is both paged out past KINOWO_MEMORY_GATE_GB and
short of free RAM (KINOWO_MEMORY_GATE_FREE_GB), and then for one of the machine's slots -- saying so once, then every
KINOWO_MEMORY_GATE_STATUS_SECONDS, in the shell gate's words. KINOWO_MEMORY_GATE_PROBE / _FREE_PROBE (a shell command
printing the GB) stand in for the machine's figures, as there."""
from __future__ import annotations

import math
import os
import re
from dataclasses import dataclass
from typing import Callable, List, MutableMapping, Optional, Protocol, Tuple

import machine


class Slots(Protocol):
    slot: str

    def slot_try(self, label: str = "tool") -> bool: ...

    def riding(self) -> bool: ...


def shell_number(command: str) -> Optional[int]:
    """The number a probe command prints (the shell gate evals it), None when it prints none."""
    out = machine.output(["bash", "-c", command])[1].strip()
    return int(out) if re.fullmatch(r"-?[0-9]+", out) else None


def command_out(argv: List[str]) -> str:
    return machine.output(argv)[1]


def vm_pages(stat: str, names: str) -> int:
    return sum(int(m.group(1).replace(".", "")) for m in re.finditer(r"Pages (?:%s):\s+([0-9.]+)" % names, stat))


def machine_paged_gb() -> Optional[int]:
    """Swap used plus the compressor's pages (macOS), in whole GB."""
    swap = re.search(r"used = ([0-9.]+)M", command_out(["sysctl", "-n", "vm.swapusage"]))
    page = command_out(["sysctl", "-n", "hw.pagesize"]).strip()
    compressed = re.search(r"occupied by compressor:\s+([0-9.]+)", command_out(["vm_stat"]))
    if not (swap and page.isdigit() and compressed):
        return None
    return int(float(swap.group(1)) / 1024 + int(page) * int(compressed.group(1).replace(".", "")) / 2 ** 30)


def machine_free_gb() -> Optional[int]:
    page = command_out(["sysctl", "-n", "hw.pagesize"]).strip()
    if not page.isdigit():
        return None
    return int(vm_pages(command_out(["vm_stat"]), "free|inactive|speculative") * int(page) / 2 ** 30)


def top_holders() -> str:
    """The three biggest processes by resident memory, "name GB (pid N)"."""
    rows: List[Tuple[int, str, str]] = []
    for line in command_out(["ps", "-axo", "rss=,pid=,comm="]).splitlines():
        fields = line.split(None, 2)
        if len(fields) == 3 and fields[0].isdigit():
            rows.append((int(fields[0]), fields[1], os.path.basename(fields[2])))
    rows.sort(reverse=True)
    return ", ".join(f"{name} {rss / 2 ** 20:.1f} GB (pid {pid})" for rss, pid, name in rows[:3])


@dataclass
class Clock:
    sleep: Callable[[float], None]


class MemoryGate:
    def __init__(self, env: MutableMapping[str, str], clock: Clock, say: Callable[[str], None],
                 governed: Callable[[], bool], system: str) -> None:
        self.env, self.clock, self.say, self.governed, self.system = env, clock, say, governed, system

    def number(self, key: str, default: str) -> float:
        return float(self.env.get(key) or default)

    def paged(self) -> Optional[int]:
        probe = self.env.get("KINOWO_MEMORY_GATE_PROBE")
        if probe:
            return shell_number(probe)
        return machine_paged_gb() if self.system == "Darwin" else None

    def free(self) -> Optional[int]:
        probe = self.env.get("KINOWO_MEMORY_GATE_FREE_PROBE")
        if probe:
            return shell_number(probe)
        return machine_free_gb() if self.system == "Darwin" else None

    def short(self) -> Optional[str]:
        """"<paged> GB paged out, <free> GB free" while memory is both paged past the budget and short of free RAM."""
        paged = self.paged()
        if paged is None or paged <= int(self.number("KINOWO_MEMORY_GATE_GB", "20")):
            return None
        free = self.free() or 0
        if free >= int(self.number("KINOWO_MEMORY_GATE_FREE_GB", "6")):
            return None
        return f"{paged} GB paged out, {free} GB free"

    def wait(self, label: str, riding: bool = False) -> None:
        """While memory is short, wait (never ahead on time alone, unless KINOWO_MEMORY_GATE_MAX_WAIT)."""
        if self.off(riding):
            return
        limit = self.env.get("KINOWO_MEMORY_GATE_MAX_WAIT", "")
        poll, every = self.number("KINOWO_MEMORY_GATE_POLL", "15"), int(self.number("KINOWO_MEMORY_GATE_STATUS_SECONDS", "180"))
        waited, said, due = 0, False, every
        state = self.short()
        while state is not None:
            if self.gave_up(label, state, limit, waited):
                return
            said, due = True, self.tell(label, state, limit, said, waited, due, every)
            self.clock.sleep(poll)
            waited = waited + math.ceil(poll)
            state = self.short()
        if said:
            self.say(f"[{label}] memory gate: open after {waited}s")

    def off(self, riding: bool) -> bool:
        """The gate is off (KINOWO_MEMORY_GATE=0), the governor admits by memory itself, or the caller rides a slot."""
        return self.env.get("KINOWO_MEMORY_GATE", "1") == "0" or self.governed() or riding

    def gave_up(self, label: str, state: str, limit: str, waited: int) -> bool:
        """Past KINOWO_MEMORY_GATE_MAX_WAIT (when set): going ahead, said."""
        if not limit or waited < int(limit):
            return False
        self.say(f"[{label}] memory gate: still {state} after {waited}s: starting anyway (KINOWO_MEMORY_GATE_MAX_WAIT={limit})")
        return True

    def tell(self, label: str, state: str, limit: str, said: bool, waited: int, due: int, every: int) -> int:
        """Said once, then every `every` s: the next time it is due."""
        if not said:
            self.say(self.first_word(label, state, limit))
        elif waited >= due:
            self.say(f"[{label}] memory gate: still waiting after {waited}s: {state}; biggest: {top_holders()}")
            return waited + every
        return due

    def first_word(self, label: str, state: str, limit: str) -> str:
        budget, free = self.env.get("KINOWO_MEMORY_GATE_GB", "20"), self.env.get("KINOWO_MEMORY_GATE_FREE_GB", "6")
        how = f"up to {limit}s" if limit else ("for as long as memory is short; KINOWO_MEMORY_GATE_MAX_WAIT=<seconds> "
                                                "goes ahead after that long")
        return (f"[{label}] memory gate: {state} (over KINOWO_MEMORY_GATE_GB={budget}, under KINOWO_MEMORY_GATE_FREE_GB="
                f"{free}): waiting for other JVMs to finish before starting one ({how})")


def holders(slot_dir: str, slots: int) -> List[str]:
    found: List[str] = []
    for n in range(1, slots + 1):
        try:
            with open(os.path.join(slot_dir, f"{n}.flock")) as f:
                pid = f.read().strip()
        except OSError:
            continue
        if pid:
            found.append(pid)
    return found


def holder_what(pid: str) -> str:
    """What a slot's holder is, as jvm-gate.sh jvm_slot_holders says it ("sbt-warm --keep, movies-wt/x"), or gone."""
    args = command_out(["ps", "-o", "args=", "-p", pid]).strip()
    if not args:
        return "gone"
    words = args.split()
    at = next((i for i, w in enumerate(words) if re.search(r"/scripts/[^/]+$", w) or w.startswith("scripts/")), None)
    if at is None:
        return args[:60]
    word = words[at]
    tool, where = re.sub(r".*/scripts/", "", word), re.sub(r".*/projects/", "", re.sub(r"/scripts/.*", "", word))
    rest = " " + words[at + 1] if at + 1 < len(words) else ""
    return tool + rest + (", " + where if where and where != word else "")


class SlotWait:
    """The wait for one of KINOWO_JVM_SLOTS slots (jvm-gate.sh jvm_slot_take's loop, after the memory gate)."""

    def __init__(self, memory: MemoryGate, slot_dir: str) -> None:
        self.memory, self.slot_dir = memory, slot_dir
        self.env, self.say = memory.env, memory.say

    def named(self, slots: int) -> str:
        return "; ".join(f"pid {p} ({holder_what(p)})" for p in holders(self.slot_dir, slots))

    def wait(self, label: str, gate: Slots) -> None:
        slots, limit = int(self.env.get("KINOWO_JVM_SLOTS", "4")), int(self.memory.number("KINOWO_JVM_SLOT_MAX_WAIT", "1800"))
        poll, every = self.memory.number("KINOWO_JVM_SLOT_POLL", "5"), int(self.memory.number("KINOWO_MEMORY_GATE_STATUS_SECONDS", "180"))
        waited, said, due = 0, False, every
        while not gate.slot_try():
            speak = not said or waited >= due
            if waited >= limit and not self.past_limit(label, waited, limit, speak):
                return
            if speak:
                self.busy_word(label, slots, waited, limit, said)
                said, due = True, waited + every
            self.memory.clock.sleep(poll)
            waited += math.ceil(poll)
        if said:
            self.say(f"[{label}] JVM slot {os.path.basename(gate.slot)} of {slots} after {waited}s")

    def busy_word(self, label: str, slots: int, waited: int, limit: int, said: bool) -> None:
        """Every slot busy, within KINOWO_JVM_SLOT_MAX_WAIT: said once, then every status period."""
        if waited >= limit:
            return
        held = ",".join(holders(self.slot_dir, slots))
        if not said:
            self.say(f"[{label}] all {slots} JVM slots busy (KINOWO_JVM_SLOTS; holders: {held}): waiting (up to {limit}s, "
                     "longer while memory is short)")
            self.say(f"[{label}] WAITING for a JVM slot, held by {self.named(slots)}; a kept sbt server holds its slot "
                     "while idle (its owner frees it: scripts/sbt-warm --stop)")
            return
        self.say(f"[{label}] all {slots} JVM slots still busy after {waited}s (holders: {held})")
        self.say(f"[{label}] still WAITING for a JVM slot, held by {self.named(slots)}")

    def past_limit(self, label: str, waited: int, limit: int, speak: bool) -> bool:
        """Past KINOWO_JVM_SLOT_MAX_WAIT: whether to go on waiting (memory short), said when `speak`."""
        state = self.memory.short() if self.env.get("KINOWO_MEMORY_GATE", "1") != "0" else None
        if state is None:
            self.say(f"[{label}] every JVM slot still busy after {waited}s: starting anyway")
            return False
        if speak:
            self.say(f"[{label}] every JVM slot still busy after {waited}s, past KINOWO_JVM_SLOT_MAX_WAIT={limit}, but "
                     f"{state}: still waiting; biggest: {top_holders()}")
        return True
