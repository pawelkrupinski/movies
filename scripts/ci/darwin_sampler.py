#!/usr/bin/env python3
"""A macOS runner's resource series, for job_metrics.py (which starts it there in the background): macOS has no /proc,
so cgroup-sampler.sh has nothing to read. The same file and column names as the Linux sampler's series, so
ci_trends.py and ci_timings.py read both alike:

    darwin_sampler.py <series file> [seconds]   a DARWIN_SERIES row every <seconds> (default 5) until the job ends

The CPU columns are Mach's host_statistics ticks (all cores together, cumulative since boot, 100 a second, like
/proc/stat's), busy = user + nice + system; memory and swap come from host_statistics64 and sysctl, and
`mem_pressure_level` is the kernel's (1 normal, 2 warn, 4 critical): macOS has no PSI. Through libSystem by ctypes:
the runner's python has nothing else.

NEVER FAILS: a row that cannot be read is skipped, never the loop.

Test: python3 scripts/ci/job_metrics_test.py (Darwin)
"""
import ci_machine
import ctypes
import ctypes.util
import os
import resource
import sys
import time
from typing import Callable, List, Optional, Protocol, Tuple, Type, TypeVar, Union

DARWIN_SERIES = ("t", "cpu_total", "cpu_busy", "cpu_user", "cpu_sys", "load1", "mem_total_kb", "mem_avail_kb",
                 "swap_total_kb", "swap_free_kb", "mem_pressure_level")
SAMPLE_SECONDS = 5

Cell = Union[int, float, str]
C = TypeVar("C", bound=ctypes.Structure)


class SwapUsage(ctypes.Structure):
    """sysctl vm.swapusage's struct xsw_usage."""
    _fields_ = [("total", ctypes.c_uint64), ("avail", ctypes.c_uint64), ("used", ctypes.c_uint64),
                ("pagesize", ctypes.c_uint32), ("encrypted", ctypes.c_bool)]


class Readings(Protocol):
    """What a row reads: DarwinHost, or a test's fixed figures."""

    def cpu_ticks(self) -> Tuple[int, int, int, int]: ...

    def available_pages(self) -> int: ...

    def memsize(self) -> Optional[int]: ...

    def swap(self) -> Optional[Tuple[int, int]]: ...

    def pressure_level(self) -> Optional[int]: ...


class DarwinHost:
    """The machine's figures through libSystem: Mach's host_statistics for the CPU ticks and the VM page counts, sysctl
    for RAM, swap and the memory-pressure level."""
    HOST_CPU_LOAD_INFO = 3
    HOST_VM_INFO64 = 4
    VM_INFO64_COUNT = 38  # sizeof(vm_statistics64_data_t) / sizeof(integer_t)

    def __init__(self) -> None:
        self.lib = ctypes.CDLL(ctypes.util.find_library("System") or "/usr/lib/libSystem.B.dylib")
        self.lib.mach_host_self.restype = ctypes.c_uint32
        self.host = ctypes.c_uint32(self.lib.mach_host_self())

    def _statistics(self, name: str, flavor: int, count: int) -> List[int]:
        info = (ctypes.c_uint32 * count)()
        size = ctypes.c_uint32(count)
        call = getattr(self.lib, name)
        if call(self.host, ctypes.c_int(flavor), ctypes.byref(info), ctypes.byref(size)) != 0:
            raise OSError("%s flavor %d failed" % (name, flavor))
        return [int(value) for value in info]

    def cpu_ticks(self) -> Tuple[int, int, int, int]:
        """(user, system, idle, nice) ticks."""
        user, system, idle, nice = self._statistics("host_statistics", self.HOST_CPU_LOAD_INFO, 4)
        return user, system, idle, nice

    def available_pages(self) -> int:
        """free + inactive + speculative pages (vm_statistics64: free_count, active, inactive at 0..2, speculative_count
        at 23): what macOS reclaims without pressure."""
        vm = self._statistics("host_statistics64", self.HOST_VM_INFO64, self.VM_INFO64_COUNT)
        return vm[0] + vm[2] + vm[23]

    def _sysctl(self, name: str, value: ctypes.Structure) -> bool:
        size = ctypes.c_size_t(ctypes.sizeof(value))
        return self.lib.sysctlbyname(name.encode(), ctypes.byref(value), ctypes.byref(size), None, ctypes.c_size_t(0)) == 0

    def _number(self, name: str, kind: Type[C]) -> Optional[C]:
        value = kind()
        return value if self._sysctl(name, value) else None

    def memsize(self) -> Optional[int]:
        value = self._number("hw.memsize", Uint64)
        return int(value.value) if value else None

    def swap(self) -> Optional[Tuple[int, int]]:
        """(total, available) bytes."""
        usage = self._number("vm.swapusage", SwapUsage)
        return (int(usage.total), int(usage.avail)) if usage else None

    def pressure_level(self) -> Optional[int]:
        value = self._number("kern.memorystatus_vm_pressure_level", Int)
        return int(value.value) if value else None


class Uint64(ctypes.Structure):
    _fields_ = [("value", ctypes.c_uint64)]


class Int(ctypes.Structure):
    _fields_ = [("value", ctypes.c_int)]


def darwin_row(host: Readings, now: Optional[float] = None) -> List[Cell]:
    """One DARWIN_SERIES row; an unreadable figure is an empty cell."""
    user, system, idle, nice = host.cpu_ticks()
    memsize, swap, pressure = host.memsize(), host.swap(), host.pressure_level()
    return [int(now if now is not None else ci_machine.wall()), user + nice + system + idle, user + nice + system,
            user + nice, system, round(os.getloadavg()[0], 2), memsize // 1024 if memsize else "",
            host.available_pages() * resource.getpagesize() // 1024, swap[0] // 1024 if swap else "",
            swap[1] // 1024 if swap else "", pressure if pressure else ""]


def darwin_sample(path: str, every: float = SAMPLE_SECONDS, host: Optional[Readings] = None,
                  rounds: Optional[int] = None, sleep: Callable[[float], None] = time.sleep) -> None:
    """Append a row to `path` every `every` seconds: `rounds` of them, else until the job's end."""
    readings: Readings = host or DarwinHost()
    if not os.path.exists(path) or not os.path.getsize(path):
        with open(path, "w", encoding="utf-8") as handle:
            handle.write("\t".join(DARWIN_SERIES) + "\n")
    done = 0
    while rounds is None or done < rounds:
        try:
            row = darwin_row(readings)
            with open(path, "a", encoding="utf-8") as handle:
                handle.write("\t".join(str(cell) for cell in row) + "\n")
        except Exception:  # noqa: BLE001 -- a sampler never fails the job
            pass
        done += 1
        if rounds is None or done < rounds:
            sleep(every)


if __name__ == "__main__":
    darwin_sample(sys.argv[1], float(sys.argv[2]) if len(sys.argv) > 2 else SAMPLE_SECONDS)
