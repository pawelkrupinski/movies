#!/usr/bin/env python3
"""The job's own memory and disk writes, read from its cgroup (the job container's, which holds the job's dockerd and
the mongod container inside it) -- and, beside them, the machine's resources as a time series, for the job's ci-metrics
artifact (scripts/ci/job_metrics.py, .github/actions/job-telemetry, scripts/ci/ci_trends.py).

  cgroup-sampler.sh start [seconds]   # in the background: the maxima of anon, shmem (tmpfs), anon+shmem, and the
                                      # java and mongod RSS, every <seconds> (default 5), into $CGROUP_SAMPLER_OUT;
                                      # and every <seconds> one row of $CGROUP_SAMPLER_OUT.series (the machine: CPU
                                      # ticks incl. steal, load, run queue, memory, swap, PSI totals, disk sectors and
                                      # free space, network bytes, open files, OOM kills), rows of .procs (per command:
                                      # processes, RSS, CPU ticks, threads, for every command over 20 MB) and each new
                                      # JVM's command line in .jvms
  cgroup-sampler.sh report            # memory.peak against memory.max, the maxima, memory.events, io.stat, GC logs

OVERHEAD: one sample reads /proc/<pid>/stat and the cgroup files and runs one df: a few ms of CPU every 5 s. Nothing it
does can fail the job: the loop ignores every error.

memory.peak alone is not the answer: it counts page cache, which the kernel reclaims before it OOM-kills. What cannot be
reclaimed is anon (the JVM's heap, mongod's cache) plus shmem (a tmpfs data directory) -- that sum is what has to stay
under memory.max.

Disk writes: io.stat's `9:3` line is the RAID1 md device itself; its two members are counted again on their own lines,
so summing every line triples a write.

CGROUP_ROOT stands in for /sys/fs/cgroup and PROC_ROOT for /proc, for a test. Stdlib only. job_metrics.py and the
convergence leg run it by the name scripts/ci/cgroup-sampler.sh; tested by cgroup_sampler_test.py beside it."""
from __future__ import annotations

import glob
import os
import re
import subprocess
import sys
import time
from typing import Dict, List, Mapping, Optional, Sequence, Set, Tuple
import ci_machine  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
# The machine's figures, one tab-separated row, cumulative counters as the kernel keeps them.
SERIES_HEADER = ("t", "cpu_total", "cpu_busy", "cpu_user", "cpu_sys", "cpu_iowait", "cpu_steal", "load1",
                 "procs_running", "mem_total_kb", "mem_avail_kb", "swap_total_kb", "swap_free_kb", "psi_cpu_some_us",
                 "psi_mem_some_us", "psi_mem_full_us", "psi_io_some_us", "psi_io_full_us", "disk_read_sectors",
                 "disk_write_sectors", "disk_free_kb", "oom_kill", "net_rx_bytes", "net_tx_bytes", "open_files")
PROCS_HEADER = "t\tcomm\tprocs\trss_kb\tticks\tthreads\n"
DISK = re.compile(r"^(sd[a-z]+|vd[a-z]+|xvd[a-z]+|nvme[0-9]+n[0-9]+)$")
GC_HEAP = re.compile(r"[0-9]+M->([0-9]+)M\(([0-9]+)M\)")
Row = Dict[str, str]


def number(text: str) -> int:
    """awk's reading of a field as a number: its leading digits, else 0."""
    found = re.match(r"-?[0-9]+", text.strip())
    return int(found.group(0)) if found else 0


def read(path: str) -> Optional[str]:
    try:
        with open(path, errors="replace") as f:
            return f.read()
    except OSError:
        return None


def gb(value: str) -> str:
    try:
        bytes_ = float(value)
    except ValueError:
        bytes_ = float(number(value))
    return "%.2f GB" % (bytes_ / 1e9)


def psi_total(line: str) -> str:
    return next((field[len("total="):] for field in line.split() if field.startswith("total=")), "")


class Machine:
    """The /proc files of one series row, each read into the row's columns."""

    def __init__(self, proc: str, env: Mapping[str, str]) -> None:
        self.proc = proc
        self.env = env

    def lines(self, name: str) -> List[List[str]]:
        text = read(os.path.join(self.proc, name))
        return [line.split() for line in text.split("\n")] if text is not None else []

    def cpu(self, row: Row) -> None:
        for fields in self.lines("stat"):
            if fields[:1] == ["cpu"]:
                f = [number(x) for x in fields[1:]] + [0] * 10
                user, sys_, idle, iowait, steal = f[0] + f[1], f[2] + f[5] + f[6], f[3], f[4], f[7]
                row.update(cpu_total=str(user + sys_ + idle + iowait + steal), cpu_busy=str(user + sys_ + steal),
                           cpu_user=str(user), cpu_sys=str(sys_), cpu_iowait=str(iowait), cpu_steal=str(steal))

    def load(self, row: Row) -> None:
        for fields in self.lines("loadavg"):
            if len(fields) >= 4:
                row.update(load1=fields[0], procs_running=fields[3].split("/")[0])

    def memory(self, row: Row) -> None:
        values = {fields[0]: fields[1] for fields in self.lines("meminfo") if len(fields) >= 2}
        row.update(mem_total_kb=values.get("MemTotal:", ""), mem_avail_kb=values.get("MemAvailable:", ""),
                   swap_total_kb=values.get("SwapTotal:", ""), swap_free_kb=values.get("SwapFree:", ""))

    def pressure(self, row: Row) -> None:
        for kind, some, full in (("cpu", "psi_cpu_some_us", None), ("memory", "psi_mem_some_us", "psi_mem_full_us"),
                                 ("io", "psi_io_some_us", "psi_io_full_us")):
            for fields in self.lines(f"pressure/{kind}"):
                if fields[:1] == ["some"]:
                    row[some] = psi_total(" ".join(fields))
                elif fields and full:
                    row[full] = psi_total(" ".join(fields))

    def disks(self, row: Row) -> None:
        sectors: List[Tuple[int, int]] = [(number(f[5]), number(f[9])) for f in self.lines("diskstats")
                                          if len(f) >= 10 and DISK.match(f[2])]
        if sectors:
            row.update(disk_read_sectors=str(sum(r for r, _ in sectors)),
                       disk_write_sectors=str(sum(w for _, w in sectors)))

    def counters(self, row: Row) -> None:
        """OOM kills and open files: the first field after a key, or a file's first field."""
        for name, key, column in (("vmstat", "oom_kill", "oom_kill"), ("sys/fs/file-nr", None, "open_files")):
            for fields in self.lines(name):
                if key is None and fields:
                    row[column] = fields[0]
                elif fields[:1] == [key] and len(fields) >= 2:
                    row[column] = fields[1]

    def network(self, row: Row) -> None:
        """Every link's bytes but loopback's."""
        links = [f for f in self.lines("net/dev") if f and f[0].endswith(":") and f[0] != "lo:" and len(f) >= 10]
        if links:
            row.update(net_rx_bytes=str(sum(number(f[1]) for f in links)),
                       net_tx_bytes=str(sum(number(f[9]) for f in links)))

    def free_kb(self) -> str:
        try:
            done = subprocess.run(["df", "-kP", self.env.get("GITHUB_WORKSPACE") or "/"], stdout=subprocess.PIPE,
                                  stderr=subprocess.DEVNULL, text=True)
        except OSError:
            return ""
        lines = done.stdout.split("\n")
        return lines[1].split()[3] if len(lines) > 1 and len(lines[1].split()) > 3 else ""

    def series_line(self) -> str:
        row: Row = {"t": str(int(ci_machine.wall())), "disk_free_kb": self.free_kb()}
        for read_into in (self.cpu, self.load, self.memory, self.pressure, self.disks, self.counters,
                          self.network):
            read_into(row)
        return "\t".join(row.get(name, "") for name in SERIES_HEADER) + "\n"


class Command:
    """One command's processes, summed."""

    def __init__(self) -> None:
        self.count = 0
        self.ticks = 0
        self.threads = 0
        self.rss_kb = 0.0


def process_rows(proc: str, page: int) -> Tuple[Dict[str, Command], List[str]]:
    """Per command, over every process's /proc/<pid>/stat: the processes, their RSS, their CPU ticks so far and their
    threads; and the pids of the JVMs, whose command lines the loop keeps."""
    commands: Dict[str, Command] = {}
    jvms: List[str] = []
    for stat in glob.glob(os.path.join(glob.escape(proc), "[0-9]*", "stat")):
        text = (read(stat) or "").split("\n")[0]
        opened, closed = text.find("("), text.rfind(")")
        if opened < 0 or closed < 0:
            continue
        comm = re.sub(r"[ \t]", "_", text[opened + 1:closed])
        # after comm: state(1) ... utime(12) stime(13) ... num_threads(18) ... rss pages(22)
        f = text[closed + 2:].split() + ["0"] * 22
        command = commands.setdefault(comm, Command())
        command.count += 1
        command.ticks += number(f[11]) + number(f[12])
        command.threads += number(f[17])
        command.rss_kb += number(f[21]) * page / 1024
        if comm == "java":
            jvms.append(text[:max(opened - 1, 0)])
    return commands, jvms


class Sampler:
    def __init__(self, env: Mapping[str, str]) -> None:
        self.env = env
        self.cg = env.get("CGROUP_ROOT") or "/sys/fs/cgroup"
        self.proc = env.get("PROC_ROOT") or "/proc"
        self.out = env.get("CGROUP_SAMPLER_OUT") or os.path.join(env.get("RUNNER_TEMP") or "/tmp",
                                                                 "cgroup-sampler.max")
        self.maxima = {"anon": 0, "shmem": 0, "anon+shmem": 0, "java-rss": 0, "mongod-rss": 0}
        self.seen: Set[str] = set()

    def stat_of(self, key: str) -> str:
        for line in (read(os.path.join(self.cg, "memory.stat")) or "").split("\n"):
            fields = line.split()
            if fields[:1] == [key] and len(fields) >= 2:
                return fields[1]
        return ""

    def append(self, suffix: str, text: str) -> None:
        with open(self.out + suffix, "a") as f:
            f.write(text)

    def headers(self) -> None:
        for suffix, header in ((".series", "\t".join(SERIES_HEADER) + "\n"), (".procs", PROCS_HEADER)):
            if not os.path.isfile(self.out + suffix) or os.path.getsize(self.out + suffix) == 0:
                with open(self.out + suffix, "w") as f:
                    f.write(header)

    def sample(self, page: int) -> None:
        self.append(".series", Machine(self.proc, self.env).series_line())
        commands, jvms = process_rows(self.proc, page)
        now = str(int(ci_machine.wall()))
        kept = {c: m for c, m in commands.items() if m.rss_kb >= 20480 or c in ("java", "mongod")}
        self.append(".procs", "".join(f"{now}\t{c}\t{m.count}\t{int(m.rss_kb)}\t{m.ticks}\t{m.threads}\n"
                                      for c, m in kept.items()))
        self.keep_jvms(jvms)
        self.keep_maxima(kept)

    def keep_jvms(self, jvms: List[str]) -> None:
        """Each new JVM's command line, once."""
        for pid in jvms:
            if pid not in self.seen:
                self.seen.add(pid)
                cmdline = (read(os.path.join(self.proc, pid, "cmdline")) or "").replace("\0", " ")[:4000]
                self.append(".jvms", f"{pid}\t{cmdline}\n")

    def keep_maxima(self, kept: Dict[str, Command]) -> None:
        anon, shmem = number(self.stat_of("anon")), number(self.stat_of("shmem"))
        rss = {name: int(kept[name].rss_kb) * 1024 if name in kept else 0 for name in ("java", "mongod")}
        now_values = {"anon": anon, "shmem": shmem, "anon+shmem": anon + shmem, "java-rss": rss["java"],
                      "mongod-rss": rss["mongod"]}
        for key, value in now_values.items():
            self.maxima[key] = max(self.maxima[key], value)
        with open(self.out + ".tmp", "w") as f:
            f.write("".join(f"{key} {value}\n" for key, value in self.maxima.items()))
        os.replace(self.out + ".tmp", self.out)

    def loop(self, every: float) -> None:
        page = number(self.env.get("SAMPLER_PAGE_SIZE") or "") or os.sysconf("SC_PAGESIZE")
        try:
            self.headers()
        except OSError:
            pass
        while True:
            try:
                self.sample(page)
            except (OSError, ValueError):
                pass
            time.sleep(every)

    def report(self) -> None:
        self.report_memory()
        self.report_io()
        for log in sorted(glob.glob("/tmp/gc-*.log")) + sorted(glob.glob(os.path.join(
                glob.escape(self.env.get("RUNNER_TEMP") or "/tmp"), "ci-gc", "gc-*.log"))):
            print(gc_summary(log))

    def report_memory(self) -> None:
        limit = (read(os.path.join(self.cg, "memory.max")) or "").strip()
        peak = (read(os.path.join(self.cg, "memory.peak")) or "").strip()
        print(f"[cgroup] memory.max {limit} ({'unlimited' if limit == 'max' else gb(limit)})")
        print(f"[cgroup] memory.peak {peak or '?'} ({gb(peak or '0')}) -- page cache included")
        for line in (read(self.out) or "").splitlines():
            key, _, value = line.strip().partition(" ")
            print(f"[cgroup] max {key} {value.strip()} ({gb(value.strip())})")
        events = (read(os.path.join(self.cg, "memory.events")) or "").replace("\n", " ")
        print(f"[cgroup] memory.events: {events}")
        print(f"[cgroup] now: anon {gb(self.stat_of('anon'))}, shmem {gb(self.stat_of('shmem'))}, "
              f"file {gb(self.stat_of('file'))}")

    def report_io(self) -> None:
        for line in (read(os.path.join(self.cg, "io.stat")) or "").splitlines():
            device, *rest = line.split() or [""]
            written = next((field[len("wbytes="):] for field in rest if field.startswith("wbytes=")), "") or "0"
            print(f"[cgroup] io.stat {device} wbytes {written} ({gb(written)})")


def gc_summary(log: str) -> str:
    """"... Pause Young (Normal) (G1 Evacuation Pause) 8000M->3000M(10240M) 12.3ms": the after-GC heap is the live
    set's bound."""
    collections = full = after = committed = 0
    for line in (read(log) or "").splitlines():
        heap = GC_HEAP.search(line)
        if heap:
            after, committed = max(after, int(heap.group(1))), max(committed, int(heap.group(2)))
            collections += 1
            full += "Pause Full" in line
    return (f"[gc] {log}: {collections} collections, max after-GC heap {after} MB, max committed {committed} MB, "
            f"{full} full")


def start(sampler: Sampler, every: float) -> int:
    """The loop in a detached child, its output nowhere: the step that started it goes on at once."""
    if os.fork() > 0:
        return 0
    os.setsid()
    null = os.open(os.devnull, os.O_RDWR)
    for fd in (0, 1, 2):
        os.dup2(null, fd)
    try:
        sampler.loop(every)
    finally:
        os._exit(0)


def main(argv: Sequence[str], env: Mapping[str, str]) -> int:
    sampler = Sampler(env)
    if argv[:1] == ["start"]:
        return start(sampler, float(argv[1]) if len(argv) > 1 and argv[1] else 5.0)
    if argv[:1] == ["report"]:
        sampler.report()
        return 0
    print(f"usage: {os.path.join(HERE, 'cgroup-sampler.sh')} start [seconds] | report", file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(ci_machine.environment())))
