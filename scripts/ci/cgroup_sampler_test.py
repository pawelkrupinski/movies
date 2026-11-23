#!/usr/bin/env python3
"""scripts/ci/cgroup-sampler.sh, black-box: the job cgroup's memory and disk writes a convergence row reports last, and
the machine's series, read from a cgroup and a /proc laid out under the scratch directory (CGROUP_ROOT, PROC_ROOT).
Ported from cgroup-sampler-test.sh; the same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import signal
import subprocess
import sys
import unittest
from typing import Dict, List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/cgroup_sampler.py", "scripts/ci/cgroup-sampler.sh")


def stat_line(pid: int, comm: str, utime: int, stime: int, threads: int, rss_pages: int) -> str:
    """/proc/<pid>/stat: after `(comm)`, state is field 1, utime 12, stime 13, threads 18, rss (pages) 22."""
    return (f"{pid} ({comm}) S 0 0 0 0 0 0 0 0 0 0 {utime} {stime} 0 0 0 0 {threads} 0 0 0 {rss_pages}\n")


class CgroupSamplerTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.write("cg/memory.stat", "anon 9000000000\nfile 3000000000\nshmem 2000000000\n")
        self.write("cg/memory.max", "17179869184\n")
        self.write("cg/memory.peak", "16000000000\n")
        self.write("cg/memory.events", "low 0\nhigh 0\nmax 0\noom 0\noom_kill 0\n")
        self.write("cg/io.stat", "259:0 rbytes=1 wbytes=3000000000 rios=1 wios=1\n"
                                 "9:3 rbytes=1 wbytes=1000000000 rios=1 wios=1\n")
        self.write("proc/101/stat", stat_line(101, "java", 100, 20, 30, 250))
        self.write("proc/102/stat", stat_line(102, "java", 300, 40, 50, 500))
        self.write("proc/103/stat", stat_line(103, "mongod", 70, 10, 9, 125))
        self.write("proc/101/cmdline", "/usr/bin/java\0-Xmx4g\0xsbt.boot.Boot\0")
        self.write("proc/stat", "cpu  100 0 50 800 20 0 5 7 0 0\ncpu0 1 0 0 0 0 0 0 0 0 0\n")
        self.write("proc/loadavg", "1.50 1.00 0.50 3/400 999\n")
        self.write("proc/meminfo", "MemTotal:       16000000 kB\nMemAvailable:   12000000 kB\n"
                                   "SwapTotal:      4000000 kB\nSwapFree:       4000000 kB\n")
        self.write("proc/pressure/cpu", "some avg10=0.00 avg60=0.00 avg300=0.00 total=1234\n"
                                        "full avg10=0.00 avg60=0.00 avg300=0.00 total=0\n")
        self.write("proc/pressure/memory", "some avg10=0.00 avg60=0.00 avg300=0.00 total=5000\n"
                                           "full avg10=0.00 avg60=0.00 avg300=0.00 total=2000\n")
        self.write("proc/diskstats", "   8       0 sda 10 0 400 0 20 0 800 0 0 0 0\n"
                                     "   8       1 sda1 10 0 400 0 20 0 800 0 0 0 0\n")
        self.write("proc/vmstat", "pgfault 1\noom_kill 2\n")
        self.write("proc/net/dev", "Inter-|\n face |bytes\n    lo: 99 0 0 0 0 0 0 0 99 0\n"
                                   "  eth0: 1000 0 0 0 0 0 0 0 3000 0\n")
        self.write("proc/sys/fs/file-nr", "1536\t0\t9223372036854775807\n")
        self.sampled = {"CGROUP_ROOT": self.path("cg"), "PROC_ROOT": self.path("proc"),
                        "CGROUP_SAMPLER_OUT": self.path("max"), "SAMPLER_PAGE_SIZE": "4096"}

    def tearDown(self) -> None:
        self.stop_samplers()
        super().tearDown()

    def stop_samplers(self) -> None:
        """The background sampler this case started: found by its command line, as the shell test found it."""
        listed = subprocess.run(["pgrep", "-f", r"cgroup[-_]sampler\.(sh|py) start 1"], capture_output=True,
                                text=True).stdout.split()
        for pid in listed:
            try:
                os.kill(int(pid), signal.SIGTERM)
            except (OSError, ValueError):
                pass

    def report(self) -> str:
        done = self.run_tool("report", env=self.sampled)
        self.assertEqual(0, done.code, done.output)
        return done.stdout

    def sampled_once(self) -> None:
        done = self.run_tool("start", "1", env=self.sampled)
        self.assertEqual(0, done.code, done.output)
        self.assertTrue(cli_harness.wait_until(lambda: os.path.exists(self.path("max"))), "no first sample")
        self.assertTrue(cli_harness.wait_until(lambda: len(self.read("max.series").splitlines()) >= 2))

    def line(self, text: str, fragment: str) -> str:
        return next((line for line in text.split("\n") if fragment in line), "")

    def series(self) -> Dict[str, str]:
        header, row = self.read("max.series").split("\n")[:2]
        return dict(zip(header.split("\t"), row.split("\t")))

    def test_the_report_reads_the_maxima_the_cgroup_and_io_stat(self) -> None:
        self.sampled_once()
        report = self.report()
        # the memory that cannot be reclaimed: anon and shmem
        self.assertEqual("[cgroup] max anon+shmem 11000000000 (11.00 GB)", self.line(report, "anon+shmem"))
        # each command's RSS is the sum over its processes
        self.assertEqual("[cgroup] max java-rss 3072000 (0.00 GB)", self.line(report, "java-rss"))
        self.assertEqual("[cgroup] memory.max 17179869184 (17.18 GB)", self.line(report, "memory.max"))
        self.assertEqual("[cgroup] memory.peak 16000000000 (16.00 GB) -- page cache included",
                         self.line(report, "memory.peak"))
        self.assertEqual("[cgroup] memory.events: low 0 high 0 max 0 oom 0 oom_kill 0 ", self.line(report, "events"))
        self.assertEqual("[cgroup] now: anon 9.00 GB, shmem 2.00 GB, file 3.00 GB", self.line(report, "now:"))
        # the md device's writes on a line of their own
        self.assertEqual("[cgroup] io.stat 9:3 wbytes 1000000000 (1.00 GB)", self.line(report, "io.stat 9:3"))

    def test_the_series_row_fills_every_column_its_header_names(self) -> None:
        self.sampled_once()
        header, row = self.read("max.series").split("\n")[:2]
        self.assertEqual(len(header.split("\t")), len(row.split("\t")))
        column = self.series()
        # CPU: total and busy ticks, steal counted as busy
        self.assertEqual(("982", "162"), (column["cpu_total"], column["cpu_busy"]))
        self.assertEqual(("100", "55", "20", "7"), (column["cpu_user"], column["cpu_sys"], column["cpu_iowait"],
                                                    column["cpu_steal"]))
        self.assertEqual(("1.50", "3"), (column["load1"], column["procs_running"]))
        self.assertEqual(("16000000", "12000000", "4000000", "4000000"),
                         (column["mem_total_kb"], column["mem_avail_kb"], column["swap_total_kb"],
                          column["swap_free_kb"]))
        self.assertEqual(("1234", "5000", "2000", "", ""),
                         (column["psi_cpu_some_us"], column["psi_mem_some_us"], column["psi_mem_full_us"],
                          column["psi_io_some_us"], column["psi_io_full_us"]))
        # disk sectors count whole disks, not their partitions; network bytes leave out loopback
        self.assertEqual(("400", "800"), (column["disk_read_sectors"], column["disk_write_sectors"]))
        self.assertEqual(("1000", "3000"), (column["net_rx_bytes"], column["net_tx_bytes"]))
        self.assertEqual(("2", "1536"), (column["oom_kill"], column["open_files"]))
        self.assertTrue(column["disk_free_kb"].isdigit(), column["disk_free_kb"])
        self.assertTrue(column["t"].isdigit())

    def test_per_command_processes_rss_ticks_and_threads_and_each_jvm_once(self) -> None:
        self.sampled_once()
        procs: List[List[str]] = [line.split("\t") for line in self.read("max.procs").splitlines()]
        self.assertEqual(["t", "comm", "procs", "rss_kb", "ticks", "threads"], procs[0])
        self.assertIn(["java", "2", "3000", "460", "80"], [row[1:] for row in procs])
        self.assertIn(["mongod", "1", "500", "80", "9"], [row[1:] for row in procs])
        self.assertTrue(cli_harness.wait_until(lambda: len(self.read("max.procs").splitlines()) >= 5))
        # each JVM once, its command line as far as it can be read (102 has none)
        self.assertEqual(["101\t/usr/bin/java -Xmx4g xsbt.boot.Boot ", "102\t"],
                         sorted(self.read("max.jvms").splitlines()))

    def test_the_maxima_file_names_each_maximum(self) -> None:
        self.sampled_once()
        self.assertEqual(["anon 9000000000", "shmem 2000000000", "anon+shmem 11000000000", "java-rss 3072000",
                          "mongod-rss 512000"], self.read("max").splitlines())

    def test_a_report_with_no_samples_and_no_limit_says_unlimited(self) -> None:
        self.write("cg/memory.max", "max\n")
        report = self.report()
        self.assertEqual("[cgroup] memory.max max (unlimited)", self.line(report, "memory.max"))
        self.assertEqual("", self.line(report, "[cgroup] max "))

    def test_an_unknown_command_is_refused(self) -> None:
        self.assertEqual(2, self.run_tool("nonsense").code)


if __name__ == "__main__":
    unittest.main()
