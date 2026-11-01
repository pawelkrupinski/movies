#!/usr/bin/env python3
"""job_metrics.py: GC log parsing, JVM command lines, JUnit totals and slowest tests, `finish` writing every line and
the artifact name, and both commands never failing.
Run: python3 scripts/ci/job_metrics_test.py"""
import io
import json
import os
import sys
import tempfile
import unittest
import unittest.mock
from contextlib import redirect_stdout

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import job_metrics as jm  # noqa: E402

GC_LOG = """[0.010s][info][gc] Using G1
[1.500s][info][gc] GC(0) Pause Young (Normal) (G1 Evacuation Pause) 24M->4M(256M) 3.000ms
[5.000s][info][gc] GC(1) Pause Young (Concurrent Start) (G1 Humongous Allocation) 300M->120M(512M) 7.500ms
[6.000s][info][gc] GC(2) Concurrent Mark Cycle 40.000ms
[9.000s][info][gc] GC(3) Pause Full (G1 Compaction Pause) 500M->200M(512M) 90.000ms
[10.000s][info][gc] GC(4) Pause Remark 210M->210M(512M) 1.500ms
"""

JUNIT = """<?xml version="1.0"?>
<testsuites><testsuite name="a.FastSpec" tests="2" time="1.5">
  <testcase name="one" time="1.0"/><testcase name="two" time="0.5"><skipped/></testcase></testsuite>
<testsuite name="b.SlowSpec" tests="2" time="30">
  <testcase name="slow" time="25"/><testcase name="broken" time="5"><failure message="x"/></testcase></testsuite>
</testsuites>"""


class Parsers(unittest.TestCase):
    def test_gc_log(self):
        with tempfile.NamedTemporaryFile("w", suffix=".log", delete=False) as handle:
            handle.write(GC_LOG)
        figures = jm.gc_of(handle.name)
        os.unlink(handle.name)
        self.assertEqual(4, figures["gc_count"])  # the concurrent cycle is not a pause
        self.assertEqual(1, figures["gc_full"])
        self.assertEqual(102.0, figures["gc_pause_ms"])
        self.assertEqual(90.0, figures["gc_max_pause_ms"])
        self.assertEqual(210, figures["heap_after_max_mb"])
        self.assertEqual(10.0, figures["uptime_s"])
        self.assertAlmostEqual(0.0102, figures["gc_overhead"], places=4)

    def test_jvm_command_lines(self):
        command = "/opt/java/bin/java -Xmx4g -XX:+UseG1GC -Xlog:gc:file=x -cp a.jar:b.jar xsbt.boot.Boot testUnit"
        self.assertEqual("xsbt.boot.Boot", jm.main_class(command))
        self.assertEqual("-Xmx4g -XX:+UseG1GC", jm.jvm_flags(command))
        self.assertEqual("sbt-launch.jar", jm.main_class("java -Xms1g -jar /x/sbt-launch.jar"))
        self.assertEqual("sbt forked test JVM", jm.main_class("java -Xmx3g /tmp/sbt_1/sbt-args1234.tmp"))

    def test_junit(self):
        with tempfile.TemporaryDirectory() as root:
            os.makedirs(os.path.join(root, "target/test-reports/unit"))
            with open(os.path.join(root, "target/test-reports/unit/TEST.xml"), "w") as handle:
                handle.write(JUNIT)
            # sbt's second copy of the same reports, under the module
            os.makedirs(os.path.join(root, "worker/target/test-reports"))
            with open(os.path.join(root, "worker/target/test-reports/TEST.xml"), "w") as handle:
                handle.write(JUNIT)
            lines = jm.junit(root)
        totals = lines[0]
        self.assertEqual({"kind": "tests", "suites": 2, "files": 2, "tests": 4, "failed": 1, "skipped": 1,
                          "time_s": 31.5}, totals)
        self.assertEqual("b.SlowSpec", lines[1]["suite"])
        slow = [l for l in lines if l["kind"] == "test"]
        self.assertEqual("slow", slow[0]["test"])
        self.assertIn("failed", [l["status"] for l in slow])

    def test_no_reports_no_lines(self):
        with tempfile.TemporaryDirectory() as root:
            self.assertEqual([], jm.junit(root))


class Finish(unittest.TestCase):
    def test_finish_writes_every_kind_and_the_artifact_name(self):
        with tempfile.TemporaryDirectory() as temp:
            env = {"RUNNER_TEMP": temp, "GITHUB_JOB": "test", "RUNNER_NAME": "GitHub Actions 12",
                   "GITHUB_OUTPUT": os.path.join(temp, "out"), "CI_JOB_STATUS": "success"}
            with unittest.mock.patch.dict(os.environ, env, clear=False):
                os.environ.pop("CI_METRICS_FILE", None)
                os.environ.pop("CGROUP_SAMPLER_OUT", None)
                sampler = jm.sampler_out()
                with open(sampler + ".series", "w") as handle:
                    handle.write("t\tcpu_total\tcpu_busy\tpsi_mem_some_us\n100\t0\t0\t0\n105\t2000\t1000\t50000\n")
                with open(sampler + ".jvms", "w") as handle:
                    handle.write("4242\tjava -Xmx2g xsbt.boot.Boot\n")
                os.makedirs(jm.gc_dir())
                with open(os.path.join(jm.gc_dir(), "gc-4242.log"), "w") as handle:
                    handle.write(GC_LOG)
                out = io.StringIO()
                with redirect_stdout(out):
                    jm.finish(env=os.environ, root=temp)
                with open(jm.metrics_file()) as handle:
                    lines = [json.loads(line) for line in handle]
                with open(env["GITHUB_OUTPUT"]) as handle:
                    output = handle.read()
        kinds = [line["kind"] for line in lines]
        self.assertEqual(["series", "jvm", "job_end"], kinds)
        jvm = lines[1]
        self.assertEqual(("4242", "xsbt.boot.Boot", "-Xmx2g"), (jvm["pid"], jvm["main"], jvm["flags"]))
        self.assertEqual("GitHub Actions 12", lines[-1]["runner_name"])
        self.assertIn("name=ci-metrics-test-GitHub-Actions-12", output)
        self.assertIn("CPU 50%", out.getvalue())
        self.assertIn("PSI mem some 1.0%", out.getvalue())

    def test_never_fails(self):
        out = io.StringIO()
        with unittest.mock.patch.object(jm, "finish", side_effect=RuntimeError("boom")), redirect_stdout(out):
            self.assertEqual(0, jm.main(["finish"]))
        self.assertIn("::warning::job metrics (finish) incomplete: boom", out.getvalue())

    def test_rate_limit_failure_is_none(self):
        def opener(*_, **__):
            raise OSError("offline")
        self.assertIsNone(jm.rate_limit("t", opener))
        self.assertIsNone(jm.rate_limit(""))


if __name__ == "__main__":
    unittest.main()
