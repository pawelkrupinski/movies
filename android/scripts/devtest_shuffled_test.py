#!/usr/bin/env python3
"""android/scripts/devtest-shuffled.sh, black-box, over a scratch copy of its layout with a fake devtest.sh in place of
the emulator run, so it runs in a second with no SDK. The fake reads its stdin the way adb and Gradle's client can: a
run must still reach every class, not just the first. Ported from devtest-shuffled-test.sh case for case; the same
cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import shutil
import sys
import unittest
from typing import List

HERE = os.path.dirname(os.path.abspath(__file__))
sys.dont_write_bytecode = True  # `python3 -I` ignores PYTHONDONTWRITEBYTECODE: no __pycache__ in the tree
sys.path.insert(0, os.path.join(HERE, "..", "..", "scripts", "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("android/scripts/devtest_shuffled.py", "android/scripts/devtest-shuffled.sh")
# What the tool takes with it into the scratch layout, whichever of them exist: the shell tool and the seeded order it
# sources, or the Python tool and the modules it imports.
LAYOUT = ["android/scripts/devtest-shuffled.sh", "android/scripts/devtest_shuffled.py", "scripts/seeded-order.sh",
          "scripts/lib"]
# devtest.sh: drains stdin, logs the class (its last argument) and its arguments
LOGGING = '''cat >/dev/null
echo "${@: -1}" >> "$STUB_LOG"
echo "$*" >> "$STUB_LOG.args"
'''
# devtest.sh: writes its class's report to the SAME Gradle paths every run writes, and fails for a.Two
REPORTING = '''cat >/dev/null
class="${@: -1}"
app="$(cd "$(dirname "${BASH_SOURCE[0]}")/../app" && pwd)"
rm -rf "$app/build/reports/androidTests/connected" "$app/build/outputs/androidTest-results/connected"
mkdir -p "$app/build/reports/androidTests/connected" "$app/build/outputs/androidTest-results/connected"
echo "report of $class" > "$app/build/reports/androidTests/connected/index.html"
echo "result of $class" > "$app/build/outputs/androidTest-results/connected/TEST.xml"
[ "$class" != a.Two ]
'''


class DevtestShuffledTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        for relative in LAYOUT:
            source = os.path.join(cli_harness.ROOT, relative)
            os.makedirs(self.path("layout", os.path.dirname(relative)), exist_ok=True)
            if os.path.isdir(source):
                shutil.copytree(source, self.path("layout", relative), ignore=shutil.ignore_patterns("__pycache__"))
            elif os.path.exists(source):
                shutil.copy2(source, self.path("layout", relative))
        for name in ("One", "Two", "Three"):
            self.write(f"layout/android/app/src/androidTest/java/a/{name}.kt", f"class {name} {{ @Test fun t() {{}} }}\n")
        self.write("layout/android/app/src/androidTest/java/a/Helper.kt", "object Helper\n")
        self.tool = self.path("layout", os.path.relpath(TOOL, cli_harness.ROOT))
        self.kept = self.path("layout/android/app/build/order-independence")

    def devtest(self, body: str) -> None:
        stub = self.write("layout/android/scripts/devtest.sh", "#!/usr/bin/env bash\n" + body)
        os.chmod(stub, 0o755)

    def shuffled(self, *args: str, **env: str) -> cli_harness.Done:
        return self.run_tool("7", *args, tool=self.tool, env={"STUB_LOG": self.path("log"), **env})

    def logged(self) -> List[str]:
        return self.read("log").splitlines()

    def test_every_class_runs_alone_in_the_seeded_order_though_a_run_reads_stdin(self) -> None:
        self.devtest(LOGGING)
        done = self.shuffled()
        self.assertEqual(0, done.code, done.output)
        # the order seeded_order gives seed 7 (scripts/seeded-order.sh: the POSIX cksum of "<seed>\n<line>")
        self.assertEqual(["a.Three", "a.One", "a.Two"], self.logged())
        self.assertEqual("devtest-shuffled: all 3 classes pass alone under seed 7", done.stdout.splitlines()[-1])

    def test_the_banner_and_each_run_carry_the_arguments_after_the_seed(self) -> None:
        self.devtest(LOGGING)
        done = self.shuffled("--skip-live-youtube")
        self.assertEqual("devtest-shuffled: seed 7 — 3 classes, each alone — reproduce with "
                         "android/scripts/devtest-shuffled.sh 7 --skip-live-youtube", done.stdout.splitlines()[0])
        self.assertEqual(["--skip-live-youtube a.Three", "--skip-live-youtube a.One", "--skip-live-youtube a.Two"],
                         self.read("log.args").splitlines())
        self.assertIn("::group::a.Three", done.stdout.splitlines())
        self.assertEqual(3, done.stdout.splitlines().count("::endgroup::"))

    def test_a_failing_class_keeps_its_own_report_and_goes_to_order_failed_file(self) -> None:
        self.devtest(REPORTING)
        done = self.shuffled(ORDER_FAILED_FILE=self.path("failed"))
        self.assertEqual(1, done.code, done.output)
        self.assertEqual("a.Two\n", self.read("failed"))
        self.assertEqual("report of a.Two\n", self.read("layout/android/app/build/order-independence/a.Two/reports/index.html"))
        self.assertEqual("result of a.Two\n", self.read("layout/android/app/build/order-independence/a.Two/results/TEST.xml"))
        self.assertEqual(["a.Two"], sorted(os.listdir(self.kept)), "only failing classes are kept")
        self.assertIn("devtest-shuffled: 1 of 3 classes fail alone under seed 7:", done.stdout)
        self.assertIn("  a.Two", done.stdout.splitlines())

    def test_a_previous_runs_kept_reports_are_cleared(self) -> None:
        self.devtest(LOGGING)
        self.write("layout/android/app/build/order-independence/a.Old/reports/index.html", "stale\n")
        self.assertEqual(0, self.shuffled().code)
        self.assertFalse(os.path.exists(self.kept))

    def test_no_test_class_is_an_error(self) -> None:
        self.devtest(LOGGING)
        shutil.rmtree(self.path("layout/android/app/src/androidTest/java/a"))
        os.makedirs(self.path("layout/android/app/src/androidTest/java"), exist_ok=True)
        done = self.shuffled()
        self.assertEqual(1, done.code, done.output)
        self.assertIn("devtest-shuffled: no androidTest classes under ", done.stderr)

    def test_no_seed_is_a_usage_error(self) -> None:
        done = self.run_tool(tool=self.tool)
        self.assertEqual(1, done.code, done.output)
        self.assertIn("usage: devtest-shuffled.sh <seed> [--skip-live-youtube]", done.stderr)


if __name__ == "__main__":
    unittest.main()
