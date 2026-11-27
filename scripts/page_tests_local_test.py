#!/usr/bin/env python3
"""scripts/page-tests-local, black-box: boots the fixture server through sbt-warm --serve, runs CI's Playwright rows
against its port, says a failing row and fails, and stops the server however it ends; on CI it installs and warms the
rows' browsers first; each row runs under a governor ticket of its own, charged its workers' cores. Fakes stand in
for sbt-warm, npx, npm, the browser install, node and agentctl: no real server or browser is ever started. Ported from
page-tests-local-test.sh case for case; the same cases hold the shell tool and its Python rewrite
(cli_harness.tool_path)."""
from __future__ import annotations

import json
import os
import re
import subprocess
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/page_tests_local.py", "scripts/page-tests-local")
# sbt-warm: --serve writes the port file its command names and serves until killed; --stop is recorded
SBT_WARM = '''
import signal
if argv[:1] == ["--serve"]:
    with open(env["SERVED"], "a") as f:
        f.write(f"{os.getpid()}\\n")
    with open(argv[1].split(" ")[-1], "w") as f:
        f.write("4321\\n")
    signal.pause()
'''
# npx: the base URL, the workers and whether it holds a governor ticket (descriptor 7) noted; fails on $NPX_FAILS
NPX = '''
held = bool(env.get("KINOWO_JVM_SLOT_HELD"))
try:
    os.fstat(7)
except OSError:
    held = False
with open(env["WORK"] + "/npx.log", "a") as f:
    f.write(f"{env.get('KINOWO_BASE_URL', '')} {env.get('PW_WORKERS', '')} {'ticket' if held else '-'}\\n")
if env.get("NPX_FAILS") and env["NPX_FAILS"] in " ".join(argv):
    exit(1)
'''
# agentctl's governor (what jvm-gate.sh greps for: def cmd_gov, def gov_route): `gov admit` (logged as every fake's call); while $GOV_HOLD is not
# there yet (a starved laptop) it says so (<GOV_HOLD>.waiting) and blocks on <GOV_HOLD>.fifo until admitted, then takes
# the ticket's lock on descriptor 7, as the real one does
AGENTCTL = '''
import fcntl
if argv[:2] == ["gov", "probe"]:  # the probe jvm-gate.sh's governor_bin reads, as the real one answers it
    print("agentctl governor: admit cores route width")
    exit(0)
hold = env.get("GOV_HOLD")
if hold and not os.path.exists(hold):
    open(hold + ".waiting", "w").close()
    open(hold + ".fifo").close()
fcntl.flock(7, fcntl.LOCK_EX | fcntl.LOCK_NB)
'''
PHONES = ["--project", "chromium-galaxy-s10*", "--project", "chromium-galaxy-s25*", "--project",
          "chromium-pixel-9-pro*", "--grep-invert", "axe-core|@agnostic"]
BEHAVIOUR = ["--project", "chromium-galaxy-s10", "--grep", "@agnostic", "--grep-invert", "axe-core"]
DESKTOP = ["--project", "chrome-desktop", "--grep-invert", "axe-core|@agnostic"]
WEBKIT = ["--project", "webkit-iphone-se*", "--project", "webkit-iphone-13*", "--project", "webkit-iphone-17-pro-max*",
          "--project", "webkit-desktop", "--grep-invert", "axe-core|@agnostic"]
CHROMIUM_ROWS = [["playwright", "test", *row] for row in (PHONES, BEHAVIOUR, DESKTOP)]
ALL_ROWS = CHROMIUM_ROWS + [["playwright", "test", *WEBKIT]]
PLAYWRIGHT_DIR = os.path.join(cli_harness.ROOT, "page-tests-playwright")


class PageTestsLocalTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.program("sbt-warm", SBT_WARM)
        self.fakes.program("npx", NPX)
        self.fakes.script("npm", ":")
        self.fakes.script("install", ":")
        self.fakes.script("node", ":")
        self.agentctl = self.fakes.program("agentctl", "# def cmd_gov def gov_route\n" + AGENTCTL)
        self.addCleanup(cli_harness.still_running, self.path("served"), 0)

    def fake(self, name: str) -> str:
        return os.path.join(self.fakes.bin, name)

    def tool_env(self, extra: Optional[Dict[str, Optional[str]]] = None) -> Dict[str, Optional[str]]:
        env: Dict[str, Optional[str]] = {
            "WORK": self.work, "SERVED": self.path("served"), "FIXTURE_POLL_SECONDS": "0",
            "PAGE_TESTS_SBT_WARM": self.fake("sbt-warm"), "PAGE_TESTS_NPX": self.fake("npx"),
            "PAGE_TESTS_NPM": self.fake("npm"), "PAGE_TESTS_INSTALL": self.fake("install"),
            "PAGE_TESTS_NODE": self.fake("node"), "CI": None, "PW_WORKERS": None}
        env.update(extra or {})
        return env

    def page_tests(self, *args: str, **extra: Optional[str]) -> cli_harness.Done:
        self.fakes.clear()
        if os.path.exists(self.path("npx.log")):
            os.remove(self.path("npx.log"))
        return self.run_tool(*args, env=self.tool_env(extra))

    def calls(self, name: str) -> List[List[str]]:
        return self.fakes.calls(name)

    def npx_runs(self) -> List[str]:
        """Each npx call's base URL, workers and ticket ("-" when none), in call order."""
        if not os.path.exists(self.path("npx.log")):
            return []
        return self.read("npx.log").splitlines()

    def served_alive(self) -> int:
        return cli_harness.still_running(self.path("served"), 5)

    def test_all_rows_pass_against_the_fixture_server_which_is_stopped_afterwards(self) -> None:
        done = self.page_tests()
        self.assertEqual(0, done.code, done.output)
        serves = [c for c in self.calls("sbt-warm") if c[:1] == ["--serve"]]
        self.assertEqual(1, len(serves), "one fixture server, served by the kept sbt server")
        self.assertRegex(serves[0][1], r"^web/PageTest/runMain tools\.FixtureServerMain /.*/port$")
        self.assertEqual(ALL_ROWS, self.calls("npx"), "CI's rows: three phones, @agnostic once, chrome-desktop, WebKit")
        self.assertEqual(["http://127.0.0.1:4321 5 -"] * 3 + ["http://127.0.0.1:4321 1 -"], self.npx_runs(),
                         "every row against its port; CI's five workers, the WebKit row one")
        self.assertEqual({PLAYWRIGHT_DIR}, set(self.npx_cwds()), "the rows run in page-tests-playwright")
        self.assertIn(["--stop"], self.calls("sbt-warm"), "the kept sbt server is stopped")
        self.assertEqual(0, self.served_alive(), "no serving process is left")

    def npx_cwds(self) -> List[str]:
        with open(self.fakes.log) as f:
            entries = [json.loads(line) for line in f if line.strip()]
        return [os.path.realpath(e["cwd"]) for e in entries if e["name"] == "npx"]

    def test_the_serve_names_this_run_its_owner(self) -> None:
        self.fakes.program("sbt-warm", "with open(env['WORK'] + '/owner', 'a') as f:\n"
                                       "    f.write(env.get('KINOWO_SBT_WARM_SERVE_OWNER', '') + '\\n')\n" + SBT_WARM)
        done = self.page_tests("chromium")
        self.assertEqual(0, done.code, done.output)
        owners = self.read("owner").splitlines()
        self.assertTrue(owners[0].isdigit(), f"the serve's owner is a pid: {owners}")

    def test_the_lines_it_prints(self) -> None:
        done = self.page_tests("chromium", NPX_FAILS="chrome-desktop")
        self.assertIn("fixture server listening on port 4321 (launch 1)\n", done.stdout)
        self.assertIn("==> chromium phones: playwright test " + " ".join(PHONES) + "\n", done.stdout)
        self.assertRegex(done.stdout, r"==> chromium phones ok \(\d+s\)\n")
        self.assertRegex(done.stdout, r"==> chromium behaviour ok \(\d+s\)\n")
        self.assertIn("==> chrome desktop: playwright test " + " ".join(DESKTOP) + "\n", done.stdout)
        self.assertRegex(done.stdout, r"==> chrome desktop FAILED \(\d+s\)\n")

    def test_the_rows_each_argument_selects(self) -> None:
        self.assertEqual(0, self.page_tests("chromium").code)
        self.assertEqual(CHROMIUM_ROWS, self.calls("npx"), "chromium: the Chromium rows only")
        done = self.page_tests("webkit")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([["playwright", "test", *WEBKIT]], self.calls("npx"), "webkit: the WebKit row only")
        self.assertIn("==> webkit phones: playwright test " + " ".join(WEBKIT) + "\n", done.stdout)
        self.assertEqual(0, self.page_tests("all").code)
        self.assertEqual(ALL_ROWS, self.calls("npx"), "all: every row, as no argument")

    def test_a_failing_row_fails_the_run_and_the_rows_after_it_still_run(self) -> None:
        done = self.page_tests(NPX_FAILS="chrome-desktop")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(1, done.stdout.count("==> chrome desktop FAILED"))
        self.assertEqual(ALL_ROWS, self.calls("npx"), "the WebKit row after it still ran")
        self.assertIn(["--stop"], self.calls("sbt-warm"), "...and the server is stopped")
        self.assertEqual(0, self.served_alive())
        self.assertEqual([], self.calls("install") + self.calls("node"), "off CI it installs and warms nothing")

    def test_npm_ci_only_without_playwright_installed(self) -> None:
        self.assertEqual(0, self.page_tests("chromium").code)
        installed = os.access(os.path.join(PLAYWRIGHT_DIR, "node_modules", ".bin", "playwright"), os.X_OK)
        self.assertEqual([] if installed else [["ci", "--no-audit", "--no-fund", "--silent"]], self.calls("npm"))

    def test_on_ci_the_rows_browsers_are_installed_and_warmed(self) -> None:
        self.assertEqual(0, self.page_tests("chromium", CI="true").code)
        self.assertEqual([["chromium"]], self.calls("install"))
        self.assertEqual([["warm-browsers.mjs", "chromium"]], self.calls("node"))
        self.assertEqual(["http://127.0.0.1:4321 5 -"] * 3, self.npx_runs(), "five workers")
        self.assertEqual(0, self.page_tests("webkit", CI="true").code)
        self.assertEqual(([["webkit"]], [["warm-browsers.mjs", "webkit"]]), (self.calls("install"), self.calls("node")))
        self.assertEqual(0, self.page_tests(CI="true").code)
        self.assertEqual([["chromium", "webkit"]], self.calls("install"), "all: both engines")
        self.assertEqual("http://127.0.0.1:4321 1 -", self.npx_runs()[-1], "the WebKit row still one worker")

    def test_a_callers_workers_are_kept_but_webkit_runs_one(self) -> None:
        self.assertEqual(0, self.page_tests(PW_WORKERS="3").code)
        self.assertEqual(["http://127.0.0.1:4321 3 -"] * 3 + ["http://127.0.0.1:4321 1 -"], self.npx_runs())

    def test_an_unknown_row_is_a_usage_error(self) -> None:
        done = self.page_tests("firefox")
        self.assertEqual(2, done.code)
        self.assertEqual("usage: scripts/page-tests-local [chromium|webkit|all]\n", done.stderr)
        self.assertEqual([], self.calls("sbt-warm"), "nothing booted")

    def test_inside_another_checkout_it_refuses(self) -> None:
        other = self.repo("other")
        done = self.run_tool(env=self.tool_env(), cwd=other)
        self.assertEqual(2, done.code, done.output)
        self.assertIn(f"but this is {other}: run {other}/scripts/page-tests-local", done.stderr)
        self.assertEqual([], self.calls("sbt-warm"))

    def governed(self, extra: Dict[str, Optional[str]]) -> Dict[str, str]:
        env = self.env(self.tool_env({
            "KINOWO_GOVERNOR": "1", "KINOWO_GOVERNOR_BIN": self.agentctl, "KINOWO_JVM_GATE_OS": "Darwin",
            "AGENTCTL_GOV_DIR": self.path("gov"), "KINOWO_JVM_SLOT_HELD": None, **extra}))
        return env

    def gov_cores(self) -> List[str]:
        pattern = re.compile(r"^gov admit .*--mem 2 --label page-tests .*--job page-tests --cores (\d+)$")
        return [m.group(1) for m in (pattern.match(" ".join(c)) for c in self.calls("agentctl")) if m]

    def test_governed_each_row_waits_for_a_ticket_of_its_own(self) -> None:
        hold = self.path("admit")
        os.mkfifo(hold + ".fifo")
        self.fakes.clear()
        run = subprocess.Popen([TOOL], env=self.governed({"GOV_HOLD": hold}), cwd=self.work, text=True,
                               stdout=subprocess.PIPE, stderr=subprocess.STDOUT, start_new_session=True)
        try:
            self.assertTrue(cli_harness.wait_until(lambda: os.path.exists(hold + ".waiting")),
                            "the first row asked the governor")
            self.assertEqual([], self.calls("npx"), "a row not admitted has not started")
            open(hold, "w").close()
            with open(hold + ".fifo", "w"):
                pass
            output, _ = run.communicate(timeout=cli_harness.HANG_BOUND)
        finally:
            if run.poll() is None:
                cli_harness.kill_group(run)
                run.wait()
        self.assertEqual(0, run.returncode, output)
        self.assertEqual(["http://127.0.0.1:4321 5 ticket"] * 3 + ["http://127.0.0.1:4321 1 ticket"], self.npx_runs(),
                         "admitted, the rows start by themselves, each holding its ticket")
        self.assertEqual(["5", "5", "5", "1"], self.gov_cores(), "one ticket a row, charged its workers' cores")

    def test_governor_off_no_ticket(self) -> None:
        self.fakes.clear()
        done = cli_harness.run_bounded([TOOL, "chromium"], env=self.governed({"KINOWO_GOVERNOR": "0"}),
                                       cwd=self.work, stdin=None)
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(([], ["http://127.0.0.1:4321 5 -"] * 3), (self.calls("agentctl"), self.npx_runs()))


if __name__ == "__main__":
    unittest.main()
