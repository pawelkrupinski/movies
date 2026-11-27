"""scripts/run-script-tests, black-box (ported from run-script-tests-test.sh): THE GOVERNOR -- the run waits for its ticket
(a starved laptop), charged its parallelism, then starts by itself; it rides a parent's ticket, takes none in a check's
hook, with KINOWO_GOVERNOR=0 or off the Mac; its tests see no ticket of its own (descriptor 7 closed, KINOWO_JVM_SLOT_HELD
unset) and hold one core each (AGENTCTL_GOV_WIDTH=1); a test declaring `# script-test-width: N` runs that wide when the
run's own ticket counts it; and THE GRANTED WIDTH: a governor that grants is asked --min-cores 1, the run fits in it.
Run: python3 scripts/run_script_tests_governor_test.py"""
from __future__ import annotations

import os
import re
import subprocess
import unittest
from typing import Dict, Optional

from run_script_tests_fixture import ROOT, ScriptTestsCase
import cli_harness

SEES = ('echo "fd7 $({ : >&7; } 2>/dev/null && echo open || echo closed) held=${KINOWO_GOV_CPU_HELD:-} '
        'slot=${KINOWO_JVM_SLOT_HELD:-} width=${AGENTCTL_GOV_WIDTH:-}" > "$SEEN"')
NO_TICKET = "fd7 closed held= slot= width=1"
GETCONF = '#!/bin/sh\n[ "$1" = _NPROCESSORS_ONLN ] && { echo "$CORES"; exit 0; }\nexec /usr/bin/getconf "$@"\n'


class GovernorTest(ScriptTestsCase):
    def setUp(self) -> None:
        super().setUp()
        self.agentctl = self.governor()
        self.put("scripts/sees-test.sh", SEES)
        self.seen = self.path("seen")

    def admits(self, pattern: str) -> int:
        return len(re.findall(pattern, self.gov_calls(), re.M))

    def test_the_run_waits_for_its_ticket_then_starts_by_itself(self) -> None:
        hold = self.path("admit")
        env = self.env(self.governed(self.agentctl, {"SEEN": self.seen, "GOV_HOLD": hold}))
        run = subprocess.Popen([self.TOOL, "--root", self.tree, "--jobs", "3", "scripts/sees-test.sh",
                                            "scripts/a-test.sh"], env=env, cwd=self.work,
                                           stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                                           stdin=subprocess.DEVNULL)
        self.assertTrue(cli_harness.wait_until(lambda: os.path.exists(hold + ".waiting")))
        self.assertFalse(os.path.exists(self.seen))
        self.write("admit", "")
        self.assertEqual(0, run.wait(cli_harness.WAIT_BOUND))
        self.assertEqual(NO_TICKET, self.read("seen").strip())
        self.assertEqual(1, self.admits(r"^gov admit .*--class agent --mem 2 --label run-script-tests .*"
                                        r"--job script-tests --cores 2$"))

    def test_under_a_parents_ticket_it_rides_it(self) -> None:
        done = self.run_tool("--job", "outer", "--", self.TOOL, "--root", self.tree, "scripts/sees-test.sh",
                             tool=os.path.join(ROOT, "scripts/gov-run"),
                             env=self.governed(self.agentctl, {"SEEN": self.seen}))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual((1, NO_TICKET), (len(self.gov_calls().splitlines()), self.read("seen").strip()))

    def test_no_ticket_in_a_checks_hook_with_the_governor_off_or_off_the_mac(self) -> None:
        for extra, seen in (({"KINOWO_GOV_CPU_HELD": "1"}, "fd7 closed held=1 slot= width=1"),
                            ({"KINOWO_GOVERNOR": "0"}, "fd7 closed held= slot= width="),
                            ({"KINOWO_JVM_GATE_OS": "Linux"}, "fd7 closed held= slot= width=")):
            done = self.runner("scripts/sees-test.sh", env=self.governed(self.agentctl, {"SEEN": self.seen, **extra}))
            self.assertEqual(0, done.code, done.output)
            self.assertEqual(seen, self.read("seen").strip(), extra)
        self.assertEqual("", self.gov_calls())

    def test_linux_runs_its_container_under_one_ticket(self) -> None:
        docker = self.write("docker-bin/docker", '#!/bin/sh\necho "$*" >> "%s"\ncase "$1" in run) cat > /dev/null ;; '
                                                 'esac\n' % self.path("docker-calls"))
        os.chmod(docker, 0o755)
        done = self.runner("--linux", "scripts/ci/b_test.py", env=self.governed(self.agentctl, {
            "HOME": self.path("home"), "PATH": self.path("docker-bin") + os.pathsep + self.env()["PATH"]}))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, self.admits(r"--label run-script-tests .*--job script-tests --cores 1$"))
        self.assertEqual(1, len(re.findall(r"(?m)^run ", self.read("docker-calls"))))

    def test_under_a_ticket_of_width_2_the_pool_runs_2_wide_taking_none(self) -> None:
        done = self.runner("--jobs", "8", "scripts/sees-test.sh", "scripts/a-test.sh", "scripts/ci/b_test.py",
                           env=self.governed(self.agentctl, {"SEEN": self.seen, "AGENTCTL_GOV_WIDTH": "2"}))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(0, self.admits("^gov admit"))
        self.assertIn("3 script test(s), 2 at a time", done.stdout)
        self.assertEqual(NO_TICKET, self.read("seen").strip())


class WidthTest(ScriptTestsCase):
    def setUp(self) -> None:
        super().setUp()
        self.agentctl = self.governor()
        self.put("scripts/wide-test.sh", '# script-test-width: 3\necho "width=${AGENTCTL_GOV_WIDTH:-}" > "$SEEN"')
        os.chmod(self.write("cores-bin/getconf", GETCONF), 0o755)

    def wide(self, cores: str, extra: Optional[Dict[str, Optional[str]]] = None) -> cli_harness.Done:
        self.write("gov-calls", "")
        return self.runner("scripts/wide-test.sh", "scripts/a-test.sh", env=self.governed(self.agentctl, {
            "SEEN": self.path("seen"), "CORES": cores,
            "PATH": self.path("cores-bin") + os.pathsep + self.env()["PATH"], **(extra or {})}))

    def test_a_wide_test_is_charged_its_extra_while_it_fits_the_machine(self) -> None:
        self.assertEqual(0, self.wide("8").code)
        self.assertEqual((1, "width=3"), (self.gov_calls().count("--job script-tests --cores 4\n"),
                                          self.read("seen").strip()))
        self.wide("3")
        self.assertEqual((1, "width=1"), (self.gov_calls().count("--job script-tests --cores 2\n"),
                                          self.read("seen").strip()))
        done = self.runner("scripts/wide-test.sh", env=self.governed(self.agentctl, {
            "SEEN": self.path("seen"), "AGENTCTL_GOV_WIDTH": "2"}))
        self.assertEqual((0, "width=1"), (done.code, self.read("seen").strip()))

    def test_the_granted_width_the_run_fits_in_what_the_governor_grants(self) -> None:
        grants = {"GOV_FEATURES": "admit cores route width grant"}
        done = self.wide("8", {**grants, "GOV_GRANT": "3"})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, self.gov_calls().count("--job script-tests --cores 4 --min-cores 1\n"))
        self.assertEqual("width=3", self.read("seen").strip())
        self.assertIn("2 script test(s), 1 at a time", done.stdout)
        self.wide("8", {**grants, "GOV_GRANT": "2"})
        self.assertEqual((1, "width=1"), (self.gov_calls().count("--min-cores 1\n"), self.read("seen").strip()))


if __name__ == "__main__":
    unittest.main()
