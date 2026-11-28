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
from harness import cli_harness

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

    def test_a_waiting_run_says_its_place_in_the_governors_queue(self) -> None:
        hold, status, stats = self.path("admit"), self.path("status"), self.path("stats")
        with open(status, "w") as f:
            f.write("governor: 5 admitted, 15.0/77 GB committed, 15/18 cores charged\n"
                    "  WAITING agent    2.0 GB    8 cores  run-script-tests  pid 1\n"
                    "  WAITING agent    2.0 GB    8 cores  run-script-tests  pid PID\n")
        with open(stats, "w") as f:
            f.write("  class admits  p50 wait  p90 wait waited>1m\n  agent   4751        0s        7s       208\n")
        env = self.env(self.governed(self.agentctl, {"SEEN": self.seen, "GOV_HOLD": hold, "GOV_STATUS": status,
                                                     "GOV_STATS": stats, "KINOWO_GOV_QUEUE_SECONDS": "0.2"}))
        log = self.path("run-stderr")
        with open(log, "w") as err:
            run = subprocess.Popen([self.TOOL, "--root", self.tree, "scripts/sees-test.sh"], env=env, cwd=self.work,
                                   stdout=subprocess.DEVNULL, stderr=err, stdin=subprocess.DEVNULL)
            self.assertTrue(cli_harness.wait_until(lambda: "2nd of 2 waiting" in self.read("run-stderr")))
            self.write("admit", "")
            self.assertEqual(0, run.wait(cli_harness.WAIT_BOUND))
        said = self.read("run-stderr")
        self.assertIn("run-script-tests: queued at the governor, 2nd of 2 waiting (5 admitted, 15/18 cores charged)", said)
        self.assertIn("agent class's admissions wait 0s at the median, 7s at the 90th percentile", said)
        self.assertEqual(1, said.count("queued at the governor"), "said once while the place is the same")

    def test_under_a_parents_ticket_it_rides_it(self) -> None:
        done = self.run_tool("--job", "outer", "--", self.TOOL, "--root", self.tree, "scripts/sees-test.sh",
                             tool=os.path.join(ROOT, "scripts/gov-run"),
                             env=self.governed(self.agentctl, {"SEEN": self.seen}))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual((1, NO_TICKET), (self.gov_calls().count("gov admit"), self.read("seen").strip()))

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
        self.assertIn("2 script test(s), 2 at a time, within 3 cores", done.stdout)
        self.wide("8", {**grants, "GOV_GRANT": "2"})
        self.assertEqual((1, "width=1"), (self.gov_calls().count("--min-cores 1\n"), self.read("seen").strip()))

    def test_wide_tests_share_the_grant_as_a_budget_not_a_reservation(self) -> None:
        """2026-11-27: a 7-core grant, wide tests' extra 6 -- the pool was ONE wide for 13 minutes. Granted 5 cores
        with two 3-wide tests: they never run together (6 > 5), the one core beside one wide test is used, and the
        rest run beside each other."""
        log = self.path("held")
        body = 'echo "+${AGENTCTL_GOV_WIDTH}" >> "$HELD"; sleep 1; echo "-${AGENTCTL_GOV_WIDTH}" >> "$HELD"'
        for n in (1, 2):
            self.put(f"scripts/budget/w{n}-test.sh", f"# script-test-width: 3\n{body}")
        for n in (1, 2, 3, 4):
            self.put(f"scripts/budget/p{n}-test.sh", body)
        done = self.runner("scripts/budget", env=self.governed(self.agentctl, {
            "GOV_FEATURES": "admit cores route width grant", "GOV_GRANT": "5", "HELD": log, "CORES": "16",
            "PATH": self.path("cores-bin") + os.pathsep + self.env()["PATH"]}))
        self.assertEqual(0, done.code, done.output)
        self.assertIn("6 script test(s), 6 at a time, within 5 cores", done.stdout)
        held, peak, tests, most = 0, 0, 0, 0
        for line in self.read("held").split():
            held += int(line[1:]) * (1 if line[0] == "+" else -1)
            tests += 1 if line[0] == "+" else -1
            peak, most = max(peak, held), max(most, tests)
        self.assertEqual((5, 3), (peak, most), "the cores held at once, the tests at once")

    def peaks(self, spare: bool) -> int:
        """The most tests running at once of four, under a grant of ONE core with `spare` cores free (or none)."""
        body = 'echo "+" >> "$HELD"; sleep 2; echo "-" >> "$HELD"'
        for n in range(4):
            self.put(f"scripts/widen/w{n}-test.sh", body)
        free = self.write("spare", "") if spare else self.path("no-spare")
        self.write("gov-calls", "")
        done = self.runner("--jobs", "4", "scripts/widen", env=self.governed(self.agentctl, {
            "GOV_FEATURES": "admit cores route width grant", "GOV_GRANT": "1", "GOV_SPARE": free,
            "HELD": self.path("held")}))
        self.assertEqual(0, done.code, done.output)
        held = 0
        peak = 0
        for line in self.read("held").split():
            held += 1 if line == "+" else -1
            peak = max(peak, held)
        return peak

    def test_a_run_granted_one_core_takes_more_as_the_governor_has_them_free(self) -> None:
        """2026-11-27: admitted at load 28 with ONE core, a run stayed one wide for its 40 minutes while the laptop
        freed up. It asks for a core each time it has tests waiting, `gov admit --try`, and runs one more test on each."""
        self.assertEqual(4, self.peaks(spare=True))
        self.assertEqual(3, self.gov_calls().count("--cores 1 --try"))

    def test_a_run_granted_one_core_stays_one_wide_while_none_is_free(self) -> None:
        self.assertEqual(1, self.peaks(spare=False))
        self.assertEqual(1, self.gov_calls().count("--cores 1 --try"), "asked again only after a pause")


if __name__ == "__main__":
    unittest.main()
