"""scripts/sbt-warm, black-box (ported from sbt-warm-test.sh): ONE CALL, ITS CORES -- under the governor (a stub agentctl
noting each call) a server's keeper ticket is its memory, charged no cores, each command takes a cores ticket of its
own, and a whole-module test the governor would hold runs on ci-runner when `gov route` says so; and ONE BUILDER PER
target/ -- --exclusive runs its command holding the checkout's build lock exclusively, and a call meanwhile waits,
naming who holds it. sbt is a stub (sbt_warm_fixture.py).
Run: python3 scripts/sbt_warm_governor_test.py"""
from __future__ import annotations

import fcntl
import os
import re
import unittest
from typing import Dict, Optional

from sbt_warm_fixture import SbtWarmCase, lock_state
from harness import cli_harness

AGENTCTL = '''#!/usr/bin/env bash
# its probe answers as the real one's does (jvm-gate.sh governor_bin)
[ "$1 $2" = "gov probe" ] && { echo "agentctl governor: admit cores route width"; exit 0; }
echo "$*" >> "@S@/gov-calls"
[ "$1 $2" = "gov route" ] && { echo "${ROUTE:-local}: stub" >&2; [ "${ROUTE:-local}" = ci ]; exit; }
exit 0
'''


class GovernorTest(SbtWarmCase):
    def setUp(self) -> None:
        super().setUp()
        self.agentctl = self.shim("gov-bin", "agentctl", AGENTCTL) + "/agentctl"
        self.ci_sbt = self.shim("gov-bin", "ci-sbt", '#!/bin/sh\necho "$*" >> "@S@/ci-sbt-calls"\n') + "/ci-sbt"
        self.governed = self.worktree("governed")

    def call(self, *args: str, env: Optional[Dict[str, str]] = None) -> cli_harness.Done:
        governor = {"KINOWO_GOVERNOR": "1", "KINOWO_GOVERNOR_BIN": self.agentctl, "KINOWO_JVM_GATE_OS": "Darwin",
                    "KINOWO_GOV_CLASS": "agent", "KINOWO_CI_SBT": self.ci_sbt, "AGENTCTL_GOV_DIR": self.path("gov")}
        return self.warm(self.governed, *args, env={**governor, **(env or {})})

    def gov_calls(self) -> str:
        return self.text(self.path("gov-calls"))

    def ci_calls(self) -> str:
        return self.text(self.path("ci-sbt-calls"))

    def test_the_keeper_ticket_is_memory_and_each_command_takes_its_cores(self) -> None:
        self.fake_server(self.governed)
        self.assertEqual(0, self.call("worker/testOnly X").code)
        self.assertEqual(1, len(re.findall(r"--mem 5 --label sbt-warm .*--job keeper", self.gov_calls())), self.gov_calls())
        self.assertEqual(1, len(re.findall(r"--mem 0 --label sbt-warm testOnly .*--job testOnly", self.gov_calls())))
        self.assertRegex(self.gov_calls(), r"gov admit --fd 7 --ticket %s/t-[0-9]+-[0-9]+\.lock --class agent "
                                           % re.escape(self.path("gov")))
        self.assertRegex(self.gov_calls(), r"gov usage --job testOnly --wall [0-9]+ --cpu [0-9]+ --meter tree")
        open(self.path("gov-calls"), "w").close()
        self.call("worker/testOnly X", env={"KINOWO_GOV_CPU_HELD": "1"})
        self.assertNotIn("--label sbt-warm testOnly", self.gov_calls())

    def test_a_whole_module_test_the_governor_would_hold_runs_on_ci_runner(self) -> None:
        done = self.call("testUnit", env={"ROUTE": "ci"})
        self.assertEqual("-- testUnit", self.ci_calls())
        self.assertIn("ci-runner's measure lane is free: running it there (--local runs it here)", done.stderr)
        open(self.path("ci-sbt-calls"), "w").close()
        self.call("worker/testOnly X", env={"ROUTE": "ci"})
        self.call("--local", "testUnit", env={"ROUTE": "ci"})
        self.call("testUnit", env={"ROUTE": "ci", "KINOWO_GOV_CLASS": "check"})
        self.call("worker/test", env={"ROUTE": "local"})
        self.assertEqual("", self.ci_calls())
        self.call("--ci", "worker/testOnly X", "web/compile")
        self.assertEqual("-- worker/testOnly X; web/compile", self.ci_calls())


class ExclusiveTest(SbtWarmCase):
    def test_exclusive_holds_the_build_lock_for_its_commands_life(self) -> None:
        checkout = self.worktree("excl")
        busy = self.state_file(checkout, ".busy")
        go = self.path("go")
        holder = self.start(checkout, "--exclusive", "it-local", "bash", "-c",
                            'touch "$1.started"; while [ ! -e "$1" ]; do sleep 0.1; done', "_", go)
        self.assertTrue(cli_harness.wait_until(lambda: os.path.exists(go + ".started")))
        self.assertEqual("held", lock_state(busy))
        self.assertEqual(f"{holder.pid} it-local", self.text(self.state_file(checkout, ".exclusive")))
        waiter = self.start(checkout, "compile", stderr=self.path("waiter.err"))
        self.assertTrue(cli_harness.wait_until(lambda: "holds the build of" in self.text(self.path("waiter.err"))))
        self.assertIn(f"another sbt holds the build of {checkout} exclusively (it-local, pid {holder.pid}): waiting for "
                      "it to end", self.text(self.path("waiter.err")))
        self.assertEqual([], self.calls())
        self.put(go, "")
        self.assertEqual(0, holder.wait())
        self.assertEqual(0, waiter.wait())
        self.assertEqual([f"{checkout}|--client compile"], self.calls())

    def test_exclusive_gives_up_past_its_deadline_and_exec_s_the_command_status(self) -> None:
        checkout = self.worktree("excl")
        self.assertEqual(3, self.warm(checkout, "--exclusive", "me", "sh", "-c", "exit 3").code)
        with open(self.state_file(checkout, ".busy"), "a") as busy:
            fcntl.flock(busy, fcntl.LOCK_SH)
            done = self.warm(checkout, "--exclusive", "me", "true", env={"KINOWO_SBT_WARM_LOCK_SECONDS": "1"})
        self.assertEqual(75, done.code, done.output)
        self.assertIn(f"a command runs in the server of {checkout}: me waits for it to end before its own sbt builds "
                      "there", done.stderr)
        self.assertIn(f"gave up after 1s waiting for the server of {checkout} to go idle", done.stderr)


if __name__ == "__main__":
    unittest.main()
