"""scripts/sbt-warm, black-box (ported from sbt-warm-test.sh; sbt is a stub, sbt_warm_fixture.py):
ONE COMMAND AT A TIME -- a call meeting a live serve fails at once, BLOCKED, and an orphaned serve's server is
stopped.
Run: python3 scripts/sbt_warm_serve_test.py
"""
# script-test-stress: clients and polls racing a loaded machine's clock (Order independence #74; f69ccf47f, d56231908)
from __future__ import annotations

import glob
import re
import unittest

from sbt_warm_stall_fixture import StallCase
from harness import cli_harness


class ServeTest(StallCase):
    def setUp(self) -> None:
        super().setUp()
        self.stub('#!/usr/bin/env bash\ncase "$*" in *runMain*) echo $$ >> "@S@/hung"; exec sleep 86400 ;; esac\n'
                  'echo served\n')

    def test_a_call_meeting_a_live_serve_is_blocked_and_an_orphaned_serves_server_stopped(self) -> None:
        owner = self.background(["sleep", "600"])
        server = self.fake_server(self.stall)
        serve = self.start(self.stall, "--serve", "web/PageTest/runMain tools.FixtureServerMain port", env={
            "KINOWO_SBT_WARM_SERVE_OWNER": str(owner.pid), "KINOWO_SBT_WARM_OWNER_POLL": "0.2"})
        self.assertTrue(cli_harness.wait_until(lambda: glob.glob(self.state + "/*.serving") != []))
        done = self.warm(self.stall, "compile", env={"KINOWO_STALL_FILE": self.path("stalled")})
        self.assertEqual(75, done.code, done.output)
        self.assertRegex(done.stderr, r"(?m)^sbt-warm BLOCKED: .*FixtureServerMain")
        self.assertEqual("sbt-warm blocked by serving web/PageTest/runMain tools.FixtureServerMain port",
                         self.text(self.path("stalled")))
        owner.kill()
        owner.wait()
        self.assertTrue(cli_harness.wait_until(lambda: re.search(r"pid=%d .*which ended" % server, self.stops())
                                               is not None), self.stops())
        self.wait_gone(server, "the orphaned serve's server")
        serve.terminate()
        serve.wait()
        self.assertEqual(0, self.left_running())

    def test_a_server_still_serving_for_a_serve_that_is_gone_is_restarted(self) -> None:
        server = self.fake_server(self.stall)
        self.put(self.state_file(self.stall, ".serving"), "999999 - web/PageTest/runMain tools.FixtureServerMain port\n")
        done = self.warm(self.stall, "compile")
        self.assertEqual((0, "served\n"), (done.code, done.stdout), done.output)
        self.wait_gone(server, "the orphaned serve's server")


if __name__ == "__main__":
    unittest.main()
