"""scripts/sbt-warm, black-box (ported from sbt-warm-test.sh; sbt is a stub, sbt_warm_fixture.py):
NO PROGRESS -- a call whose server and client use no CPU is ended (thread dump, server stopped, one retry in a
fresh server, then exit 75 and THE STALL SIGNAL), and a client whose server vanished fails fast.
Run: python3 scripts/sbt_warm_stall_test.py
"""
# script-test-stress: clients and polls racing a loaded machine's clock (Order independence #74; f69ccf47f, d56231908)
from __future__ import annotations

import glob
import unittest

from sbt_warm_stall_fixture import HANG, StallCase


class NoProgressTest(StallCase):
    def test_a_call_making_no_progress_fails_with_the_stall_signal_after_one_retry(self) -> None:
        self.stub(HANG)
        server = self.fake_server(self.stall)
        done = self.warm(self.stall, "common/Test/compile",
                         env={"KINOWO_SBT_WARM_STALL_SECONDS": "2", "KINOWO_STALL_FILE": self.path("stalled")})
        self.assertEqual(75, done.code, done.output)
        self.assertRegex(done.stderr, r"(?m)^sbt-warm STALLED: common/Test/compile \(no progress for 2s, twice")
        self.assertIn("once more, in a fresh server", done.stderr)
        self.assertRegex(done.stderr, r"(?m)^\[sbt-warm\].* \.\.\.its processes: *[0-9]+ [A-Z][^ ]* [0-9:.]+ ")
        self.wait_gone(server, "the stalled server")
        self.assertRegex(self.stops(), r"pid=%d .*no progress for" % server)
        self.assertNotEqual([], glob.glob(self.state + "/*.stall-*.txt"))
        self.assertEqual("sbt-warm common/Test/compile", self.text(self.path("stalled")))
        self.assertEqual(0, self.left_running())

    def test_a_stall_the_fresh_server_clears_is_the_calls_result(self) -> None:
        self.stub(HANG)
        self.fake_server(self.stall)
        done = self.warm(self.stall, "compile",
                         env={"KINOWO_SBT_WARM_STALL_SECONDS": "2", "STUB_HANG_ONCE": self.path("hung-once")})
        self.assertEqual((0, "served\n"), (done.code, done.stdout), done.output)
        self.assertEqual(0, self.left_running())

    def test_a_client_whose_server_vanished_fails_fast_term_ignored_or_not(self) -> None:
        self.stub("#!/usr/bin/env bash\ntrap '' TERM\necho $$ >> \"@S@/hung\"; while :; do sleep 1; done\n")
        server = self.fake_server(self.stall)
        killer = self.background(["bash", "-c", 'while [ ! -s "$1" ]; do sleep 0.1; done; kill "$2"', "_",
                                  self.path("hung"), str(server)])
        done = self.warm(self.stall, "common/testOnly x", env={
            "KINOWO_SBT_WARM_STALL_SECONDS": "600", "KINOWO_SBT_WARM_LIVENESS_SECONDS": "1",
            "KINOWO_SBT_WARM_START_SECONDS": "4"}, bound=300)
        killer.wait()
        self.assertEqual(75, done.code, done.output)
        self.assertRegex(done.stderr, r"(?m)^sbt-warm STALLED: common/testOnly x \(its server vanished")
        self.assertEqual(0, self.left_running())


if __name__ == "__main__":
    unittest.main()
