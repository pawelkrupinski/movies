"""scripts/sbt-warm, black-box (ported from sbt-warm-test.sh; sbt is a stub, sbt_warm_fixture.py):
NO PROGRESS -- a busy or winding-down client is no stall while one only spawning short-lived children is.
Run: python3 scripts/sbt_warm_progress_test.py
"""
# script-test-stress: clients and polls racing a loaded machine's clock (Order independence #74; f69ccf47f, d56231908)
from __future__ import annotations

import os
import unittest
from typing import Mapping, Optional

from sbt_warm_stall_fixture import BUSY, WINDING, SLOW_PS, StallCase


class ProgressTest(StallCase):
    def busy_call(self, bound: str, env: Optional[Mapping[str, str]] = None) -> str:
        done = self.warm(self.stall, "compile",
                         env={"KINOWO_SBT_WARM_STALL_SECONDS": bound, "KINOWO_STALL_CPU_CENTIS": "10", **(env or {})})
        said = f"{done.code} {done.stdout.strip()}"
        return said if said == "0 done" else f"{said} | sbt-warm said: {done.stderr}"

    def test_a_command_busy_past_the_bound_is_no_stall_and_touches_the_progress_file(self) -> None:
        self.stub(BUSY)
        listing = self.shimmed("slow-listing", '#!/usr/bin/env bash\ncase " $* " in *" -axo pid=,ppid= "*) sleep 2.5 ;; '
                                               'esac\nexec @PS@ "$@"\n')
        self.assertEqual("0 done", self.busy_call("2", {"KINOWO_PROGRESS_FILE": self.path("progress"), "PATH": listing}))
        self.assertTrue(os.path.exists(self.path("progress")))

    def test_a_client_ending_is_no_stall_even_while_a_poll_measures_it(self) -> None:
        self.stub(BUSY)
        self.assertEqual("0 done", self.busy_call("1"))
        self.stub(WINDING)
        self.assertEqual("0 done", self.busy_call("1", {"PATH": self.shimmed("slow-ps", SLOW_PS)}))

    def test_a_client_that_only_spawns_short_lived_children_is_a_stall(self) -> None:
        self.stub('#!/usr/bin/env bash\necho $$ >> "@S@/hung"; while :; do sleep 0.3; done\n')
        measure = self.shimmed("slow-measure", '#!/usr/bin/env bash\ncase " $* " in *" -o time= "*) sleep 0.6 ;; esac\n'
                                               'exec @PS@ "$@"\n')
        done = self.warm(self.stall, "compile", env={"KINOWO_SBT_WARM_STALL_SECONDS": "2", "PATH": measure})
        self.assertEqual(75, done.code, done.output)
        self.assertRegex(done.stderr, r"(?m)^sbt-warm STALLED: compile")
        self.assertEqual(0, self.left_running())


if __name__ == "__main__":
    unittest.main()
