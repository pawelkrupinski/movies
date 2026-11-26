"""scripts/sbt-warm, black-box (ported from sbt-warm-test.sh): NO PROGRESS -- a call whose server and client use no CPU
is ended (thread dump, server stopped, one retry in a fresh server, then exit 75 and THE STALL SIGNAL), a client whose
server vanished fails fast, a busy or winding-down client is no stall while one only spawning short-lived children is;
and ONE COMMAND AT A TIME -- a call meeting a live serve fails at once, BLOCKED, and an orphaned serve's server is
stopped. sbt is a stub (sbt_warm_fixture.py).
Run: python3 scripts/sbt_warm_stall_test.py"""
# script-test-stress: clients and polls racing a loaded machine's clock (Order independence #74; f69ccf47f, d56231908)
from __future__ import annotations

import glob
import os
import re
import unittest
from typing import Mapping, Optional

from sbt_warm_fixture import SbtWarmCase
import cli_harness

HANG = '#!/usr/bin/env bash\nif [ -n "${STUB_HANG_ONCE:-}" ] && [ -f "$STUB_HANG_ONCE" ]; then echo served; exit 0; fi\n' \
       '[ -n "${STUB_HANG_ONCE:-}" ] && touch "$STUB_HANG_ONCE"\necho $$ >> "@S@/hung"; exec sleep 86400\n'
BUSY = '#!/usr/bin/env bash\nend=$((SECONDS + 4))\nwhile [ "$SECONDS" -lt "$end" ] ||\n' \
       '  { [ -n "${KINOWO_PROGRESS_FILE:-}" ] && [ ! -e "$KINOWO_PROGRESS_FILE" ] && [ "$SECONDS" -lt 60 ]; }; do :; done\n' \
       'echo done\n'
WINDING = '#!/usr/bin/env bash\nend=$((SECONDS + 2)); while [ "$SECONDS" -lt "$end" ]; do :; done\n' \
          'touch "@S@/winding-down"\nend=$((SECONDS + 3)); while [ "$SECONDS" -lt "$end" ]; do :; done; echo done\n'
# a `ps` that holds every CPU measure made once the stub winds down until the processes it names have ended
SLOW_PS = r'''#!/usr/bin/env bash
measure=0 pids="" previous=""
for argument in "$@"; do
  [ "$previous $argument" = "-o time=" ] && measure=1
  [ "$previous" = -p ] && pids=$argument
  previous=$argument
done
if [ "$measure" = 1 ] && [ -n "$pids" ] && [ -e "@S@/winding-down" ]; then
  for _ in $(seq 1 100); do @PS@ -o stat= -p "$pids" 2> /dev/null | grep -qv '^Z' || break; sleep 0.1; done
fi
exec @PS@ "$@"
'''


class StallCase(SbtWarmCase):
    def setUp(self) -> None:
        super().setUp()
        self.extra["KINOWO_STALL_POLL_SECONDS"] = "1"
        self.stall = self.worktree("stall")
        self.warm(self.stall, "--seed")
        self.ps = "/bin/ps"

    def shimmed(self, directory: str, body: str) -> str:
        """A PATH with `ps` shimmed by `body` (@PS@ the real one) first."""
        return self.shim(directory, "ps", body.replace("@PS@", self.ps)) + os.pathsep + self.env()["PATH"]

    def left_running(self) -> int:
        return cli_harness.still_running(self.path("hung"))


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
