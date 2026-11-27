#!/usr/bin/env python3
"""END TO END: scripts/sbt-warm over a REAL worktree of a clone of this checkout (scripts/e2e/e2e_world.py) with a stub
`sbt` at the process boundary (scripts/e2e/sbt_stub.py): `sbt --client <command>` starts a stand-in server process when
there is none, and fails, or reports a stale TASTy once, as the scenario says. The real keeper, slot lock and
scripts/ci/sbt_guard.py run.

  - a first call starts ONE server and a keeper holding its JVM slot (a kernel lock); a second call reuses the server
    (one start in all) and --status names its pid;
  - a command that fails exits non-zero; a stale-TASTy failure is retried after `staleTastyDrop` and passes;
  - --stop ends the server and the keeper, frees the slot, and a later --status says no server.
Mutations (scripts/e2e/mutations): --stop leaving the server running; a stale-TASTy failure not retried."""
import fcntl
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))
import e2e_world  # noqa: E402
import sbt_stub  # noqa: E402


class SbtWarmE2ETest(e2e_world.Scenario):
    def setUp(self) -> None:
        super().setUp()
        self.repo = self.w.movies_clone()
        self.tree = self.repo.worktree("warm")
        self.sbt = sbt_stub.StubSbt(self.w)
        self.addCleanup(self.sbt.stop, self.tree)

    def warm(self, *args: str) -> e2e_world.cli_harness.Done:
        return self.w.run([os.path.join(self.tree, "scripts/sbt-warm"), *args], self.tree, env=self.sbt.env(), bound=120)

    def alive(self, pid: int) -> bool:
        return e2e_world.cli_harness.alive(pid)

    def slot_state(self) -> str:
        path = os.path.join(self.w.root, "identity", "jvm-slots", "1.flock")
        if not os.path.exists(path):
            return "free"
        with open(path, "a") as f:
            try:
                fcntl.flock(f, fcntl.LOCK_EX | fcntl.LOCK_NB)
            except OSError:
                return "held"
        return "free"

    def test_one_server_is_kept_between_calls_and_stop_frees_everything(self) -> None:
        self.assertRan(self.warm("compile"), 0, "starting one")
        (server,) = self.sbt.servers()
        self.assertTrue(self.alive(server))
        self.assertEqual("held", self.slot_state(), "the keeper does not hold the server's JVM slot")
        self.assertRan(self.warm("test:compile"), 0)
        self.assertEqual([server], self.sbt.servers(), "a second call started a second server")
        self.assertRan(self.warm("--status"), 0, f"this checkout ({self.tree}): server pid {server}")
        self.assertEqual(["--client", "compile"], self.w.calls("sbt")[0]["argv"][:2])
        self.sbt.set_mode("fail")
        self.assertNotEqual(0, self.warm("compile").code, "a failing sbt command exited 0")
        self.sbt.set_mode("stale-once")
        self.assertRan(self.warm("compile"), 0)
        self.assertIn(["--client", "staleTastyDrop"], [c["argv"] for c in self.w.calls("sbt")])
        self.assertRan(self.warm("--stop"), 0)
        self.assertTrue(e2e_world.cli_harness.wait_until(lambda: not self.alive(server)), "--stop left the server running")
        self.assertTrue(e2e_world.cli_harness.wait_until(lambda: self.slot_state() == "free"), "the slot stayed held")
        self.assertRan(self.warm("--status"), 0, f"this checkout ({self.tree}): no server")


if __name__ == "__main__":
    unittest.main()
