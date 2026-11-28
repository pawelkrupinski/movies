"""scripts/sbt-warm, black-box (ported from sbt-warm-test.sh; sbt is a stub, sbt_warm_fixture.py):
a changed build or other KINOWO_* settings restart or reload the server, one server a checkout, and --stop, --reap
and --status act on this checkout's own server only.
Run: python3 scripts/sbt_warm_server_test.py
"""
from __future__ import annotations

import glob
import os
import re
import subprocess
import unittest

from sbt_warm_fixture import SbtWarmCase, lock_state, state_word
from harness import cli_harness


class ServerSettingsTest(SbtWarmCase):
    def test_a_changed_build_is_reloaded_first_once(self) -> None:
        other = self.worktree("other")
        server = self.fake_server(other)
        self.warm(other, "compile")
        self.put(other + "/build.sbt", "lazy val x = 1\n")
        self.warm(other, "compile")
        self.assertEqual("--client reload; compile", self.last_call())
        self.warm(other, "compile")
        self.assertEqual("--client compile", self.last_call())
        self.end_server(server)

    def test_other_kinowo_settings_restart_the_server_but_the_tool_knobs_do_not(self) -> None:
        other = self.worktree("other")
        self.warm(other, "compile")  # a fresh server's: the settings it starts with recorded
        server = self.fake_server(other)
        self.warm(other, "compile")
        self.assertEqual("running", state_word(server))
        # land-precheck's KINOWO_GOV_CLASS=check restarted the agent's warm server on every land (2026-11-22)
        self.warm(other, "compile", env={"KINOWO_GOV_CLASS": "check", "KINOWO_GOV_CORES": "4"})
        self.assertFalse(self.gone(server), "the governor's knobs restarted the server")
        done = self.warm(other, "compile", env={"KINOWO_IDENTITY_CORPUS_DIR": "/corpora"})
        self.wait_gone(server, "the restarted server")
        self.assertIn("settings differ", done.stderr)
        line = re.compile(r"^[0-9-]{10} [0-9:]{8} server pid=%d checkout=%s why=\"this call's KINOWO_.* settings "
                          r"differ.*\" by=[0-9]+:.*sbt-warm compile" % (server, re.escape(other)), re.M)
        self.assertEqual(1, len(line.findall(self.stops())), self.stops())

    def test_a_settings_restart_waits_for_another_calls_running_command(self) -> None:
        other = self.worktree("other")
        corpora = {"KINOWO_IDENTITY_CORPUS_DIR": "/corpora"}
        self.warm(other, "compile", env=corpora)  # a fresh server's: started with these settings
        server = self.fake_server(other)
        os.mkfifo(self.path("slow-go"))
        slow = self.start(other, "test", env={**corpora, "STUB_SLOW_SERVER": str(server)})
        self.assertTrue(cli_harness.wait_until(lambda: os.path.exists(self.path("slow-started"))))
        restart = self.start(other, "compile", stderr=self.path("restart.err"))
        self.assertTrue(cli_harness.wait_until(lambda: "waiting for it to end" in self.text(self.path("restart.err"))))
        with open(self.path("slow-go"), "w") as go:
            go.write("go\n")
        slow.wait()
        restart.wait()
        self.assertEqual("running", self.text(self.path("slow")))

class OneServerTest(SbtWarmCase):
    def test_a_seeding_call_leaves_a_live_servers_active_json_and_several_servers_are_stopped(self) -> None:
        other = self.worktree("other")
        self.warm(other, "--seed", env={"KINOWO_SBT_WARM_SEED": self.path("old")})
        os.makedirs(other + "/e2e/src")
        server = self.fake_server(other)
        self.put(other + "/project/target/active.json", '{"uri":"local://elsewhere/sock"}\n')
        self.warm(other, "compile")
        self.assertIn("local://elsewhere/sock", self.text(other + "/project/target/active.json"))
        second = self.fake_server(other)
        self.warm(other, "compile")
        self.wait_gone(server, "the first of two servers")
        self.wait_gone(second, "the second of two servers")

class StartLockTest(SbtWarmCase):
    def test_a_second_call_without_a_server_waits_for_the_first_ones_start(self) -> None:
        locked = self.worktree("locked")
        self.warm(locked, "--seed")
        self.stub('#!/usr/bin/env bash\necho "start $(pwd -P)" >> "@S@/spans"; sleep 1; echo "end $(pwd -P)" >> "@S@/spans"\n')
        first = self.start(locked, "compile")
        self.assertTrue(cli_harness.wait_until(lambda: self.text(self.path("spans")) != ""))
        second = self.start(locked, "compile")
        first.wait()
        second.wait()
        self.assertEqual("start end start end", " ".join(w.split()[0] for w in self.text(self.path("spans")).splitlines()))
        # a call killed while it held the start lock (kill -9: no clean-up ran) leaves it free at once
        holder = self.hold_lock(self.state_file(locked, ".flock"))
        holder.kill()
        holder.wait()
        # the kernel lets go of a killed holder's flock a few ms after wait() returns, so wait for it, don't race it
        lock = self.state_file(locked, ".flock")
        self.assertTrue(cli_harness.wait_until(lambda: lock_state(lock) == "free"), lock_state(lock))
        done = self.warm(locked, "compile", env={"KINOWO_SBT_WARM_LOCK_SECONDS": "0"})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("free", lock_state(self.state_file(locked, ".flock")))

class ReapTest(SbtWarmCase):
    def test_a_removed_checkouts_server_is_stopped_and_forgotten_never_a_bystander(self) -> None:
        fresh, other = self.worktree("fresh"), self.worktree("other")
        self.warm(fresh, "compile")
        server = self.fake_server(fresh)
        self.put(self.state_file(fresh, ".pid"), f"{server}\n")
        subprocess.run(["rm", "-rf", fresh], check=True)
        self.git(self.main, "worktree", "prune")
        self.warm(other, "compile")
        self.wait_gone(server, "the removed checkout's server")
        self.assertEqual([], [f for f in glob.glob(self.state + "/*.root") if self.text(f) == fresh])
        # a pid file outlives its process, and the pid comes back as someone else's
        bystander = self.background(["sleep", "300"])
        self.put(self.state + "/gone.root", self.path("gone") + "\n")
        self.put(self.state + "/gone.pid", f"{bystander.pid}\n")
        self.warm(other, "compile")
        self.assertIsNone(bystander.poll(), "a bystander was killed")

class StopAndStatusTest(SbtWarmCase):
    def test_stop_and_reap_stop_only_this_checkouts_own_server(self) -> None:
        idle = self.worktree("idle")
        theirs = self.fake_server(self.path("old"))
        self.warm(idle, "--seed")
        self.put(idle + "/project/target/active.json", '{"uri":"local://%s/sock"}\n' % self.path("old"))
        self.warm(idle, "--stop")
        self.put(self.state_file(idle, ".root"), idle + "\n")
        subprocess.run(["touch", "-t", "202001010000", self.state_file(idle, ".used")], check=True)
        status = self.warm(idle, "--status")
        self.assertEqual(0, status.code)
        self.assertRegex(status.stdout, r"kept: %s \(.*, last used 2020-01-01 00:00\)" % re.escape(idle))
        self.assertIn(f"this checkout ({idle}): no server", status.stdout)
        self.warm(idle, "--reap")
        self.assertNotIn("shutdown", self.text(self.path("sbt-calls")))
        self.assertFalse(self.gone(theirs))
        mine = self.fake_server(idle)
        self.assertIn(f"this checkout ({idle}): server pid {mine}", self.warm(idle, "--status").stdout)
        self.warm(idle, "--stop")
        self.wait_gone(mine, "this checkout's own server")
        self.assertFalse(self.gone(theirs))

    def test_stop_in_the_root_checkout_is_refused_unless_root_is_given(self) -> None:
        roots = self.fake_server(self.main)
        done = self.warm(self.main, "--stop")
        self.assertEqual(2, done.code, done.output)
        self.assertIn("refusing --stop in the root checkout", done.stderr)
        self.assertFalse(self.gone(roots))
        self.assertEqual(0, self.warm(self.main, "--stop", "--root").code)
        self.wait_gone(roots, "the root checkout's server")
        self.assertIn(f"server pid={roots} checkout={self.main} why=\"--stop (SIGTERM)\"", self.stops())


if __name__ == "__main__":
    unittest.main()
