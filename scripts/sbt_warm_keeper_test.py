"""scripts/sbt-warm, black-box (ported from sbt-warm-test.sh): ONE JVM, ONE SLOT -- a live server holds one JVM slot through
its keeper, a command on it takes none, an idle server is stopped by its keeper (never under a running command), and
--stop and --reap stop a checkout's keeper with its server; a call keeps its server marked in use while it runs or
waits at the gate, and sbt inherits no lock. sbt is a stub (sbt_warm_fixture.py).
Run: python3 scripts/sbt_warm_keeper_test.py"""
# script-test-stress: keepers, clients and polls racing a loaded machine's clock (Order independence #74)
from __future__ import annotations

import os
import subprocess
import unittest
from typing import List

from sbt_warm_fixture import SbtWarmCase, lock_state, state_word
import cli_harness


def aged(path: str) -> None:
    subprocess.run(["touch", "-t", "202001010000", path], check=True)


class InUseTest(SbtWarmCase):
    def test_a_running_or_waiting_call_keeps_its_server_marked_in_use(self) -> None:
        slotted = self.worktree("slotted")
        used = self.state_file(slotted, ".used")
        self.warm(slotted, "testUnit", env={"KINOWO_SBT_WARM_HEARTBEAT_SECONDS": "1", "STUB_AGE_USED": used})
        self.assertEqual("busy", self.text(self.path("busy")))
        # ...and so does a call waiting at the memory gate before its command starts (once the last keeper ended)
        keep = self.state_file(slotted, ".keep.flock")
        self.assertTrue(cli_harness.wait_until(lambda: lock_state(keep) != "held"))
        once, waited = self.path("aged-once"), self.path("waited")
        probe = (f"if [ -e '{once}' ]; then [ -n \"$(find '{used}' -mmin +1)\" ] && echo idle > '{waited}' || "
                 f"echo busy > '{waited}'; else touch '{once}' && touch -t 202001010000 '{used}'; fi; echo 99")
        self.warm(slotted, "compile", env={
            "KINOWO_SBT_WARM_HEARTBEAT_SECONDS": "1", "KINOWO_MEMORY_GATE": "1", "KINOWO_MEMORY_GATE_FREE_PROBE": "echo 0",
            "KINOWO_MEMORY_GATE_POLL": "1", "KINOWO_MEMORY_GATE_MAX_WAIT": "3", "KINOWO_MEMORY_GATE_PROBE": probe})
        self.assertEqual("busy", self.text(waited))

    def test_a_new_server_waits_on_the_memory_gate_while_the_mac_is_paging(self) -> None:
        done = self.warm(self.worktree("gated"), "compile", env={
            "KINOWO_MEMORY_GATE": "1", "KINOWO_MEMORY_GATE_PROBE": "echo 99", "KINOWO_MEMORY_GATE_FREE_PROBE": "echo 0",
            "KINOWO_MEMORY_GATE_MAX_WAIT": "0"})
        self.assertIn("[sbt-warm] memory gate: still 99 GB paged out", done.stderr)

    def test_sbt_is_sized_for_the_cores_and_inherits_no_lock(self) -> None:
        slotted = self.worktree("slotted")
        self.warm(slotted, "compile", env={"KINOWO_JVM_SLOTS": "4", "KINOWO_JVM_CPUS": "4", "KINOWO_JVM_GATE_OS": "Darwin"})
        self.assertEqual("-XX:ActiveProcessorCount=4||", self.text(self.path("sbt-env")).splitlines()[-1])
        done = self.warm(slotted, "--serve", "web/PageTest/runMain tools.FixtureServerMain port",
                         env={"KINOWO_JVM_GATE_OS": "Darwin"})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("--client web/PageTest/runMain tools.FixtureServerMain port", self.last_call())
        self.assertFalse(os.path.exists(self.state_file(slotted, ".serving")), "a serve's mark outlived it")


class KeeperSlotTest(SbtWarmCase):
    def test_a_live_server_holds_one_slot_through_its_keeper_until_it_is_gone(self) -> None:
        counted = self.worktree("counted")
        slot = os.path.join(self.identity, "jvm-slots/1.flock")
        server = self.fake_server(counted)
        self.put(self.path("jvm-reaper.log"), f"2026-10-19 15:14:01 REAP pid={server} ppid=1 rule=hog age=1h42m\n")
        one = {"KINOWO_JVM_SLOTS": "1", "KINOWO_JVM_REAPER_LOG": self.path("jvm-reaper.log")}
        self.warm(counted, "compile", env=one)
        keeper = int(self.text(self.state_file(counted, ".keep.flock")) or 0)
        self.assertEqual(("held", str(keeper)), (lock_state(slot), self.text(slot)))
        self.assertTrue(cli_harness.alive(keeper))
        done = self.warm(counted, "compile", env={**one, "KINOWO_JVM_SLOT_MAX_WAIT": "0"})
        self.assertIn("this command takes none", done.stderr)
        self.assertNotRegex(done.stderr, "slots busy|slot still busy")
        self.end_server(server)
        self.assertTrue(cli_harness.wait_until(lambda: not cli_harness.alive(keeper)), "the keeper outlived its server")
        self.assertEqual("free", lock_state(slot))
        # a server ended by something else is a "vanished" line naming the JVM reaper's kill of it
        self.assertRegex(self.stops(), r"vanished pid=%d checkout=%s why=\"ended without sbt-warm stopping it; "
                                       r".*REAP pid=%d .*rule=hog" % (server, counted, server))
        self.assertIn("keeper of %s: pid %d, holding JVM slot 1.flock" % (counted, keeper),
                      self.text(self.state_file(counted, ".log")))

    def test_an_idle_server_is_stopped_by_its_keeper_never_under_a_running_command(self) -> None:
        idler = self.worktree("idler")
        used = self.state_file(idler, ".used")
        server = self.fake_server(idler)
        self.warm(idler, "testUnit", env={"KINOWO_SBT_WARM_HEARTBEAT_SECONDS": "100", "STUB_AGE_USED": used})
        keeper = int(self.text(self.state_file(idler, ".keep.flock")) or 0)
        self.assertEqual("idle running", self.text(self.path("busy")) + " " + state_word(server))
        aged(used)
        self.wait_gone(server, "the idle server")
        self.assertTrue(cli_harness.wait_until(lambda: not cli_harness.alive(keeper)), "the idle server's keeper")
        self.assertIn(f"server pid={server} checkout={idler} why=\"idle for 10 minutes (keeper) (SIGTERM)\"",
                      self.stops())
        open(self.path("sbt-calls"), "w").close()
        self.warm(idler, "compile", env={"KINOWO_SBT_WARM_KEEPER_GRACE_SECONDS": "600"})
        fresh = int(self.text(self.state_file(idler, ".keep.flock")) or 0)
        self.assertEqual([f"{idler}|--client compile"], self.calls())
        self.assertTrue(cli_harness.alive(fresh))


class StopKeeperTest(SbtWarmCase):
    def keepers_of(self, checkout: str) -> List[int]:
        """The keepers whose working directory is `checkout`, less a keeper's own subprocesses."""
        found = subprocess.run(["pgrep", "-f", "sbt-warm --keep"], capture_output=True, text=True).stdout.split()
        mine: List[int] = []
        for pid in found:
            parent = subprocess.run(["ps", "-o", "ppid=", "-p", pid], capture_output=True, text=True).stdout.strip()
            if parent and "sbt-warm --keep" in self.args_of(int(parent)):
                continue
            cwd = subprocess.run(["lsof", "-a", "-p", pid, "-d", "cwd", "-Fn"], capture_output=True, text=True).stdout
            if f"\nn{checkout}\n" in "\n" + cwd:
                mine.append(int(pid))
        return mine

    def test_stop_and_reap_stop_the_keeper_with_its_server(self) -> None:
        restopped = self.worktree("restopped")
        slot = self.path("own-slots/jvm-slots/1.flock")
        own = {"KINOWO_IDENTITY_CACHE": self.path("own-slots"), "KINOWO_JVM_SLOTS": "1"}
        server = self.fake_server(restopped)
        # Main #1259: a keeper not yet exec'd still carries the call's own command line; a slow nohup holds it there
        nohup = subprocess.run(["sh", "-c", "command -v nohup"], capture_output=True, text=True).stdout.strip()
        slow = self.shim("slow-nohup", "nohup", "#!/bin/sh\nkeeper=\"$*\" exec /bin/sh -c 'touch \"@S@/slow-nohup/waiting\";"
                                                " sleep 1; exec %s $keeper'\n" % nohup)
        path = slow + os.pathsep + self.env()["PATH"]
        self.warm(restopped, "compile", env={**own, "PATH": path, "KINOWO_SBT_WARM_KEEPER_GRACE_SECONDS": "60"})
        old = int(self.text(self.state_file(restopped, ".keep.flock")) or 0)
        self.assertTrue(cli_harness.wait_until(lambda: os.path.exists(self.path("slow-nohup/waiting"))))
        self.warm(restopped, "--stop")
        self.wait_gone(server, "the stopped server")
        self.assertEqual((False, "free"), (cli_harness.alive(old), lock_state(slot)))
        server = self.fake_server(restopped)
        done = self.warm(restopped, "compile", env={**own, "KINOWO_JVM_SLOT_MAX_WAIT": "4", "KINOWO_MEMORY_GATE": "0"})
        self.assertNotRegex(done.stderr, "slots busy|slot still busy")
        new = int(self.text(self.state_file(restopped, ".keep.flock")) or 0)
        self.assertTrue(cli_harness.wait_until(lambda: self.keepers_of(restopped) == [new]), self.keepers_of(restopped))
        aged(self.state_file(restopped, ".used"))
        self.warm(restopped, "--reap", env={"KINOWO_SBT_WARM_IDLE_MINUTES": "0"})
        self.wait_gone(server, "the reaped server")
        self.assertEqual([], self.keepers_of(restopped))


if __name__ == "__main__":
    unittest.main()
