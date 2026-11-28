#!/usr/bin/env python3
# script-test-reads: scripts/local-mirror/mirror* scripts/local-mirror/*.js scripts/local-mirror/prod-tunnel.sh scripts/local-mirror/local-mongo.sh scripts/local-mirror/log-path.sh scripts/local-mirror/mongosh-at.sh scripts/lib/prod_tunnel.py scripts/lib/mongo_local.py scripts/jvm-gate.sh scripts/vault-env.sh
"""scripts/local-mirror/mirror.sh black-box (see mirror_test.py): the tunnel keeper and a TERM in a wait.
Run: python3 scripts/local-mirror/mirror_tunnel_test.py
"""
from __future__ import annotations

import os
import signal
import subprocess
import unittest
from typing import Dict, Optional

from mirror_test import MirrorCase, TERM_IN_WAIT, described, gone


class TunnelTest(MirrorCase):
    def test_supervisors_wait_for_the_keeper_never_open_the_tunnel_and_log_the_outage_once(self) -> None:
        dbs = ["kinowo_a", "kinowo_b", "kinowo_c"]
        self.start(KINOWO_MIRROR_DBS=" ".join(dbs), KINOWO_MIRROR_TUNNEL_MISSES="1000")
        tailers = self.until_tailing(3)
        self.write("ping-fails", "")
        for pid in tailers:
            os.kill(pid, signal.SIGTERM)
        for db in dbs:
            self.until_said(f"{db}: prod tunnel not answering — waiting for the tunnel keeper")
        self.until("the supervisors did not keep waiting", lambda: self.sleeps().count("5") >= 6)
        os.remove(self.path("ping-fails"))
        for db in dbs:
            self.until_said(f"{db}: prod tunnel answers again")
        for db in dbs:
            self.assertEqual(1, len(self.lines(f"{db}: prod tunnel not answering")), self.output())
            self.assertEqual(1, len(self.lines(f"{db}: prod tunnel answers again")), self.output())
        self.assertEqual(1, len(self.ssh_pids()))

    def test_the_keeper_keeps_a_live_tunnel_through_missed_probes_then_replaces_it(self) -> None:
        self.start(KINOWO_MIRROR_TUNNEL_MISSES="3")
        self.until_tailing()
        first = self.ssh_pids()[0]
        self.write("ping-fails", "")
        self.until_said("tunnel to prod Mongo is down — restarting it")
        os.remove(self.path("ping-fails"))
        said = self.output()
        missed = [said.find("probe missed (1 of 3) — keeping the open tunnel"),
                  said.find("probe missed (2 of 3) — keeping the open tunnel"),
                  said.find("tunnel to prod Mongo is down — restarting it")]
        self.assertTrue(-1 < missed[0] < missed[1] < missed[2], said)
        self.until("the replaced ssh kept running", lambda: gone(first))
        self.until("no second tunnel", lambda: len(self.ssh_pids()) == 2)

    def test_a_parent_killed_outright_leaves_no_tailer_and_no_tunnel(self) -> None:
        self.start()
        [tailer] = self.until_tailing()
        [ssh] = self.ssh_pids()
        assert self.daemon is not None
        os.kill(self.daemon.pid, signal.SIGKILL)
        self.daemon.wait()  # reaped, as its own parent (launchd, service.sh) does: a zombie still answers kill -0
        self.until("the tailer outlived its parent", lambda: gone(tailer))
        self.until("the tunnel outlived its parent", lambda: gone(ssh))
        self.until_said("kinowo: parent mirror.sh is gone — stopping this database's tailer")
        self.until_said("tunnel keeper: parent is gone — closing the tunnel and exiting")

    def test_a_term_ends_the_daemon_and_stops_its_tailers_and_tunnel(self) -> None:
        self.start(KINOWO_MIRROR_DBS="kinowo kinowo_uk")
        tailers = self.until_tailing(2)
        assert self.daemon is not None
        os.kill(self.daemon.pid, signal.SIGTERM)
        self.assertEqual(143, self.ended(), self.output())
        self.assertNotIn("Exception in thread", self.output())
        for pid in tailers + self.ssh_pids():
            self.until(f"pid {pid} outlived the TERMed daemon", lambda: gone(pid))

    def test_a_supervisor_that_dies_takes_the_whole_daemon_down(self) -> None:
        self.start(KINOWO_MIRROR_DBS="kinowo kinowo_uk")
        [tailer] = self.until_tailing(db="kinowo_uk")
        supervisor = int(subprocess.run(["ps", "-o", "ppid=", "-p", str(tailer)], capture_output=True, text=True,
                                        check=True).stdout.strip())
        os.kill(supervisor, signal.SIGKILL)
        self.until_said(f"database supervisor {supervisor} is gone — exiting so launchd restarts the sync")
        self.assertEqual(143, self.ended())


class TermInWaitTest(MirrorCase):
    """A TERM ends each of the daemon's processes at its next pause, never by an exception raised mid-call: raised inside
    a timed child-wait, it left subprocess's lock held, and the wait that cleanup then made never returned (ci-runner,
    pre-push scripttests STALLED: the keeper hung on the lock, its ssh holding the forward and the run's output open)."""

    def armed(self) -> Dict[str, Optional[str]]:
        os.makedirs(self.path("site"))
        self.write("site/sitecustomize.py", TERM_IN_WAIT)
        return {"PYTHONPATH": self.path("site"), "FAKE_TERM_IN_WAIT": self.path("term-in-wait")}

    def term_in_wait(self, role: str) -> None:
        self.write("term-in-wait", role + "\n")

    def fired(self) -> None:
        self.until("the TERM never landed", lambda: not os.path.exists(self.path("term-in-wait")))

    def test_the_keeper_termed_mid_probe_closes_its_tunnel_and_ends(self) -> None:
        self.start(**self.armed())
        self.until_tailing()
        self.term_in_wait("keeper")
        [ssh] = self.ssh_pids()
        keeper = int(described(ssh).split()[1])
        self.fired()
        self.until(f"the TERMed keeper kept its tunnel: {described(ssh)}", lambda: gone(ssh))
        self.until(f"the TERMed keeper never ended: {described(keeper)}", lambda: gone(keeper))

    def test_a_supervisor_termed_mid_probe_ends_and_takes_the_daemon_down(self) -> None:
        self.term_in_wait("supervisor")
        self.start(**self.armed())
        self.fired()
        self.until_said("kinowo: supervisor ended (exit 143)")
        self.assertEqual(143, self.ended())

    def test_the_daemon_termed_mid_probe_ends_with_its_children(self) -> None:
        self.term_in_wait("daemon")
        self.start(**self.armed())
        self.fired()
        self.assertEqual(143, self.ended(), self.output())
        self.assertNotIn("Exception in thread", self.output())  # a thread still writing it at the exit aborted it (-6)
        for pid in self.ssh_pids():
            self.until(f"pid {pid} outlived the TERMed daemon", lambda: gone(pid))


if __name__ == "__main__":
    unittest.main()
