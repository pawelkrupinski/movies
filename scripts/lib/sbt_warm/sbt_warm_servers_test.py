"""sbt-warm's server lookup (sbt_warm_servers.Servers.pids over Procs), on canned `ps` and `lsof` text: no process runs,
so the cases cost milliseconds where the black-box ones (sbt_warm_server_test.py) pay a real ps and lsof a lookup.
Run: python3 scripts/lib/sbt_warm/sbt_warm_servers_test.py"""
from __future__ import annotations

import datetime
import os
import tempfile
import unittest
import sys
from typing import Dict, List

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from sbt_warm.sbt_warm_servers import Procs, Servers  # noqa: E402

PS = """\
  100     1 java -jar /x/sbt-launch.jar --detach-stdio
  101     1 java -cp xsbt.boot.Boot --detach-stdio
  102     1 java -jar /x/sbt-launch.jar
  103     1 sleep 300
  104     1 java -jar /y/sbt-launch.jar --detach-stdio
"""


class Machine:
    """What the canned commands answer, and the commands run."""

    def __init__(self, cwds: Dict[int, str]) -> None:
        self.cwds = cwds
        self.ran: List[List[str]] = []

    def __call__(self, argv: List[str]) -> str:
        self.ran.append(argv)
        if argv[0] == "ps":
            return PS
        wanted = [int(p) for p in argv[argv.index("-p") + 1].split(",")]
        return "".join(f"p{pid}\nfcwd\nn{self.cwds[pid]}\n" for pid in wanted if pid in self.cwds)


class ServersTest(unittest.TestCase):
    def servers(self, cwds: Dict[int, str]) -> "tuple[Servers, Machine]":
        machine = Machine(cwds)
        state = tempfile.mkdtemp()
        self.addCleanup(os.rmdir, state)
        return Servers(Procs(machine), state, lambda line: None, lambda seconds: None, datetime.datetime.now), machine

    def test_the_servers_of_a_checkout_are_the_detached_sbt_jvms_working_there_oldest_first(self) -> None:
        root = os.path.realpath(tempfile.gettempdir())
        servers, machine = self.servers({100: root, 101: root, 104: "/elsewhere"})
        self.assertEqual([100, 101], servers.pids(root))
        # one ps for the table and one lsof for every candidate, however many servers run
        self.assertEqual(["ps", "lsof"], [argv[0] for argv in machine.ran])
        self.assertEqual("100,101,104", machine.ran[1][machine.ran[1].index("-p") + 1])

    def test_a_server_whose_directory_lsof_cannot_read_belongs_to_no_checkout(self) -> None:
        servers, _ = self.servers({})
        self.assertEqual([], servers.pids("/anywhere"))
        self.assertIsNone(servers.pid("/anywhere"))

    def test_with_no_detached_sbt_running_lsof_is_never_run(self) -> None:
        ran: List[List[str]] = []

        def only_sleep(argv: List[str]) -> str:
            ran.append(argv)
            return "  7     1 sleep 1\n"
        state = tempfile.mkdtemp()
        self.addCleanup(os.rmdir, state)
        servers = Servers(Procs(only_sleep), state, lambda line: None, lambda seconds: None, datetime.datetime.now)
        self.assertEqual([], servers.pids("/anywhere"))
        self.assertEqual(["ps"], [argv[0] for argv in ran])


if __name__ == "__main__":
    unittest.main()
