"""close_mongo_tunnel's decisions over a recording runner, in-memory files and a scripted port -- no process, no socket,
no sleep (the real processes and sockets: close_mongo_tunnel_test.py)."""
from __future__ import annotations

import os
import sys
import unittest
from typing import List

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = [HERE, os.path.join(os.path.dirname(HERE), "lib")]
import close_mongo_tunnel as tunnel  # noqa: E402
from tool_files import Files  # noqa: E402
from tool_process import Captured, Command  # noqa: E402

ENV = {"RUNNER_TEMP": "/runner", "GITHUB_ENV": "/runner-env/github-env"}


class RecordingRunner:
    def __init__(self) -> None:
        self.commands: List[Command] = []

    def run(self, command: Command) -> int:
        return self.capture(command).code

    def capture(self, command: Command) -> Captured:
        self.commands.append(command)
        return Captured(0, "", "")


class CloseTest(unittest.TestCase):
    def setUp(self) -> None:
        self.files, self.runner = Files.in_memory(), RecordingRunner()
        for name in ("mongo-ci-read.key", "mongo-ci-read.known_hosts", "mongo-relay.sh", "mongo-tunnel-supervisor.sh"):
            self.files.write_text(f"/runner/{name}", "x")
        self.files.write_text("/runner-env/github-env", "A=1\n")

    def test_it_ends_the_supervisor_first_then_the_listener_then_the_key_users(self) -> None:
        tunnel.close("27018", ENV, self.runner, self.files)
        self.assertEqual(["/runner/mongo-tunnel-supervisor.sh", "TCP4-LISTEN:27018", "/runner/mongo-ci-read.key"],
                         [c.argv[2] for c in self.runner.commands])

    def test_it_removes_the_files_and_blanks_the_credential_keeping_what_the_env_file_held(self) -> None:
        tunnel.close("27018", ENV, self.runner, self.files)
        self.assertEqual([], [p for p in ("mongo-ci-read.key", "mongo-relay.sh") if self.files.exists(f"/runner/{p}")])
        self.assertEqual("A=1\nKINOWO_CONVERGENCE_SCRAPES_URI=\n", self.files.read_text("/runner-env/github-env"))

    def test_closing_what_was_never_opened_is_a_success(self) -> None:
        code = tunnel.run([], {"RUNNER_TEMP": "/runner-none"}, self.runner, Files.in_memory(), lambda port: False, lambda s: None)
        self.assertEqual(0, code)

    def test_a_port_that_keeps_answering_fails_after_the_proof_window(self) -> None:
        pauses: List[float] = []
        code = tunnel.run(["27020"], ENV, self.runner, self.files, lambda port: True, pauses.append)
        self.assertEqual((1, tunnel.PROOF_SECONDS - 1), (code, len(pauses)))

    def test_a_port_that_stops_answering_within_the_window_succeeds(self) -> None:
        answers = iter([True, True, False])
        self.assertEqual(0, tunnel.run([], ENV, self.runner, self.files, lambda port: next(answers), lambda s: None))


if __name__ == "__main__":
    unittest.main()
