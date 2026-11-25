#!/usr/bin/env python3
"""scripts/ci/close-mongo-tunnel.sh, black-box, against stand-ins for what wait-for-mongo-tunnel.sh leaves running: a
supervisor, a listener on the port, an ssh relay holding the key, and the files. Ported from close-mongo-tunnel-test.sh;
the same cases hold the shell tool and its Python rewrite (cli_harness.tool_path).

The port is never released between stages: this process binds it (port 0, the kernel's pick) and hands the listening
socket to each stand-in child, whose command line carries socat's `TCP4-LISTEN:<port>` as the real listener's does."""
from __future__ import annotations

import os
import socket
import subprocess
import sys
import unittest
from typing import List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/close_mongo_tunnel.py", "scripts/ci/close-mongo-tunnel.sh")
# holds the listening socket it is handed (fd argv[1]) and accepts on it until ended
HOLD = """import socket, sys, time
s = socket.socket(fileno=int(sys.argv[1]))
s.settimeout(0.2)
while True:
    try:
        s.accept()[0].close()
    except OSError:
        time.sleep(0.05)"""
SLEEP = "import time; time.sleep(300)"


def answers(port: int) -> bool:
    try:
        socket.create_connection(("127.0.0.1", port), timeout=1).close()
        return True
    except OSError:
        return False


class CloseMongoTunnelTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.started: List["subprocess.Popen[bytes]"] = []
        self.socket = socket.socket()
        self.socket.bind(("127.0.0.1", 0))
        self.socket.listen()
        self.port = self.socket.getsockname()[1]
        self.key = self.path("mongo-ci-read.key")
        self.known = self.path("mongo-ci-read.known_hosts")
        self.relay = self.path("mongo-relay.sh")
        self.supervisor = self.path("mongo-tunnel-supervisor.sh")

    def tearDown(self) -> None:
        for proc in self.started:
            if proc.poll() is None:
                proc.kill()
            proc.wait()
        self.socket.close()
        super().tearDown()

    def start(self, *argv: str, holds_port: bool = False) -> "subprocess.Popen[bytes]":
        fds = (self.socket.fileno(),) if holds_port else ()
        proc = subprocess.Popen(list(argv), pass_fds=fds, stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL,
                                stderr=subprocess.DEVNULL)
        self.started.append(proc)
        return proc

    def listener(self, *tag: str) -> "subprocess.Popen[bytes]":
        return self.start(sys.executable, "-c", HOLD, str(self.socket.fileno()), *tag, holds_port=True)

    def open_tunnel(self) -> List["subprocess.Popen[bytes]"]:
        self.write(self.key, "key\n")
        self.write(self.known, "host\n")
        self.write(self.relay, "#!/usr/bin/env bash\nexec sleep 300\n")
        self.write(self.supervisor, "#!/usr/bin/env bash\nwhile true; do sleep 1; done\n")
        os.chmod(self.relay, 0o755)
        os.chmod(self.supervisor, 0o755)
        supervisor = self.start(self.supervisor)
        listener = self.listener(f"TCP4-LISTEN:{self.port},bind=127.0.0.1")
        relay = self.start(sys.executable, "-c", SLEEP, "ssh", "-i", self.key)
        self.write("github-env", f"KINOWO_CONVERGENCE_SCRAPES_URI=mongodb://user:secret@127.0.0.1:{self.port}/\n")
        return [supervisor, listener, relay]

    def close(self) -> cli_harness.Done:
        # the test's own copy of the socket shut: only the stand-ins hold the port from here
        return self.run_tool(str(self.port), env={"RUNNER_TEMP": self.work, "GITHUB_ENV": self.path("github-env")})

    def test_closes_an_open_tunnel_ending_everything_it_left_and_blanking_the_credential(self) -> None:
        running = self.open_tunnel()
        self.socket.close()
        self.assertTrue(cli_harness.wait_until(lambda: answers(self.port)))
        done = self.close()
        self.assertEqual(0, done.code, done.output)
        self.assertTrue(cli_harness.wait_until(lambda: all(p.poll() is not None for p in running)),
                        [p.poll() for p in running])
        self.assertEqual([], [f for f in (self.key, self.known, self.relay, self.supervisor) if os.path.exists(f)])
        self.assertEqual("KINOWO_CONVERGENCE_SCRAPES_URI=", self.read("github-env").splitlines()[-1])

    def test_fails_when_the_port_still_answers_after_closing(self) -> None:
        self.listener()
        self.socket.close()
        self.assertTrue(cli_harness.wait_until(lambda: answers(self.port)))
        done = self.close()
        self.assertEqual(1, done.code, done.output)
        self.assertIn(f"something still listens on :{self.port}", done.output)

    def test_closing_a_tunnel_that_was_never_opened_is_a_no_op_success(self) -> None:
        self.socket.close()
        done = self.close()
        self.assertEqual(0, done.code, done.output)
        self.assertIn(f"closed: nothing listens on :{self.port}", done.output)


if __name__ == "__main__":
    unittest.main()
