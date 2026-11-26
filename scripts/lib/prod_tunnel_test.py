#!/usr/bin/env python3
"""prod_tunnel.ProdTunnel.answers: a mongosh ping that stalls on a half-open tunnel (the port accepts, mongod does not
answer) ends at PING_TIMEOUT as "does not answer", never hangs; a ping that succeeds answers."""
from __future__ import annotations

import os
import socket
import stat
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import prod_tunnel  # noqa: E402


class AnswersTest(unittest.TestCase):
    def setUp(self) -> None:
        self.listener = socket.socket()
        self.listener.bind(("127.0.0.1", 0))
        self.listener.listen(5)
        self.addCleanup(self.listener.close)
        self.port = str(self.listener.getsockname()[1])
        self.dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.dir.cleanup)

    def tunnel(self, body: str) -> prod_tunnel.ProdTunnel:
        stub = os.path.join(self.dir.name, "mongosh-at")
        with open(stub, "w") as out:
            out.write("#!/bin/sh\n" + body + "\n")
        os.chmod(stub, os.stat(stub).st_mode | stat.S_IXUSR)
        uri = f"mongodb://u:p@127.0.0.1:{self.port}/db"
        return prod_tunnel.ProdTunnel("t", uri, {}, [stub], lambda _s: None)

    def test_a_ping_that_stalls_is_no_answer_not_a_hang(self) -> None:
        tunnel = self.tunnel("exec sleep 30")
        original, prod_tunnel.PING_TIMEOUT = prod_tunnel.PING_TIMEOUT, 1
        self.addCleanup(setattr, prod_tunnel, "PING_TIMEOUT", original)
        self.assertFalse(tunnel.answers())

    def test_a_ping_that_succeeds_answers(self) -> None:
        self.assertTrue(self.tunnel("exit 0").answers())


if __name__ == "__main__":
    unittest.main()
