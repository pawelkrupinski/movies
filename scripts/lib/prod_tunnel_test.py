#!/usr/bin/env python3
"""prod_tunnel.ProdTunnel.answers: a mongosh ping that stalls on a half-open tunnel (the port accepts, mongod does not
answer) ends at PING_TIMEOUT as "does not answer", never hangs; a ping that succeeds answers."""
from __future__ import annotations

import io
import os
import socket
import stat
import sys
import tempfile
import unittest
from contextlib import redirect_stderr, redirect_stdout

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


class CommandLineTest(unittest.TestCase):
    """What the shell library asks of this file: the URI rewrite and the free-port pick, written once."""

    def run_main(self, argv: list, env: dict) -> tuple:
        out, err = io.StringIO(), io.StringIO()
        pin = ["--pin", env["PROD_TUNNEL_PORT"]] if "PROD_TUNNEL_PORT" in env else []
        with redirect_stdout(out), redirect_stderr(err):
            code = prod_tunnel.main(argv[:1] + pin + argv[1:])
        return code, out.getvalue(), err.getvalue()

    def test_uri_names_the_pinned_port_and_keeps_the_rest(self) -> None:
        code, out, _ = self.run_main(["uri", "--pid", "7", "mongodb://u:p@10.20.0.13:27017/db?x=1&directConnection=false"],
                                     {"PROD_TUNNEL_PORT": "27555"})
        self.assertEqual((0, "mongodb://u:p@127.0.0.1:27555/db?x=1&directConnection=true\n"), (code, out))

    def test_free_port_is_the_pinned_one_or_the_pids_slot(self) -> None:
        self.assertEqual((0, "27555\n"), self.run_main(["free-port", "--pid", "7"], {"PROD_TUNNEL_PORT": "27555"})[:2])
        code, out, _ = self.run_main(["free-port", "--pid", "5"], {})
        self.assertEqual(0, code)
        self.assertGreaterEqual(int(out), prod_tunnel.PORT_LOW + 5)

    def test_no_free_port_fails_instead_of_naming_27017(self) -> None:
        original = prod_tunnel.mongo_local.answers
        prod_tunnel.mongo_local.answers = lambda *_a, **_k: True  # every port taken
        self.addCleanup(setattr, prod_tunnel.mongo_local, "answers", original)
        code, out, err = self.run_main(["uri", "--pid", "9", "mongodb://u:p@h:27017/db"], {})
        self.assertEqual((1, ""), (code, out))
        self.assertIn("no free local port", err)

    def test_a_bad_command_line_is_a_usage_error(self) -> None:
        self.assertEqual(64, self.run_main(["uri"], {})[0])
        self.assertEqual(64, self.run_main(["uri", "--pid", "9"], {"PROD_TUNNEL_PORT": "1"})[0])


if __name__ == "__main__":
    unittest.main()
