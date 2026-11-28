#!/usr/bin/env python3
"""port_probe.py: ONE contract suite over the real loopback probe and the in-memory one -- a taken port answers, one nobody
listens on and one that is no number do not."""
from __future__ import annotations

import os
import socket
import sys
import unittest
from typing import Callable, Tuple

sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from mongo import port_probe  # noqa: E402


def listening() -> Tuple[socket.socket, str]:
    server = socket.socket()
    server.bind(("127.0.0.1", 0))
    server.listen(5)
    return server, str(server.getsockname()[1])


def bound_not_listening() -> Tuple[socket.socket, str]:
    """A port this test holds but nobody listens on: no other process can take it while the test runs."""
    holder = socket.socket()
    holder.bind(("127.0.0.1", 0))
    return holder, str(holder.getsockname()[1])


class Contract:
    """What every PortProbe does; `probe_over(taken)` builds one that sees exactly the ports `taken`."""
    probe_over: Callable[[str], port_probe.PortProbe]

    def test_a_taken_port_answers(self) -> None:
        server, port = listening()
        self.addCleanup(server.close)  # type: ignore[attr-defined]
        self.assertTrue(self.probe_over(port).answers(port))  # type: ignore[attr-defined]

    def test_a_port_nobody_listens_on_does_not(self) -> None:
        holder, port = bound_not_listening()
        self.addCleanup(holder.close)  # type: ignore[attr-defined]
        self.assertFalse(self.probe_over("").answers(port))  # type: ignore[attr-defined]

    def test_a_port_that_is_no_number_does_not(self) -> None:
        self.assertFalse(self.probe_over("x").answers("x"))  # type: ignore[attr-defined]


class SocketProbeTest(Contract, unittest.TestCase):
    def probe_over(self, taken: str) -> port_probe.PortProbe:
        return port_probe.SocketProbe()


class OpenPortsTest(Contract, unittest.TestCase):
    def probe_over(self, taken: str) -> port_probe.PortProbe:
        return port_probe.OpenPorts({taken})


if __name__ == "__main__":
    unittest.main()
