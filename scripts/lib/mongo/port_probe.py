"""WHETHER SOMETHING ANSWERS ON A LOCAL PORT, injected (scripts/lib; CLAUDE.md, "Injected effects": the network reaches a
function through its parameters). The prod tunnel's free-port scan, the tunnel's own liveness and the local Mongo's
reuse all ask it; each gets a `PortProbe` from its composition root -- `SocketProbe` on the real loopback, `OpenPorts`
in a unit test, held alike by port_probe_test.py's contract suite.

  probe.answers(port) -> bool      whether `port` (a string, as the tools carry ports) accepts a TCP connection"""
from __future__ import annotations

import socket
from typing import AbstractSet, Protocol


class PortProbe(Protocol):
    def answers(self, port: str) -> bool: ...


class SocketProbe:
    """The loopback's own answer. A loopback listener answers in milliseconds (its backlog takes the connection whatever
    its process is doing); only macOS lets a connection to a bound, never listening port hang to the timeout instead of
    refusing it, so the bound is short. A port that is no number answers nothing."""

    def __init__(self, host: str = "127.0.0.1", timeout: float = 0.5) -> None:
        self.host = host
        self.timeout = timeout

    def answers(self, port: str) -> bool:
        try:
            with socket.create_connection((self.host, int(port)), timeout=self.timeout):
                return True
        except (OSError, ValueError):
            return False


class OpenPorts:
    """The ports a unit test says are taken; a port that is no number answers nothing, as on the real loopback."""

    def __init__(self, taken: AbstractSet[str]) -> None:
        self.taken = taken

    def answers(self, port: str) -> bool:
        return port.isdigit() and port in self.taken
