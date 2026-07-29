#!/usr/bin/env python3
"""Exit 0 only if a Mongo server ANSWERS on 127.0.0.1:<port>.

A TCP connect is not enough. `flyctl proxy` binds its local port before it has a
working upstream, so `nc -z` reports success against a tunnel that will then time
out every query — which is exactly how three convergence legs spent eight minutes
failing against a "ready" tunnel.

So this speaks the wire protocol: a legacy OP_QUERY `{isMaster: 1}` against
`admin.$cmd`, the smallest handshake every mongod answers without authentication.
If a reply header comes back, bytes flow end to end and the database is reachable.

Usage: mongo-ping.py [port]   (default 27018)
"""
import socket
import struct
import sys


def ping(port: int, timeout: float = 5.0) -> bool:
    # BSON for {isMaster: 1}: int32 length, 0x10 = int32 field, name, value, terminator.
    document = b"\x13\x00\x00\x00\x10isMaster\x00\x01\x00\x00\x00\x00"
    # OP_QUERY body: flags, "admin.$cmd", numberToSkip, numberToReturn, query.
    body = struct.pack("<i", 0) + b"admin.$cmd\x00" + struct.pack("<ii", 0, -1) + document
    # Standard header: messageLength, requestID, responseTo, opCode (2004 = OP_QUERY).
    message = struct.pack("<iiii", 16 + len(body), 1, 0, 2004) + body

    try:
        with socket.create_connection(("127.0.0.1", port), timeout=timeout) as connection:
            connection.settimeout(timeout)
            connection.sendall(message)
            # Any reply header at all proves the far end is a live server, not just
            # an open socket. We deliberately don't parse it — an auth error or an
            # "unsupported opcode" is still proof the tunnel carries traffic.
            return len(connection.recv(4)) == 4
    except OSError:
        return False


if __name__ == "__main__":
    sys.exit(0 if ping(int(sys.argv[1]) if len(sys.argv) > 1 else 27018) else 1)
