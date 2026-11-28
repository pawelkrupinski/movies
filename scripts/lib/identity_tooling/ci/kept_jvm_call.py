"""ONE REQUEST TO A KEPT JVM (scripts/lib; identity-tools.sh's kept_jvm_call): tools.KeptJvm's framing -- the args
NUL-separated behind their length -- on its unix socket, the answer on stdout as it streams.

  python3 -I -B scripts/lib/identity_tooling/ci/kept_jvm_call.py <socket> <args...>
        -> the request's exit code; 111 when nothing listens at <socket>, 112 when the JVM went away mid-request.
        identity-slice's and identity-what-if's kept JVMs."""
from __future__ import annotations

import socket
import struct
import sys
from typing import List

MARKER = b"\0EXIT "
CHUNK = 65536
NOT_LISTENING, WENT_AWAY = 111, 112


def call(path: str, args: List[str]) -> int:
    conn = socket.socket(socket.AF_UNIX)
    try:
        conn.connect(path)
    except OSError:
        return NOT_LISTENING
    payload = "\0".join(args).encode()
    conn.sendall(struct.pack(">i", len(payload)) + payload)
    held, out = b"", sys.stdout.buffer
    while True:
        chunk = conn.recv(CHUNK)
        if not chunk:
            break
        held += chunk
        at = held.find(MARKER)
        if at >= 0:
            out.write(held[:at])
            out.flush()
            return int(held[at + len(MARKER):].split(b"\n")[0] or b"1")
        keep = len(MARKER) - 1
        out.write(held[:-keep])
        out.flush()
        held = held[-keep:]
    out.write(held)
    out.flush()
    return WENT_AWAY


def main(argv: List[str]) -> int:
    return call(argv[0], argv[1:])


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
