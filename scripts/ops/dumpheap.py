#!/usr/bin/env python3
"""A heap dump (or a live class histogram) of a containerised JVM, asked from the NODE as root
without jcmd or a JDK in the image: HotSpot's attach protocol v1 over the JVM's own socket, reached
through /proc/<host pid>/root. For a worker whose HTTP side (`POST /heapdump`) is wedged or gone.

  sudo python3 dumpheap.py <host pid> <path inside the container>    # dumpheap -live: one full GC, then the dump
  sudo python3 dumpheap.py <host pid> --histo                         # inspectheap -live: jmap -histo:live

The dump lands at the path INSIDE the container — /data/heapdumps/... keeps it on the node's
hostPath (docs/heap-dumps.md). Linux only: it reads /proc/<pid>/status (NSpid) and /proc/<pid>/root."""
import argparse
import os
import signal
import socket
import sys
import time


def ns_pid(status_text):
    """The JVM's pid inside its own pid namespace: the last NSpid field (1 for a container's main process)."""
    for line in status_text.splitlines():
        if line.startswith("NSpid:"):
            return int(line.split()[-1])
    return 1


def request(command, *args):
    """An attach protocol v1 request: version, command, then exactly three arguments, each NUL-terminated."""
    padded = (list(args) + ["", "", ""])[:3]
    return b"".join(part.encode() + b"\0" for part in ["1", command, *padded])


def attach(host_pid, payload, wait_s=10.0):
    """Sends `payload` to the JVM's attach listener, starting it (trigger file + SIGQUIT) when absent;
    returns the JVM's whole reply."""
    with open(f"/proc/{host_pid}/status") as f:
        pid = ns_pid(f.read())
    root = f"/proc/{host_pid}/root"
    sock_path = f"{root}/tmp/.java_pid{pid}"
    triggers = [f"/proc/{host_pid}/cwd/.attach_pid{pid}", f"{root}/tmp/.attach_pid{pid}"]
    try:
        if not os.path.exists(sock_path):
            for t in triggers:
                try:
                    open(t, "w").close()
                except OSError:
                    pass
            os.kill(host_pid, signal.SIGQUIT)
            deadline = time.monotonic() + wait_s
            while not os.path.exists(sock_path) and time.monotonic() < deadline:
                time.sleep(0.1)
        s = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        s.connect(sock_path)
        s.sendall(payload)
        data = b""
        while chunk := s.recv(65536):
            data += chunk
        s.close()
        return data.decode(errors="replace")
    finally:
        for t in triggers:
            try:
                os.remove(t)
            except OSError:
                pass


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    parser.add_argument("pid", type=int, help="the JVM's pid on the node")
    target = parser.add_mutually_exclusive_group(required=True)
    target.add_argument("path", nargs="?", help="where the dump is written, inside the container")
    target.add_argument("--histo", action="store_true", help="print a live class histogram instead")
    parser.add_argument("--top", type=int, default=30, help="histogram rows shown")
    args = parser.parse_args(argv)
    if args.histo:
        lines = attach(args.pid, request("inspectheap", "-live")).splitlines()
        print("\n".join(lines[:args.top + 3]))
        print(lines[-1] if lines else "no output")
    else:
        print(attach(args.pid, request("dumpheap", args.path, "-live")).strip() or "no output")


if __name__ == "__main__":
    main(sys.argv[1:])
