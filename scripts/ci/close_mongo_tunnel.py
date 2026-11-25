#!/usr/bin/env python3
"""Close what wait-for-mongo-tunnel.sh opened, and PROVE it is closed.

  close-mongo-tunnel.sh [local-port]     default 27018, as the opener

WHY IT EXISTS. The corpus used to be recorded in a job of its own, which ended a few seconds after the read -- the
runner died, and the tunnel, the ssh key on disk and the read credential in $GITHUB_ENV died with it. Now the read is
the first thing a country's enrichment leg does, and that job then runs project code for up to six hours with a
write-scoped token and the TMDB/OMDb keys. Left open, the route to prod Mongo would be reachable from every one of
those steps. Closing it right after the read keeps the window exactly as wide as it was: one step.

What it does, in the order that keeps anything from coming back:
  1. ends the supervisor FIRST -- it restarts a listener that exits, so ending the listener first would only race a
     respawn;
  2. ends the local listener on the port, and every ssh relay still using the key;
  3. removes the key, the known_hosts line and the generated scripts;
  4. blanks KINOWO_CONVERGENCE_SCRAPES_URI in $GITHUB_ENV -- the configuration reads an empty value as absent, so no
     later step can even try the dead address;
  5. fails unless nothing is listening on the port any more.

The workflows run it by the name scripts/ci/close-mongo-tunnel.sh; tested by close_mongo_tunnel_test.py."""
from __future__ import annotations

import os
import socket
import subprocess
import sys
import time
from typing import List, Mapping

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from mongo_tunnel import DEFAULT_PORT, TunnelPaths  # noqa: E402

PROOF_SECONDS = 5  # a killed process can hold its socket for a moment; a few seconds, not forever


def listening(port: str) -> bool:
    try:
        socket.create_connection(("127.0.0.1", int(port)), timeout=1).close()
        return True
    except OSError:
        return False


def end_matching(pattern: str) -> None:
    """Every process whose command line holds `pattern` (the runner's own: these names are this tunnel's)."""
    subprocess.run(["pkill", "-f", pattern], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)


def close(port: str, env: Mapping[str, str]) -> None:
    paths = TunnelPaths.of(env)
    for pattern in (paths.supervisor, f"TCP4-LISTEN:{port}", paths.key):
        end_matching(pattern)
    for path in (paths.key, paths.known_hosts, paths.relay, paths.supervisor):
        if os.path.lexists(path):
            os.remove(path)
    github_env = env.get("GITHUB_ENV")
    if github_env:
        with open(github_env, "a") as f:
            f.write("KINOWO_CONVERGENCE_SCRAPES_URI=\n")


def main(argv: List[str], env: Mapping[str, str]) -> int:
    port = argv[0] if argv else DEFAULT_PORT
    close(port, env)
    for second in range(PROOF_SECONDS):
        if not listening(port):
            print(f"[tunnel] closed: nothing listens on :{port}, key and credential gone", flush=True)
            return 0
        if second + 1 < PROOF_SECONDS:
            time.sleep(1)
    print(f"[tunnel] something still listens on :{port} after closing the tunnel", file=sys.stderr, flush=True)
    return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], os.environ))
