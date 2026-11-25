#!/usr/bin/env python3
"""Run a command against the image registry, and when it lost the registry, wait for it to answer and run it once more.

  wait_out_registry_outage.py <registry host> -- <command>...
  env: IMAGE_PUSH_WAIT_SECONDS (default 600), IMAGE_PUSH_POLL_SECONDS (15)

main.yml's publish-web / publish-worker run their `docker login` + `docker buildx imagetools create` through it.

THE REGISTRY GONE. registry.kinowo.net sits on k3s-worker-1, whose nixos-auto-apply switches the host once a pushed
commit's closure is staged -- for an infra commit, while that same commit's Main run is still going. Main #2906
(2026-11-25 20:57Z, a nixpkgs bump staged at 20:42) met Caddy restarting: `docker login` got "dial tcp ...:443: connect:
connection refused" and both publish jobs failed, with nothing wrong in the image. The daily 04:00 UTC registry-prune
stops the registry the same way (infra/nix/modules/roles/registry.nix). Such an outage is minutes long and ENDS, so,
as scripts/ci/build-image.sh does for the build, a command whose output has a line naming the registry with a refused
connection, a reset, a timeout or a 502/503 waits, bounded, until the registry's /v2/ answers (200, or 401 asking for a
login) and runs once more, saying on the run what it lost and how long it waited -- never a silent retry. A registry
still gone at the deadline fails naming it; any other failure is the command's verdict as it stands.

Stdlib only: the publish jobs check out .github and scripts/ci, nothing else. Tested by wait_out_registry_outage_test.py."""
from __future__ import annotations

import os
import re
import subprocess
import sys
import time
from typing import List, Optional, Tuple

LOST = re.compile(r"connection refused|connection reset by peer|i/o timeout|no such host|502 Bad Gateway"
                  r"|503 Service Unavailable")
SERVING = ("200", "401")


def run(command: List[str]) -> Tuple[int, str]:
    """The command's exit status and what it said (stdout and stderr, merged), echoed as it ends."""
    done = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
    sys.stdout.write(done.stdout or "")
    sys.stdout.flush()
    return done.returncode, done.stdout or ""


def lost_registry(host: str, said: str) -> Optional[str]:
    """What took the registry away, when a line naming it says it went (else None)."""
    for line in said.splitlines():
        found = LOST.search(line) if host in line else None
        if found:
            return found.group(0)
    return None


def probe(host: str) -> str:
    done = subprocess.run(["curl", "-s", "-o", "/dev/null", "-w", "%{http_code}", "--max-time", "10",
                           f"https://{host}/v2/"], capture_output=True, text=True)
    return (done.stdout or "").strip() or "000"


def wait_for(host: str, lost: str, limit: float, poll: float) -> Tuple[str, float]:
    """Probe until the registry serves or `limit` seconds pass: (its last answer, seconds waited)."""
    started = time.monotonic()
    while True:
        answer = probe(host)
        waited = time.monotonic() - started
        if answer in SERVING or waited >= limit:
            return answer, waited
        print(f"lost {host} ({lost}): it answers {answer}, waiting for it ({waited:.0f} s of {limit:.0f} s)", flush=True)
        time.sleep(poll)


def why_gone(hhmm: str) -> str:
    if "0400" <= hhmm < "0420":
        return "the image registry was stopped by its daily 04:00 UTC registry-prune (infra/nix/modules/roles/registry.nix)"
    return "the image registry was down or restarting (a host switch on k3s-worker-1, or its daily prune)"


def main(argv: List[str]) -> int:
    if len(argv) < 3 or argv[1] != "--":
        print("usage: wait_out_registry_outage.py <registry host> -- <command>...", file=sys.stderr)
        return 2
    host, command = argv[0], argv[2:]
    status, said = run(command)
    lost = lost_registry(host, said) if status else None
    if not lost:
        return status
    why = why_gone(time.strftime("%H%M", time.gmtime()))
    limit = float(os.environ.get("IMAGE_PUSH_WAIT_SECONDS", "600"))
    answer, waited = wait_for(host, lost, limit, float(os.environ.get("IMAGE_PUSH_POLL_SECONDS", "15")))
    if answer not in SERVING:
        print(f"::error::lost {host}: {lost}, and it still answered {answer} after {limit:.0f} s -- {why}, not the command")
        return status
    print(f"::warning::lost {host}: {lost} -- {why}; it answered again after {waited:.0f} s: running once more", flush=True)
    return run(command)[0]


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
