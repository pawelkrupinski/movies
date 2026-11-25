"""WHERE THE PROD-MONGO TUNNEL KEEPS ITS PIECES ON A RUNNER, and the CI reader's connection string: read by the tool
that opens it (wait_for_mongo_tunnel.py) and the one that closes it (close_mongo_tunnel.py), so "close" can never miss
a file or a process "open" left behind under a renamed path; scripts/mongo_grants.py reads the reader's auth database
off the same URI (the user record the grants go to). Stdlib only, in scripts/ci: every job running the tunnel has it."""
from __future__ import annotations

import os
from dataclasses import dataclass
from typing import Mapping

DEFAULT_PORT = "27018"  # 27017 is a leg's own throwaway Mongo


@dataclass(frozen=True)
class TunnelPaths:
    work: str

    @staticmethod
    def of(env: Mapping[str, str]) -> "TunnelPaths":
        """Under the runner's temp directory (RUNNER_TEMP, else TMPDIR, else /tmp)."""
        return TunnelPaths(env.get("RUNNER_TEMP") or env.get("TMPDIR") or "/tmp")

    @property
    def key(self) -> str:
        return os.path.join(self.work, "mongo-ci-read.key")

    @property
    def known_hosts(self) -> str:
        return os.path.join(self.work, "mongo-ci-read.known_hosts")

    @property
    def relay(self) -> str:
        return os.path.join(self.work, "mongo-relay.sh")

    @property
    def supervisor(self) -> str:
        return os.path.join(self.work, "mongo-tunnel-supervisor.sh")


def ci_read_uri(userinfo: str, port: str) -> str:
    """The CI reader's connection string, spelled once (wait_for_mongo_tunnel.py's header says why each option)."""
    return f"mongodb://{userinfo}@127.0.0.1:{port}/?authSource=admin&directConnection=true&maxPoolSize=8"
