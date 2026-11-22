#!/usr/bin/env python3
"""THE IDENTITY TAKES CHECK'S PATHS (identity_takes_check.py): the paths whose change can move a take -- anything else,
docs, the web, workflows, cannot -- and, with the corpus-shape checks and their allowlists, those that can move a
corpus-shape finding. A side's results are stored keyed by its tree under IDENTITY_PATHS (its identity code), so the
identity measure (identity-measure) reads the same list to find a side's trace: `identity_takes_check_paths.py` prints
it, a path a line. (IdentityResolveDumpIntegrationSpec: the resolve as a spec, before it was a tool -- still a base's.)"""
from __future__ import annotations

import sys
from typing import Tuple

IDENTITY_PATHS: Tuple[str, ...] = (
    "geodata/src/main", "base/src/main", "http/src/main", "common/src/main", "identity/src/main", "scrapers/src/main",
    "worker/src/main", "worker/src/it/scala/IdentityResolveDump.scala",
    "worker/src/it/scala/IdentityResolveDumpIntegrationSpec.scala", "worker/src/it/scala/IdentityShadow.scala",
    "testkit/src/main", "build.sbt", "project")
SHAPE_PATHS: Tuple[str, ...] = IDENTITY_PATHS + ("e2e/src/test/scala/services/movies", "worker/src/fixtures")

if __name__ == "__main__":
    sys.stdout.write("".join(p + "\n" for p in IDENTITY_PATHS))
