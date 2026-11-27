"""EVERY CHECK, by name (pre_push_checks.Processes starts them, scripts/pre_push.py --check answers one):
the light checks (pre_push_light), the script self-tests (pre_push_scripttests), the sbt check (pre_push_sbt), the
identity area (pre_push_area), the flip backlog, the identity drift check and the heavy rows (pre_push_heavy). A name
not here is no check (exit 127)."""
from __future__ import annotations

from typing import Dict

import pre_push_area as area
import pre_push_heavy as heavy
import pre_push_light as light
import pre_push_sbt as sbt
import pre_push_scripttests as scripttests

CHECKS: Dict[str, light.Light] = {
    **light.CHECKS, "scripttests": scripttests.scripttests, "sbt": sbt.sbt, "identity": area.identity,
    "flipbacklog": heavy.flipbacklog, "identitydrift": heavy.identitydrift, "itrest": heavy.itrest,
    "itidentity": heavy.itidentity, "e2ecorpus": heavy.e2ecorpus}
