"""EVERY CHECK THAT IS PYTHON'S, by name (pre_push_checks.Python runs them, scripts/pre_push.py --check answers one):
the light checks (pre_push_light) and the script self-tests (pre_push_scripttests). Each later migration stage adds
its own; a name not here is still the hook's shell function `check_<name>`."""
from __future__ import annotations

from typing import Dict

import pre_push_light as light
import pre_push_scripttests as scripttests

CHECKS: Dict[str, light.Light] = {**light.CHECKS, "scripttests": scripttests.scripttests}
