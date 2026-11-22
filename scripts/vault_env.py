"""The Python twin of scripts/vault-env.sh: a secret from the environment, else from the Touch ID vault.

There is no .env.local any more (deleted 2026-10-09, after a credential theft). A script that needs a
secret calls `ensure_vault_env("ZYTE_API_KEY")` before using it: when every named variable is set it
returns and nothing happens; otherwise it re-runs the script under
`secrets run movies <only the missing specs> -- python <script> <args>` (one Touch ID), which execs it
again with the keys in the environment. With no `secrets` CLI on PATH it exits loudly, naming what is
missing — never silently carrying on without the secret.

A spec is VAR or VAR=KEY (the vault key, when it is named differently), as `secrets run` takes them.
"""
from __future__ import annotations

import os
import shutil
import sys
from typing import List, Optional, Sequence


def missing_specs(specs: Sequence[str]) -> List[str]:
    return [spec for spec in specs if not os.environ.get(spec.split("=", 1)[0])]


def exec_under_vault(specs: Sequence[str], command: Optional[Sequence[str]] = None) -> bool:
    """When a spec's variable is unset and the `secrets` CLI is on PATH, `command` (this script again, by default)
    re-run under `secrets run movies <the missing specs>`, this process replaced (it does not return). True when
    nothing is missing; False when something is and there is no vault to ask."""
    missing = missing_specs(specs)
    if not missing:
        return True
    if not shutil.which("secrets"):
        return False
    os.execvp("secrets", ["secrets", "run", "movies", *missing, "--", *(command or [sys.executable, *sys.argv])])


def ensure_vault_env(*specs: str) -> None:
    if not exec_under_vault(specs):
        names = " ".join(spec.split("=", 1)[0] for spec in missing_specs(specs))
        sys.exit(f"{names} not set, and no `secrets` CLI on PATH to read it from the Touch ID vault")
