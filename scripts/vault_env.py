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
import sys
from typing import Callable, List, Mapping, NoReturn, Optional, Sequence

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import machine  # noqa: E402


class Vault:
    """What a script asks of the Touch ID vault: the environment it reads, whether the `secrets` CLI is on PATH, and the
    exec that replaces this process (each injected: `here()` is the machine's, a unit test hands in fakes)."""

    def __init__(self, env: Mapping[str, str], secrets_on_path: bool, replace: Callable[[Sequence[str]], NoReturn]) -> None:
        self.env, self.secrets_on_path, self.replace = env, secrets_on_path, replace

    @classmethod
    def here(cls) -> "Vault":
        on_path = machine.status(["/bin/sh", "-c", "command -v secrets"], out=machine.QUIET) == 0
        return cls(machine.environment(), on_path, machine.replace)

    def missing_specs(self, specs: Sequence[str]) -> List[str]:
        return [spec for spec in specs if not self.env.get(spec.split("=", 1)[0])]

    def exec_under_vault(self, specs: Sequence[str], command: Optional[Sequence[str]] = None) -> bool:
        """When a spec's variable is unset and the `secrets` CLI is on PATH, `command` (this script again, by default)
        re-run under `secrets run movies <the missing specs>`, this process replaced (it does not return). True when
        nothing is missing; False when something is and there is no vault to ask."""
        missing = self.missing_specs(specs)
        if not missing:
            return True
        if not self.secrets_on_path:
            return False
        self.replace(["secrets", "run", "movies", *missing, "--", *(command or [sys.executable, *sys.argv])])


def missing_specs(specs: Sequence[str]) -> List[str]:
    return Vault.here().missing_specs(specs)


def exec_under_vault(specs: Sequence[str], command: Optional[Sequence[str]] = None) -> bool:
    return Vault.here().exec_under_vault(specs, command)


def ensure_vault_env(*specs: str) -> None:
    vault = Vault.here()
    if not vault.exec_under_vault(specs):
        names = " ".join(spec.split("=", 1)[0] for spec in vault.missing_specs(specs))
        sys.exit(f"{names} not set, and no `secrets` CLI on PATH to read it from the Touch ID vault")
