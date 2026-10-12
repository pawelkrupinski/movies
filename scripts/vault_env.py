"""The Python twin of scripts/vault-env.sh: a secret from the environment, else from the Touch ID vault.

There is no .env.local any more (deleted 2026-10-09, after a credential theft). A script that needs a
secret calls `ensure_vault_env("ZYTE_API_KEY")` before using it: when every named variable is set it
returns and nothing happens; otherwise it re-runs the script under
`secrets run movies <only the missing specs> -- python <script> <args>` (one Touch ID), which execs it
again with the keys in the environment. With no `secrets` CLI on PATH it exits loudly, naming what is
missing — never silently carrying on without the secret.

A spec is VAR or VAR=KEY (the vault key, when it is named differently), as `secrets run` takes them.
"""
import os
import shutil
import sys


def ensure_vault_env(*specs):
    missing = [spec for spec in specs if not os.environ.get(spec.split("=", 1)[0])]
    if not missing:
        return
    names = " ".join(spec.split("=", 1)[0] for spec in missing)
    if not shutil.which("secrets"):
        sys.exit(f"{names} not set, and no `secrets` CLI on PATH to read it from the Touch ID vault")
    os.execvp("secrets", ["secrets", "run", "movies", *missing, "--", sys.executable, *sys.argv])
