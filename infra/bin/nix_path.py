#!/usr/bin/env python3
"""Where nix is: PATH if it has it, else the standard install locations. The ONE place infra/bin's tools
(deploy, stage-nixos-closures, nix-update, fleet_hosts.py, check) find nix.

WHY (2026-11-17). Nix was reinstalled (Determinate, 2026-11-15) at /nix/var/nix/profiles/default/bin/nix, and a
login shell's PATH does not always carry it: the installer's shell hook is in /etc/zshrc, which a macOS upgrade
rewrites and a non-interactive shell never reads. `infra/bin/deploy monitoring-1` then answered
`COULD NOT MEASURE: nix is not on PATH` with nix installed and working. The tools now look where nix installs, and
put its bin directory on their own PATH, so every nix-* they start (nix-store, nix-env, nix's own helpers) is found
the same way. Not found anywhere is an ERROR that says what to do -- never a skipped measurement.

    import nix_path; nix_path.put_nix_on_path()     # in a Python tool, before its first nix call
    PATH="$(python3 bin/nix_path.py):$PATH"         # in a shell tool: prints nix's bin directory, or exits 1

NIX_PATH_CANDIDATES (colon-separated) overrides the install locations; the tests use it.
"""
import os
import shutil
import sys

# Determinate's and the upstream multi-user installer's default profile; a single-user install's profile; NixOS.
CANDIDATES = ("/nix/var/nix/profiles/default/bin/nix", "~/.nix-profile/bin/nix", "/run/current-system/sw/bin/nix")


class NixNotFound(Exception):
    """nix is on neither PATH nor any install location."""


def _candidates():
    override = os.environ.get("NIX_PATH_CANDIDATES")
    return tuple(override.split(":")) if override else CANDIDATES


def find_nix(path=None, home=None, candidates=None):
    """nix's absolute path: the one PATH finds, else the first executable install location."""
    found = shutil.which("nix", path=os.environ.get("PATH", os.defpath) if path is None else path)
    if found:
        return os.path.abspath(found)
    home = os.path.expanduser("~") if home is None else home
    candidates = _candidates() if candidates is None else candidates
    for candidate in candidates:
        resolved = home + candidate[1:] if candidate.startswith("~/") else candidate
        if os.path.isfile(resolved) and os.access(resolved, os.X_OK):
            return resolved
    raise NixNotFound(
        "nix not found on PATH or at " + ", ".join(candidates) + "; install it "
        "(curl -fsSL https://install.determinate.systems/nix | sh -s -- install) or add its bin directory to "
        "PATH (export PATH=/nix/var/nix/profiles/default/bin:$PATH)")


def put_nix_on_path(environ=None, candidates=None):
    """Find nix and make sure `environ`'s PATH (the process's own by default) reaches it, so a bare `nix` in an
    argv -- and every nix-* tool beside it -- runs. Returns nix's path; raises NixNotFound."""
    environ = os.environ if environ is None else environ
    path = environ.get("PATH", os.defpath)
    nix = find_nix(path=path, home=environ.get("HOME"), candidates=candidates)
    if not shutil.which("nix", path=path):
        environ["PATH"] = os.path.dirname(nix) + (":" + path if path else "")
    return nix


def main():
    try:
        print(os.path.dirname(find_nix()))
    except NixNotFound as exc:
        print(exc, file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
