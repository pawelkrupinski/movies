"""Running commands, and the exceptions a pass ends in.

A part of nix/files/nixos-auto-apply.py, which carries the argument for all of it (the header there).
"""
from __future__ import annotations
import subprocess


class Undetermined(Exception):
    """Something could not be measured. Never the same as finding nothing wrong."""


class LockContended(Undetermined):
    """dry-activate could not get the Nix profile lock -- a KNOWN, self-resolving race.

    Found 2026-08-25: the module's own timer, adopted on a host that has been up for a while,
    computes its OnBootSec trigger as already elapsed and fires within the same activation that
    installs it -- while `nixos-rebuild switch` is itself still holding this exact lock. Every
    host that adopts the module hits this on its FIRST run.

    It is still `undetermined` -- nothing was measured -- but it is not the open-ended
    "something is wrong" the base class means. The next scheduled pass (or the next switch)
    finds the lock free and succeeds; verified by hand on the host that found this, first try.
    A unit failing for a race it did not cause and will resolve on its own is worse than useless:
    it pages fleet-wide (failed units are alarmed everywhere) and makes nixos_deploy refuse to
    call a perfectly good deploy finished, on a host that ends up fine two minutes later.
    """


class NeverStaged(Undetermined):
    """CI has never pinned a closure here.

    A SEPARATE CLAIM from "this pass could not measure", and separate because the repair is
    different: a host CI has never reached needs staging wired up or a first deploy from a
    checkout, whereas a pass that could not measure needs somebody to read why. Measured on
    10.0.0.12 on 2026-08-24, this is not hypothetical -- the host runs the exact tip of main and
    has `nixdeploy` present, and still has no pin, because it was last deployed by hand rather
    than through CI. Reporting that as a generic error would leave every such host amber for a
    reason nobody could act on.
    """


class SwitchFailed(Exception):
    """`switch-to-configuration switch` exited non-zero AFTER it started activating, so the host is
    part-way onto the candidate. A verdict (`blocked`, `switch_failed`), never `could_not_measure`:
    what is wrong is known, and it needs a person (or the next switch) to finish the job."""


class Blocked(Exception):
    """A change is pending that this script must not apply on its own."""

    def __init__(self, reason: str, detail: str) -> None:
        super().__init__(detail)
        self.reason = reason
        self.detail = detail


def unrunnable(argv: list[str], exc: Exception, timeout: int) -> str:
    if isinstance(exc, FileNotFoundError):
        return f"{argv[0]} is not on PATH: {exc}"
    return f"{' '.join(argv)} did not finish within {timeout}s"


def checked(argv: list[str], completed: subprocess.CompletedProcess[str], check: bool) -> None:
    if check and completed.returncode != 0:
        raise Undetermined(
            f"{' '.join(argv)} exited {completed.returncode}: "
            f"{(completed.stderr or completed.stdout).strip()[:400]}")


def run(argv: list[str], *, cwd: str | None = None, env: dict[str, str] | None = None,
        check: bool = True, timeout: int = 3600) -> subprocess.CompletedProcess[str]:
    try:
        completed = subprocess.run(
            argv, cwd=cwd, env=env, capture_output=True, text=True, timeout=timeout)
    except (FileNotFoundError, subprocess.TimeoutExpired) as exc:
        raise Undetermined(unrunnable(argv, exc, timeout)) from exc
    checked(argv, completed, check)
    return completed
