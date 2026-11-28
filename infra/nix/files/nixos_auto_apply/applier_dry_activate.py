"""The dry activation, run and classified.

A part of nix/files/nixos-auto-apply.py, which carries the argument for all of it (the header there).
"""
from __future__ import annotations
import filecmp
import os
import pathlib
from applier_classify import DryActivate, classify_output
from applier_process import LockContended, Undetermined, run
from applier_units import Leniency, Patterns


def pid1_reexec_is_inert(candidate: pathlib.Path, pid1_exe: str = "/proc/1/exe",
                         system_conf: str = "/etc/systemd/system.conf") -> bool:
    """True when re-executing PID 1 into the candidate's systemd would change nothing at all.

    switch-to-configuration-ng prints `would restart systemd` when `canonicalize("/proc/1/exe")`
    differs from the candidate's `lib/systemd/systemd`, or `/etc/systemd/system.conf` resolves
    elsewhere. The first half has a FALSE POSITIVE this fleet hit: the nightly `nix-store --optimise`
    (fleet/nix-gc.nix) hard-links a duplicate over the running binary's store path, the kernel then
    reports PID 1's exe as "<path> (deleted)", the canonicalize fails, and the switch compares
    against the literal "/unknown" -- so every closure after it "restarts systemd" and the unit tree,
    which rightly sees nothing, disagrees. Measured 2026-09-28/29 on all three hosts: 4.5h and 18h of
    `classifier_disagreement`, cleared only by a hand switch re-executing the identical binary.

    BYTES, NOT PATHS, because the path is exactly what the optimiser broke. `open("/proc/1/exe")`
    still reaches the inode PID 1 runs, deleted name or not. Anything unreadable answers False, so
    this can only ever forgive the one line, never block more than before.
    """
    try:
        if os.path.realpath(system_conf) != os.path.realpath(candidate / "etc" / "systemd" / "system.conf"):
            return False
        return filecmp.cmp(pid1_exe, candidate / "systemd" / "lib" / "systemd" / "systemd",
                           shallow=False)
    except OSError:
        return False


def dry_activate(candidate: pathlib.Path, timeout: int, restartable: Patterns = (), never: Patterns = (),
                 leniency: Leniency = Leniency()) -> DryActivate:
    """Run the dry activation and classify EVERY line it prints.

    Output goes to stderr and the exit code is 0 whatever it finds, so neither of those is the
    signal; the lines are. Note that a genuine no-op is not silent -- measured on 10.0.0.12, an
    unchanged system still prints the inhibitor note, "would activate the configuration..." and two
    sops-install-secrets lines.
    """
    binary = candidate / "bin" / "switch-to-configuration"
    if not binary.exists():
        raise Undetermined(f"{binary} does not exist, so the candidate could not be dry-activated")
    completed = run([str(binary), "dry-activate"], check=False, timeout=timeout)
    if completed.returncode != 0:
        output = (completed.stderr or completed.stdout).strip()
        # Text match, not exit code alone: the code that accompanies "could not acquire lock" is
        # nix's own and not guaranteed stable across versions, but that message is nix's fixed
        # wording for exactly this contention and nothing else raises it here.
        if "could not acquire lock" in output.lower():
            raise LockContended(f"dry-activate exited {completed.returncode}: {output[:400]}")
        raise Undetermined(f"dry-activate exited {completed.returncode}: {output[:400]}")
    return classify_output(completed.stderr + "\n" + completed.stdout, restartable, never,
                           pid1_unchanged=pid1_reexec_is_inert(candidate), leniency=leniency)
