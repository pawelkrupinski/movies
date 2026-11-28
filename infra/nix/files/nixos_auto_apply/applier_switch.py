"""The real switch: the profile first, the logind race, the leftover lists, the rollback.

A part of nix/files/nixos-auto-apply.py, which carries the argument for all of it (the header there).
"""
from __future__ import annotations
import fcntl
import os
import pathlib
import re
import subprocess
import sys
import time
from applier_closure import CURRENT_SYSTEM, SYSTEM_PROFILE, failed_units
from applier_process import LockContended, SwitchFailed, Undetermined, run


# Where switch-to-configuration-ng keeps what a switch still owes (`start-list`, `restart-list`,
# `reload-list`): written before it stops anything, removed only once that phase completes, never
# written by dry-activate. A non-empty one is a switch that stopped midway; the next switch (and
# every dry-activate) picks it up.
SWITCH_STATE_DIRECTORY = "/run/nixos"

SWITCH_LEFTOVER_LISTS = (("start-list", "start"), ("restart-list", "restart"),
                         ("reload-list", "reload"))

# WHAT `switch-to-configuration switch` EXITING 4 CAN MEAN, from the pinned nixpkgs'
# switch-to-configuration-ng. It is one code for every failure it noticed, system and per-user alike.
# The per-user one is `warning: user activation for <name> failed`: it re-runs itself inside each
# logged-in user's systemd manager, and a manager that logind is stopping at that moment (ten
# seconds after that user's last session closes -- CI's own `nixdeploy` session, which starts this
# unit and disconnects) fails the whole switch while every system unit is fine. That race failed
# monitoring-1's pass on 2026-09-02 and 2026-09-25 and paged `SystemdUnitFailed` for deploys that
# had landed. Every OTHER failure line below is system scope, and any one of them keeps exit 4 fatal.
USER_ACTIVATION_FAILED = re.compile(r"^warning: user activation for \S+ failed$")

SYSTEM_SCOPE_FAILURES = re.compile(
    r"^(Unable to list users with logind"
    r"|Failed to run activate script"
    r"|Failed to restart sysinit-reactivation\.target"
    r"|warning: the following units failed:"
    r"|Failed to (start|stop|restart|reload) (?!user unit )\S)")

# THE LOGIND RACE, 2026-11-10 08:29Z on monitoring-1 (exit 1, not 4). switch-to-configuration-ng
# lists logind's users, then reads each one's GID over D-Bus with `?`: a user whose manager logind
# removes between those two calls -- root's ssh sessions open and close every 10-15 s -- aborts the
# WHOLE switch before its start phase, after /run/current-system already moved. Re-running the
# same switch is exactly what finishes it: the leftover lists above carry what it still owes.
LOGIND_USER_VANISHED = re.compile(r"^\s*Unknown object '/org/freedesktop/login1/user/_\d+'\.?$")

ACTIVATION_STARTED = "activating the configuration..."

LOGIND_RACE_RETRY_DELAY_SECONDS = 15  # past logind's UserStopDelaySec (10 s) for the leaving user


def lock_is_held(lock: pathlib.Path) -> bool:
    with open(lock) as handle:
        try:
            fcntl.flock(handle, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            return True
        fcntl.flock(handle, fcntl.LOCK_UN)
    return False


def switch_in_progress() -> bool:
    """Whether a switch-to-configuration holds its lock right now."""
    lock = pathlib.Path(SWITCH_STATE_DIRECTORY) / "switch-to-configuration.lock"
    return lock.exists() and lock_is_held(lock)


def leftover_lists() -> list[tuple[str, str]]:
    """`(verb, text)` of each list an earlier switch left behind; a list that is absent is skipped."""
    found = []
    for name, verb in SWITCH_LEFTOVER_LISTS:
        try:
            found.append((verb, (pathlib.Path(SWITCH_STATE_DIRECTORY) / name).read_text()))
        except FileNotFoundError:
            continue
    return found


def owed_lines(lists: list[tuple[str, str]]) -> list[str]:
    return [f"{verb} {unit}" for verb, text in lists for unit in sorted(set(text.split()))]


def interrupted_switch() -> list[str]:
    """What an earlier switch left undone, as `start x.target` lines; empty when nothing is owed.

    The lists also exist WHILE a switch runs (an `infra/bin/deploy --apply` beside this pass), so a
    held switch lock is LockContended -- the next pass decides -- never a stopped switch."""
    if switch_in_progress():
        raise LockContended("a switch-to-configuration is running right now")
    return owed_lines(leftover_lists())


def host_is_on(candidate: pathlib.Path) -> bool:
    """Both halves of the switch agree: the running system AND the boot profile name the candidate."""
    target = os.path.realpath(candidate)
    return (os.path.realpath(CURRENT_SYSTEM) == target
            and os.path.realpath(SYSTEM_PROFILE) == target)


def only_user_activation_failed(output: str) -> bool:
    """True when exit 4 is explained by per-user activation alone, so the system switch completed."""
    lines = [line.strip() for line in output.splitlines()]
    return (any(USER_ACTIVATION_FAILED.match(line) for line in lines)
            and not any(SYSTEM_SCOPE_FAILURES.match(line) for line in lines))


def switch_output(switched: subprocess.CompletedProcess[str]) -> str:
    return (switched.stderr or "") + (switched.stdout or "")


def logind_race(returncode: int, output: str, on_candidate: bool) -> bool:
    return (returncode != 0 and on_candidate
            and any(LOGIND_USER_VANISHED.match(line) for line in output.splitlines()))


def logind_retry_note(output: str) -> str:
    # ONE retry, bounded by the switch's own timeout: the race is a window of milliseconds,
    # and a second failure is reported below like any other.
    return ("nixos-auto-apply: the switch stopped midway because a logind user vanished under it; "
            "retrying once: " + " | ".join(line.strip() for line in output.splitlines()
                                           if "Error:" in line or "Unknown object" in line))


def wait_out_logind_race(output: str) -> None:
    print(logind_retry_note(output), file=sys.stderr)
    time.sleep(LOGIND_RACE_RETRY_DELAY_SECONDS)


def run_switch(candidate: pathlib.Path) -> tuple[int, str]:
    """`switch-to-configuration switch`, retried ONCE when the logind race stopped it midway: the
    race is a window of milliseconds, and a second failure is reported like any other."""
    switch = [str(candidate / "bin" / "switch-to-configuration"), "switch"]
    switched = run(switch, check=False, timeout=3600)
    output = switch_output(switched)
    if logind_race(switched.returncode, output, host_is_on(candidate)):
        wait_out_logind_race(output)
        switched = run(switch, check=False, timeout=3600)
        output = switch_output(switched)
    return switched.returncode, output


def check_switch(candidate: pathlib.Path, returncode: int, output: str) -> None:
    """Raise when the switch failed, SwitchFailed once it had started and Undetermined before."""
    if returncode == 4 and only_user_activation_failed(output):
        print("nixos-auto-apply: the system switch completed; only a per-user activation failed "
              "(a user manager stopping mid-switch): "
              + " | ".join(line for line in output.splitlines() if "fail" in line.lower()),
              file=sys.stderr)
    elif returncode != 0:
        # THE TAIL, NOT THE HEAD: switch-to-configuration names what failed at the END of its
        # output, and the first 400 characters are the preamble -- which is all the journal kept
        # of both exit-4 incidents above.
        switch = str(candidate / "bin" / "switch-to-configuration")
        failure = f"{switch} switch exited {returncode}: ...{output.strip()[-600:]}"
        if ACTIVATION_STARTED in output or host_is_on(candidate):
            raise SwitchFailed(failure)
        raise Undetermined(failure)  # refused before activating: nothing on the host changed


def failures_after_settling(before: set[str]) -> set[str]:
    time.sleep(5)  # systemd settles; a unit that fails on activation does so within seconds
    return failed_units() - before


def rollback_message(previous: pathlib.Path, newly_failed: set[str], undo_failed: bool,
                     undo_output: str) -> str:
    failed = ", ".join(sorted(newly_failed))
    if undo_failed:
        return (f"units failed after the switch ({failed}) AND the rollback to {previous.name} "
                f"failed: {undo_output.strip()[:300]}")
    return f"units failed after the switch and it was rolled back to {previous.name}: {failed}"


def roll_back(previous: pathlib.Path, newly_failed: set[str]) -> str:
    """Named explicitly rather than `nix-env --rollback`, which walks to the previous GENERATION --
    not necessarily the closure this pass replaced, if anything else touched the profile in
    between. What is being undone is this switch, so say which path that was."""
    undo = run(["nix-env", "--profile", SYSTEM_PROFILE, "--set", str(previous)],
               check=False, timeout=600)
    if undo.returncode == 0:
        undo = run([str(previous / "bin" / "switch-to-configuration"), "switch"],
                   check=False, timeout=3600)
    return rollback_message(previous, newly_failed, undo.returncode != 0,
                            undo.stderr or undo.stdout)


def aftermath(newly_failed: set[str], previous: pathlib.Path, rollback_on_failure: bool) -> str:
    """The failure text for units the switch broke, rolled back when asked; empty when none."""
    if not newly_failed:
        return ""
    if not rollback_on_failure:
        return ("units failed after the switch and no rollback was attempted: "
                + ", ".join(sorted(newly_failed)))
    return roll_back(previous, newly_failed)


def activate(candidate: pathlib.Path, rollback_on_failure: bool) -> str:
    """Switch to the candidate, then confirm the host is no worse than it was.

    THE VERIFICATION IS A COMPARISON, NOT AN ABSOLUTE. A host with a unit already failing before
    the switch would fail an "are there any failed units" test every time, so this records the set
    beforehand and objects only to units that were healthy and are not any more. Otherwise the
    first pre-existing fault on a host disables auto-apply on it permanently and silently.

    Rolling back is cheap here in a way it is not in general: the gate has already established that
    this switch restarts nothing, so the only things to undo are files. A rollback that itself
    fails is reported and not retried -- at that point the host needs a person, and the useful
    thing this script can do is stop touching it.
    """
    before = failed_units()
    previous = pathlib.Path(os.path.realpath(SYSTEM_PROFILE))

    # THE PROFILE IS SET FIRST, AND THAT ORDER IS LOAD-BEARING. `switch-to-configuration switch`
    # runs the bootloader installer, and the installer builds the boot menu from the generations in
    # /nix/var/nix/profiles/system. Activate without setting the profile and the running system is
    # the new one while the DEFAULT BOOT ENTRY still points at the old generation -- so the machine
    # is correct until it reboots and then silently is not. That is also why this does not simply
    # run the command the staging tool prints: `sudo <path>/bin/switch-to-configuration switch`
    # alone leaves the profile untouched, records no generation, and gives `--rollback` nothing to
    # roll back to.
    run(["nix-env", "--profile", SYSTEM_PROFILE, "--set", str(candidate)], timeout=600)
    returncode, output = run_switch(candidate)
    check_switch(candidate, returncode, output)
    return aftermath(failures_after_settling(before), previous, rollback_on_failure)
