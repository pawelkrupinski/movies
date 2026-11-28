"""What `bin/deploy` DECIDES from what it measured (infra/bin/deploy; unit-tested in infra/test/test_deploy.py): how to
reach a host, which closure to activate, whether the checkout may deploy, and whether the host came out of the switch
healthy. Nothing here reads the host, the checkout or the clock."""

from __future__ import annotations

from typing import Any, List, Mapping, NamedTuple, Optional, Set, Tuple

BOOT_CHAIN = ("kernel", "initrd", "kernel-modules", "systemd")


class Refused(Exception):
    """A deploy that must not go ahead. Exit 1."""


class Unmeasured(Exception):
    """Something could not be read. Exit 2 -- never the same as "nothing to do"."""


def staleness_verdict(behind: int, ahead: int, dirty: bool) -> Tuple[bool, str]:
    """(ok, message). Behind main is refused: deploying it rolls the host back. Ahead (an unpushed
    fix being tried) and dirty are allowed and SAID, because the revision on the host will say so."""
    if behind > 0:
        return False, (f"this checkout is {behind} commit(s) behind origin main: deploying it would "
                       "roll the host back. `git pull --ff-only origin main` first "
                       "(or --allow-stale for a deliberate rollback).")
    notes: List[str] = []
    if ahead:
        notes.append(f"{ahead} commit(s) ahead of origin main (not pushed)")
    if dirty:
        notes.append("uncommitted changes (the revision will be a -dirty one)")
    return True, "checkout: " + ("; ".join(notes) if notes else "at origin main")


def choose_target(host: str, address: str, ssh_g_hostname: str, ssh_user: str = "") -> str:
    """How to ssh to `host`. An alias NAMED LIKE THE HOST wins (`ssh -G` resolves it to some other
    hostname), because a bare IP can bind a restricted key (monitoring-1, borg-ovh-1). An alias that
    points somewhere OTHER than the flake's address is refused: the IP moved and the alias did not,
    so it would reach whatever machine holds the old address now."""
    if ssh_g_hostname and ssh_g_hostname != host:
        if address and ssh_g_hostname != address:
            raise Refused(f"ssh alias {host} points at {ssh_g_hostname} but the flake says {host} is "
                          f"{address}; fix ~/.ssh/config before deploying")
        return f"{ssh_user}@{host}" if ssh_user else host
    if not address:
        raise Unmeasured(f"no ssh alias {host} and the flake gives it no address")
    return f"{ssh_user}@{address}" if ssh_user else address


def choose_candidate(expected: str, expected_present: bool, pinned: str, pinned_rev: str,
                     same_tree: bool) -> Optional[Tuple[str, str]]:
    """(closure, why) to deploy, or None when nothing suitable is on the host.

    The closure this checkout evaluates to wins when it is there. Otherwise CI's PIN is used when it
    was built from a commit whose flake tree is IDENTICAL to this checkout's: staging runs only for
    commits that touch the flake's tree, and every other commit still changes `self.rev` -- so the
    exact closure of HEAD is usually never staged, while the pin is the same configuration stamped
    with the last commit that changed it."""
    if expected_present:
        return expected, "this checkout's closure"
    if pinned and pinned_rev and same_tree:
        return pinned, f"CI's staged closure, built from {pinned_rev[:12]} (same flake tree as HEAD)"
    return None


def boot_changes(facts: Mapping[str, Any]) -> List[str]:
    return [c for c in BOOT_CHAIN if facts.get(f"boot_{c}") == "differs"]


def unit_ok(fields: List[str]) -> bool:
    """ActiveState, Result, Type -> healthy? A oneshot that ran and exited is `inactive`+`success`."""
    active, result, kind = (fields + ["", "", ""])[:3]
    if active in ("active", "activating", "reloading"):
        return True
    return kind == "oneshot" and active == "inactive" and result == "success"


def _revision_problem(expected_rev: str, running_rev: str) -> List[str]:
    if expected_rev and running_rev != expected_rev:
        return [f"running revision is {running_rev or 'unknown'}, expected {expected_rev}"]
    return []


def _failed_problem(failed_before: Set[str], failed_after: Set[str]) -> List[str]:
    new_failed = sorted(failed_after - failed_before)
    return [f"newly failed units: {', '.join(new_failed)}"] if new_failed else []


def _unhealthy_problem(unit_fields: Mapping[str, List[str]]) -> List[str]:
    bad = sorted(u for u, f in unit_fields.items() if not unit_ok(f))
    if not bad:
        return []
    return ["not active after the switch: " + ", ".join(f"{u} ({' '.join(unit_fields[u]) or '?'})" for u in bad)]


def verify_verdict(candidate: str, now: str, expected_rev: str, running_rev: str,
                   failed_before: Set[str], failed_after: Set[str], unit_fields: Mapping[str, List[str]]) -> List[str]:
    """Problems, empty when the host runs the candidate, at the expected revision, with no unit
    newly failed and every started/restarted unit healthy."""
    running = [] if now == candidate else [f"/run/current-system is {now}, not {candidate}"]
    return (running + _revision_problem(expected_rev, running_rev) + _failed_problem(failed_before, failed_after)
            + _unhealthy_problem(unit_fields))


class SwitchOutcome(NamedTuple):
    status: Optional[int]   # switch-to-configuration's exit status; None: the unit ended without one
    journal: List[str]


def switch_ended(facts: Mapping[str, Any]) -> Optional[SwitchOutcome]:
    """The switch's outcome once the status poll shows it ended, None while it runs. The status file is written before
    the unit exits: a unit gone without one is a killed switch."""
    if "status" in facts:
        return SwitchOutcome(facts["status"], facts["journal"])
    if facts.get("state", "") in ("inactive", "failed"):
        return SwitchOutcome(None, facts["journal"])
    return None


def switch_problem(unit: str, outcome: SwitchOutcome) -> Optional[str]:
    """What the switch's own result says is wrong; None when it exited 0."""
    if outcome.status == 0:
        return None
    what = (f"exited {outcome.status}" if outcome.status is not None
            else "ended without an exit status (killed?)")
    return f"switch-to-configuration in {unit} {what}"
