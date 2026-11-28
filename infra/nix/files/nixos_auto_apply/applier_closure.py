"""A staged closure read from the host: its unit tree, boot chain, revision and failed units.

A part of nix/files/nixos-auto-apply.py, which carries the argument for all of it (the header there).
"""
from __future__ import annotations
import hashlib
import json
import os
import pathlib
import typing
from applier_process import NeverStaged, Undetermined, run
from applier_units import ENABLE_DIRECTORY_SUFFIXES, Leniency, Patterns, Tree, unit_forgiven, unit_named_by


# Components of the boot chain that a switch cannot bring into use on its own. `systemd` is here
# and is not in nixpkgs' allowReboot list; see the module header for why this script is stricter.
BOOT_CHAIN = ("kernel", "initrd", "kernel-modules", "systemd")

# What `nixos-rebuild` sets and what the bootloader installer reads to build the boot menu.
SYSTEM_PROFILE = "/nix/var/nix/profiles/system"

CURRENT_SYSTEM = "/run/current-system"


def inactive_units(units: typing.Iterable[str]) -> set[str]:
    """The given units systemd reports `inactive` right now. Any doubt -- systemctl failing, or
    output that does not line up with the request -- answers NONE, which forgives nothing."""
    units = sorted(units)
    if not units:
        return set()
    completed = run(["systemctl", "show", "--property=ActiveState", "--value", "--", *units],
                    check=False, timeout=120)
    states = completed.stdout.split()
    if completed.returncode != 0 or len(states) != len(units):
        return set()
    return {unit for unit, state in zip(units, states) if state == "inactive"}


# -------------------------------------------------------------------------------------------------
# What CI staged
# -------------------------------------------------------------------------------------------------

def staged_system(pin: pathlib.Path) -> pathlib.Path:
    """The closure CI last pinned here, or Undetermined.

    A MISSING PIN IS NOT "NOTHING TO DO". It means CI has never successfully staged onto this host,
    or the pin was removed -- and a host nobody has staged to is exactly the host most likely to be
    behind. Reporting that as up to date would be the vacuous pass this whole mechanism exists to
    prevent, so it is undetermined and the build says so.
    """
    if not pin.is_symlink() and not pin.exists():
        raise NeverStaged(
            f"{pin} does not exist, so no closure has been staged here. Either CI has never "
            f"reached this host or the pin was removed; what this host should be running is "
            f"unknown, which is not the same as it being current.")
    resolved = pin.resolve()
    if not resolved.exists():
        raise Undetermined(f"{pin} points at {resolved}, which is not in the store")
    if not (resolved / "bin" / "switch-to-configuration").is_file():
        raise Undetermined(
            f"{resolved} carries no bin/switch-to-configuration, so it is not a system closure")
    return resolved


# -------------------------------------------------------------------------------------------------
# Test 1 -- the unit tree
# -------------------------------------------------------------------------------------------------

def unit_tree(system: pathlib.Path) -> dict[str, str]:
    """Map every path under the system's systemd unit directory to what it resolves to.

    Symlink targets are compared rather than file contents, and that is not a shortcut: every unit
    in a NixOS closure is a symlink into the store, so an identical target IS identical content, by
    the store's own construction. Regular files (which drop-ins occasionally are) are hashed. The
    directory structure is part of the map because `.wants/` and `.requires/` links are how a unit
    is enabled, and enabling something is not nothing.
    """
    root = system / "etc" / "systemd" / "system"
    tree: dict[str, str] = {}
    if not root.exists():
        raise Undetermined(f"{root} does not exist, so the unit tree could not be read")
    for path in sorted(root.rglob("*")):
        relative = str(path.relative_to(root))
        if path.is_symlink():
            tree[relative] = "link:" + os.readlink(path)
        elif path.is_dir():
            tree[relative] = "dir:"
        elif path.is_file():
            tree[relative] = "file:" + hashlib.sha256(path.read_bytes()).hexdigest()
        else:
            raise Undetermined(f"{path} is neither a link, a directory nor a file")
    return tree


class UnitChanges(typing.NamedTuple):
    blocked: list[str]    # changes no allow-list covers -- these are what refuse a switch
    forgiven: list[str]   # changes a host declared acceptable, kept so they can be REPORTED


def describe_change(name: str, before: Tree, after: Tree) -> str:
    if name not in before:
        return f"{name} (new)"
    if name not in after:
        return f"{name} (removed)"
    return name


def change_forgiven(name: str, restartable: Patterns, never: Patterns, leniency: Leniency) -> bool:
    unit = unit_named_by(name)
    return unit is not None and (
        unit_forgiven(unit, restartable, never)
        or leniency.forgives(unit, never, structural=True))


def enable_directory_forgiven(name: str, decided: dict[str, tuple[str, bool]], before: Tree) -> bool:
    """A NEW `.wants`/`.requires` directory names no unit and is otherwise never forgiven. It is
    forgiven only when it is new and EVERYTHING inside it was forgiven -- i.e. it exists only to
    enable units the rules above already accepted."""
    if name in before or not name.endswith(ENABLE_DIRECTORY_SUFFIXES):
        return False
    inside = [ok for other, (_, ok) in decided.items() if other.startswith(name + "/")]
    return bool(inside) and all(inside)


def unit_changes(running: pathlib.Path, candidate: pathlib.Path,
                 restartable: Patterns = (), never: Patterns = (), leniency: Leniency = Leniency()) -> UnitChanges:
    """Every difference in the unit tree, split by whether this host has declared it acceptable.

    THE FORGIVEN HALF IS KEPT RATHER THAN DISCARDED. A switch that restarts six units is not the
    same event as one that restarts nothing, and an allow-list must not make the difference
    invisible -- it makes it ALLOWED. What is forgiven here is named in the log line and counted in
    `nixos_auto_apply_forgiven_units`, so "auto-apply restarted your CI runners at 04:12" is a
    question the journal can answer afterwards.
    """
    before, after = unit_tree(running), unit_tree(candidate)
    decided = {name: (describe_change(name, before, after),
                      change_forgiven(name, restartable, never, leniency))
               for name in sorted(set(before) | set(after)) if before.get(name) != after.get(name)}
    # A NEW `.wants`/`.requires` directory names no unit and is otherwise never forgiven. It is
    # forgiven here only when it is new and EVERYTHING inside it was forgiven -- i.e. it exists
    # only to enable units the rules above already accepted.
    for name, (description, ok) in list(decided.items()):
        if not ok and enable_directory_forgiven(name, decided, before):
            decided[name] = (description, True)
    blocked = [description for description, ok in decided.values() if not ok]
    forgiven = [description for description, ok in decided.values() if ok]
    return UnitChanges(blocked, forgiven)


def unit_differences(running: pathlib.Path, candidate: pathlib.Path,
                     restartable: Patterns = (), never: Patterns = ()) -> list[str]:
    """The changes that REFUSE a switch. With no allow-list this is every change, which is the
    fleet default and the behaviour every host had before allow-lists existed.
    """
    return unit_changes(running, candidate, restartable, never).blocked


# -------------------------------------------------------------------------------------------------
# Test 2 -- the boot chain
# -------------------------------------------------------------------------------------------------

def boot_chain_differences(booted: pathlib.Path, candidate: pathlib.Path) -> list[str]:
    differences = []
    for component in BOOT_CHAIN:
        old, new = booted / component, candidate / component
        # A component missing from BOTH is a NixOS that does not ship it and is not a difference;
        # missing from one only is unreadable rather than equal, and must not pass quietly.
        if not old.exists() and not new.exists():
            continue
        if not old.exists() or not new.exists():
            raise Undetermined(
                f"{component} exists on one of the booted system and the candidate but not the "
                f"other, so whether a reboot is owed could not be determined")
        if os.path.realpath(old) != os.path.realpath(new):
            differences.append(component)
    return differences


class BootChain(typing.NamedTuple):
    owed: list[str]        # components the candidate replaces relative to /run/booted-system
    introduced: list[str]  # components THIS switch changes (candidate differs from /run/current-system)


def boot_chain(booted: pathlib.Path, running: pathlib.Path, candidate: pathlib.Path) -> BootChain:
    """The reboot the candidate leaves owed, and the part of it this switch is responsible for.

    SPLIT ON 2026-11-10. A reboot already owed by the RUNNING system -- a kernel bump switched by
    hand, pid 1 re-executed, the kernel waiting -- is owed whether or not the next closure is
    applied, so it is reported (`reboot_owed`, `reboot_component`) and blocks nothing. Only what the
    switch itself replaces is `reboot_owed` as a refusal. Before the split, every files-only commit
    after a hand-switched kernel bump was refused on k3s-worker-1 and mongo-1 until a reboot, as
    `classifier_disagreement`, because dry-activate cannot see a reboot it is not causing.
    """
    owed = boot_chain_differences(booted, candidate)
    changed = set(boot_chain_differences(running, candidate))
    return BootChain(owed, [component for component in BOOT_CHAIN if component in changed])


# -------------------------------------------------------------------------------------------------
# Applying
# -------------------------------------------------------------------------------------------------

def configuration_revision(system: pathlib.Path) -> str:
    """The commit the staged closure was built from, for the metric's label.

    Best effort by design: this is a label a human reads, never something a decision turns on. CI
    stamps it via `system.configurationRevision` (nix/modules/fleet/deploy-staging.nix), so a
    closure built from a tree that was not a checkout reports the sentinel rather than a commit --
    which bin/check-nixos-closure-current already treats as its own state, and which is worth
    carrying here for the same reason.
    """
    version = system / "sw" / "bin" / "nixos-version"
    if not version.is_file():
        return "unknown"
    completed = run([str(version), "--json"], check=False, timeout=60)
    if completed.returncode != 0:
        return "unknown"
    try:
        return json.loads(completed.stdout).get("configurationRevision") or "unknown"
    except ValueError:
        return "unknown"


def failed_units() -> set[str]:
    completed = run(["systemctl", "list-units", "--state=failed", "--no-legend",
                     "--plain", "--no-pager"], check=False, timeout=120)
    return {line.split()[0] for line in completed.stdout.splitlines() if line.split()}
