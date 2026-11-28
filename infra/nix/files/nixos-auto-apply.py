#!/usr/bin/env python3
"""Activate the closure CI already staged here, but ONLY when doing so would disturb nothing.

WHY THIS EXISTS. `.github/workflows/nix-stage-closures.yml` builds every host's closure on merge,
signs it and `nix copy`s it here, then deliberately stops: the staging key is restricted to a forced
command that cannot activate anything, and `nixdeploy` has no sudo. The closure is pinned at
`/var/lib/nixdeploy/staged-system`; the one step nobody automated is activating it, because that is
the step that can break a running machine. This is that step, taken only when it provably breaks
nothing; anything that would disturb a running service, or that this script cannot classify, is left
for a person.

IT DOES NOT FETCH, BUILD, OR TRUST ANYTHING NEW: no git remote, no deploy key, no evaluation. It acts
on a path CI pinned through a channel that can do nothing else, whose store paths carry a signature
from `trustedPublicKeys`. The trust decision was made upstream; this file decides only WHEN. That is
worth the machinery because routine commits (a package, an ssh key) are the ones nobody schedules a
deploy for, and they accumulate until a real deploy carries six months of unrelated change.

WHAT DECIDES. Three tests, and a deployment happens only if all three agree it is inert:

  1. THE UNIT TREE, byte for byte. `restartTriggers` and `reloadTriggers` are rendered INTO the
     unit files as `X-Restart-Triggers`/`X-Reload-Triggers`, and switch-to-configuration acts on
     units whose files differ, that are gone, or that are new. So an identical set of unit paths
     with identical contents is a SUFFICIENT condition for "nothing is stopped, started, restarted
     or reloaded" -- and, unlike parsing a message, it stays true across NixOS releases. This is
     the primary test.

  2. THE BOOT CHAIN, against `/run/booted-system` and never `/run/current-system`: a new kernel
     activates silently and the machine keeps running the old one, so comparing generation symlinks
     answers a different question. Like nixpkgs' `allowReboot` it compares kernel, initrd and
     kernel-modules, plus `systemd` (replacing pid 1's package is not for unattended). Only the debt
     THIS switch introduces refuses it; a reboot already owed (current != booted with all four
     components matching, the residue of any post-boot switch) is reported and blocks nothing.

  3. `switch-to-configuration dry-activate`, as a CROSS-CHECK and never alone: its man page says "The
     list of changes is not guaranteed to be complete" and its wording is not a stable interface. It
     earns its place by being able to disagree: if the unit tree says nothing changes and dry-activate
     names a unit it would restart, the model in test 1 is wrong, and the response is to refuse.

FAIL CLOSED, INCLUDING ON SILENCE. An unrecognised line from dry-activate blocks the deployment: new
wording turns the gate OFF, not on. The cost is that a nixpkgs bump can wedge auto-apply until
somebody adds a pattern here, visible in the metric; the alternative is a gate that silently matches
nothing and applies everything.

NOT COVERED: ACTIVATION SCRIPTS. `sops-install-secrets` genuinely runs during a dry run, and activation
snippets may do work neither test predicts. Differences are REPORTED (`activation_changed`) and do not
block. That is the residual risk of this design, the reason the first hosts to run it are development
and sandbox ones, and the thing to revisit before a host holding real money is added.

EXIT CODES follow terraform's `plan -detailed-exitcode`: 0 nothing to do or a change was applied, 1 a
change is pending that a person must decide on, 2 nothing could be determined. 2 must never be read as
0: "nobody could ask" and "there is nothing to ask about" are different claims.
"""
# PORT NOTE. A FAITHFUL PORT of bitcashier's nix/files/nixos-auto-apply.py: every rule in the
# classification table was put there by a specific measurement, and a rewrite would have kept the
# rules and lost the reasons. The host names and addresses in the comments (`dev-bitcoind`,
# `dev-postgres`, `ci-runner`, `10.0.0.12`) belong to the fleet it came from; they are kept, with
# their dates, because "measured on a real host on this date" is what makes a rule worth obeying.
# test/test_auto_apply_classify.py drives `classify_output` with recorded switch output.

# `from __future__ import annotations` so this file parses on any Python from 3.7 up: it runs in a
# NixOS host's systemd unit (3.12+), a CI container and an operator's Mac (system Python 3.9, which
# evaluates `str | None` at import time as a TypeError); the test imports the module directly.
from __future__ import annotations
import argparse
import fcntl
import filecmp
import fnmatch
import hashlib
import json
import os
import pathlib
import re
import subprocess
import sys
import tempfile
import time
import typing

Patterns = typing.Collection[str]
Tree = dict[str, str]
EXIT_CURRENT = 0
EXIT_PENDING = 1
EXIT_UNDETERMINED = 2
# A unit's own installing switch racing this pass's dry-activate for the same Nix lock. Distinct
# from EXIT_UNDETERMINED because systemd must NOT mark the unit failed for it -- see LockContended.
EXIT_LOCK_CONTENDED = 3
# A host CI has never staged to: the ordinary state of a freshly converted machine, so NOT a failure.
# Exit 2 made the unit fail during activation and the FIRST deploy to any new host report failure
# while succeeding (k3s-worker-1, 2026-08-29). The verdict is still published as
# `undetermined`/`never_staged`; the alert on a host stuck here fires once it has persisted.
EXIT_NEVER_STAGED = 4

# Components of the boot chain that a switch cannot bring into use on its own. `systemd` is here
# and is not in nixpkgs' allowReboot list; see the module header for why this script is stricter.
BOOT_CHAIN = ("kernel", "initrd", "kernel-modules", "systemd")

# What `nixos-rebuild` sets and what the bootloader installer reads to build the boot menu.
SYSTEM_PROFILE = "/nix/var/nix/profiles/system"
CURRENT_SYSTEM = "/run/current-system"

# Where switch-to-configuration-ng keeps what a switch still owes (`start-list`, `restart-list`,
# `reload-list`): written before it stops anything, removed only once that phase completes, never
# written by dry-activate. A non-empty one is a switch that stopped midway; the next switch (and
# every dry-activate) picks it up.
SWITCH_STATE_DIRECTORY = "/run/nixos"
SWITCH_LEFTOVER_LISTS = (("start-list", "start"), ("restart-list", "restart"),
                         ("reload-list", "reload"))

# Every line switch-to-configuration-ng can print that does NOT describe a unit being disturbed.
# Anchored whole-line matches: a substring match would let "would restart the following units: x"
# be swallowed by a prefix rule.
BENIGN_LINES = (
    re.compile(r"^Not checking switch inhibitors \(action = dry-activate\)$"),
    re.compile(r"^would activate the configuration\.\.\.$"),
    # Runs for real during a dry run, on every invocation, and says only that it read the host key.
    re.compile(r"^sops-install-secrets: Imported /etc/ssh/ssh_host_[a-z0-9]+_key as (?:GPG|age) key with fingerprint \S+$"),
    # ADDED 2026-08-27. A secret with no prior existence cannot be depended on by anything already
    # running; a unit that reads the new path prints its own line and is classified by UNIT_LINES.
    # Only the secret's ARRIVAL is covered: a secret CHANGED or REMOVED under a process that already
    # read it stays a person's decision.
    # `secrets?` SINCE 2026-09-05: sops-nix pluralises against the number of secrets added, and
    # monitoring-1 adding exactly one (`would add secret:`) refused a closure as `unrecognised_output`.
    # Recorded verbatim in test/test_auto_apply_classify.py.
    re.compile(r"^would add secrets?: (.+)$"),
)

# Lines that name units. `kind` is what would happen; `reload` is the only one an allow-list can
# forgive, because a reload leaves the process running. "would NOT stop"/"would NOT restart" are
# counted as disruptive on purpose: they do not disturb anything today, but they are emitted only
# when a unit CHANGED and was skipped, which means the running service no longer matches its
# definition -- a state that must be a person's decision, not a silent one.
UNIT_LINES = (
    (re.compile(r"^would stop the following units: (.+)$"), "stop"),
    (re.compile(r"^would NOT stop the following changed units: (.+)$"), "changed-but-skipped"),
    # ADDED 2026-08-25, found by a prover case rather than by a host. The user-unit variant of this
    # line was in the table and the SYSTEM one was not, so the commonest way a unit reports
    # "changed, and I was told to leave it alone" -- `restartIfChanged = false`, which this module
    # sets on its own unit -- fell through to `unrecognised`. That still BLOCKED, which is why no
    # host ever misapplied anything and why nothing surfaced it: the gate refused with
    # "switch-to-configuration printed lines this classifier has no rule for" instead of naming the
    # unit. Fails safe, reads as a mystery.
    (re.compile(r"^would NOT restart the following changed units: (.+)$"), "changed-but-skipped"),
    (re.compile(r"^would restart the following units: (.+)$"), "restart"),
    (re.compile(r"^would start the following units: (.+)$"), "start"),
    (re.compile(r"^would reload the following units: (.+)$"), "reload"),
    (re.compile(r"^would stop the following user units: (.+)$"), "stop"),
    (re.compile(r"^would NOT restart the following changed user units: (.+)$"), "changed-but-skipped"),
    (re.compile(r"^would restart the following user units: (.+)$"), "restart"),
    (re.compile(r"^would start the following user units: (.+)$"), "start"),
    (re.compile(r"^would reload the following user units: (.+)$"), "reload"),
)

# Not unit-shaped and never forgivable.
HARD_LINES = (
    (re.compile(r"^would restart systemd$"), "restart-systemd"),
    (re.compile(r"^would stop swap device: (.+)$"), "stop-swap"),
)

# WHICH KINDS AN ALLOW-LIST MAY FORGIVE: the three ways a switch disturbs a unit -- stopped (removed),
# started (added), restarted (changed); naming a unit restartable accepts all three.
# `changed-but-skipped` IS DELIBERATELY NOT HERE: the running process no longer matches its definition,
# a divergence rather than a disturbance, and an allow-list is permission to disturb, not to leave the
# host quietly inconsistent with main.
FORGIVABLE_KINDS = ("stop", "start", "restart")

# The suffixes systemd gives units, used to find the unit a path under etc/systemd/system belongs
# to. Listed rather than pattern-matched because the point is to recognise ONLY these: a path whose
# unit cannot be identified is never forgiven, and a wildcard would make "unidentifiable" forgivable.
UNIT_SUFFIXES = (".service", ".socket", ".target", ".timer", ".path", ".mount",
                 ".automount", ".swap", ".slice", ".scope", ".device")


def unit_named_by(path: str) -> str | None:
    """The unit a path under `etc/systemd/system` belongs to, or None if it names no unit.

    Three shapes reach here: `x.service` (the unit), `multi-user.target.wants/x.service` (the link that
    ENABLES it) and `x.service.d/override.conf` (a drop-in that configures it). A component ending `.d`
    is a drop-in directory and is stripped; `multi-user.target.wants` ends `.wants`, not a unit suffix,
    so the link is not read as a change to `multi-user.target`. Last-wins: the change is to the leaf.
    A `.wants` directory itself names no unit and returns None, so creating one is never forgiven:
    enabling a unit is visible as the link inside it.
    """
    found = None
    for component in path.split("/"):
        name = component[:-2] if component.endswith(".d") else component
        if name.endswith(UNIT_SUFFIXES):
            found = name
    return found


def matches_any(unit: str, patterns: Patterns) -> bool:
    return any(fnmatch.fnmatchcase(unit, pattern) for pattern in patterns)


def unit_forgiven(unit: str, patterns: Patterns, never: Patterns = ()) -> bool:
    """Whether an allow-list covers this unit. `fnmatchcase`: systemd unit names are case-sensitive.

    `never` OUTRANKS EVERY ALLOW-LIST and is checked first. Both allow-lists are globs whose useful
    value is `*`, which alone is all-or-nothing; the unit this carves out is `sshd.service`, because a
    reload that loads a configuration refusing every key SUCCEEDS, so nothing rolls back, and on a
    Hetzner Cloud machine the way back in is then a rescue boot. A deny-list is the one list whose failure
    mode is refusing a switch somebody wanted, the direction this module errs in anyway.
    """
    if matches_any(unit, never):
        return False
    return matches_any(unit, patterns)


# -------------------------------------------------------------------------------------------------
# Leniency -- NEW units, and a short ROUTINE list, without a per-host `restartable = '*'`
# -------------------------------------------------------------------------------------------------
#
# ADDED 2026-10-23. A host with a narrow `restartableUnits` refused every closure that ADDED a unit or
# touched a timer or this applier's own unit, and a person then switched by hand -- the step this
# module exists to end. None of these restarts anything that holds state:
#   NEW UNITS (`--start-new-units`): absent from the RUNNING tree, so no process to disturb. By unit,
#   not path (a new drop-in for an existing unit is not new); its `.wants` link goes with it. Mounts
#   and swaps are excluded: mounting changes what an existing process sees at a path.
#   ROUTINE UNITS (`--routine`, globs): disturb nothing anybody depends on, including
#   changed-but-skipped (how this applier's own unit arrives). A routine timer also covers its
#   same-named `.service`, STRUCTURALLY ONLY: if dry-activate says it would restart, both refuse.
#   IDLE UNITS (always on, structural only; 2026-11-02): a changed `.service` that is `inactive` now
#   has no process to disturb, which is why dry-activate is silent about it; without this every
#   such closure raised `classifier_disagreement` (monitoring-1 held back 21 commits).
# `never` outranks both, and STATEFUL_UNITS is a floor no host setting can widen: `routine = [ "*" ]`
# still cannot restart postgres. `restartable` is not fenced by it: that is a host's reviewed decision.
STATEFUL_UNITS = (
    "postgresql*", "pgbouncer*", "mongod.service", "mongodb.service", "bitcoind*", "vault*",
    "consul*", "nomad*", "k3s*", "containerd*", "docker*", "podman*", "redis*", "etcd*", "sshd*",
    "haproxy*", "forgejo*", "gitea-runner-*", "prometheus.service", "alertmanager.service",
    "victorialogs*", "loki*", "borg*", "*.mount", "*.automount", "*.swap",
)
NEW_UNIT_EXCLUDED = ("*.mount", "*.automount", "*.swap")
ENABLE_DIRECTORY_SUFFIXES = (".wants", ".requires", ".upholds")


class Leniency(typing.NamedTuple):
    new_units: frozenset[str] = frozenset()       # units the running tree does not have at all
    routine: tuple[str, ...] = ()                 # glob patterns: routine units
    timer_services: frozenset[str] = frozenset()  # `.service` of a routine timer; structural only
    idle: frozenset[str] = frozenset()            # changed `.service` units inactive now; structural only

    def forgives(self, unit: str, never: Patterns = (), structural: bool = False) -> bool:
        if matches_any(unit, tuple(never) + STATEFUL_UNITS):
            return False
        if unit in self.new_units:
            return True
        if matches_any(unit, self.routine):
            return True
        return structural and (unit in self.timer_services or unit in self.idle)

    def structural_only(self) -> frozenset[str]:
        """Units forgiven by the unit tree alone, which dry-activate naming them must take back."""
        return self.timer_services | self.idle


def units_named(tree: Tree) -> set[str]:
    return {unit for unit in (unit_named_by(path) for path in tree) if unit is not None}


def changed_units(running_tree: Tree, candidate_tree: Tree) -> set[str]:
    """Units present in both trees with some path of theirs differing -- the only ones `idle` may
    cover, and the ones classify_and_apply asks systemd about."""
    before, after = units_named(running_tree), units_named(candidate_tree)
    differing = {unit_named_by(path) for path in set(running_tree) | set(candidate_tree)
                 if running_tree.get(path) != candidate_tree.get(path)}
    return {unit for unit in differing if unit in before and unit in after}


def new_units_forgiven(before: set[str], after: set[str], start_new_units: bool) -> frozenset[str]:
    return frozenset(unit for unit in after - before
                     if start_new_units and not matches_any(unit, NEW_UNIT_EXCLUDED))


def routine_timer_services(after: set[str], routine) -> frozenset[str]:
    return frozenset(unit[:-len(".timer")] + ".service" for unit in after
                     if unit.endswith(".timer") and matches_any(unit, routine))


def leniency_for(running_tree: Tree, candidate_tree: Tree, start_new_units: bool, routine: Patterns = (),
                 inactive: Patterns = ()) -> Leniency:
    """Which units the lenient rules cover for THIS pair of trees. Pure, so it is provable:
    `inactive` is what systemd said, read by the caller (`inactive_units`)."""
    before, after = units_named(running_tree), units_named(candidate_tree)
    idle = frozenset(unit for unit in changed_units(running_tree, candidate_tree)
                     if unit.endswith(".service") and unit in set(inactive))
    return Leniency(new_units_forgiven(before, after, start_new_units), tuple(routine),
                    routine_timer_services(after, routine), idle)


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


class Undetermined(Exception):
    """Something could not be measured. Never the same as finding nothing wrong."""


class LockContended(Undetermined):
    """dry-activate could not get the Nix profile lock -- a KNOWN, self-resolving race (2026-08-25).

    The module's own timer, adopted on a host that has been up a while, fires within the activation that
    installs it, while `nixos-rebuild switch` still holds this lock. Still `undetermined`, but the next
    pass finds the lock free; a unit failing for a race it did not cause would page fleet-wide and make
    nixos_deploy refuse to call a good deploy finished.
    """


class NeverStaged(Undetermined):
    """CI has never pinned a closure here.

    A SEPARATE CLAIM from "this pass could not measure": the repair differs (staging wired up, or a first
    deploy from a checkout, versus reading why). 10.0.0.12 on 2026-08-24 runs the tip of main with no pin
    because it was last deployed by hand, and a generic error would leave it amber for no actionable reason.
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

    Symlink targets are compared, not contents: every unit in a NixOS closure is a store symlink, so an
    identical target IS identical content; regular files (drop-ins) are hashed. The directory structure
    is part of the map because `.wants/` and `.requires/` links enable a unit, and that is not nothing.
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

    THE FORGIVEN HALF IS KEPT, NOT DISCARDED: an allow-list makes a switch that restarts six units
    ALLOWED, not invisible. What is forgiven is named in the log line and counted in
    `nixos_auto_apply_forgiven_units`, so "auto-apply restarted your CI runners at 04:12" has an answer.
    """
    before, after = unit_tree(running), unit_tree(candidate)
    decided = {name: (describe_change(name, before, after),
                      change_forgiven(name, restartable, never, leniency))
               for name in sorted(set(before) | set(after)) if before.get(name) != after.get(name)}
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
    owed: list         # components the candidate replaces relative to /run/booted-system
    introduced: list   # components THIS switch changes (candidate differs from /run/current-system)


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
# Test 3 -- dry-activate, as a cross-check
# -------------------------------------------------------------------------------------------------

class DryActivate(typing.NamedTuple):
    disruptive: list[str]        # what would be disturbed AND no allow-list covers -- these block
    reloads: list[str]           # unit names that would merely be reloaded
    unrecognised: list[str]      # lines no rule matched -- each one blocks
    forgiven: tuple[str, ...] = ()         # disturbances a host declared acceptable; reported, never decisive
    # Every SYSTEM unit this classifier refused, as bare names. Only the units the unit tree alone
    # forgives (a routine timer's service, an idle unit: Leniency.structural_only) are fed back
    # to the structural test, so a unit that is running after all is refused by both halves
    # instead of being forgiven structurally and raising a disagreement.
    refused: tuple = ()


def pid1_reexec_is_inert(candidate: pathlib.Path, pid1_exe="/proc/1/exe",
                         system_conf="/etc/systemd/system.conf") -> bool:
    """True when re-executing PID 1 into the candidate's systemd would change nothing at all.

    switch-to-configuration-ng prints `would restart systemd` when `canonicalize("/proc/1/exe")` differs
    from the candidate's `lib/systemd/systemd`. That has a FALSE POSITIVE this fleet hit: the nightly
    `nix-store --optimise` hard-links a duplicate over the running binary's store path, the kernel reports
    PID 1's exe as "<path> (deleted)", and the switch compares against "/unknown" -- 4.5h and 18h of
    `classifier_disagreement` on 2026-09-28/29. BYTES, NOT PATHS: `open("/proc/1/exe")` still reaches the
    inode PID 1 runs. Anything unreadable answers False, so this only ever forgives the one line.
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

    Output goes to stderr and the exit code is 0 whatever it finds, so the lines are the signal. A genuine
    no-op is not silent: it still prints the inhibitor note, "would activate the configuration..." and
    two sops-install-secrets lines (measured on 10.0.0.12).
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


def is_benign(line: str, pid1_unchanged: bool) -> bool:
    return (any(pattern.match(line) for pattern in BENIGN_LINES)
            or (pid1_unchanged and line == "would restart systemd"))


def hard_disruption(line: str) -> str | None:
    for pattern, kind in HARD_LINES:
        match = pattern.match(line)
        if match:
            return kind if not match.groups() else f"{kind}: {match.group(1)}"
    return None


def unit_line(line: str) -> tuple[str, list[str], bool] | None:
    """`(kind, units, system_scope)` of a line naming units. The lenient rules read the SYSTEM unit
    tree, so they say nothing about a user unit of the same name."""
    for pattern, kind in UNIT_LINES:
        match = pattern.match(line)
        if match:
            units = [unit.strip() for unit in match.group(1).split(",") if unit.strip()]
            return kind, units, "user units" not in pattern.pattern
    return None


def unit_forgivable(kind: str, unit: str, system_scope: bool, restartable: Patterns, never: Patterns,
                    leniency: Leniency) -> bool:
    forgivable = kind in FORGIVABLE_KINDS and unit_forgiven(unit, restartable, never)
    # changed-but-skipped is forgiven ONLY by the lenient rules (a routine unit such as this
    # applier's own, or a new one), never by restartable.
    if system_scope and kind in FORGIVABLE_KINDS + ("changed-but-skipped",):
        forgivable = forgivable or leniency.forgives(unit, never)
    return forgivable


def judge_units(kind: str, units: list[str], system_scope: bool, restartable: Patterns, never: Patterns,
                leniency: Leniency) -> tuple[list[str], list[str], list[str]]:
    """`(forgiven, disruptive, refused)` for the units one line names."""
    forgiven: list[str] = []
    disruptive: list[str] = []
    refused: list[str] = []
    for unit in units:
        forgivable = unit_forgivable(kind, unit, system_scope, restartable, never, leniency)
        if not forgivable and system_scope:
            refused.append(unit)
        (forgiven if forgivable else disruptive).append(f"{kind} {unit}")
    return forgiven, disruptive, refused


def classify_output(text: str, restartable: Patterns = (), never: Patterns = (), pid1_unchanged=False,
                    leniency: Leniency = Leniency()) -> DryActivate:
    """Sort every line into disturbs-something, merely-reloads, or not-recognised.

    SPLIT OUT FROM THE SUBPROCESS so the test can drive it with output recorded from a real host: a
    classifier nobody can exercise fails open into "nothing would happen" if its patterns stop matching.
    `pid1_unchanged` is `pid1_reexec_is_inert` for the candidate, passed in so the cases can state it:
    when true, `would restart systemd` re-executes the binary PID 1 already runs, and is benign.
    """
    disruptive: list[str] = []
    reloads: list[str] = []
    unrecognised: list[str] = []
    forgiven: list[str] = []
    refused: list[str] = []

    for line in (raw.strip() for raw in text.splitlines()):
        if not line or is_benign(line, pid1_unchanged):
            continue
        hard = hard_disruption(line)
        named = unit_line(line)
        if hard is not None:
            disruptive.append(hard)
        elif named is None:
            unrecognised.append(line)
        elif named[0] == "reload":
            reloads.extend(named[1])
        else:
            kind, units, system_scope = named
            judged = judge_units(kind, units, system_scope, restartable, never, leniency)
            for into, found in zip((forgiven, disruptive, refused), judged):
                into.extend(found)

    return DryActivate(disruptive, reloads, unrecognised, tuple(forgiven), tuple(sorted(set(refused))))


# -------------------------------------------------------------------------------------------------
# The metric
# -------------------------------------------------------------------------------------------------
#
# A WIRE CONTRACT with the alert rules on monitoring-1: renaming a series turns an alert GREEN, since
# a PromQL query that matches nothing returns no rows. Three timestamps rather than one, because a
# textfile collector is NOT a heartbeat: a stopped publisher's last numbers are scraped as current.
#   last_attempt    -- every pass that finished, even one that determined nothing: the applier is alive.
#   last_verdict    -- a pass that produced an answer about this host: "nothing known for six hours"
#                      is not the same claim as "the applier is dead".
#   last_deployment -- a pass that actually switched; absent on a host with nothing to apply.
# `run_produced_verdict` states the second as a gauge: an absent series and a series saying zero are
# different claims, and inferring one from the other is how a broken publisher reads as a clean fleet.

def escape(value: str) -> str:
    return value.replace("\\", "\\\\").replace('"', '\\"').replace("\n", " ")


class Metrics:
    def __init__(self, path: pathlib.Path, host: str) -> None:
        self.path = path
        self.host = host
        self.lines: list[str] = []
        self.described: set[str] = set()

    def gauge(self, name: str, value, labels: dict[str, str] | None = None,
              help_text: str | None = None) -> None:
        # ONCE PER FAMILY. A second `# HELP` for a name makes node_exporter refuse the WHOLE file,
        # so every series here vanishes: a reboot owed for four components (one sample each) did
        # that to monitoring-1 on 2026-11-10, and the dashboard called the host "not covered".
        if help_text and name not in self.described:
            self.described.add(name)
            self.lines.append(f"# HELP {name} {help_text}")
            self.lines.append(f"# TYPE {name} gauge")
        rendered = dict(labels or {})
        rendered["host"] = self.host
        pairs = ",".join(f'{key}="{escape(str(value_))}"' for key, value_ in sorted(rendered.items()))
        self.lines.append(f"{name}{{{pairs}}} {value}")

    def write(self) -> None:
        """Write atomically, because node_exporter reads this file whenever it likes.

        A partially written textfile is not ignored by node_exporter -- it is parsed, and a
        truncated final line makes the whole file fail to parse, which drops EVERY series in it.
        Rename is the only way to publish a set of numbers that were all true at one moment.
        """
        self.path.parent.mkdir(parents=True, exist_ok=True)
        handle = tempfile.NamedTemporaryFile(
            "w", dir=str(self.path.parent), prefix=self.path.name + ".", delete=False)
        try:
            handle.write("\n".join(self.lines) + "\n")
            handle.flush()
            os.fsync(handle.fileno())
            handle.close()
            os.chmod(handle.name, 0o644)
            os.replace(handle.name, self.path)
        except BaseException:
            handle.close()
            pathlib.Path(handle.name).unlink(missing_ok=True)
            raise


# -------------------------------------------------------------------------------------------------
# Applying
# -------------------------------------------------------------------------------------------------

def configuration_revision(system: pathlib.Path) -> str:
    """The commit the staged closure was built from, for the metric's label.

    Best effort: a label a human reads, never something a decision turns on. A closure built from a tree
    that was not a checkout reports the sentinel, which bin/check-nixos-closure-current treats as its own state.
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


# WHAT `switch-to-configuration switch` EXITING 4 CAN MEAN: one code for every failure it noticed,
# system and per-user alike. `warning: user activation for <name> failed` is a user manager logind is
# stopping (ten seconds after CI's own `nixdeploy` session disconnects) failing the whole switch while
# every system unit is fine: it paged `SystemdUnitFailed` for landed deploys on monitoring-1
# (2026-09-02, 09-25). Every OTHER failure line below is system scope and keeps exit 4 fatal.
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

    THE VERIFICATION IS A COMPARISON, NOT AN ABSOLUTE: the set of failed units is recorded beforehand and
    only units that were healthy and are not any more are objected to, or the first pre-existing fault
    would disable auto-apply on a host permanently and silently. Rolling back is cheap because the gate
    established that this switch restarts nothing, so only files are undone; a rollback that itself fails
    is reported and not retried -- the host needs a person.
    """
    before = failed_units()
    previous = pathlib.Path(os.path.realpath(SYSTEM_PROFILE))

    # THE PROFILE IS SET FIRST, AND THAT ORDER IS LOAD-BEARING: `switch-to-configuration switch` runs
    # the bootloader installer, which builds the boot menu from the generations in the profile.
    # Without it the DEFAULT BOOT ENTRY still points at the old generation -- correct until a reboot,
    # and `--rollback` has no generation to roll back to.
    run(["nix-env", "--profile", SYSTEM_PROFILE, "--set", str(candidate)], timeout=600)
    returncode, output = run_switch(candidate)
    check_switch(candidate, returncode, output)
    return aftermath(failures_after_settling(before), previous, rollback_on_failure)


# -------------------------------------------------------------------------------------------------
# One pass
# -------------------------------------------------------------------------------------------------

class Verdict(typing.NamedTuple):
    state: str          # up_to_date | applied | blocked | undetermined
    reason: str
    detail: str
    revision: str
    candidate: str
    reboot_owed: bool
    activation_changed: bool
    # WHICH components force the reboot, not merely that one is owed: a replaced `kernel` may carry
    # a CVE, while a replaced `initrd` usually means a mount option changed (2026-08-24: all four hosts
    # owed a reboot for `initrd` alone, after one commit added `nofail` to a data volume).
    reboot_components: tuple = ()
    # WHAT AN ALLOW-LIST LET THROUGH: non-empty only when a per-host decision is being exercised, so
    # it is said out loud in the log line and the metric ("applied" would mean two sizes of event).
    forgiven: tuple[str, ...] = ()


def structural_changes(running: pathlib.Path, candidate: pathlib.Path, dry: DryActivate,
                       restartable: Patterns = (), reload_forgiven: Patterns = (),
                       never: Patterns = (),
                       leniency: Leniency = Leniency()) -> UnitChanges:
    """The unit-tree test, given what dry-activate refused. A routine timer's service or an idle unit that
    dry-activate would RESTART or START (it is running now) is denied here too, so both halves refuse it
    together. Split out so the test drives the code that ships rather than a copy of it."""
    taken_back = tuple(unit for unit in dry.refused if unit in leniency.structural_only())
    return unit_changes(running, candidate, set(restartable) | set(reload_forgiven),
                        tuple(never) + taken_back, leniency)


def refusal(units, chain: BootChain, dry: DryActivate, reloadable: Patterns = (), never: Patterns = ()) -> str | None:
    """Why this pass must refuse, or None when the switch is inert. Pure, so the recorded-output cases
    in test/test_auto_apply_classify.py drive the decision that ships.

    THE CROSS-CHECK COMPARES ONLY WHAT BOTH HALVES CAN SEE: the unit tree and the one boot-chain
    component dry-activate reports, a `systemd` this switch replaces. A new kernel, initrd or module tree
    is invisible to dry-activate, and a reboot already owed is not caused by this switch; counting either
    on the structural side alone made dry-activate "disagree" with a fact it cannot state (k3s-worker-1
    and mongo-1, 2026-11-09/10). Both stay refusals or reports in their own right, never disagreements.
    """
    structural_noop = not units and "systemd" not in chain.introduced
    dry_noop = not dry.disruptive and not dry.unrecognised and all(
        unit_forgiven(unit, reloadable, never) for unit in dry.reloads)
    if dry.unrecognised:
        return "unrecognised_output"
    if structural_noop != dry_noop:
        return "classifier_disagreement"
    if chain.introduced:
        return "reboot_owed"
    if units:
        return "units_would_change"
    return None


class PassFacts(typing.NamedTuple):
    """What a pass knows of the candidate before it decides: shared by every blocked verdict."""
    revision: str
    candidate: str
    reboot: tuple[str, ...]    # the boot components the candidate replaces, owed or introduced
    activation_changed: bool


def blocked_verdict(facts: PassFacts, reason: str, detail: str, forgiven: tuple[str, ...] = (),
                    reboot_owed: bool | None = None) -> Verdict:
    owed = bool(facts.reboot) if reboot_owed is None else reboot_owed
    return Verdict("blocked", reason, detail, facts.revision, facts.candidate, owed,
                   facts.activation_changed, facts.reboot, forgiven)


def listed(items: typing.Sequence[str], shown: int = 8) -> str:
    return ", ".join(items[:shown]) + (f" and {len(items) - shown} more" if len(items) > shown else "")


def refusal_detail(reason: str, units: list[str], chain: BootChain, dry: DryActivate) -> str:
    if reason == "unrecognised_output":
        return ("switch-to-configuration printed lines this classifier has no rule for, so what "
                "the switch would do is unknown: " + " | ".join(dry.unrecognised[:5]))
    if reason == "classifier_disagreement":
        # The two tests are meant to be redundant. When they are not, the model is wrong, and the
        # convenient half is exactly the one that must not be believed.
        return (f"the unit tree says this change is {'inert' if not units else 'not inert'} "
                f"(changed units: {len(units)}, systemd replaced: {'systemd' in chain.introduced}) "
                f"but dry-activate says otherwise ({'; '.join(dry.disruptive[:5]) or 'nothing'}"
                f"{', reloads ' + ', '.join(dry.reloads) if dry.reloads else ''}). "
                "Refusing until the disagreement is understood.")
    if reason == "reboot_owed":
        return (f"this change replaces {', '.join(chain.introduced)}, which a switch cannot bring "
                "into use. Nothing here reboots a host; schedule one.")
    return (f"{len(units)} systemd unit(s) differ, so this switch would disturb running services: "
            f"{listed(units)}. Deploy it by hand.")


def activation_script_changed(running: pathlib.Path, candidate: pathlib.Path) -> bool:
    """Reported, never decisive. See the module header: activation scripts are the part of a switch
    that neither test can predict, and the man page says as much."""
    if not (running / "activate").exists() or not (candidate / "activate").exists():
        return True
    return os.path.realpath(running / "activate") != os.path.realpath(candidate / "activate")


def owed_switch_verdict(running: pathlib.Path, owed: list[str], revision: str,
                        candidate: pathlib.Path) -> Verdict:
    return Verdict(
        "blocked", "switch_failed",
        f"a switch to {running.name} stopped midway and still owes: {listed(owed)}. Finish it "
        "with `infra/bin/deploy <host> --apply`.",
        revision, str(candidate), False, False)


def inspect_switch(args: argparse.Namespace, running: pathlib.Path, candidate: pathlib.Path):
    """`(units, dry, forgiven)`: what the unit tree and dry-activate each say about the candidate.

    BOTH TESTS GET THE SAME ALLOW-LIST, and must: they are deliberately redundant (`refusal` calls
    any difference a disagreement), so an allow-list applied to one only would turn every forgiven
    change into a `classifier_disagreement`. `reloadableUnits = [ "*" ]` once looked for a unit
    literally named `*` (exact match, while `restartable` used globs) and permitted nothing.

    DRY-ACTIVATE RUNS FIRST, because only it knows a changed unit would be RELOADED rather than
    restarted; reloads this host permits are then forgiven structurally too. THE REDUNDANCY
    SURVIVES: a changed unit dry-activate mentions NOWHERE is still blocked and still disagrees --
    the dangerous case, a change systemd did not tell us about.
    """
    running_tree, candidate_tree = unit_tree(running), unit_tree(candidate)
    leniency = leniency_for(running_tree, candidate_tree, args.start_new_units,
                            tuple(sorted(args.routine)),
                            inactive_units(changed_units(running_tree, candidate_tree)))
    dry = dry_activate(candidate, args.dry_activate_timeout, args.restartable, args.never, leniency)
    reload_forgiven = {unit for unit in dry.reloads
                       if unit_forgiven(unit, args.reloadable, args.never)}
    changes = structural_changes(running, candidate, dry, args.restartable, reload_forgiven,
                                 args.never, leniency)
    return changes.blocked, dry, tuple(sorted(set(changes.forgiven) | set(dry.forgiven)))


def dry_run_verdict(facts: PassFacts, forgiven: tuple[str, ...]) -> Verdict:
    return blocked_verdict(
        facts, "dry_run",
        "this change is inert and would have been applied, but --dry-run was given"
        + (f", forgiving {len(forgiven)} declared-restartable unit change(s): "
           + ", ".join(forgiven[:8]) if forgiven else ""), forgiven)


def applied_detail(forgiven: tuple[str, ...]) -> str:
    """NAMED, NOT COUNTED, in the detail an operator reads. A switch that disturbed nothing and one
    that restarted every CI runner are both "applied", and the difference is exactly what
    somebody chasing a killed job needs -- the journal line is the durable record, because the
    metrics file is rewritten from scratch by the next pass."""
    if not forgiven:
        return ""
    return (f"restarted {listed(forgiven)} (declared restartable, new or routine on this host)")


def record_deployment(metrics: Metrics) -> None:
    metrics.gauge("nixos_auto_apply_last_deployment_timestamp_seconds", int(time.time()),
                  help_text="When this host last actually switched to a new closure.")


def apply_candidate(args: argparse.Namespace, metrics: Metrics, facts: PassFacts, forgiven: tuple[str, ...]) -> Verdict:
    try:
        failure = activate(pathlib.Path(facts.candidate), args.rollback_on_failure)
    except SwitchFailed as exc:
        return blocked_verdict(facts, "switch_failed", str(exc), forgiven)
    if failure:
        return blocked_verdict(facts, "verification_failed", failure, forgiven)
    record_deployment(metrics)
    return Verdict("applied", "", applied_detail(forgiven), facts.revision, facts.candidate,
                   bool(facts.reboot), facts.activation_changed, facts.reboot, forgiven)


def early_verdict(running: pathlib.Path, candidate: pathlib.Path, revision: str) -> Verdict | None:
    """The verdicts that need no classifying. BEFORE ANY CLASSIFYING, a switch that stopped midway
    leaves its owed units in the leftover lists, and every later dry-activate prints them
    ("start local-fs.target") -- which read as a classifier_disagreement against a candidate, or
    not at all once the candidate is what runs."""
    owed = interrupted_switch()
    if owed:
        return owed_switch_verdict(running, owed, revision, candidate)
    if candidate == running:
        return Verdict("up_to_date", "", "", revision, str(candidate), False, False)
    return None


def classify_and_apply(args: argparse.Namespace, metrics: Metrics) -> Verdict:
    running = pathlib.Path(CURRENT_SYSTEM).resolve()
    candidate = staged_system(pathlib.Path(args.staged_system))
    revision = configuration_revision(candidate)
    early = early_verdict(running, candidate, revision)
    if early is not None:
        return early

    chain = boot_chain(pathlib.Path("/run/booted-system").resolve(), running, candidate)
    # Reported on every verdict; refuses only via chain.introduced.
    facts = PassFacts(revision, str(candidate), tuple(chain.owed),
                      activation_script_changed(running, candidate))
    units, dry, forgiven = inspect_switch(args, running, candidate)
    reason = refusal(units, chain, dry, args.reloadable, args.never)
    if reason is not None:
        return blocked_verdict(facts, reason, refusal_detail(reason, units, chain, dry),
                               reboot_owed=True if reason == "reboot_owed" else None)
    if args.dry_run:
        return dry_run_verdict(facts, forgiven)
    return apply_candidate(args, metrics, facts, forgiven)


def split_patterns(value: str) -> set[str]:
    return {pattern.strip() for pattern in value.split(",") if pattern.strip()}


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--host", required=True, help="flake attribute in nixosConfigurations")
    parser.add_argument("--staged-system", default="/var/lib/nixdeploy/staged-system",
                        help="the pin CI writes; see nix/modules/fleet/deploy-staging.nix")
    parser.add_argument("--metrics-file", required=True)
    parser.add_argument("--lock-file", default="/run/nixos-auto-apply.lock")
    parser.add_argument("--reloadable", default="",
                        help="comma-separated glob patterns naming units a reload may touch")
    parser.add_argument("--restartable", default="",
                        help="comma-separated glob patterns naming units this host accepts being "
                             "stopped, started or restarted by an unattended switch")
    parser.add_argument("--never-disturb", dest="never", default="",
                        help="comma-separated glob patterns naming units NO allow-list may forgive")
    parser.add_argument("--start-new-units", action="store_true",
                        help="forgive units the running system does not have at all (never "
                             "mounts/swaps, never STATEFUL_UNITS, never --never-disturb)")
    parser.add_argument("--routine", default="",
                        help="comma-separated glob patterns naming units whose restart or "
                             "changed-but-skipped state disturbs nothing (timers, exporters, this "
                             "applier); fenced by STATEFUL_UNITS and --never-disturb")
    parser.add_argument("--dry-activate-timeout", type=int, default=600)
    parser.add_argument("--no-rollback-on-failure", dest="rollback_on_failure",
                        action="store_false", default=True)
    parser.add_argument("--dry-run", action="store_true",
                        help="classify and report, never switch")
    args = parser.parse_args(argv)
    for name in ("reloadable", "restartable", "never", "routine"):
        setattr(args, name, split_patterns(getattr(args, name)))
    return args


def undetermined(reason: str, detail: str) -> Verdict:
    return Verdict("undetermined", reason, detail, "", "", False, False)


# Subclasses first: the first kind an exception is an instance of names its reason.
UNDETERMINED_REASONS = ((NeverStaged, "never_staged"), (LockContended, "lock_contended"),
                        (Undetermined, "could_not_measure"))


def failure_verdict(exc: Exception) -> Verdict:
    if isinstance(exc, Blocked):
        return Verdict("blocked", exc.reason, exc.detail, "", "", False, False)
    for kind, reason in UNDETERMINED_REASONS:
        if isinstance(exc, kind):
            return undetermined(reason, str(exc))
    return undetermined("unexpected_error", f"{type(exc).__name__}: {exc}")


def pass_verdict(args: argparse.Namespace, metrics: Metrics) -> Verdict:
    try:
        return classify_and_apply(args, metrics)
    except Exception as exc:  # noqa: BLE001 -- an unexpected failure must still publish
        return failure_verdict(exc)


def seconds_held(lock_path: pathlib.Path) -> int:
    return int(time.time() - lock_path.stat().st_mtime)


def locked_pass(args: argparse.Namespace, metrics: Metrics) -> Verdict:
    """THE LOCK IS HELD FOR THE WHOLE PASS AND IS NEVER STOLEN. Two rebuilds on one host is the
    failure this fleet has already had -- a host reverted three times in an afternoon by parallel
    deploys, each of which exited 0. flock is released by the kernel when the holder dies, so
    there is no stale lock to time out; what a TTL would be for is instead reported, because
    kured's lesson is that an un-expiring lock wedges silently. A pass that cannot take the lock
    publishes how long the holder has held it and returns "undetermined" rather than nothing."""
    lock_path = pathlib.Path(args.lock_file)
    lock_path.parent.mkdir(parents=True, exist_ok=True)
    lock_handle = open(lock_path, "a+")
    try:
        fcntl.flock(lock_handle, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        return undetermined("already_running",
                            f"another pass has held the lock for {seconds_held(lock_path)}s")
    os.utime(lock_path, None)
    try:
        return pass_verdict(args, metrics)
    finally:
        fcntl.flock(lock_handle, fcntl.LOCK_UN)
        lock_handle.close()


# What a verdict exits with, beyond the state's own code.
REASON_EXITS = {("undetermined", "lock_contended"): EXIT_LOCK_CONTENDED,
                ("undetermined", "never_staged"): EXIT_NEVER_STAGED}
STATE_EXITS = {
    "up_to_date": EXIT_CURRENT,
    "applied": EXIT_CURRENT,
    "blocked": EXIT_PENDING,
    "undetermined": EXIT_UNDETERMINED,
}


def exit_code(verdict: Verdict) -> int:
    return REASON_EXITS.get((verdict.state, verdict.reason), STATE_EXITS[verdict.state])


def verdict_message(verdict: Verdict) -> str:
    message = f"nixos-auto-apply: {verdict.state}"
    return message + (f" ({verdict.reason}): {verdict.detail}" if verdict.reason else "")


def main() -> int:
    args = parse_args()
    metrics = Metrics(pathlib.Path(args.metrics_file), args.host)
    started = int(time.time())
    verdict = locked_pass(args, metrics)
    publish(metrics, verdict, started)
    print(verdict_message(verdict), file=sys.stderr if verdict.state != "applied" else sys.stdout)
    return exit_code(verdict)


def publish_liveness(metrics: Metrics, verdict: Verdict, started: int) -> None:
    produced = verdict.state in ("up_to_date", "applied", "blocked")
    metrics.gauge("nixos_auto_apply_last_attempt_timestamp_seconds", started,
                  help_text="When a pass last finished, whatever it concluded. Its age says "
                            "whether the applier itself is still running.")
    metrics.gauge("nixos_auto_apply_run_produced_verdict", 1 if produced else 0,
                  help_text="1 when this pass determined the host's state; 0 when it could not "
                            "measure. Absent is a third claim and is not the same as 0.")
    if produced:
        metrics.gauge("nixos_auto_apply_last_verdict_timestamp_seconds", started,
                      help_text="When this host's state was last actually determined.")


def publish_reboot(metrics: Metrics, verdict: Verdict) -> None:
    metrics.gauge("nixos_auto_apply_reboot_owed", 1 if verdict.reboot_owed else 0,
                  help_text="1 when the pending change replaces the kernel, initrd, kernel "
                            "modules or systemd, which a switch cannot bring into use.")
    for component in verdict.reboot_components:
        metrics.gauge("nixos_auto_apply_reboot_component", 1, labels={"component": component},
                      help_text="One series per boot component the staged closure replaces, so an "
                                "alert can say WHICH -- a replaced kernel and a replaced initrd "
                                "are the same gauge and very different news. Labelled by host and "
                                "component only, both stable while the same reboot is owed.")


def publish_pending(metrics: Metrics, verdict: Verdict) -> None:
    metrics.gauge("nixos_auto_apply_pending", 0 if verdict.state in ("up_to_date", "applied") else 1,
                  help_text="1 when main would give this host a closure it is not running.")
    metrics.gauge("nixos_auto_apply_blocked", 1 if verdict.state == "blocked" else 0,
                  help_text="1 when a pending change must be decided on by a person.")
    # Zero on every host that has declared nothing restartable, which is most of them -- and zero
    # rather than absent on purpose, so an alert can distinguish "this host forgives nothing" from
    # "this host is not publishing", the same rule the rest of this file follows.
    metrics.gauge("nixos_auto_apply_forgiven_units", len(verdict.forgiven),
                  help_text="How many unit changes this pass applied (or would have) only because "
                            "fleet.autoApply.restartableUnits declared them acceptable on this "
                            "host. 0 means the switch disturbed nothing at all.")
    metrics.gauge("nixos_auto_apply_activation_changed", 1 if verdict.activation_changed else 0,
                  help_text="1 when the activation script differs. Reported, never decisive: "
                            "activation is the part of a switch no pre-check can predict.")


def publish_blocked_reason(metrics: Metrics, verdict: Verdict) -> None:
    """THE SERIES ALERTS ARE WRITTEN AGAINST, and it is separate from the info gauge for one
    reason: `nixos_auto_apply_info` carries a `detail` label that changes whenever the pending
    change does. In Prometheus a changed label is a NEW SERIES, which resets every `for:` clause
    watching it -- so an alert keyed on the info gauge would restart its clock at each merge and
    a host that is permanently a restart behind would never fire one. This carries `host` and
    `reason` only, both stable for as long as the same kind of change is waiting."""
    if verdict.state == "blocked":
        metrics.gauge("nixos_auto_apply_blocked_reason", 1,
                      labels={"reason": verdict.reason or "unknown"},
                      help_text="Present, with value 1, only while a change is pending that a "
                                "person must decide on. Labelled by WHY, and by nothing that "
                                "churns, so an alert's `for:` clause measures how long the host "
                                "has been waiting rather than how long ago the detail last changed.")


def publish(metrics: Metrics, verdict: Verdict, started: int) -> None:
    publish_liveness(metrics, verdict, started)
    publish_pending(metrics, verdict)
    publish_reboot(metrics, verdict)
    publish_blocked_reason(metrics, verdict)
    metrics.gauge("nixos_auto_apply_info", 1,
                  labels={"state": verdict.state, "reason": verdict.reason or "none",
                          "revision": verdict.revision or "unknown",
                          "candidate": verdict.candidate or "unknown",
                          "running": os.path.realpath(CURRENT_SYSTEM),
                          "detail": verdict.detail[:200] or "none"},
                  help_text="What this host is running, what main would give it, and why that "
                            "difference has not been applied.")
    metrics.write()


if __name__ == "__main__":
    sys.exit(main())
