#!/usr/bin/env python3
"""Activate the closure CI already staged here, but ONLY when doing so would disturb nothing.

WHY THIS EXISTS. `.github/workflows/nix-stage-closures.yaml` builds every host's closure on merge,
signs it and `nix copy`s it here, then deliberately stops: the staging key is restricted to a
forced command that cannot activate anything, and `nixdeploy` has no sudo. So the closure is
already on this disk, pinned at `/var/lib/nixdeploy/staged-system`, and the only step left is the
one nobody automated -- because activating is the step that can break a running machine.

This is that step, taken only when it provably breaks nothing. Anything that would disturb a
running service, and anything at all that this script cannot classify, is left for a person.

IT DOES NOT FETCH, BUILD, OR TRUST ANYTHING NEW. There is no git remote here, no deploy key and no
evaluation -- an earlier draft of this file did all three, and staging made every one of them
unnecessary. What it acts on is a path CI pinned through a channel that can do nothing else, whose
store paths were accepted only because they carry a signature from `trustedPublicKeys`. The trust
decision was made upstream; this file decides only WHEN.

The reason that division is worth the machinery: the routine commits (a package, an ssh key, a
comment in a vendored script) are the ones nobody schedules a deploy for, and they are the ones
that accumulate until a real deploy carries six months of unrelated change with it. Applying them
continuously means the only pending change is ever the one somebody meant to make.

WHAT DECIDES. Three tests, and a deployment happens only if all three agree it is inert:

  1. THE UNIT TREE, byte for byte. `restartTriggers` and `reloadTriggers` are rendered INTO the
     unit files as `X-Restart-Triggers`/`X-Reload-Triggers`, and switch-to-configuration acts on
     units whose files differ, that are gone, or that are new. So an identical set of unit paths
     with identical contents is a SUFFICIENT condition for "nothing is stopped, started, restarted
     or reloaded" -- and, unlike parsing a message, it stays true across NixOS releases. This is
     the primary test.

  2. THE BOOT CHAIN, against `/run/booted-system` and never `/run/current-system`. A new kernel
     activates silently and the machine keeps running the old one, so comparing the generation
     symlinks answers a different question than the one being asked. nixpkgs' own
     `system.autoUpgrade.allowReboot` compares kernel, initrd and kernel-modules; `systemd` is
     included here as well, because a switch that would replace pid 1's package is not something
     to do unattended. Measured on this fleet on 2026-08-24: dev-bitcoind and dev-postgres both
     have current != booted while all four components match, which is the ordinary residue of a
     post-boot switch -- a symlink comparison would have called both hosts reboot-pending forever.

  3. `switch-to-configuration dry-activate`, as a CROSS-CHECK and never alone. Its own man page
     says "The list of changes is not guaranteed to be complete", so it cannot be the gate; and
     its wording is not a stable interface (nixpkgs pins these strings only in its own
     switch-test.nix, and nix-community/nixos-cli already prints different ones). It earns its
     place by being able to disagree: if the unit tree says nothing changes and dry-activate names
     a unit it would restart, then the model in test 1 is wrong, and the right response to that is
     to refuse and say so rather than to trust the half that gives the convenient answer.

FAIL CLOSED, INCLUDING ON SILENCE. An unrecognised line from dry-activate blocks the deployment.
That is deliberate and it is the direction a NixOS upgrade should break in: new wording turns the
gate OFF, not on. The cost is that a nixpkgs bump can wedge auto-apply until somebody adds a
pattern here, which is a morning's work and is visible in the metric; the alternative cost is a
gate that silently matches nothing and applies everything, which is the same shape of defect as a
check that passes because it measured nothing.

WHAT IS NOT COVERED, stated plainly because the man page says it and this script cannot fix it:
ACTIVATION SCRIPTS. `sops-install-secrets` genuinely runs during a dry run -- measured, it imports
the host key as an age identity on every invocation -- and activation snippets may do work that
neither test predicts. Differences in the activation script are therefore REPORTED (the metric
carries `activation_changed`) and do not by themselves block. That is the residual risk of this
design, it is why the first hosts to run it are development and sandbox ones, and it is the thing
to revisit before a host holding real money is added.

EXIT CODES follow terraform's `plan -detailed-exitcode`, which is the clearest three-way split
anybody has shipped: 0 nothing to do or a change was applied, 1 a change is pending that a person
must decide on, 2 nothing could be determined. 2 must never be read as 0 -- "nobody could ask" and
"there is nothing to ask about" are different claims, and the whole point of the metric below is
that the build can tell them apart.
"""
# -------------------------------------------------------------------------------------------------
# PORT NOTE -- read this before the incident citations below confuse you.
# -------------------------------------------------------------------------------------------------
#
# This file is a FAITHFUL PORT of bitcashier's nix/files/nixos-auto-apply.py. It is ported rather
# than rewritten because every rule in the classification table below was put there by a specific
# measurement, and a rewrite would have kept the rules and lost the reasons -- which is how a
# classifier quietly stops matching and starts activating everything.
#
# THE HOST NAMES AND ADDRESSES IN THE COMMENTS BELONG TO THE FLEET IT CAME FROM. `dev-bitcoind`,
# `dev-postgres`, `ci-runner`, `10.0.0.12` and the wireguard gateway are not machines in this
# project and never will be. They are kept, with their dates, because "measured on a real host on
# this date" is what makes a rule worth obeying and "it seemed sensible" is not. Where such a note
# says something that would be FALSE here it has been corrected in place and says so.
#
# WHY THE WHOLE MECHANISM APPLIES TO A THREE-HOST FLEET. It is not about scale. It is about the
# fact that a NixOS host changes only when somebody deploys it, so "merged" and "deployed" are
# different claims and nothing on the machine distinguishes them. Three hosts drift exactly as
# silently as thirty; there are simply fewer people likely to notice.
#
# WHAT IS NOT PORTED YET: `bin/prove-nixos-auto-apply`, the harness that drives `classify_output`
# with recorded switch-to-configuration output and fails when a rule stops matching. Everything in
# this file is written to be driven by it -- `classify_output` is split out from the subprocess for
# exactly that reason -- and until it exists, THE TABLE BELOW IS UNGUARDED. That is the single
# biggest gap in this port, and the failure mode is not a red build: it is a classifier that
# recognises nothing, decides nothing would be disturbed, and applies.

# `from __future__ import annotations` so that this file parses on any Python from 3.7 up. It runs
# in three places -- a NixOS host's systemd unit (Python 3.12+), a CI container, and an operator's
# Mac, where the system Python is still 3.9 and evaluates `str | None` at import time as a TypeError.
# a prover imports this module directly (see the PORT NOTE: that harness is not ported yet), so a
# version floor here is a prover that cannot run, which is the same as no prover.
from __future__ import annotations
import argparse
import fcntl
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

EXIT_CURRENT = 0
EXIT_PENDING = 1
EXIT_UNDETERMINED = 2
# A unit's own installing switch racing this pass's dry-activate for the same Nix lock. Distinct
# from EXIT_UNDETERMINED because systemd must NOT mark the unit failed for it -- see LockContended.
EXIT_LOCK_CONTENDED = 3
# A host CI has never staged to. Distinct from EXIT_UNDETERMINED, and NOT a failure, because it is
# the ordinary state of a freshly converted machine rather than an anomaly: every host spends the
# window between `convert-host` and its first staged closure here. Exit 2 for it made the unit fail
# during activation, which made `nixos-rebuild switch` itself return non-zero -- so the FIRST deploy
# to any new host reported failure while having succeeded completely. Observed on k3s-worker-1,
# 2026-08-29, immediately after its conversion.
#
# The signal is not lost by this: the verdict is still `undetermined`/`never_staged`, it is still
# published to the textfile so `nixos_auto_apply_*` carries it, and the alert rule that watches for
# a host stuck here is what should complain -- after it has persisted, not the instant the machine
# is born. A red systemd unit on every new host is an alarm nobody can act on and everybody learns
# to ignore.
EXIT_NEVER_STAGED = 4

# Components of the boot chain that a switch cannot bring into use on its own. `systemd` is here
# and is not in nixpkgs' allowReboot list; see the module header for why this script is stricter.
BOOT_CHAIN = ("kernel", "initrd", "kernel-modules", "systemd")

# What `nixos-rebuild` sets and what the bootloader installer reads to build the boot menu.
SYSTEM_PROFILE = "/nix/var/nix/profiles/system"

# Every line switch-to-configuration-ng can print that does NOT describe a unit being disturbed.
# Anchored whole-line matches: a substring match would let "would restart the following units: x"
# be swallowed by a prefix rule.
BENIGN_LINES = (
    re.compile(r"^Not checking switch inhibitors \(action = dry-activate\)$"),
    re.compile(r"^would activate the configuration\.\.\.$"),
    # Runs for real during a dry run, on every invocation, and says only that it read the host key.
    re.compile(r"^sops-install-secrets: Imported /etc/ssh/ssh_host_[a-z0-9]+_key as (?:GPG|age) key with fingerprint \S+$"),
    # ADDED 2026-08-27, found the same way the changed-but-skipped rule above was: a host that had
    # never added a NEW secret before this one, and so had never printed this line. A secret with
    # no prior existence cannot be depended on by anything already running -- there is no process
    # today whose behavior changes because a file it never read now exists. Whatever DOES depend on
    # it (a unit that reads the new path and gets restarted because of it) prints its own line and
    # is classified by UNIT_LINES exactly as any other restart would be; this rule only covers the
    # secret's own arrival, never a unit's reaction to it. A secret being CHANGED or REMOVED is
    # deliberately NOT covered by a lookalike rule here -- an existing secret disappearing or
    # changing value out from under a process that already read it is the "changed-but-skipped"
    # shape, not the "arrived from nothing" shape, and stays a person's decision.
    #
    # `secrets?` SINCE 2026-09-05, AND THE MISSING "s" COST A BLOCKED HOST. sops-nix pluralises
    # this line against the NUMBER OF SECRETS IT IS ADDING, and the 2026-08-27 observation happened
    # to be of a switch adding more than one. monitoring-1 then added exactly one --
    # `alertmanager/smtp-password`, for the alerting email path -- printed `would add secret:` in
    # the singular, matched nothing here, and refused the whole closure as `unrecognised_output`.
    # Recorded verbatim as a case in test/test_auto_apply_classify.py so the singular cannot go
    # missing again.
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

# WHICH KINDS AN ALLOW-LIST MAY FORGIVE. `stop`, `start` and `restart` are the three ways a unit is
# disturbed by a switch, and a host that has named a unit as restartable has said it accepts all
# three -- a unit removed from a closure is stopped, one added is started, and one whose definition
# changed is restarted, and no operator naming `github-runner-*` means "restart yes, start no".
#
# `changed-but-skipped` IS DELIBERATELY NOT HERE, and it is the one exclusion worth arguing. Those
# lines are printed when a unit CHANGED and systemd was told not to act on it, which leaves the
# running process no longer matching its own definition -- a divergence, not a disturbance. An
# allow-list is permission to disturb something; it is not permission to leave the host quietly
# inconsistent with main, which is the state this whole script exists to end.
FORGIVABLE_KINDS = ("stop", "start", "restart")

# The suffixes systemd gives units, used to find the unit a path under etc/systemd/system belongs
# to. Listed rather than pattern-matched because the point is to recognise ONLY these: a path whose
# unit cannot be identified is never forgiven, and a wildcard would make "unidentifiable" forgivable.
UNIT_SUFFIXES = (".service", ".socket", ".target", ".timer", ".path", ".mount",
                 ".automount", ".swap", ".slice", ".scope", ".device")


def unit_named_by(path: str) -> str | None:
    """The unit a path under `etc/systemd/system` belongs to, or None if it names no unit.

    Three shapes reach here and all three matter:

      github-runner-x.service                         -> the unit itself
      multi-user.target.wants/github-runner-x.service -> the link that ENABLES it
      github-runner-x.service.d/override.conf         -> a drop-in that configures it

    A component ending `.d` is the unit's drop-in directory and is stripped. What keeps
    `multi-user.target.wants/x.service` from being read as a change to `multi-user.target` is that
    `multi-user.target.wants` ends `.wants`, which is not a unit suffix and so matches nothing --
    NOT the last-wins tie-break below, which on every path shape systemd actually produces has only
    one candidate to choose from and is therefore unobservable. It is written last-wins anyway
    because that is the semantically right answer if a second candidate ever appears: the change is
    to the leaf, and the directories above it are only where the leaf lives.

    A `.wants` directory itself names no unit and returns None, so creating one is never forgiven:
    enabling a unit is visible as the link inside it, and the directory appearing is a structural
    change nobody has declared acceptable.
    """
    found = None
    for component in path.split("/"):
        name = component[:-2] if component.endswith(".d") else component
        if name.endswith(UNIT_SUFFIXES):
            found = name
    return found


def unit_forgiven(unit: str, patterns, never=()) -> bool:
    """Whether an allow-list covers this unit. `fnmatchcase`, so the match is never locale- or
    case-folded -- systemd unit names are case-sensitive and `SSHD.service` is not `sshd.service`.

    `never` OUTRANKS EVERY ALLOW-LIST and is checked first, which is the whole reason it exists.
    Both allow-lists are written as globs and the useful value for each is `*` -- the fleet permits
    reloads that way, and ci-runner permits restarts that way -- and `*` with no way to carve
    anything back out is an all-or-nothing switch. The unit this is for is `sshd.service`: a reload
    or restart that loads a configuration refusing every key SUCCEEDS, so `rollbackOnFailure` never
    sees a failed unit and nothing rolls back. On a Hetzner Cloud machine the way back in is then
    the web console, which reaches a login prompt no account on this fleet has a password for (see
    nix/modules/fleet/accounts.nix), so the recovery is a rescue boot and a disk mount. A deny-list
    is the only one of the three lists whose failure mode is refusing a switch somebody wanted,
    which is the direction this whole module errs in anyway.
    """
    if any(fnmatch.fnmatchcase(unit, pattern) for pattern in never):
        return False
    return any(fnmatch.fnmatchcase(unit, pattern) for pattern in patterns)


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


class Blocked(Exception):
    """A change is pending that this script must not apply on its own."""

    def __init__(self, reason: str, detail: str) -> None:
        super().__init__(detail)
        self.reason = reason
        self.detail = detail


def run(argv: list[str], *, cwd: str | None = None, env: dict | None = None,
        check: bool = True, timeout: int = 3600) -> subprocess.CompletedProcess:
    try:
        completed = subprocess.run(
            argv, cwd=cwd, env=env, capture_output=True, text=True, timeout=timeout)
    except FileNotFoundError as exc:
        raise Undetermined(f"{argv[0]} is not on PATH: {exc}") from exc
    except subprocess.TimeoutExpired as exc:
        raise Undetermined(f"{' '.join(argv)} did not finish within {timeout}s") from exc
    if check and completed.returncode != 0:
        raise Undetermined(
            f"{' '.join(argv)} exited {completed.returncode}: "
            f"{(completed.stderr or completed.stdout).strip()[:400]}")
    return completed


# -------------------------------------------------------------------------------------------------
# The repository
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


def unit_changes(running: pathlib.Path, candidate: pathlib.Path,
                 restartable=(), never=()) -> UnitChanges:
    """Every difference in the unit tree, split by whether this host has declared it acceptable.

    THE FORGIVEN HALF IS KEPT RATHER THAN DISCARDED. A switch that restarts six units is not the
    same event as one that restarts nothing, and an allow-list must not make the difference
    invisible -- it makes it ALLOWED. What is forgiven here is named in the log line and counted in
    `nixos_auto_apply_forgiven_units`, so "auto-apply restarted your CI runners at 04:12" is a
    question the journal can answer afterwards.
    """
    before, after = unit_tree(running), unit_tree(candidate)
    blocked: list[str] = []
    forgiven: list[str] = []
    for name in sorted(set(before) | set(after)):
        if before.get(name) == after.get(name):
            continue
        if name not in before:
            description = f"{name} (new)"
        elif name not in after:
            description = f"{name} (removed)"
        else:
            description = name
        unit = unit_named_by(name)
        if unit is not None and unit_forgiven(unit, restartable, never):
            forgiven.append(description)
        else:
            blocked.append(description)
    return UnitChanges(blocked, forgiven)


def unit_differences(running: pathlib.Path, candidate: pathlib.Path,
                     restartable=(), never=()) -> list[str]:
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


# -------------------------------------------------------------------------------------------------
# Test 3 -- dry-activate, as a cross-check
# -------------------------------------------------------------------------------------------------

class DryActivate(typing.NamedTuple):
    disruptive: list[str]        # what would be disturbed AND no allow-list covers -- these block
    reloads: list[str]           # unit names that would merely be reloaded
    unrecognised: list[str]      # lines no rule matched -- each one blocks
    forgiven: tuple = ()         # disturbances a host declared acceptable; reported, never decisive


def dry_activate(candidate: pathlib.Path, timeout: int, restartable=(), never=()) -> DryActivate:
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
    return classify_output(completed.stderr + "\n" + completed.stdout, restartable, never)


def classify_output(text: str, restartable=(), never=()) -> DryActivate:
    """Sort every line into disturbs-something, merely-reloads, or not-recognised.

    SPLIT OUT FROM THE SUBPROCESS ON PURPOSE, so that bin/prove-nixos-auto-apply can drive it with
    recorded output -- including output recorded from a real host -- rather than needing a NixOS
    machine and a genuine pending change to find out whether the table below still matches. A
    classifier nobody can exercise is a classifier nobody knows the state of, and this one fails
    open into "nothing would happen" if its patterns ever stop matching.
    """
    disruptive: list[str] = []
    reloads: list[str] = []
    unrecognised: list[str] = []
    forgiven: list[str] = []

    for raw in text.splitlines():
        line = raw.strip()
        if not line:
            continue
        if any(pattern.match(line) for pattern in BENIGN_LINES):
            continue
        for pattern, kind in HARD_LINES:
            match = pattern.match(line)
            if match:
                disruptive.append(kind if not match.groups() else f"{kind}: {match.group(1)}")
                break
        else:
            for pattern, kind in UNIT_LINES:
                match = pattern.match(line)
                if match:
                    units = [unit.strip() for unit in match.group(1).split(",") if unit.strip()]
                    if kind == "reload":
                        reloads.extend(units)
                    else:
                        for unit in units:
                            forgivable = (kind in FORGIVABLE_KINDS
                                          and unit_forgiven(unit, restartable, never))
                            (forgiven if forgivable else disruptive).append(f"{kind} {unit}")
                    break
            else:
                unrecognised.append(line)

    return DryActivate(disruptive, reloads, unrecognised, tuple(forgiven))


# -------------------------------------------------------------------------------------------------
# The metric
# -------------------------------------------------------------------------------------------------
#
# THE SHAPE HERE IS A WIRE CONTRACT with the alert rules on monitoring-1, and CHANGING A NAME HERE
# TURNS AN ALERT GREEN RATHER THAN RED: a PromQL query that matches nothing returns no rows, and no
# rows is indistinguishable from a fleet with nothing pending unless the rule insists on the roster.
# So renaming a series here is a two-file change, and the second file is the one that fails silently.
#
# Three timestamps rather than one, and the distinction is the whole value of the metric. It is
# taken straight from `bitcashier::fleet_spec`, which learned it the expensive way: a textfile
# collector is NOT a heartbeat, so a publisher that stops running leaves its last good numbers
# being scraped and reported as current for ever.
#
#   last_attempt   -- every pass that finished, including one that could determine nothing.
#                     Its age is what says the applier itself is alive.
#   last_verdict   -- a pass that actually produced an answer about this host. A run that could not
#                     reach git, or could not build, does NOT advance this, so "we have not known
#                     anything for six hours" is expressible and is not the same claim as
#                     "the applier is dead".
#   last_deployment -- a pass that actually switched. Absent forever on a host that has simply had
#                     nothing to apply, which is why it is not a health signal on its own.
#
# `run_produced_verdict` states the second of those as a gauge rather than leaving a reader to
# infer it from a missing timestamp, for the reason the ops screen records: an absent series and a
# series saying zero are different claims, and inferring one from the other is how a broken
# publisher comes to read as a clean fleet.


def escape(value: str) -> str:
    return value.replace("\\", "\\\\").replace('"', '\\"').replace("\n", " ")


class Metrics:
    def __init__(self, path: pathlib.Path, host: str) -> None:
        self.path = path
        self.host = host
        self.lines: list[str] = []

    def gauge(self, name: str, value, labels: dict[str, str] | None = None,
              help_text: str | None = None) -> None:
        if help_text:
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
    run([str(candidate / "bin" / "switch-to-configuration"), "switch"], timeout=3600)

    time.sleep(5)  # systemd settles; a unit that fails on activation does so within seconds
    newly_failed = failed_units() - before

    if not newly_failed:
        return ""

    if not rollback_on_failure:
        return f"units failed after the switch and no rollback was attempted: {', '.join(sorted(newly_failed))}"

    # Named explicitly rather than `nix-env --rollback`, which walks to the previous GENERATION --
    # not necessarily the closure this pass replaced, if anything else touched the profile in
    # between. What is being undone is this switch, so say which path that was.
    undo = run(["nix-env", "--profile", SYSTEM_PROFILE, "--set", str(previous)],
               check=False, timeout=600)
    if undo.returncode == 0:
        undo = run([str(previous / "bin" / "switch-to-configuration"), "switch"],
                   check=False, timeout=3600)
    if undo.returncode != 0:
        return (f"units failed after the switch ({', '.join(sorted(newly_failed))}) AND the "
                f"rollback to {previous.name} failed: "
                f"{(undo.stderr or undo.stdout).strip()[:300]}")
    return (f"units failed after the switch and it was rolled back to {previous.name}: "
            f"{', '.join(sorted(newly_failed))}")


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
    # WHICH components force the reboot, not merely that one is owed. A bare 0/1 tells an operator
    # a reboot is needed and nothing about what it buys them -- and the answer changes the urgency
    # completely: a replaced `kernel` may carry a published CVE, while a replaced `initrd` usually
    # means a mount option or a stage-1 module changed and the running machine is simply still on
    # the old boot path. Measured on 2026-08-24: all four hosts owed a reboot for `initrd` alone,
    # from one commit that added `nofail` to a data volume -- which is worth knowing precisely,
    # because until the reboot the host still has the boot behaviour that halts into emergency
    # mode when the volume does not attach.
    reboot_components: tuple = ()
    # WHAT AN ALLOW-LIST LET THROUGH. Empty on every host that has declared nothing restartable,
    # which is the fleet default -- so a non-empty value is always a deliberate per-host decision
    # being exercised, and is worth saying out loud in the log line and the metric rather than
    # leaving "applied" to mean two different sizes of event.
    forgiven: tuple = ()


def classify_and_apply(args, metrics: Metrics) -> Verdict:
    running = pathlib.Path("/run/current-system").resolve()
    booted = pathlib.Path("/run/booted-system").resolve()

    candidate = staged_system(pathlib.Path(args.staged_system))
    revision = configuration_revision(candidate)

    if candidate == running:
        return Verdict("up_to_date", "", "", revision, str(candidate), False, False)

    # Reported, never decisive. See the module header: activation scripts are the part of a switch
    # that neither test below can predict, and the man page says as much.
    activation_changed = (
        os.path.realpath(running / "activate") != os.path.realpath(candidate / "activate")
        if (running / "activate").exists() and (candidate / "activate").exists() else True)

    reboot = boot_chain_differences(booted, candidate)

    # BOTH TESTS ARE GIVEN THE SAME ALLOW-LIST, and they have to be. They are deliberately
    # redundant -- `structural_noop != dry_noop` refuses outright -- so an allow-list applied to one
    # and not the other would turn every forgiven change into a `classifier_disagreement` and make
    # the feature look broken rather than strict.
    #
    # THE RELOAD LIST USED TO BREAK THAT RULE AND IT WAS INERT, WHICH IS WHY NOBODY NOTICED. It was
    # consulted only in `dry_noop`, and by exact string match (`unit in args.reloadable`) while
    # `restartable` went through globs -- so `reloadableUnits = [ "*" ]` looked for a unit literally
    # named `*`, matched none, and permitted nothing. Making it a glob without this block would have
    # been worse than leaving it broken: `dry_noop` would go true while `structural_noop` stayed
    # false, and every benign reload on every host would raise `classifier_disagreement`, which is
    # the one reason in this module that pages at CRITICAL.
    #
    # DRY-ACTIVATE RUNS FIRST NOW, because it is the only thing that knows a changed unit would be
    # RELOADED rather than restarted -- the unit tree shows a file differing and cannot tell which.
    # Units it names as reloads, and that this host permits reloading, are then forgiven
    # structurally too, so the two tests describe the same event.
    #
    # THE REDUNDANCY SURVIVES, which is the point of doing it this way rather than widening the
    # structural test: a changed unit that dry-activate mentions NOWHERE is still blocked and still
    # disagrees, and that is the dangerous case -- a change systemd did not tell us about.
    dry = dry_activate(candidate, args.dry_activate_timeout, args.restartable, args.never)
    reload_forgiven = {unit for unit in dry.reloads
                       if unit_forgiven(unit, args.reloadable, args.never)}
    changes = unit_changes(running, candidate,
                           set(args.restartable) | reload_forgiven, args.never)
    units = changes.blocked
    forgiven = tuple(sorted(set(changes.forgiven) | set(dry.forgiven)))

    structural_noop = not units and not reboot
    dry_noop = not dry.disruptive and not dry.unrecognised and all(
        unit_forgiven(unit, args.reloadable, args.never) for unit in dry.reloads)

    if dry.unrecognised:
        return Verdict(
            "blocked", "unrecognised_output",
            "switch-to-configuration printed lines this classifier has no rule for, so what the "
            "switch would do is unknown: " + " | ".join(dry.unrecognised[:5]),
            revision, str(candidate), bool(reboot), activation_changed)

    if structural_noop != dry_noop:
        # The two tests are meant to be redundant. When they are not, the model is wrong, and the
        # convenient half is exactly the one that must not be believed.
        return Verdict(
            "blocked", "classifier_disagreement",
            f"the unit tree says this change is inert (changed units: {len(units)}, boot chain: "
            f"{reboot or 'unchanged'}) but dry-activate says otherwise ({'; '.join(dry.disruptive[:5]) or 'nothing'}"
            f"{', reloads ' + ', '.join(dry.reloads) if dry.reloads else ''}), or the reverse. "
            "Refusing until the disagreement is understood.",
            revision, str(candidate), bool(reboot), activation_changed)

    if reboot:
        return Verdict(
            "blocked", "reboot_owed",
            f"this change replaces {', '.join(reboot)}, which a switch cannot bring into use. "
            "Nothing here reboots a host; schedule one.",
            revision, str(candidate), True, activation_changed, tuple(reboot))

    if units:
        shown = ", ".join(units[:8]) + (f" and {len(units) - 8} more" if len(units) > 8 else "")
        return Verdict(
            "blocked", "units_would_change",
            f"{len(units)} systemd unit(s) differ, so this switch would disturb running services: "
            f"{shown}. Deploy it by hand.",
            revision, str(candidate), False, activation_changed)

    if args.dry_run:
        return Verdict("blocked", "dry_run",
                       "this change is inert and would have been applied, but --dry-run was given"
                       + (f", forgiving {len(forgiven)} declared-restartable unit change(s): "
                          + ", ".join(forgiven[:8]) if forgiven else ""),
                       revision, str(candidate), False, activation_changed, (), forgiven)

    failure = activate(candidate, args.rollback_on_failure)
    if failure:
        return Verdict("blocked", "verification_failed", failure, revision, str(candidate),
                       False, activation_changed, (), forgiven)

    metrics.gauge("nixos_auto_apply_last_deployment_timestamp_seconds", int(time.time()),
                  help_text="When this host last actually switched to a new closure.")
    # NAMED, NOT COUNTED, in the detail an operator reads. A switch that disturbed nothing and one
    # that restarted every CI runner are both "applied", and the difference is exactly what
    # somebody chasing a killed job needs -- the journal line is the durable record, because the
    # metrics file is rewritten from scratch by the next pass.
    detail = ("restarted " + ", ".join(forgiven[:8])
              + (f" and {len(forgiven) - 8} more" if len(forgiven) > 8 else "")
              + " (declared restartable on this host)") if forgiven else ""
    return Verdict("applied", "", detail, revision, str(candidate), False, activation_changed,
                   (), forgiven)


def main() -> int:
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
    parser.add_argument("--dry-activate-timeout", type=int, default=600)
    parser.add_argument("--no-rollback-on-failure", dest="rollback_on_failure",
                        action="store_false", default=True)
    parser.add_argument("--dry-run", action="store_true",
                        help="classify and report, never switch")
    args = parser.parse_args()
    args.reloadable = {p.strip() for p in args.reloadable.split(",") if p.strip()}
    args.restartable = {p.strip() for p in args.restartable.split(",") if p.strip()}
    args.never = {p.strip() for p in args.never.split(",") if p.strip()}

    metrics = Metrics(pathlib.Path(args.metrics_file), args.host)
    started = int(time.time())

    # THE LOCK IS HELD FOR THE WHOLE PASS AND IS NEVER STOLEN. Two rebuilds on one host is the
    # failure this fleet has already had -- a host reverted three times in an afternoon by parallel
    # deploys, each of which exited 0. flock is released by the kernel when the holder dies, so
    # there is no stale lock to time out; what a TTL would be for is instead reported, because
    # kured's lesson is that an un-expiring lock wedges silently. A pass that cannot take the lock
    # publishes how long the holder has held it and returns "undetermined" rather than nothing.
    lock_path = pathlib.Path(args.lock_file)
    lock_path.parent.mkdir(parents=True, exist_ok=True)
    lock_handle = open(lock_path, "a+")
    try:
        fcntl.flock(lock_handle, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError:
        held_for = int(time.time() - lock_path.stat().st_mtime)
        verdict = Verdict("undetermined", "already_running",
                          f"another pass has held the lock for {held_for}s", "", "", False, False)
        publish(metrics, verdict, started, deployment_written=False)
        print(f"nixos-auto-apply: {verdict.detail}", file=sys.stderr)
        return EXIT_UNDETERMINED

    os.utime(lock_path, None)
    try:
        verdict = classify_and_apply(args, metrics)
    except Blocked as exc:
        verdict = Verdict("blocked", exc.reason, exc.detail, "", "", False, False)
    except NeverStaged as exc:
        verdict = Verdict("undetermined", "never_staged", str(exc), "", "", False, False)
    except LockContended as exc:
        verdict = Verdict("undetermined", "lock_contended", str(exc), "", "", False, False)
    except Undetermined as exc:
        verdict = Verdict("undetermined", "could_not_measure", str(exc), "", "", False, False)
    except Exception as exc:  # noqa: BLE001 -- an unexpected failure must still publish
        verdict = Verdict("undetermined", "unexpected_error",
                          f"{type(exc).__name__}: {exc}", "", "", False, False)
    finally:
        fcntl.flock(lock_handle, fcntl.LOCK_UN)
        lock_handle.close()

    publish(metrics, verdict, started, deployment_written=True)

    message = f"nixos-auto-apply: {verdict.state}"
    if verdict.reason:
        message += f" ({verdict.reason}): {verdict.detail}"
    print(message, file=sys.stderr if verdict.state != "applied" else sys.stdout)

    if verdict.state == "undetermined" and verdict.reason == "lock_contended":
        return EXIT_LOCK_CONTENDED

    if verdict.state == "undetermined" and verdict.reason == "never_staged":
        return EXIT_NEVER_STAGED

    return {
        "up_to_date": EXIT_CURRENT,
        "applied": EXIT_CURRENT,
        "blocked": EXIT_PENDING,
        "undetermined": EXIT_UNDETERMINED,
    }[verdict.state]


def publish(metrics: Metrics, verdict: Verdict, started: int, deployment_written: bool) -> None:
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
    metrics.gauge("nixos_auto_apply_pending", 0 if verdict.state in ("up_to_date", "applied") else 1,
                  help_text="1 when main would give this host a closure it is not running.")
    metrics.gauge("nixos_auto_apply_blocked", 1 if verdict.state == "blocked" else 0,
                  help_text="1 when a pending change must be decided on by a person.")
    metrics.gauge("nixos_auto_apply_reboot_owed", 1 if verdict.reboot_owed else 0,
                  help_text="1 when the pending change replaces the kernel, initrd, kernel "
                            "modules or systemd, which a switch cannot bring into use.")
    for component in verdict.reboot_components:
        metrics.gauge("nixos_auto_apply_reboot_component", 1, labels={"component": component},
                      help_text="One series per boot component the staged closure replaces, so an "
                                "alert can say WHICH -- a replaced kernel and a replaced initrd "
                                "are the same gauge and very different news. Labelled by host and "
                                "component only, both stable while the same reboot is owed.")
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
    # THE SERIES ALERTS ARE WRITTEN AGAINST, and it is separate from the info gauge below for one
    # reason: `nixos_auto_apply_info` carries a `detail` label that changes whenever the pending
    # change does. In Prometheus a changed label is a NEW SERIES, which resets every `for:` clause
    # watching it -- so an alert keyed on the info gauge would restart its clock at each merge and
    # a host that is permanently a restart behind would never fire one. This carries `host` and
    # `reason` only, both stable for as long as the same kind of change is waiting.
    if verdict.state == "blocked":
        metrics.gauge("nixos_auto_apply_blocked_reason", 1,
                      labels={"reason": verdict.reason or "unknown"},
                      help_text="Present, with value 1, only while a change is pending that a "
                                "person must decide on. Labelled by WHY, and by nothing that "
                                "churns, so an alert's `for:` clause measures how long the host "
                                "has been waiting rather than how long ago the detail last changed.")
    metrics.gauge("nixos_auto_apply_info", 1,
                  labels={"state": verdict.state, "reason": verdict.reason or "none",
                          "revision": verdict.revision or "unknown",
                          "candidate": verdict.candidate or "unknown",
                          "running": os.path.realpath("/run/current-system"),
                          "detail": verdict.detail[:200] or "none"},
                  help_text="What this host is running, what main would give it, and why that "
                            "difference has not been applied.")
    metrics.write()


if __name__ == "__main__":
    sys.exit(main())
