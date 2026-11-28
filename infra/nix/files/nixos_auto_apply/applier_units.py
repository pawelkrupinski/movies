"""Which changed units an allow-list forgives: the patterns, the floor under them and the leniency rules.

A part of nix/files/nixos-auto-apply.py, which carries the argument for all of it (the header there).
"""
from __future__ import annotations
import fnmatch
import typing


Patterns = typing.Collection[str]

Tree = dict[str, str]

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


def matches_any(unit: str, patterns: Patterns) -> bool:
    return any(fnmatch.fnmatchcase(unit, pattern) for pattern in patterns)


def unit_forgiven(unit: str, patterns: Patterns, never: Patterns = ()) -> bool:
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
    if matches_any(unit, never):
        return False
    return matches_any(unit, patterns)


# -------------------------------------------------------------------------------------------------
# Leniency -- NEW units, and a short ROUTINE list, without a per-host `restartable = '*'`
# -------------------------------------------------------------------------------------------------
#
# ADDED 2026-10-23. A host with a narrow `restartableUnits` refused every closure that ADDED a unit
# (a new timer, a new exporter) or touched a timer or this applier's own unit, as
# `units_would_change` -- and the person then ran nix-env + switch-to-configuration by hand, which is
# the exact hand step this module exists to end. Neither kind of change restarts anything that
# holds state:
#
#   NEW UNITS (`--start-new-units`). A unit absent from the RUNNING unit tree has no process to
#   disturb; starting it is the whole of its effect. It is named in the candidate's tree and not the
#   running one -- by unit, not by path, so a new drop-in for an EXISTING unit is not "new". Its
#   `.wants`/`.requires` link is forgiven with it, and so is a NEW `.wants` directory whose every
#   entry is forgiven. Mounts and swaps are excluded: mounting something changes what an existing
#   process sees at a path.
#
#   ROUTINE UNITS (`--routine`, globs). Units whose stop/start/restart -- and changed-but-skipped,
#   which is how this applier's own unit (`restartIfChanged = false`) arrives -- disturbs nothing
#   anybody depends on: timers, exporters, the applier itself. A routine timer also covers its
#   same-named `.service`, STRUCTURALLY ONLY: a changed oneshot between firings is inert, but if
#   dry-activate says it would restart (it is running now -- a backup mid-flight), that is refused
#   on both sides.
#
#   IDLE UNITS (always on, structural only; ADDED 2026-11-02). A changed `.service` that exists in
#   both trees and is `inactive` on the host right now has no process to disturb: the switch leaves
#   it alone and its next start reads the new definition -- which is why dry-activate, correctly,
#   says nothing about it. bin/deploy has always shown these as "unit files changed, unit idle".
#   Without this rule the unit tree counted them while dry-activate stayed silent, and every closure
#   touching one raised `classifier_disagreement`: monitoring-1 on 2026-11-02, held back 21 commits
#   by `generate-shutdown-ramfs.service` (a nixpkgs bump; it runs only at shutdown). If dry-activate
#   DOES name an idle unit (it started between the two reads), that refusal is fed back to the
#   structural test as for a routine timer's service, so both halves refuse it together.
#
# NONE OF THEM CAN REACH A STATEFUL SERVICE. `never` outranks both, as it outranks every allow-list, and
# STATEFUL_UNITS is a floor under these two rules that no host setting can widen -- so `routine =
# [ "*" ]` written by mistake still cannot restart postgres. `restartable` is NOT fenced by it: that
# list is a host's own explicit, reviewed decision (monitoring-1 names prometheus.service in it).
#
# PORTED FROM bitcashier's applier on the same day, rules and floor unchanged; bitcashier's prover
# drives the same functions, test/test_auto_apply_classify.py drives them here.
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


def routine_timer_services(after: set[str], routine: Patterns) -> frozenset[str]:
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
