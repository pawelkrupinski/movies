"""The table of lines `switch-to-configuration dry-activate` prints and the verdict on a whole output.

A part of nix/files/nixos-auto-apply.py, which carries the argument for all of it (the header there).
"""
from __future__ import annotations
import re
import typing
from applier_units import Leniency, Patterns, unit_forgiven


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
    refused: tuple[str, ...] = ()


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


def classify_output(text: str, restartable: Patterns = (), never: Patterns = (), pid1_unchanged: bool = False,
                    leniency: Leniency = Leniency()) -> DryActivate:
    """Sort every line into disturbs-something, merely-reloads, or not-recognised.

    SPLIT OUT FROM THE SUBPROCESS ON PURPOSE, so that bin/prove-nixos-auto-apply can drive it with
    recorded output -- including output recorded from a real host -- rather than needing a NixOS
    machine and a genuine pending change to find out whether the table below still matches. A
    classifier nobody can exercise is a classifier nobody knows the state of, and this one fails
    open into "nothing would happen" if its patterns ever stop matching.

    `pid1_unchanged` is `pid1_reexec_is_inert` for the candidate, passed in rather than computed so
    the recorded-output cases can state it: when true, `would restart systemd` re-executes the very
    binary and config PID 1 already runs, and is benign.
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
