"""Deciding whether a pass applies, refuses or only reports.

A part of nix/files/nixos-auto-apply.py, which carries the argument for all of it (the header there).
"""
from __future__ import annotations
import argparse
import os
import pathlib
import time
from applier_classify import DryActivate
from applier_closure import (
    BootChain, CURRENT_SYSTEM, UnitChanges, boot_chain, configuration_revision, inactive_units,
    staged_system, unit_changes, unit_tree
)
from applier_dry_activate import dry_activate
from applier_metrics import Metrics
from applier_process import SwitchFailed
from applier_switch import activate, interrupted_switch
from applier_units import Leniency, Patterns, changed_units, leniency_for, unit_forgiven
from applier_verdict import PassFacts, Verdict, blocked_verdict, listed


def structural_changes(running: pathlib.Path, candidate: pathlib.Path, dry: DryActivate,
                       restartable: Patterns = (), reload_forgiven: Patterns = (),
                       never: Patterns = (),
                       leniency: Leniency = Leniency()) -> UnitChanges:
    """The unit-tree test, given what dry-activate refused. A routine timer's service or an idle
    unit that dry-activate would RESTART or START (it is running now) is denied here too, so both halves refuse it
    together instead of disagreeing. Split out so test/test_auto_apply_classify.py drives the code
    that ships rather than a copy of it."""
    taken_back = tuple(unit for unit in dry.refused if unit in leniency.structural_only())
    return unit_changes(running, candidate, set(restartable) | set(reload_forgiven),
                        tuple(never) + taken_back, leniency)


def refusal(units: list[str], chain: BootChain, dry: DryActivate, reloadable: Patterns = (), never: Patterns = ()) -> str | None:
    """Why this pass must refuse, or None when the switch is inert. Pure, so the recorded-output
    cases in test/test_auto_apply_classify.py drive the decision that ships.

    THE CROSS-CHECK COMPARES ONLY WHAT BOTH HALVES CAN SEE: the unit tree (`units`, what no list
    forgives) and the one boot-chain component dry-activate reports, a `systemd` this switch
    replaces ("would restart systemd"). A new kernel, initrd or module tree is invisible to
    dry-activate, and a reboot the running system already owes is not caused by this switch at
    all; counting either on the structural side alone made dry-activate "disagree" with a fact it
    has no way to state (k3s-worker-1 and mongo-1, 2026-11-09/10). Both stay refusals or reports in
    their own right below, never disagreements.
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
    # BOTH TESTS ARE GIVEN THE SAME ALLOW-LIST, and they have to be. They are deliberately
    # redundant -- `refusal` calls any difference a disagreement -- so an allow-list applied to one
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
