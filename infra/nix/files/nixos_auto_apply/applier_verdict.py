"""What a pass concludes, and the exit code and message that say it.

A part of nix/files/nixos-auto-apply.py, which carries the argument for all of it (the header there).
"""
from __future__ import annotations
import typing
from applier_process import Blocked, LockContended, NeverStaged, Undetermined


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
    reboot_components: tuple[str, ...] = ()
    # WHAT AN ALLOW-LIST LET THROUGH: non-empty only when a per-host decision is being exercised, so
    # it is said out loud in the log line and the metric ("applied" would mean two sizes of event).
    # WHAT AN ALLOW-LIST LET THROUGH. Empty on every host that has declared nothing restartable,
    # which is the fleet default -- so a non-empty value is always a deliberate per-host decision
    # being exercised, and is worth saying out loud in the log line and the metric rather than
    # leaving "applied" to mean two different sizes of event.
    forgiven: tuple[str, ...] = ()


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
