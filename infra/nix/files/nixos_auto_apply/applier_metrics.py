"""The textfile metric, a wire contract with the alert rules.

A part of nix/files/nixos-auto-apply.py, which carries the argument for all of it (the header there).
"""
from __future__ import annotations
import os
import pathlib
import tempfile
from applier_closure import CURRENT_SYSTEM
from applier_verdict import Verdict


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
        self.described: set[str] = set()

    def gauge(self, name: str, value: float, labels: dict[str, str] | None = None,
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
