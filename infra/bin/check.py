#!/usr/bin/env python3
"""The gate for infra/: everything this tree can be verified for without touching a live machine.

It is the closest thing this directory has to a test suite, and it is genuinely load-bearing
rather than ceremonial: a NixOS fleet's failure mode is that a mistake is invisible until
activation, ON the host, AFTER the closure has been built and copied. Every check below moves one
class of that failure earlier -- to a laptop, before anything is deployed.

  terraform fmt      formatting, so a diff is about intent
  terraform validate provider schemas and references
  dashboard tests    the provisioned Grafana JSON -- structure, and the host/job/mountpoint
                     literals its queries pin, against the fleet that has to produce them
  nix eval           EVERY host's toplevel derivation. This is the expensive one and the one that
                     matters: it type-checks the whole module tree, runs every `assertions` entry,
                     and proves each host would build. A module that references a missing option
                     or an unset secret fails here rather than at 03:00 on the auto-apply timer.
  alert rule tests   the alerting rules, fed a series and asked what they do -- the question
                     `promtool check rules` does not ask and Prometheus never asks at all.
  pod log fields     the vector transform, run by the REAL vector: is a line from worker-de
                     labelled country=de, and is the ReplicaSet hash stripped rather than left to
                     mint a new stream on every deploy
  alertmanager       the same question one step later: given an alert's labels, WHICH RECEIVER
                     does it reach. Routing is first-match-wins, so a misordered route is dead on
                     arrival and reads as correct.
  reboot-required    the decision behind `nixos_reboot_required`, whose only other test is to
                     reboot a machine and read a metric afterwards -- which is how it stayed
                     wrong.
  mongodb off-box    the pull of mongo-1's dumps onto monitoring-1: newest COMPLETED dump only,
                     rotation by count, the metric its alerts read.
  replica-set origin WHICH hosts may CREATE the replica set. `nix eval` proves each host builds;
                     it cannot notice that two of them would both build a unit that calls
                     `rs.initiate()`, which is a silent, data-losing mistake rather than a broken
                     closure.

It deliberately does NOT run `terraform plan` -- that needs a Hetzner token and network, and this
has to stay runnable offline in CI on a fork.

Its steps live beside it: check_steps.py (terraform, the dashboard, the test/ scripts), check_hosts.py (the questions
asked of each host's evaluated config), check_gate.py (how a step runs and prints). infra/bin/check is the name its
callers run. It takes no arguments; exit 0 all green, 1 anything failed (or nix not found: a FAILED check naming the
fix, never a skip)."""
from __future__ import annotations

import os
import sys

from check_gate import Gate, SubprocessRunner, nix_bin  # (check_gate puts scripts/lib on sys.path)
import check_hosts
import check_steps

HERE = os.path.dirname(os.path.abspath(__file__))


def nix_on_path() -> bool:
    """nix from PATH or its install location (bin/nix_path.py), its bin directory put first on PATH; not found is a
    FAILED check naming the fix, never a skip."""
    found = nix_bin(SubprocessRunner(), os.environ)
    if found is None:
        print("FAILED -- the nix eval steps below cannot run", file=sys.stderr)
        return False
    os.environ["PATH"] = found + ":" + os.environ.get("PATH", "")
    return True


def run_all(gate: Gate) -> None:
    check_steps.terraform_fmt(gate)
    check_steps.terraform_validate(gate)
    check_steps.plist_syntax(gate)
    check_steps.dashboard(gate)
    for script in check_steps.SCRIPTS_BEFORE_EVAL:
        check_steps.run_script(gate, script)
    check_hosts.toplevels(gate)
    for script in check_steps.SCRIPTS_AFTER_EVAL:
        check_steps.run_script(gate, script)
    check_hosts.key_placement(gate)
    check_hosts.shipper_restart(gate, HERE)
    check_hosts.replica_set_origin(gate)


def main() -> int:
    infra = os.path.dirname(HERE)
    os.chdir(infra)
    if not nix_on_path():
        return 1
    gate = Gate(SubprocessRunner(), os.environ, infra)
    run_all(gate)
    if gate.failed:
        gate.say("\n\033[1;31minfra/bin/check: FAILED\033[0m")
    else:
        gate.say("\n\033[1;32minfra/bin/check: all green\033[0m")
    return 1 if gate.failed else 0


if __name__ == "__main__":
    sys.exit(main())
