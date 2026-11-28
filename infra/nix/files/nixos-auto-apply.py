#!/usr/bin/env python3
"""Activate the closure CI already staged here, but ONLY when doing so would disturb nothing.

WHY THIS EXISTS. `.github/workflows/nix-stage-closures.yml` builds every host's closure on merge,
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
     Only the part of that debt THIS switch introduces (the candidate differs from the running
     system too) refuses it; a reboot already owed is reported and blocks nothing (`boot_chain`).

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
import os
import pathlib
import sys

# The pieces beside it. Run as a script, this file's own directory leads sys.path and `nixos_auto_apply/` is
# found from it; fleet/auto-apply.nix swaps this one line for the store copy of that directory, because the
# built script lives in the store and its siblings do not.
sys.path.insert(0, os.path.join(os.path.dirname(os.path.realpath(__file__)), "nixos_auto_apply"))
from applier_clock import now  # noqa: E402
from applier_decide import classify_and_apply  # noqa: E402
from applier_metrics import Metrics, publish  # noqa: E402
from applier_verdict import Verdict, exit_code, failure_verdict, undetermined, verdict_message  # noqa: E402


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


def pass_verdict(args: argparse.Namespace, metrics: Metrics) -> Verdict:
    try:
        return classify_and_apply(args, metrics)
    except Exception as exc:  # noqa: BLE001 -- an unexpected failure must still publish
        return failure_verdict(exc)


def seconds_held(lock_path: pathlib.Path) -> int:
    return int(now() - lock_path.stat().st_mtime)


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


def main() -> int:
    args = parse_args()
    metrics = Metrics(pathlib.Path(args.metrics_file), args.host)
    started = int(now())
    verdict = locked_pass(args, metrics)
    publish(metrics, verdict, started)
    print(verdict_message(verdict), file=sys.stderr if verdict.state != "applied" else sys.stdout)
    return exit_code(verdict)


if __name__ == "__main__":
    sys.exit(main())
