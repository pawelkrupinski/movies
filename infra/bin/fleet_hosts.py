"""WHERE A HOST IS, answered from the flake and from nowhere else.

THIS IS THE ONE PLACE THAT ANSWERS IT, which is the entire point of the module existing rather than
the two callers each doing it. `bin/stage-nixos-closures` has read `fleet.publicAddress` out of the
flake since it was ported, for the reason its `address_of` docstring gives at length: an address
written in two places is an address that will disagree with itself the first time a primary IP is
replaced, and the failure that produces is an ssh timeout against a machine that is running
perfectly. `bin/fleet-ssh` needs exactly the same answer for a person, so the lookup moved here
instead of being copied -- a second copy would be the very drift that docstring warns about, in the
file that warns about it.

WHY A PERSON NEEDED IT AT ALL. The knowledge was real but locked in a CI tool. On 2026-09-08 an
investigation into a Flux alert took four attempts to get a shell on monitoring-1 -- `ssh
monitoring-1` (no such name), `ssh -J fleet-jump 10.20.0.11`, tailscale, and finally a `dig` at
grafana.kinowo.net that returned the public address the flake had all along. The middle attempt is
the instructive one: `fleet-jump` resolves and gives a shell, because it is a real machine in
ANOTHER fleet on 10.0.0.0/8, and it does not route this fleet's 10.20.0.0/16. So the wrong path
fails at the SECOND hop, as a timeout against the target, which is indistinguishable from the host
being down -- the exact confusion `address_of` was written to prevent, arriving by a door nobody
had covered.

EVALUATION IS NOT FREE, and this deliberately does not cache. `nix eval` on a warm store is a few
seconds; a cache keyed on anything cheaper than the flake itself would hold the stale address after
a primary IP change, which is the failure mode this module exists to remove. `fleet-ssh
--ssh-config` is the answer for anybody who wants it instant: it writes the addresses into
~/.ssh/config once, so `ssh monitoring-1` needs no wrapper and no evaluation at all.
"""

import json
import os
import subprocess

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

# EMPTY TODAY, AND KEPT ANYWAY -- see the long note under the same name in bin/stage-nixos-closures.
# A configuration that describes no machine (an installer image, a builder-only variant) goes here
# BY NAME, so that "this is not a machine" stays a different claim from "this machine's address
# could not be evaluated".
NOT_A_MACHINE: set = set()


class Flake:
    """The real flake. Split out as an object so tests can drive the callers without evaluating it,
    which would make them slow and dependent on a working nix."""

    def __init__(self, repo=REPO):
        self.repo = repo

    def _run(self, argv):
        result = subprocess.run(argv, cwd=self.repo, capture_output=True, text=True)
        if result.returncode != 0:
            detail = (result.stderr or result.stdout).strip().splitlines()
            return None, detail[-1] if detail else f"exit {result.returncode}"
        return result.stdout.strip(), None

    def hosts(self):
        """Every machine this flake declares. THE LIST IS NEVER WRITTEN DOWN -- a hand-kept roster
        is a thing that silently stops matching the fleet."""
        out, error = self._run(["nix", "eval", "--json", ".#nixosConfigurations",
                                "--apply", "builtins.attrNames"])
        if error:
            return None, error
        return [n for n in json.loads(out) if n not in NOT_A_MACHINE], None

    def address_of(self, name):
        return self._run(["nix", "eval", "--raw",
                          f".#nixosConfigurations.{name}.config.fleet.publicAddress"])

    def private_address_of(self, name):
        return self._run(["nix", "eval", "--raw",
                          f".#nixosConfigurations.{name}.config.fleet.privateAddress"])


def resolve(name, flake):
    """The public address of `name`, or an error that says what to do about it.

    AN UNKNOWN NAME IS ANSWERED WITH THE NAMES THAT EXIST rather than attempted. Plain `ssh
    monitoring` fails as a DNS error, which reads like the host being gone; a typo and a
    decommission should not look alike.

    AN EMPTY VALUE IS A FAILURE, NOT A SKIP. bin/stage-nixos-closures turns the same condition into
    `unreachable-by-declaration` and fails the run, because the alternative is a machine that
    quietly stops tracking main while every run stays green. Both callers need that direction, so it
    lives here.
    """
    known, error = flake.hosts()
    if error:
        return None, error
    if name not in known:
        return None, f"no host named {name!r} in this flake. It declares: {', '.join(sorted(known))}"

    address, error = flake.address_of(name)
    if error:
        return None, error
    if not address:
        return None, ("fleet.publicAddress is empty, so there is no address to reach this host at. "
                      "Set it in this host's file from infra/terraform/primary_ips.tf.")
    return address, None
