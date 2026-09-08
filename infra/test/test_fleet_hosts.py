#!/usr/bin/env python3
"""`bin/fleet_hosts.py` and `bin/fleet-ssh` -- WHERE A HOST IS, asked once and answered the same way.

WHY THIS EXISTS. Reaching these machines was folklore. `bin/stage-nixos-closures` has always known
the answer -- it reads `fleet.publicAddress` out of the flake precisely so an address is never
written down twice -- but that knowledge was locked inside a CI tool, and a person who wanted a
shell had to go and read its source. On 2026-09-08 an investigation into a Flux alert spent four
attempts getting onto monitoring-1: `ssh monitoring-1` (no such name), `ssh -J fleet-jump
10.20.0.11` (that gateway is a DIFFERENT project's, on 10.0.0.0/8, and does not route the kinowo
fleet's 10.20.0.0/16), tailscale (these hosts are not on it), before `dig grafana.kinowo.net`
happened to return the public address the flake had all along.

THE TRAP THAT MAKES A GUESS PLAUSIBLE. `fleet-jump` resolves, accepts the key, and gives a shell --
it is simply a machine in another fleet. So the wrong path FAILS AT THE SECOND HOP, as a timeout
against `10.20.0.11`, which is exactly what a host being down looks like.

WHAT IS PINNED HERE. That the address comes from the flake and nowhere else; that an empty
`publicAddress` is an ERROR rather than a skip (bin/stage-nixos-closures depends on that direction
-- a host that quietly stops tracking main is the failure it was built against); and that a
mistyped host name answers with the names that exist rather than with a connection attempt.

Run: python3 infra/test/test_fleet_hosts.py   (also run by infra/bin/check)
"""

import importlib.machinery
import importlib.util
import os
import pathlib
import unittest

HERE = pathlib.Path(os.path.dirname(os.path.abspath(__file__)))


def _load(name, path):
    # An explicit loader, because `fleet-ssh` carries no `.py` suffix -- it is a command on
    # PATH, not a module -- and `spec_from_file_location` can only infer one from a suffix it
    # recognises.
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_file_location(name, path, loader=loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


fleet_hosts = _load("fleet_hosts", HERE / ".." / "bin" / "fleet_hosts.py")
fleet_ssh = _load("fleet_ssh", HERE / ".." / "bin" / "fleet-ssh")


class FakeFlake:
    """The flake, without evaluating it. Keyed the way `nix eval` is asked."""

    def __init__(self, addresses):
        self.addresses = addresses
        self.asked = []

    def hosts(self):
        return sorted(self.addresses), None

    def address_of(self, name):
        self.asked.append(name)
        if name not in self.addresses:
            return None, f"attribute '{name}' missing"
        return self.addresses[name], None


FLEET = {"mongo-1": "2.28.56.140", "monitoring-1": "128.140.49.167", "k3s-worker-1": "2.28.47.31"}


class AddressComesFromTheFlake(unittest.TestCase):
    def test_a_declared_host_resolves_to_its_public_address(self):
        flake = FakeFlake(FLEET)
        address, error = fleet_hosts.resolve("monitoring-1", flake)
        self.assertIsNone(error)
        self.assertEqual("128.140.49.167", address)

    def test_an_empty_public_address_is_an_error_and_not_a_skip(self):
        # THE DIRECTION IS THE POINT. bin/stage-nixos-closures turns this into
        # `unreachable-by-declaration` and FAILS the run; softening it to "skip" is how a machine
        # stops tracking main while every run stays green.
        flake = FakeFlake(dict(FLEET, **{"monitoring-1": ""}))
        address, error = fleet_hosts.resolve("monitoring-1", flake)
        self.assertIsNone(address)
        self.assertIn("publicAddress", error)

    def test_an_unknown_host_answers_with_the_names_that_exist(self):
        # Not a connection attempt against a name nobody declared -- which is what plain `ssh`
        # does, and it fails as a DNS error that reads like the host being gone.
        flake = FakeFlake(FLEET)
        address, error = fleet_hosts.resolve("monitoring", flake)
        self.assertIsNone(address)
        self.assertIn("monitoring-1", error)
        self.assertEqual([], flake.asked, "an unknown name must not be evaluated")


class WhoYouArriveAs(unittest.TestCase):
    def test_the_login_is_a_person_and_never_the_staging_account(self):
        # `nixdeploy`'s key is a forced command with `restrict` and has no sudo, so it cannot give
        # anybody a shell. A human path that defaulted to it would fail in a way that looks like a
        # permissions problem on the host.
        self.assertNotEqual("nixdeploy", fleet_ssh.login({}))

    def test_ssh_user_overrides_the_local_account(self):
        self.assertEqual("root", fleet_ssh.login({"SSH_USER": "root", "USER": "pawel"}))

    def test_it_falls_back_to_the_local_account(self):
        self.assertEqual("pawel", fleet_ssh.login({"USER": "pawel"}))


class TheSshConfigBlock(unittest.TestCase):
    """`--ssh-config` exists so `ssh monitoring-1` works with no wrapper at all, which is the form
    somebody reaches for under pressure."""

    def test_it_names_every_declared_host_with_its_address(self):
        block = fleet_ssh.ssh_config(FLEET, login="pawel")
        for host, address in FLEET.items():
            self.assertIn(f"Host {host}", block)
            self.assertIn(f"HostName {address}", block)
        self.assertIn("User pawel", block)

    def test_it_does_not_route_through_a_jump_host(self):
        # THE MISTAKE THIS WHOLE FILE IS ABOUT. These hosts are reached on the public NIC; a
        # ProxyJump here would reintroduce the exact wrong turn, and `fleet-jump` belongs to
        # another fleet entirely.
        block = fleet_ssh.ssh_config(FLEET, login="pawel")
        self.assertNotIn("ProxyJump", block)
        self.assertNotIn("fleet-jump", block)


class WhereTheServicesListen(unittest.TestCase):
    """The second half of the same wrong turn: being ON the host and still finding nothing."""

    def test_prometheus_is_advertised_on_the_private_address_not_localhost(self):
        # `curl localhost:9090` on monitoring-1 returns NOTHING -- prometheus binds
        # `fleet.privateAddress`. Answering "how do I reach it" with only an ssh command leaves
        # somebody one failed curl short of the answer.
        hint = fleet_ssh.service_hint("monitoring-1", "10.20.0.11")
        self.assertIn("10.20.0.11:9090", hint)
        self.assertNotIn("localhost:9090", hint)


if __name__ == "__main__":
    unittest.main(verbosity=2)
