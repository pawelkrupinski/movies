#!/usr/bin/env python3
"""END TO END: infra/bin/fleet-ssh --ssh-config, which writes every host of the flake into ~/.ssh/config, run through
fleet_hosts.py against a stub `nix` (the flake's host list and addresses; scripts/e2e/e2e_world.py) and a temp HOME.

  - a config holding a person's own hosts gets the generated block appended and its own text and mode left exactly as
    they were; a second run changes nothing (idempotent: one block, never a duplicate);
  - a host that moved gets its new address in the same single block, text after the block kept; a host whose address
    is empty is skipped, said so, and the rest written;
  - the login written is $SSH_USER, never `nixdeploy`.
Mutations (scripts/e2e/mutations): the block never replaced (a duplicate per run); the person's text before the block
dropped."""
import json
import os
import sys
import unittest
from typing import Dict

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))
import e2e_world  # noqa: E402

OWN = "Host mine\n    HostName 192.0.2.1\n    User me\n"
AFTER = "Host last\n    HostName 192.0.2.9\n"
# the flake: its hosts, and each one's fleet.publicAddress from a file the scenario edits
NIX = """
addresses = json.load(open(os.environ["E2E_ADDRESSES"]))
if argv[:3] == ["eval", "--json", ".#nixosConfigurations"]:
    out(json.dumps(sorted(addresses)), end="")
    sys.exit(0)
name = argv[-1].split("nixosConfigurations.", 1)[1].split(".config.", 1)[0]
if argv[-1].endswith("fleet.publicAddress") and addresses.get(name):
    out(addresses[name], end="")
    sys.exit(0)
if argv[-1].endswith("fleet.publicAddress"):
    sys.exit(0)
err("no answer for " + name); sys.exit(1)
"""


class FleetSshE2ETest(e2e_world.Scenario):
    def setUp(self) -> None:
        super().setUp()
        self.w.shim("nix", NIX)
        self.addresses = os.path.join(self.w.root, "addresses.json")
        self.config = os.path.join(self.w.home, ".ssh", "config")
        self.w.files.disk.make_directories(os.path.dirname(self.config))

    def fleet(self, addresses: Dict[str, str]) -> None:
        self.w.write(self.w.root, "addresses.json", json.dumps(addresses))

    def write_config(self) -> e2e_world.cli_harness.Done:
        return self.w.run([self.w.tool("infra/bin/fleet-ssh"), "--ssh-config"], self.w.root,
                          env={"E2E_ADDRESSES": self.addresses, "SSH_USER": "alice"})

    def test_the_block_is_written_once_replaced_in_place_and_the_persons_own_config_kept(self) -> None:
        self.w.files.replace_text(self.config, OWN, 0o640)
        self.fleet({"monitoring-1": "10.0.0.1", "web-1": "10.0.0.2"})
        self.assertRan(self.write_config(), 0, "wrote 2 host(s) to " + self.config)
        first = self.w.read(self.config)
        self.assertTrue(first.startswith(OWN), "the person's own config was changed")
        self.assertEqual(1, first.count("# BEGIN fleet-ssh"))
        self.assertIn("Host monitoring-1\n    HostName 10.0.0.1\n    User alice\n", first)
        self.assertNotIn("nixdeploy", first)
        self.assertEqual(0o640, os.stat(self.config).st_mode & 0o777, "the config's mode changed")
        self.assertRan(self.write_config(), 0)
        self.assertEqual(first, self.w.read(self.config), "a second run changed the file")
        self.w.files.replace_text(self.config, first + AFTER, 0o640)
        self.fleet({"monitoring-1": "10.0.0.9", "web-1": "", "db-1": "10.0.0.3"})
        self.assertRan(self.write_config(), 0, "skipping web-1: fleet.publicAddress is empty", "wrote 2 host(s)")
        moved = self.w.read(self.config)
        self.assertEqual(1, moved.count("# BEGIN fleet-ssh"), "the block was duplicated, not replaced")
        self.assertIn("HostName 10.0.0.9", moved)
        self.assertNotIn("HostName 10.0.0.1\n", moved)
        self.assertNotIn("Host web-1", moved)
        self.assertTrue(moved.startswith(OWN) and moved.endswith(AFTER), "text around the block was lost")


if __name__ == "__main__":
    unittest.main()
