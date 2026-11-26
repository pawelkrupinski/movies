#!/usr/bin/env python3
"""Offline tests for infra/bin/fleet-ssh's `--ssh-config` writer: re-running replaces its own block in place,
leaves the human's stanzas after it intact, and is idempotent.

    python3 infra/bin/fleet_ssh_test.py
"""
import importlib.machinery
import importlib.util
import os
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
_loader = importlib.machinery.SourceFileLoader("fleet_ssh", os.path.join(HERE, "fleet-ssh"))
_spec = importlib.util.spec_from_loader("fleet_ssh", _loader)
assert _spec is not None
fleet_ssh = importlib.util.module_from_spec(_spec)
_loader.exec_module(fleet_ssh)


class InstallSshConfig(unittest.TestCase):
    def setUp(self) -> None:
        self.dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.dir.cleanup)
        self.path = os.path.join(self.dir.name, "config")

    def write(self, text: str) -> None:
        with open(self.path, "w") as f:
            f.write(text)

    def read(self) -> str:
        with open(self.path) as f:
            return f.read()

    def test_rerun_keeps_the_stanza_after_the_block_on_its_own_line(self) -> None:
        block = fleet_ssh.ssh_config({"a": "1.1.1.1"}, "me")
        self.write("Host before\n    User x\n" + block + "Host zed\n    User z\n")
        fleet_ssh.install_ssh_config(block, self.path)
        self.assertIn("# END fleet-ssh\nHost zed\n", self.read())

    def test_rerun_is_idempotent(self) -> None:
        block = fleet_ssh.ssh_config({"a": "1.1.1.1"}, "me")
        self.write("Host before\n" + block + "Host zed\n")
        fleet_ssh.install_ssh_config(block, self.path)
        once = self.read()
        fleet_ssh.install_ssh_config(block, self.path)
        fleet_ssh.install_ssh_config(block, self.path)
        self.assertEqual(once, self.read())
        self.assertEqual(1, once.count(fleet_ssh.SSH_CONFIG_BEGIN))

    def test_new_block_replaces_the_old_one(self) -> None:
        self.write(fleet_ssh.ssh_config({"a": "1.1.1.1"}, "me") + "Host zed\n")
        fleet_ssh.install_ssh_config(fleet_ssh.ssh_config({"b": "2.2.2.2"}, "me"), self.path)
        text = self.read()
        self.assertIn("2.2.2.2", text)
        self.assertNotIn("1.1.1.1", text)
        self.assertTrue(text.endswith("Host zed\n"))


if __name__ == "__main__":
    unittest.main()
