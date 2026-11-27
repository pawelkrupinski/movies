#!/usr/bin/env python3
"""scripts/vault_env.py over fakes: nothing missing is nothing done; a missing variable re-runs the script under
`secrets run movies <the missing specs>` (one exec, naming only those, the vault key of VAR=KEY); no `secrets` on PATH is
False. The real re-run, through a fake vault on PATH, is vault-env-test.sh's."""
from __future__ import annotations

import os
import sys
import unittest
from typing import List, NoReturn, Sequence

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from vault_env import Vault  # noqa: E402


class Replaced(Exception):
    pass


class VaultTest(unittest.TestCase):
    def vault(self, env: dict[str, str], on_path: bool = True) -> Vault:
        self.execs: List[Sequence[str]] = []

        def replace(argv: Sequence[str]) -> NoReturn:
            self.execs.append(argv)
            raise Replaced()
        return Vault(env, on_path, replace)

    def test_nothing_missing_is_true_and_nothing_runs(self) -> None:
        self.assertTrue(self.vault({"A": "1", "B": "2"}).exec_under_vault(["A", "B=KEY_B"], ["x"]))
        self.assertEqual([], self.execs)

    def test_only_the_missing_specs_are_asked_of_the_vault_and_the_command_re_run(self) -> None:
        vault = self.vault({"A": "1"})
        with self.assertRaises(Replaced):
            vault.exec_under_vault(["A", "B=KEY_B", "C"], ["python", "tool.py", "--x"])
        self.assertEqual([["secrets", "run", "movies", "B=KEY_B", "C", "--", "python", "tool.py", "--x"]], self.execs)

    def test_a_missing_spec_with_no_vault_is_false(self) -> None:
        vault = self.vault({}, on_path=False)
        self.assertFalse(vault.exec_under_vault(["A"]))
        self.assertEqual(["A", "B=K"], vault.missing_specs(["A", "B=K"]))
        self.assertEqual([], self.execs)


if __name__ == "__main__":
    unittest.main()
