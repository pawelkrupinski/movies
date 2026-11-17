#!/usr/bin/env python3
"""Offline tests for infra/bin/nix_path.py: nix is found on PATH, else at the standard install locations (the
Determinate default profile first), and nowhere is an error that names the fix -- never a silent skip. Also that
bin/deploy reports a missing nix with that fix instead of "nix is not on PATH".

    python3 infra/bin/nix_path_test.py
"""
import contextlib
import importlib.machinery
import importlib.util
import io
import os
import pathlib
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import nix_path  # noqa: E402


def executable(path: pathlib.Path) -> pathlib.Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("#!/bin/sh\necho fake nix\n")
    path.chmod(0o755)
    return path


class FindNix(unittest.TestCase):
    def setUp(self):
        self.tmp = pathlib.Path(tempfile.mkdtemp())
        self.empty_bin = self.tmp / "empty-bin"
        self.empty_bin.mkdir()
        self.home = self.tmp / "home"
        self.home.mkdir()
        self.default = self.tmp / "nix/var/nix/profiles/default/bin/nix"
        self.system = self.tmp / "run/current-system/sw/bin/nix"
        self.candidates = (str(self.default), "~/.nix-profile/bin/nix", str(self.system))

    def find(self, path):
        return nix_path.find_nix(path=path, home=str(self.home), candidates=self.candidates)

    def test_only_at_the_default_profile_is_found(self):
        executable(self.default)
        self.assertEqual(self.find(str(self.empty_bin)), str(self.default))

    def test_path_wins_over_the_install_locations(self):
        on_path = executable(self.tmp / "on-path/nix")
        executable(self.default)
        self.assertEqual(self.find(f"{self.empty_bin}:{on_path.parent}"), str(on_path))

    def test_the_user_profile_is_found_under_home(self):
        user = executable(self.home / ".nix-profile/bin/nix")
        self.assertEqual(self.find(str(self.empty_bin)), str(user))

    def test_a_non_executable_file_is_not_nix(self):
        self.default.parent.mkdir(parents=True)
        self.default.write_text("not a program")
        self.default.chmod(0o644)
        with self.assertRaises(nix_path.NixNotFound):
            self.find(str(self.empty_bin))

    def test_nowhere_is_an_error_naming_the_fix(self):
        with self.assertRaises(nix_path.NixNotFound) as raised:
            self.find(str(self.empty_bin))
        message = str(raised.exception)
        self.assertTrue(message.startswith("nix not found"), message)
        self.assertIn("install", message)
        self.assertIn("install.determinate.systems", message)
        self.assertIn("export PATH=/nix/var/nix/profiles/default/bin:$PATH", message)
        for candidate in self.candidates:
            self.assertIn(candidate, message)

    def test_put_on_path_prepends_the_found_directory(self):
        executable(self.default)
        environ = {"PATH": str(self.empty_bin), "HOME": str(self.home)}
        found = nix_path.put_nix_on_path(environ, candidates=self.candidates)
        self.assertEqual(found, str(self.default))
        self.assertEqual(environ["PATH"], f"{self.default.parent}:{self.empty_bin}")

    def test_put_on_path_leaves_a_path_that_already_has_nix(self):
        on_path = executable(self.tmp / "on-path/nix")
        environ = {"PATH": str(on_path.parent), "HOME": str(self.home)}
        nix_path.put_nix_on_path(environ, candidates=self.candidates)
        self.assertEqual(environ["PATH"], str(on_path.parent))

    def test_the_command_line_prints_the_bin_directory_or_fails_with_the_fix(self):
        executable(self.default)
        script = os.path.join(HERE, "nix_path.py")
        env = {"PATH": f"{self.empty_bin}:/usr/bin:/bin", "HOME": str(self.home),
               "NIX_PATH_CANDIDATES": ":".join(self.candidates)}
        found = subprocess.run([sys.executable, script], env=env, capture_output=True, text=True)
        self.assertEqual((found.returncode, found.stdout.strip()), (0, str(self.default.parent)), found.stderr)
        self.default.unlink()
        missing = subprocess.run([sys.executable, script], env=env, capture_output=True, text=True)
        self.assertEqual(missing.returncode, 1)
        self.assertIn("nix not found", missing.stderr)


class DeployReportsTheFix(unittest.TestCase):
    def test_deploy_without_nix_says_how_to_fix_it(self):
        path = os.path.join(HERE, "deploy")
        loader = importlib.machinery.SourceFileLoader("deploy_for_nix_path_test", path)
        spec = importlib.util.spec_from_loader("deploy_for_nix_path_test", loader)
        deploy = importlib.util.module_from_spec(spec)
        loader.exec_module(deploy)
        err = io.StringIO()
        with mock.patch.object(deploy.nix_path, "CANDIDATES", ("/nonexistent/nix",)), \
                mock.patch.dict(os.environ, {"PATH": tempfile.mkdtemp()}), \
                mock.patch.object(deploy.os, "chdir"), contextlib.redirect_stderr(err):
            rc = deploy.main(["--list"])
        self.assertEqual(rc, 2, err.getvalue())
        self.assertIn("COULD NOT MEASURE: nix not found", err.getvalue())
        self.assertIn("export PATH=/nix/var/nix/profiles/default/bin:$PATH", err.getvalue())


if __name__ == "__main__":
    unittest.main(verbosity=1)
