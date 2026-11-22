#!/usr/bin/env python3
"""record-country-fixture: the whole country's fixture corpus recorded by the real recorder, black-box over a scratch
checkout with a fake `sbt` (the recorder) and a fake `secrets` (the Touch ID vault). Locally a missing key is read from
the vault, the tool re-run under `secrets run` naming only the missing ones; with no vault (CI) a missing key fails
loudly; the country picks the fixture directory (today, today-<cc>, or KINOWO_FIXTURE_DIR) and both country variables;
an unknown country, a failed recorder and an empty capture fail; given a zip path the tree is wrapped in one zip, the
recorder's colon names included. The same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import shutil
import sys
import unittest
import zipfile
from typing import Dict, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))),
                                "scripts", "lib"))
import cli_harness  # noqa: E402

PYTHON = ".github/scripts/record_country_fixture.py"
SHELL = ".github/scripts/record-country-fixture.sh"
TOOL = cli_harness.tool_path(PYTHON, SHELL)
# What the tool reads of its checkout besides itself: copied into the scratch checkout, whichever exist
BESIDE = ("scripts/vault-env.sh", "scripts/vault_env.py", "scripts/lib")
KEYS = {"TMDB_API_KEY": "t", "DECODO_PROXY_USER": "u", "DECODO_PROXY_PASS": "p"}
# The recorder: its environment, and FAKE_FILES fixture files (one named after a URL, colon and all) in the directory
SBT = """
dirname = os.path.join("test/resources/fixtures", env["KINOWO_FIXTURE_DIR"])
os.makedirs(dirname, exist_ok=True)
with open(os.environ["FAKE_SEEN"], "w") as f:
    f.write(" ".join(f"{k}={env.get(k, '')}" for k in ("KINOWO_COUNTRY", "KINOWO_COUNTRIES", "KINOWO_FIXTURE_DIR")))
files = int(env.get("FAKE_FILES", "2"))
if files:
    with open(os.path.join(dirname, "CAPTURE_DATE"), "w") as f:
        f.write("2026-11-22\\n")
for n in range(files - 1):
    with open(os.path.join(dirname, f"https___x.example_{n},ts:1781388000"), "w") as f:
        f.write("{}")
exit(int(env.get("FAKE_SBT_EXIT", "0")))
"""


class RecordCountryFixtureTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        self.root = self.path("checkout")
        relative = os.path.relpath(TOOL, cli_harness.ROOT)
        for path in (relative,) + BESIDE:
            source, target = os.path.join(cli_harness.ROOT, path), os.path.join(self.root, path)
            os.makedirs(os.path.dirname(target), exist_ok=True)
            if os.path.isdir(source):
                shutil.copytree(source, target, ignore=shutil.ignore_patterns("__pycache__", "*_test.py"))
            elif os.path.exists(source):
                shutil.copy2(source, target)
        self.tool = os.path.join(self.root, relative)
        self.fakes.program("sbt", SBT)

    def record(self, *args: str, keys: Optional[Dict[str, str]] = None,
               env: Optional[Dict[str, Optional[str]]] = None) -> cli_harness.Done:
        """The tool from a directory that is not its checkout, the vault's own PATH entries left out."""
        given: Dict[str, Optional[str]] = {k: None for k in KEYS}
        given.update(keys if keys is not None else KEYS)
        given.update({"PATH": f"{self.fakes.bin}:/usr/bin:/bin", "FAKE_SEEN": self.path("seen"),
                      "KINOWO_COUNTRY": None, "KINOWO_COUNTRIES": None, "KINOWO_FIXTURE_DIR": None, **(env or {})})
        done = cli_harness.run_bounded([self.tool, *args], env=cli_harness.isolated_env(os.environ, given),
                                       cwd=self.work, stdin=None)
        return done

    def seen(self) -> str:
        return self.read("seen")

    def test_poland_by_default_into_today(self) -> None:
        done = self.record()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("KINOWO_COUNTRY=pl KINOWO_COUNTRIES=pl KINOWO_FIXTURE_DIR=today", self.seen())
        self.assertEqual([["worker/Test/runMain clients.tools.RecordAllDataToFixture"]], self.fakes.calls("sbt"))
        self.assertIn("Recording the pl fixture corpus into test/resources/fixtures/today …\n", done.stdout)
        self.assertIn("Captured 2 fixture files into test/resources/fixtures/today (2026-11-22 )\n", done.stdout)

    def test_another_country_into_its_own_directory_or_the_one_named(self) -> None:
        self.assertEqual(0, self.record(env={"KINOWO_COUNTRY": "de"}).code)
        self.assertEqual("KINOWO_COUNTRY=de KINOWO_COUNTRIES=de KINOWO_FIXTURE_DIR=today-de", self.seen())
        done = self.record(env={"KINOWO_COUNTRY": "uk", "KINOWO_FIXTURE_DIR": "mine"})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("KINOWO_COUNTRY=uk KINOWO_COUNTRIES=uk KINOWO_FIXTURE_DIR=mine", self.seen())
        self.assertTrue(os.path.isfile(os.path.join(self.root, "test/resources/fixtures/mine/CAPTURE_DATE")))

    def test_an_unknown_country_fails_before_recording(self) -> None:
        done = self.record(env={"KINOWO_COUNTRY": "fr"})
        self.assertEqual(1, done.code)
        self.assertIn("::error::KINOWO_COUNTRY='fr' is not one of pl|de|uk|us|es.", done.stderr)
        self.assertEqual([], self.fakes.calls("sbt"))

    def test_without_a_vault_a_missing_key_fails_loudly(self) -> None:
        done = self.record(keys={})
        self.assertEqual(1, done.code)
        self.assertIn("::error::Missing required key(s): TMDB_API_KEY DECODO_PROXY_USER DECODO_PROXY_PASS — set them in "
                      "the environment (or the Touch ID vault, namespace movies) before recording.", done.stderr)
        self.assertEqual([], self.fakes.calls("sbt"))

    def test_locally_the_missing_keys_alone_come_from_the_vault(self) -> None:
        self.fakes.script("secrets", "exit 0")
        done = self.record("out.zip", keys={"TMDB_API_KEY": "t"})
        self.assertEqual(0, done.code, done.output)
        [call] = self.fakes.calls("secrets")
        self.assertEqual(["run", "movies", "DECODO_PROXY_USER", "DECODO_PROXY_PASS", "--"], call[:5])
        self.assertEqual([self.tool, "out.zip"], call[-2:])
        self.assertEqual([], self.fakes.calls("sbt"))

    def test_a_failed_recorder_fails_the_run(self) -> None:
        done = self.record(env={"FAKE_SBT_EXIT": "3"})
        self.assertEqual(3, done.code)
        self.assertNotIn("Captured", done.stdout)

    def test_an_empty_capture_is_no_success(self) -> None:
        done = self.record(env={"FAKE_FILES": "0"})
        self.assertEqual(1, done.code)
        self.assertIn("Captured 0 fixture files into test/resources/fixtures/today ()", done.stdout)
        self.assertIn("::error::Recorder produced no fixture files — refusing to treat this as a successful capture.",
                      done.stderr)

    def test_a_zip_wraps_the_tree_colon_names_and_all(self) -> None:
        target = self.path("country-fixture.zip")
        self.write("checkout/test/resources/fixtures/today-de/CAPTURE_DATE", "another country's\n")
        done = self.record(target, env={"FAKE_FILES": "3"})
        self.assertEqual(0, done.code, done.output)
        with zipfile.ZipFile(target) as z:
            names = sorted(n for n in z.namelist() if not n.endswith("/"))
        self.assertEqual(["today/CAPTURE_DATE", "today/https___x.example_0,ts:1781388000",
                          "today/https___x.example_1,ts:1781388000"], names)
        self.assertRegex(done.stdout, r"\nWrote \S+ zip with 3 fixtures to " + target.replace(".", r"\.") + "\n$")


if __name__ == "__main__":
    unittest.main()
