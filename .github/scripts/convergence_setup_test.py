#!/usr/bin/env python3
"""convergence-setup against a fake `gh` serving a fake release and recording runs, black-box: `pinned` (the one reader
of the pin: printed, absent, malformed, a failed read), `pair` (a recording leg, the pinned pair with its fills, a given
pair, a named recording's bare pair, no or a malformed pin, no release tag), `pins` (the run's pairs once), `corpus`
(the pinned run's, expired, the archive release, a throttled storage asked again or failing, this job's own, the newest
green run, none) and `unpack` (the staged tree merged, counted, a tree status passed on per mode). The same cases hold
the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import shutil
import subprocess
import sys
import unittest
from typing import Dict, Optional

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.path.insert(0, os.path.join(ROOT, "scripts/lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path(".github/scripts/convergence_setup.py", ".github/scripts/convergence-setup.sh")
# Release assets by pattern, a run's artifact by name, a run's createdAt, the green-run listing (STUB_RUN_LIST) and the
# release's asset listing (for the fills). STUB_FAIL makes every read 5xx; STUB_THROTTLED makes the first that many
# artifact downloads answer as Actions' storage did in run 38763455103.
GH = r"""
import glob, shutil
if env.get("STUB_FAIL"):
    err("HTTP 502: Bad Gateway"); exit(1)
runs = env.get("STUB_RUNS", "")
if argv[:2] == ["run", "download"] and env.get("STUB_THROTTLED"):
    counter = os.path.join(runs, "throttled")
    n = (int(open(counter).read()) if os.path.exists(counter) else 0) + 1
    open(counter, "w").write(str(n))
    if n <= int(env["STUB_THROTTLED"]):
        err("error downloading scrape-fixtures-de: HTTP 503: 503 Egress is over the account limit. (https://x/x.zip)")
        exit(1)
def flag(name):
    return argv[argv.index(name) + 1] if name in argv else ""
release = env.get("STUB_RELEASE", "")
verb = " ".join(argv[:2])
if verb == "release view":
    out("\n".join(sorted(os.listdir(release)))); exit(0)
if verb == "release download":
    matches = [f for f in glob.glob(os.path.join(release, flag("--pattern"))) if os.path.exists(f)]
    if not matches:
        err("no assets match the file pattern"); exit(1)
    os.makedirs(flag("--dir"), exist_ok=True)
    for f in matches:
        shutil.copy(f, flag("--dir"))
    exit(0)
if verb == "run download":
    src = os.path.join(runs, argv[2], flag("--name"))
    if not os.path.isdir(src):
        err("no artifact matches any of the names or patterns provided"); exit(1)
    os.makedirs(flag("--dir"), exist_ok=True)
    for f in os.listdir(src):
        shutil.copy(os.path.join(src, f), flag("--dir"))
    exit(0)
if verb == "run view":
    out("2026-10-01T01:00:00Z"); exit(0)
if verb == "run list":
    words = env.get("STUB_RUN_LIST", "").split()
    if words:
        out("\n".join(words))
    exit(0)
err("unexpected gh " + " ".join(argv)); exit(2)
"""


class ConvergenceSetupTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.program("gh", GH)
        # A pinned pair for `de` with one fill published over it; `uk` pins nothing; `es` pins a line without a tree.
        self.write("release/hermetic-de.txt", "42 2026-10-11T02:00:00Z enrichment-de-42.tar.zst\n")
        self.write("release/fill-de-42-77-1-convergence.tar.zst", "")
        self.write("release/hermetic-es.txt", "43 2026-10-11T02:00:00Z\n")
        os.makedirs(self.path("runs"))
        self.code = 0
        self.log = ""

    def base_env(self) -> Dict[str, Optional[str]]:
        return {"STUB_RELEASE": self.path("release"), "STUB_RUNS": self.path("runs"), "FIXTURE_RELEASE_TAG": "t",
                "CONVERGENCE_FILL_LIST_BACKOFF": "0", "GH_DOWNLOAD_BACKOFF": "0"}

    def step(self, *args: str, **extra: Optional[str]) -> None:
        """The subcommand in a fresh ws/ with fresh GITHUB_ENV / GITHUB_OUTPUT: its exit in .code, its output in .log."""
        shutil.rmtree(self.path("ws"), ignore_errors=True)
        os.makedirs(self.path("ws"))
        self.write("env", "")
        self.write("output", "")
        env = dict(self.base_env(), GITHUB_ENV=self.path("env"), GITHUB_OUTPUT=self.path("output"), **extra)
        done = self.run_tool(*args, env=env, cwd=self.path("ws"))
        self.code, self.log = done.code, done.output

    def env_line(self) -> str:
        return " ".join(self.read("env").split("\n")).strip()

    def logged(self, text: str) -> bool:
        return text in self.log

    # ── pinned: the one reader of the pin ──────────────────────────────────────────────────────

    def test_pinned(self) -> None:
        done = self.run_tool("pinned", "de", env=self.base_env())
        self.assertEqual((0, "42 2026-10-11T02:00:00Z enrichment-de-42.tar.zst\n"), (done.code, done.stdout))
        done = self.run_tool("pinned", "uk", env=self.base_env())
        self.assertEqual((0, ""), (done.code, done.stdout), "no pin prints nothing and succeeds")
        self.assertEqual(2, self.run_tool("pinned", "es", env=self.base_env()).code, "a pin without a tree")
        self.assertEqual(1, self.run_tool("pinned", "de", env=dict(self.base_env(), STUB_FAIL="1")).code,
                         "a failed read is not an absent pin")

    # ── pair ───────────────────────────────────────────────────────────────────────────────────

    def test_a_recording_leg_pins_the_corpus_it_is_given_or_the_newest(self) -> None:
        self.step("pair", "de", "record", "", "900")
        self.assertEqual("KINOWO_CONVERGENCE_PIN_CORPUS_RUN=900 KINOWO_CONVERGENCE_TREE_ASSET=enrichment-de.tar.*",
                         self.env_line())
        self.step("pair", "de", "record")
        self.assertEqual("KINOWO_CONVERGENCE_TREE_ASSET=enrichment-de.tar.*\n", self.read("env"))

    def test_a_hermetic_leg_resolves_the_pinned_pair_with_its_fills(self) -> None:
        self.step("pair", "de", "hermetic")
        self.assertEqual(0, self.code, self.log)
        self.assertEqual("pair=42 2026-10-11T02:00:00Z enrichment-de-42.tar.zst fill-de-42-77-1-convergence.tar.zst\n",
                         self.read("output"))
        self.assertEqual("KINOWO_CONVERGENCE_PIN_CORPUS_RUN=42 KINOWO_CONVERGENCE_TREE_ASSET=enrichment-de-42.tar.zst "
                         "KINOWO_CONVERGENCE_FILL_ASSETS=fill-de-42-77-1-convergence.tar.zst", self.env_line())

    def test_a_given_pair_is_replayed_as_given(self) -> None:
        self.step("pair", "de", "hermetic", "7 2026-09-01T00:00:00Z enrichment-de-7.tar.zst fill-a fill-b")
        self.assertEqual("pair=7 2026-09-01T00:00:00Z enrichment-de-7.tar.zst fill-a fill-b\n", self.read("output"))
        self.assertIn("KINOWO_CONVERGENCE_FILL_ASSETS=fill-a fill-b\n", self.read("env"))

    def test_a_named_recording_is_its_bare_pair(self) -> None:
        self.step("pair", "de", "hermetic", "", "528")
        self.assertEqual((0, "pair=528 2026-10-01T01:00:00Z enrichment-de-528.tar.*\n"), (self.code, self.read("output")))
        self.assertEqual("KINOWO_CONVERGENCE_PIN_CORPUS_RUN=528 KINOWO_CONVERGENCE_TREE_ASSET=enrichment-de-528.tar.* "
                         "KINOWO_CONVERGENCE_FILL_ASSETS= KINOWO_CONVERGENCE_NAMED_RECORDING=528", self.env_line())

    def test_no_or_a_malformed_pin_fails_the_hermetic_leg(self) -> None:
        self.step("pair", "uk", "hermetic")
        self.assertEqual((1, True), (self.code, self.logged("::error::No recorded pair pinned for uk")))
        self.step("pair", "es", "hermetic")
        self.assertEqual((1, True), (self.code, self.logged("::error::No recorded pair pinned for es")))

    def test_an_unset_release_tag_fails_by_name_never_as_an_absent_pin(self) -> None:
        # Identity family explorer #1 (run 42182907464): no FIXTURE_RELEASE_TAG, and every row said "No recorded pair"
        self.step("pair", "de", "hermetic", FIXTURE_RELEASE_TAG=None)
        self.assertEqual((1, True, False), (self.code, self.logged("::error::FIXTURE_RELEASE_TAG is not set"),
                                            self.logged("No recorded pair pinned")))
        done = self.run_tool("pinned", "de", env=dict(self.base_env(), FIXTURE_RELEASE_TAG=None))
        self.assertEqual((1, True), (done.code, "FIXTURE_RELEASE_TAG is not set" in done.output))

    # ── pins: the run's pairs, once ────────────────────────────────────────────────────────────

    def test_pins_names_each_pair_leaving_out_one_it_cannot_resolve(self) -> None:
        self.step("pins", "de,uk,es")
        self.assertEqual(0, self.code, self.log)
        self.assertIn('\n{"de":"42 2026-10-11T02:00:00Z enrichment-de-42.tar.zst fill-de-42-77-1-convergence.tar.zst"}\n',
                      "\n" + self.log)
        self.assertTrue(self.logged("could not pin uk's pair"))
        self.assertEqual(("", ""), (self.read("env"), self.read("output")), "it exports nothing of its own")

    # ── corpus ─────────────────────────────────────────────────────────────────────────────────

    def corpus_runs(self) -> None:
        self.write("runs/42/scrape-fixtures-de/scrapes-de.tar.gz", "pinned\n")
        self.write("runs/50/scrape-fixtures-de/scrapes-de.tar.gz", "newest\n")

    def archive(self) -> str:
        path = self.path("ws/scrape-archive/scrapes-de.tar.gz")
        return self.read("ws/scrape-archive/scrapes-de.tar.gz") if os.path.exists(path) else ""

    def test_the_pinned_runs_corpus_is_downloaded_with_its_provenance(self) -> None:
        self.corpus_runs()
        self.step("corpus", "de", KINOWO_CONVERGENCE_PIN_CORPUS_RUN="42")
        self.assertEqual((0, "pinned\n"), (self.code, self.archive()), self.log)
        self.assertEqual("KINOWO_CONVERGENCE_CORPUS_RUN=42 KINOWO_CONVERGENCE_CORPUS_RECORDED_AT=2026-10-01T01:00:00Z",
                         self.env_line())

    def test_an_expired_pinned_corpus_ends_the_leg(self) -> None:
        self.corpus_runs()
        self.step("corpus", "de", KINOWO_CONVERGENCE_PIN_CORPUS_RUN="41")
        self.assertEqual(1, self.code)
        self.assertRegex(self.log, r"::error::recording run 41's corpus is no longer downloadable.*re-run Record scrape fixtures")

    def test_an_artifact_gone_is_read_from_the_archive_release(self) -> None:
        self.corpus_runs()
        self.write("release/scrapes-de-40.tar.zst", "archived\n")
        self.step("corpus", "de", KINOWO_CONVERGENCE_PIN_CORPUS_RUN="40")
        self.assertEqual((0, "archived\n"), (self.code, self.archive()), self.log)
        self.assertTrue(self.logged("recording run 40's corpus from the archive release identity-recordings"))
        self.assertIn("KINOWO_CONVERGENCE_CORPUS_RUN=40\n", self.read("env"))

    def test_a_throttled_storage_is_asked_again(self) -> None:
        self.corpus_runs()
        self.step("corpus", "de", KINOWO_CONVERGENCE_PIN_CORPUS_RUN="42", STUB_THROTTLED="2")
        self.assertEqual((0, "pinned\n"), (self.code, self.archive()), self.log)

    def test_a_throttle_outlasting_every_attempt_is_a_failed_read(self) -> None:
        self.corpus_runs()
        self.step("corpus", "de", KINOWO_CONVERGENCE_PIN_CORPUS_RUN="42", STUB_THROTTLED="99")
        self.assertEqual((1, True, True, False), (
            self.code, self.logged("Egress is over the account limit"),
            self.logged("::error::could not download recording run 42's corpus"), self.logged("the pinned pair has expired")))

    def test_this_jobs_own_recording_downloads_nothing(self) -> None:
        self.corpus_runs()
        self.step("corpus", "de", KINOWO_CONVERGENCE_PIN_CORPUS_RUN="77", GITHUB_RUN_ID="77")
        self.assertEqual((0, "KINOWO_CONVERGENCE_CORPUS_RUN=77", ""),
                         (self.code, self.read("env").split("\n")[0], self.archive()))

    def test_unpinned_the_newest_green_run_with_a_corpus(self) -> None:
        self.corpus_runs()
        self.step("corpus", "de", STUB_RUN_LIST="60 2026-10-03T00:00:00Z 50 2026-10-02T00:00:00Z")
        self.assertEqual((0, "newest\n", "KINOWO_CONVERGENCE_CORPUS_RUN=50"),
                         (self.code, self.archive(), self.read("env").split("\n")[0]), self.log)
        self.step("corpus", "de", STUB_RUN_LIST="60 2026-10-03T00:00:00Z")
        self.assertEqual((0, True), (self.code, self.logged("no recorded scrape corpus for de")))

    # ── unpack ─────────────────────────────────────────────────────────────────────────────────

    def unpack(self, tree_status: int, mode: str) -> None:
        shutil.rmtree(self.path("stage"), ignore_errors=True)
        fixtures = "stage/tree-stage/test/resources/fixtures/enrichment-de"
        self.write(f"{fixtures}/one.json", "answer\n")
        self.write(f"{fixtures}/.enrichment-cache/a.entry", "")
        os.makedirs(self.path("bg"), exist_ok=True)
        subprocess.run(["bash", os.path.join(ROOT, "scripts/ci/in-background.sh"), "start", "tree-restore", "bash", "-c",
                        f"exit {tree_status}"], env=dict(os.environ, BACKGROUND_DIR=self.path("bg")), check=True)
        shutil.rmtree(self.path("ws"), ignore_errors=True)
        self.write("ws/test/resources/fixtures/enrichment-de/kept.json", "kept\n")
        done = self.run_tool("unpack", "de", mode, env={"BACKGROUND_DIR": self.path("bg"),
                                                        "RUNNER_TEMP": self.path("stage")}, cwd=self.path("ws"))
        self.code, self.log = done.code, done.output

    def test_the_staged_tree_is_merged_and_counted(self) -> None:
        self.unpack(0, "hermetic")
        self.assertEqual(0, self.code, self.log)
        self.assertEqual(("answer\n", "kept\n"), (self.read("ws/test/resources/fixtures/enrichment-de/one.json"),
                                                  self.read("ws/test/resources/fixtures/enrichment-de/kept.json")))
        self.assertRegex(self.log, r"fixture files available: *[1-9]")
        self.assertRegex(self.log, r"remembered enrichment answers restored: *1")
        self.assertTrue(self.logged("no scrape-archive/scrapes-de.tar.gz"))

    def test_a_tree_status_is_passed_on_by_mode(self) -> None:
        self.unpack(3, "hermetic")
        self.assertEqual(3, self.code, "a hermetic leg's missing pin ends the leg")
        self.unpack(3, "record")
        self.assertEqual(0, self.code, "a recording leg's missing pin falls through to fetching live")
        self.unpack(4, "record")
        self.assertEqual(4, self.code, "an unreadable release ends even a recording leg")

    def test_an_unknown_subcommand_is_a_usage_error(self) -> None:
        self.assertEqual(64, self.run_tool("bogus").code)


if __name__ == "__main__":
    unittest.main()
