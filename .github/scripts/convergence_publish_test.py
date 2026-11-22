#!/usr/bin/env python3
"""convergence-publish against a fake `gh` whose release is a directory, black-box: `tree` (nothing packed, the working
tree, its pinned copy in the one upload, the gzip tree retired, a failed upload), `verified-pin` (the corpus archived
first or no pin, the newest five pairs kept with only their fills and lists, a first pin, THE PIN GUARD: a failed
verify, a finding, no served check, no pinned tree, the deadline), `latest` (the prod-latest marker, its pruning, an
unreadable marker pruning nothing) and `dead` (overlays whose tree is gone, the takes results). The same cases hold the
shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import shutil
import sys
import unittest
from typing import Dict, Optional

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.dont_write_bytecode = True  # `python3 -I` ignores PYTHONDONTWRITEBYTECODE: no __pycache__ in the tree
sys.path.insert(0, os.path.join(ROOT, "scripts/lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path(".github/scripts/convergence_publish.py", ".github/scripts/convergence-publish.sh")
# The release is <work>/release (absent until created); an asset's createdAt is in <work>/created/<name>, else a fixed
# time. Every write is logged to <work>/calls. A run's artifact <n> is <work>/artifacts/<run>/<n>/.
GH = r"""
import json, shutil, signal, subprocess
root = env["STUB_ROOT"]
release = os.path.join(root, "release")
def flag(args, name):
    return args[args.index(name) + 1] if name in args else ""
def log(line):
    with open(os.path.join(root, "calls"), "a") as f:
        f.write(line + "\n")
if argv[0] == "run":
    if argv[1] != "download":
        out("2026-11-14T23:31:00Z"); exit(0)
    src = os.path.join(root, "artifacts", argv[2], flag(argv, "--name"))
    if not os.path.isdir(src):
        err("no valid artifacts found to download"); exit(1)
    if env.get("STUB_RUN_DOWNLOAD_HANGS"):  # an artifact storage that never answers (HUNG STUBS)
        with open(env["STUB_RUN_DOWNLOAD_HANGS"], "a") as f:
            f.write(str(os.getpid()) + "\n")
        signal.pause()  # blocks until killed: only the tool's own bound ends it
    os.makedirs(flag(argv, "--dir"), exist_ok=True)
    for f in os.listdir(src):
        shutil.copy(os.path.join(src, f), flag(argv, "--dir"))
    exit(0)
if argv[0] != "release":
    exit(1)
verb, tag, rest = argv[1], argv[2], argv[3:]
if verb == "view":
    if not os.path.isdir(release):
        err("release not found"); exit(1)
    if rest[:1] == ["--json"]:
        def created(name):
            p = os.path.join(root, "created", name)
            return open(p).read().strip() if os.path.exists(p) else "2026-10-15T12:00:00Z"
        assets = [{"name": n, "createdAt": created(n)} for n in sorted(os.listdir(release))]
        done = subprocess.run(["jq", "-r", rest[3]], input=json.dumps({"assets": assets}), capture_output=True, text=True)
        sys.stdout.write(done.stdout); sys.stderr.write(done.stderr); exit(done.returncode)
    exit(0)
if verb == "create":
    log("create " + tag); os.makedirs(release, exist_ok=True); exit(0)
if verb == "upload":
    log("upload " + tag + " " + " ".join(rest))
    if env.get("STUB_UPLOAD_FAILS"):
        err("HTTP 422"); exit(1)
    into = os.path.join(root, "archive") if tag == "identity-recordings" else release
    for f in [a for a in rest if not a.startswith("--")]:
        shutil.copy(f, into)
    exit(0)
if verb == "delete-asset":
    log("delete " + rest[0])
    p = os.path.join(release, rest[0])
    if os.path.exists(p):
        os.remove(p)
    exit(0)
if verb == "download":
    if env.get("STUB_DOWNLOAD_FAILS"):
        err("HTTP 502: Bad Gateway"); exit(1)
    p = os.path.join(release, flag(rest, "--pattern"))
    if not os.path.isfile(p):
        err("no assets match the file pattern"); exit(1)
    shutil.copy(p, flag(rest, "--dir")); exit(0)
exit(1)
"""


class ConvergencePublishTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.program("gh", GH)
        for d in ("leg", "archive", "created"):
            os.makedirs(self.path(d))
        self.output = ""

    def run_publish(self, *args: str, **extra: Optional[str]) -> int:
        env: Dict[str, Optional[str]] = {
            "STUB_ROOT": self.work, "FIXTURE_RELEASE_TAG": "convergence-fixtures", "GITHUB_ENV": self.path("github-env"),
            "GH_RELEASE_RETRY_DELAY": "0", "GH_DOWNLOAD_BACKOFF": "0", "GH_DOWNLOAD_TRANSIENT_ATTEMPTS": "2", **extra}
        done = self.run_tool(*args, env=env, cwd=self.path("leg"))
        self.output = done.output
        return done.code

    def calls(self, verb: str) -> int:
        if not os.path.exists(self.path("calls")):
            return 0
        return sum(1 for line in self.read("calls").splitlines() if line.startswith(verb))

    def assets(self) -> str:
        return " ".join(sorted(os.listdir(self.path("release"))))

    def asset(self, name: str, created: str = "", text: str = "") -> None:
        self.write(f"release/{name}", text)
        if created:
            self.write(f"created/{name}", created + "\n")

    def first_word(self, name: str) -> str:
        return self.read(f"release/{name}").split()[0]

    def clear_release(self) -> None:
        for d in ("release", "created"):
            shutil.rmtree(self.path(d), ignore_errors=True)
            os.makedirs(self.path(d))

    # ── tree ───────────────────────────────────────────────────────────────────────────────────

    def test_tree(self) -> None:
        self.assertEqual((0, 0), (self.run_publish("tree", "pl", "true", "leg.log"), self.calls("upload")),
                         "a leg that packed nothing publishes nothing")
        self.write("leg/enrichment-upload/enrichment-pl.tar.zst", "tree\n")
        self.write("leg/leg.log", "[pl] boot complete\n")
        self.assertEqual(0, self.run_publish("tree", "pl", "false", "leg.log", KINOWO_CONVERGENCE_CORPUS_RUN="100"))
        self.assertEqual(1, self.calls("create"), "creating the release it did not find")
        self.assertEqual(("enrichment-pl.tar.zst", False), (self.assets(), os.path.exists(self.path("github-env"))),
                         "and pins nothing")
        self.asset("enrichment-pl.tar.gz")
        self.assertEqual(0, self.run_publish("tree", "pl", "true", "leg.log", KINOWO_CONVERGENCE_CORPUS_RUN="100"))
        uploads = [line for line in self.read("calls").splitlines() if line.startswith("upload")]
        self.assertEqual("upload convergence-fixtures enrichment-upload/enrichment-pl.tar.zst "
                         "enrichment-upload/enrichment-pl-100.tar.zst --clobber", uploads[-1])
        self.assertEqual("KINOWO_PINNED_TREE=enrichment-pl-100.tar.zst\n", self.read("github-env"))
        self.assertEqual(("enrichment-pl-100.tar.zst enrichment-pl.tar.zst", 1), (self.assets(), self.calls("create")),
                         "retiring the gzip working tree, and creating no second release")
        self.write("github-env", "")
        self.write("leg/leg.log", "[uk] boot complete\n")
        self.run_publish("tree", "pl", "true", "leg.log", KINOWO_CONVERGENCE_CORPUS_RUN="101")
        self.assertEqual("", self.read("github-env"), "another country's boot line pins nothing")
        self.assertEqual(1, self.run_publish("tree", "pl", "false", "leg.log", STUB_UPLOAD_FAILS="1",
                                             GH_RELEASE_ATTEMPTS="1"), "a failed upload fails the step")

    # ── verified-pin: the pin, its corpus archive and its pruning ──────────────────────────────

    def clean_verify(self, code: str, run: str) -> None:
        shutil.rmtree(self.path("diag"), ignore_errors=True)
        self.write(f"diag/convergence-diagnostics-{code}-convergence/{code}-served-served.txt", f"# corpus-run {run}\n")

    def corpus_artifact(self, code: str, run: str, text: str) -> None:
        self.write(f"artifacts/{run}/scrape-fixtures-{code}/scrapes-{code}.tar.gz", text + "\n")

    def pin(self, *args: str, **extra: Optional[str]) -> int:
        return self.run_publish("verified-pin", *args, **extra)

    def test_a_pin_archives_its_corpus_first_and_keeps_the_newest_five_pairs(self) -> None:
        for n in range(1, 8):
            for name in (f"enrichment-pl-{n}.tar.zst", f"fill-pl-{n}-900.tar.zst", f"refetch-pl-{n}-900.tsv",
                         f"refused-pl-{n}-900-2-takes.tsv"):
                self.asset(name, f"2026-10-0{n}T00:00:00Z")
        self.asset("enrichment-pl-0.tar.gz", "2026-09-30T00:00:00Z")
        for name in ("enrichment-uk-1.tar.zst", "fill-uk-1-900.tar.zst", "enrichment-pl.tar.zst"):
            self.asset(name)
        self.asset("prod-latest-pl.txt", text="7 2026-10-07T00:00:00Z enrichment-pl-7.tar.zst\n")
        self.clean_verify("pl", "7")
        args = ("7", "pl", "success", self.path("diag"))
        # Identity takes #76 (run 43317254260): no corpus to archive, or an archive refusing it, is no pin
        self.assertEqual(1, self.pin(*args, KINOWO_KEPT_RECORDINGS=""))
        self.assertFalse(os.path.exists(self.path("release/hermetic-pl.txt")))
        self.assertIn("could not be archived", self.output)
        self.corpus_artifact("pl", "7", "corpus7")
        self.assertEqual(1, self.pin(*args, KINOWO_KEPT_RECORDINGS="", STUB_UPLOAD_FAILS="1", GH_RELEASE_ATTEMPTS="1"))
        self.assertFalse(os.path.exists(self.path("release/hermetic-pl.txt")))
        self.assertEqual(0, self.pin(*args, KINOWO_KEPT_RECORDINGS=""), self.output)
        self.assertEqual("7 2026-10-07T00:00:00Z enrichment-pl-7.tar.zst\n", self.read("release/hermetic-pl.txt"))
        self.assertEqual("corpus7\n", self.read("archive/scrapes-pl-7.tar.gz"))
        uploads = [line for line in self.read("calls").splitlines() if line.startswith("upload")]
        archived = max(i for i, line in enumerate(uploads) if "identity-recordings" in line and "scrapes-pl-7.tar.gz" in line)
        self.assertIn("hermetic-pl.txt", uploads[archived + 1], "the corpus archived before the marker")
        kept = [f"enrichment-pl-{n}.tar.zst" for n in range(3, 8)] + ["enrichment-pl.tar.zst", "enrichment-uk-1.tar.zst"]
        kept += [f"fill-pl-{n}-900.tar.zst" for n in range(3, 8)] + ["fill-uk-1-900.tar.zst", "hermetic-pl.txt",
                                                                       "prod-latest-pl.txt"]
        kept += [f"refetch-pl-{n}-900.tsv" for n in range(3, 8)] + [f"refused-pl-{n}-900-2-takes.tsv" for n in range(3, 8)]
        self.assertEqual(" ".join(sorted(kept)), self.assets())

    def test_a_first_pin_with_no_fills_to_prune_passes(self) -> None:
        # record run 161, Spain: the prune's grep found nothing, which under pipefail failed the step after the pin
        self.asset("enrichment-es-161.tar.zst")
        self.clean_verify("es", "161")
        self.corpus_artifact("es", "161", "corpus")
        self.assertEqual(0, self.pin("161", "es", "success", self.path("diag")), self.output)

    # ── prod-latest ────────────────────────────────────────────────────────────────────────────

    def test_latest(self) -> None:
        os.makedirs(self.path("release"))
        self.assertEqual((0, False), (self.run_publish("latest", "pl"), os.path.exists(self.path("release/prod-latest-pl.txt"))),
                         "no pinned tree leaves prod-latest as it was")
        for n in range(1, 9):
            self.asset(f"enrichment-pl-{n}.tar.zst", f"2026-10-0{n}T00:00:00Z")
        self.asset("hermetic-pl.txt", text="2 2026-10-02T00:00:00Z enrichment-pl-2.tar.zst\n")
        self.assertEqual(0, self.run_publish("latest", "pl", KINOWO_PINNED_TREE="enrichment-pl-8.tar.zst",
                                             KINOWO_CONVERGENCE_CORPUS_RUN="8", KINOWO_KEPT_RECORDINGS="1",
                                             KINOWO_CONVERGENCE_CORPUS_RECORDED_AT="2026-10-08T00:00:00Z"), self.output)
        self.assertEqual("8 2026-10-08T00:00:00Z enrichment-pl-8.tar.zst\n", self.read("release/prod-latest-pl.txt"))
        self.assertEqual("2 2026-10-02T00:00:00Z enrichment-pl-2.tar.zst\n", self.read("release/hermetic-pl.txt"))
        self.assertEqual(" ".join([f"enrichment-pl-{n}.tar.zst" for n in (1, 2, 4, 5, 6, 7, 8)]
                                  + ["hermetic-pl.txt", "prod-latest-pl.txt"]), self.assets(),
                         "the newest five, never the hermetic pair or a kept landing recording")
        # A marker read that fails is no evidence the pair is unpinned: it prunes nothing
        self.asset("enrichment-pl-0.tar.zst", "2026-09-00T00:00:00Z")
        self.asset("enrichment-pl-9.tar.zst", "2026-10-09T00:00:00Z")
        before = self.assets()
        self.assertEqual(0, self.run_publish("latest", "pl", STUB_DOWNLOAD_FAILS="1", KINOWO_PINNED_TREE="enrichment-pl-9.tar.zst",
                                             KINOWO_CONVERGENCE_CORPUS_RUN="9", KINOWO_KEPT_RECORDINGS="",
                                             KINOWO_CONVERGENCE_CORPUS_RECORDED_AT="2026-10-09T00:00:00Z"))
        self.assertEqual(before, self.assets())
        self.assertIn("pruning nothing", self.output)

    # ── dead ───────────────────────────────────────────────────────────────────────────────────

    def test_dead(self) -> None:
        # Run 41351315090: the release at GitHub's 1000-asset cap, 101 of them overlays of a mode since removed
        for name in ("enrichment-uk-573.tar.zst", "identity-overlay-uk-573.tar.gz", "identity-overlay-uk-36691290304.tar.gz",
                     "identity-overlay-us-36691290304.tar.gz", "fill-uk-573-9-1-convergence.tar.zst",
                     "takes-decisions-uk-0123abcd.gz", "takes-full-uk.txt", "takes-final-us-0123abcd.gz"):
            self.asset(name)
        self.assertEqual(0, self.run_publish("dead", "uk", TAKES_RELEASE_TAG="identity-takes"))
        self.assertEqual("enrichment-uk-573.tar.zst fill-uk-573-9-1-convergence.tar.zst identity-overlay-uk-573.tar.gz "
                         "identity-overlay-us-36691290304.tar.gz takes-final-us-0123abcd.gz", self.assets())
        os.remove(self.path("calls"))
        self.assertEqual((0, 0), (self.run_publish("dead", "uk", TAKES_RELEASE_TAG="identity-takes"), self.calls("delete")),
                         "a release with nothing dead deletes nothing")
        self.assertEqual(0, self.run_publish("dead", "us", TAKES_RELEASE_TAG="convergence-fixtures"))
        self.assertEqual("delete identity-overlay-us-36691290304.tar.gz", self.read("calls").strip(),
                         "a takes store still on the fixtures' release is the store, never pruned")

    # ── verified-pin: THE PIN GUARD ────────────────────────────────────────────────────────────

    def guard_release(self) -> str:
        for name in ("enrichment-pl-573.tar.zst", "enrichment-pl-900.tar.zst", "enrichment-de-573.tar.zst",
                     "enrichment-de-900.tar.zst"):
            self.asset(name)
        self.asset("hermetic-pl.txt", text="573 2026-10-20T13:35:26Z enrichment-pl-573.tar.zst\n")
        self.asset("hermetic-de.txt", text="573 2026-10-20T13:35:26Z enrichment-de-573.tar.zst\n")
        self.asset("prod-latest-pl.txt", text="900 2026-11-15T00:02:11Z enrichment-pl-900.tar.zst\n")
        self.corpus_artifact("pl", "900", "corpus900")
        self.corpus_artifact("de", "900", "corpus900")
        self.write("diag/convergence-diagnostics-pl-convergence/pl-served-served.txt", "# corpus-run 900\n")
        self.write("diag/convergence-diagnostics-pl-convergence/pl-served-emit.txt", "# corpus-run 900\n")
        self.write("diag/convergence-diagnostics-de-convergence/de-served-served.txt", "# corpus-run 900\n")
        return self.path("diag")

    def test_the_pin_guard(self) -> None:
        diag = self.guard_release()
        self.assertEqual((1, "573", "573"), (self.pin("900", "pl,de", "failure", diag), self.first_word("hermetic-pl.txt"),
                                             self.first_word("hermetic-de.txt")))
        self.assertEqual(2, self.output.count("verify ended failure"))
        self.write("diag/convergence-diagnostics-de-convergence/de-served-served.txt",
                   "# corpus-run 900\nvenue slot(s) whose own year AND director both deny the film they are served under"
                   "\tx\tAcktan at Kino X\n")
        self.assertEqual(1, self.pin("900", "pl,de", "success", diag))
        self.assertEqual(("900 2026-11-15T00:02:11Z enrichment-pl-900.tar.zst\n", "573"),
                         (self.read("release/hermetic-pl.txt"), self.first_word("hermetic-de.txt")))
        self.assertIn("Acktan at Kino X", self.output)
        shutil.rmtree(self.path("diag/convergence-diagnostics-de-convergence"))
        self.assertEqual((1, "573"), (self.pin("900", "de", "success", diag), self.first_word("hermetic-de.txt")),
                         "diagnostics that do not show the served-corpus check ran prove nothing")
        self.write("diag/convergence-diagnostics-de-convergence/de-served-served.txt", "# corpus-run 901\n")
        self.assertEqual((1, "573"), (self.pin("901", "de", "success", diag), self.first_word("hermetic-de.txt")),
                         "a recording with no pinned copy of its tree is refused")

    def test_a_pin_out_of_time_refuses_each_country_left(self) -> None:
        diag = self.guard_release()
        self.write("release/hermetic-pl.txt", "900 2026-11-15T00:02:11Z enrichment-pl-900.tar.zst\n")
        hung = self.write("hung-downloads", "")
        self.assertEqual(1, self.pin("900", "pl,de", "success", diag, VERIFIED_PIN_SECONDS="2", STUB_RUN_DOWNLOAD_HANGS=hung))
        self.assertEqual(("900", "573"), (self.first_word("hermetic-pl.txt"), self.first_word("hermetic-de.txt")))
        self.assertEqual(2, self.output.count("pin's deadline"), self.output)
        self.assertEqual(0, cli_harness.still_running(hung), "the hung download is ended with it")
        self.assertEqual(0, self.pin("900", "de", "success", diag), self.output)
        self.assertEqual("900 2026-11-14T23:31:00Z enrichment-de-900.tar.zst\n", self.read("release/hermetic-de.txt"),
                         "timed by the run's start when prod-latest names another recording")

    def test_an_unknown_command_is_refused(self) -> None:
        self.assertEqual(64, self.run_publish("publish", "pl"))


if __name__ == "__main__":
    unittest.main()
