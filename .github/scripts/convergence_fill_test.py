#!/usr/bin/env python3
"""convergence-fill against a fake `gh` whose release holds a pair's fills and refetch lists, black-box: `pack` (an
archive only when something was fetched), `fills` (the pair's own, by run id, attempt and name; a release that cannot
be listed fails loudly after three asks, a hung listing is cut off, one failed listing is asked again, none is none),
`unpack` (laid over the tree; one that cannot be restored fails after three asks, the rest still laid), `gaps` (the
newest list, asked again, a re-run's newer, an origin's refusals skipped for a while) and `fold` (past the threshold
one fold, which later fills follow; the next fold deletes what the one before holds; an unlistable release deletes
nothing). The same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import shutil
import sys
import unittest
from typing import List, Optional

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
sys.dont_write_bytecode = True  # `python3 -I` ignores PYTHONDONTWRITEBYTECODE: no __pycache__ in the tree
sys.path.insert(0, os.path.join(ROOT, "scripts/lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path(".github/scripts/convergence_fill.py", ".github/scripts/convergence-fill.sh")
GH = r"""
import shutil, signal
root = env["STUB_ROOT"]
release = os.path.join(root, "release")
def note(name, line):
    with open(os.path.join(root, name), "a") as f:
        f.write(line + "\n")
def flag(name):
    return argv[argv.index(name) + 1] if name in argv else ""
verb = " ".join(argv[:2])
if verb == "release view":
    note("views", "x")
    if env.get("STUB_UNREACHABLE"):
        exit(1)
    if env.get("STUB_HANGS"):  # never answers: blocks until killed
        with open(env["STUB_HANGS"], "a") as f:
            f.write(str(os.getpid()) + "\n")
        signal.pause()  # blocks until killed: only the tool's own bound ends it
    if env.get("STUB_FAILS_ONCE") and not os.path.exists(os.path.join(root, "failed-once")):  # a 5xx, once
        open(os.path.join(root, "failed-once"), "w").close(); exit(1)
    names = sorted(os.listdir(release))
    if names:
        out("\n".join(names))
    exit(0)
if verb == "release download":
    pattern = flag("--pattern")
    note("downloads", pattern)
    once = os.path.join(root, "failed-download-once")
    if env.get("STUB_DOWNLOAD_FAILS_ONCE") and not os.path.exists(once):
        open(once, "w").close(); exit(1)
    if not os.path.isfile(os.path.join(release, pattern)):
        exit(1)
    shutil.copy(os.path.join(release, pattern), flag("--output") or flag("--dir")); exit(0)
if verb == "release upload":
    f = [a for a in argv[3:] if not a.startswith("--") and os.path.isfile(a)][-1]
    if os.path.exists(os.path.join(release, os.path.basename(f))):
        err("HTTP 422: already_exists"); exit(1)
    shutil.copy(f, release); exit(0)
if verb == "release delete-asset":
    note("deleted", argv[3])
    if os.path.exists(os.path.join(release, argv[3])):
        os.remove(os.path.join(release, argv[3]))
    exit(0)
exit(1)
"""
PAGE = "test/resources/fixtures/enrichment-us/www.flicks.us/movie/toy-story-5"


class ConvergenceFillTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.program("gh", GH)
        os.makedirs(self.path("release"))

    def fill(self, *args: str, **extra: Optional[str]) -> cli_harness.Done:
        return self.run_tool(*args, env={"STUB_ROOT": self.work, "FIXTURE_RELEASE_TAG": "convergence-fixtures",
                                         "CONVERGENCE_FILL_LIST_BACKOFF": "0", **extra})

    def out(self, *args: str, **extra: Optional[str]) -> str:
        return self.fill(*args, **extra).stdout.strip()

    def lines(self, name: str) -> List[str]:
        return self.read(name).splitlines() if os.path.exists(self.path(name)) else []

    def seed(self) -> None:
        """A packed fill (one page, under the tree's own paths), two fills and two lists for pair 100, others for
        another pair and country."""
        self.write(f"fetched/{PAGE}", "toy story 5\n")
        self.fill("pack", self.path("fetched"), self.path("release/fill-us-100-201.tar.zst"))
        shutil.copy(self.path("release/fill-us-100-201.tar.zst"), self.path("release/fill-us-100-35.tar.zst"))
        self.write("release/refetch-us-100-35.tsv", "k\tGET\thttps://old\n")
        self.write("release/refetch-us-100-201.tsv", "# 2 fetchable gap(s)\na\tGET\thttps://www.flicks.us/movie/a/\n"
                                                     "b\tGET\thttps://www.flicks.us/movie/b/\n")
        for name in ("fill-us-99-300.tar.zst", "fill-uk-100-301.tar.zst", "enrichment-us-100.tar.zst"):
            self.write(f"release/{name}", "")

    def test_pack(self) -> None:
        self.seed()
        self.assertGreater(os.path.getsize(self.path("release/fill-us-100-201.tar.zst")), 0)
        os.makedirs(self.path("empty"))
        self.fill("pack", self.path("empty"), self.path("none.tar.zst"))
        self.assertFalse(os.path.exists(self.path("none.tar.zst")), "a fill that fetched nothing packs no archive")

    def test_fills_the_pairs_own_by_run_attempt_and_name(self) -> None:
        self.seed()
        self.assertEqual("fill-us-100-35.tar.zst fill-us-100-201.tar.zst", self.out("fills", "us", "100"))
        for name in ("fill-us-100-201-convergence", "fill-us-100-201-2-convergence", "fill-us-100-201-1-convergence",
                     "fill-us-100-35-sample", "fill-us-100-201-Bogus"):
            self.write(f"release/{name}.tar.zst", "")
        self.assertEqual("fill-us-100-35-sample.tar.zst fill-us-100-35.tar.zst fill-us-100-201-convergence.tar.zst "
                         "fill-us-100-201.tar.zst fill-us-100-201-1-convergence.tar.zst "
                         "fill-us-100-201-2-convergence.tar.zst", self.out("fills", "us", "100"))

    def test_a_release_that_cannot_be_listed_fails_loudly_after_three_asks(self) -> None:
        # A listing that FAILED is not an empty release: read as "no fills", a leg would replay a tree its pair lacks
        self.seed()
        done = self.fill("fills", "us", "100", STUB_UNREACHABLE="1")
        self.assertEqual((1, ""), (done.code, done.stdout))
        self.assertIn("::error::could not list release convergence-fixtures", done.stderr)
        self.assertEqual(3, len(self.lines("views")))
        self.assertEqual(1, self.fill("gaps", "us", "100", self.path("unreachable.tsv"), STUB_UNREACHABLE="1").code)

    def test_a_hung_listing_is_cut_off(self) -> None:
        self.seed()
        hung = self.write("hung", "")
        done = self.fill("fills", "us", "100", STUB_HANGS=hung, CONVERGENCE_FILL_GH_TIMEOUT="1")
        self.assertEqual((1, 0), (done.code, cli_harness.still_running(hung)))

    def test_one_failed_listing_is_asked_again_and_no_fills_is_none(self) -> None:
        self.seed()
        done = self.fill("fills", "us", "100", STUB_FAILS_ONCE="1")
        self.assertEqual((0, "fill-us-100-35.tar.zst fill-us-100-201.tar.zst"), (done.code, done.stdout.strip()))
        self.assertEqual(2, len(self.lines("views")))
        # every leg's setup asks, under `bash -e`, before any fill exists (run 37608599525)
        done = self.fill("fills", "es", "100")
        self.assertEqual((0, ""), (done.code, done.stdout.strip()))

    def test_unpack(self) -> None:
        self.seed()
        self.assertEqual(0, self.fill("unpack", self.path("stage"), "fill-us-100-201.tar.zst").code)
        self.assertEqual("toy story 5\n", self.read(f"stage/{PAGE}"))
        os.remove(self.path("downloads"))
        done = self.fill("unpack", self.path("stage2"), "fill-us-100-gone.tar.zst", "fill-us-100-201.tar.zst")
        self.assertEqual(1, done.code, "a fill that cannot be restored fails the unpack")
        self.assertIn("::error::fill fill-us-100-gone.tar.zst could not be restored", done.output)
        self.assertEqual(3, self.lines("downloads").count("fill-us-100-gone.tar.zst"))
        self.assertTrue(os.path.isfile(self.path(f"stage2/{PAGE}")), "and still laid the ones it could")
        done = self.fill("unpack", self.path("stage3"), "fill-us-100-201.tar.zst", STUB_DOWNLOAD_FAILS_ONCE="1")
        self.assertEqual(0, done.code, done.output)

    def test_gaps(self) -> None:
        self.seed()
        self.assertEqual("2", self.out("gaps", "us", "100", self.path("gaps.tsv")))
        self.assertIn("movie/b/", self.read("gaps.tsv"))
        self.assertEqual("2", self.out("gaps", "us", "100", self.path("blip.tsv"), STUB_DOWNLOAD_FAILS_ONCE="1"))
        self.assertEqual("0", self.out("gaps", "es", "100", self.path("none.tsv")))
        self.write("release/refetch-us-100-201-2.tsv", "# 1\nc\tGET\thttps://www.flicks.us/movie/c/\n")
        self.fill("gaps", "us", "100", self.path("rerun.tsv"))
        self.assertIn("movie/c/", self.read("rerun.tsv"), "a re-run's list is newer than its first attempt's")

    def test_gaps_an_origin_refused_lately_are_skipped(self) -> None:
        self.seed()
        self.write("release/refused-us-100-200-1.tsv", "# 1 refused gap(s)\nb\tGET\thttps://www.flicks.us/movie/b/\n")
        self.assertEqual("1", self.out("gaps", "us", "100", self.path("skipped.tsv")))
        self.assertIn("movie/a/", self.read("skipped.tsv"))
        self.assertNotIn("movie/b/", self.read("skipped.tsv"))
        for run in (202, 203, 204, 205):
            self.write(f"release/refused-us-100-{run}-1.tsv", "# 0 refused gap(s)\n")
        self.assertEqual("2", self.out("gaps", "us", "100", self.path("again.tsv")))

    # ── fold: a pinned pair's fills stay bounded ───────────────────────────────────────────────

    def mkfill(self, run: int, page: str, body: str) -> None:
        shutil.rmtree(self.path("f"), ignore_errors=True)
        self.write(f"f/enrichment-de/{page}", body + "\n")
        self.fill("pack", self.path("f"), self.path(f"release/fill-de-7-{run}-1-convergence.tar.zst"))

    def fold(self, **extra: Optional[str]) -> int:
        return self.fill("fold", "de", "7", CONVERGENCE_FILL_FOLD_AT="3", **extra).code

    def test_fold(self) -> None:
        for n in (11, 12, 13):
            self.mkfill(n, f"page-{n}", f"body {n}")
        self.assertEqual(0, self.fold())
        self.assertEqual("fill-de-7-11-1-convergence.tar.zst fill-de-7-12-1-convergence.tar.zst "
                         "fill-de-7-13-1-convergence.tar.zst", self.out("fills", "de", "7"), "not past the threshold")
        self.mkfill(14, "page-11", "body 14")
        self.assertEqual(0, self.fold())
        self.assertEqual("fill-de-7-14-1-folded.tar.zst", self.out("fills", "de", "7"))
        self.fill("unpack", self.path("folded"), "fill-de-7-14-1-folded.tar.zst")
        self.assertEqual(["body 14", "body 12", "body 13"],
                         [self.read(f"folded/enrichment-de/page-{n}").strip() for n in (11, 12, 13)])
        self.assertEqual([], self.lines("deleted"), "a leg that listed them before the fold still finds each")
        self.mkfill(15, "page-15", "body 15")
        self.assertEqual("fill-de-7-14-1-folded.tar.zst fill-de-7-15-1-convergence.tar.zst", self.out("fills", "de", "7"))
        for n in (16, 17):
            self.mkfill(n, f"page-{n}", f"body {n}")
        self.fold()
        self.assertEqual("fill-de-7-17-1-folded.tar.zst", self.out("fills", "de", "7"))
        self.fill("unpack", self.path("folded2"), "fill-de-7-17-1-folded.tar.zst")
        self.assertEqual(["body 14", "body 12", "body 15", "body 17"],
                         [self.read(f"folded2/enrichment-de/page-{n}").strip() for n in (11, 12, 15, 17)])
        self.assertEqual([f"fill-de-7-{n}-1-convergence.tar.zst" for n in (11, 12, 13, 14)], self.lines("deleted"))
        self.assertEqual(["fill-de-7-14-1-folded.tar.zst", "fill-de-7-15-1-convergence.tar.zst",
                          "fill-de-7-16-1-convergence.tar.zst", "fill-de-7-17-1-convergence.tar.zst",
                          "fill-de-7-17-1-folded.tar.zst"],
                         sorted((n for n in os.listdir(self.path("release")) if n.startswith("fill-de-7-")),
                                key=lambda n: (int(n.split("-")[3]), int(n.split("-")[4].split(".")[0]), n)))
        for n in (18, 19, 20, 21):
            self.mkfill(n, f"page-{n}", f"body {n}")
        self.assertEqual((1, 4), (self.fold(STUB_UNREACHABLE="1"), len(self.lines("deleted"))),
                         "a fold of a release that cannot be listed fails, and deletes nothing")


if __name__ == "__main__":
    unittest.main()
