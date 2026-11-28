#!/usr/bin/env python3
""".github/scripts/restore-enrichment-tree.sh, black-box, against a stub `gh` whose release holds, or lacks, the tree.
Ported from restore-enrichment-tree-test.sh; the same cases hold the shell tool and its Python rewrite
(cli_harness.tool_path)."""
from __future__ import annotations

import os
import shutil
import subprocess
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))),
                                "scripts", "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path(".github/scripts/restore_enrichment_tree.py", ".github/scripts/restore-enrichment-tree.sh")
SEARCH = "test/resources/fixtures/enrichment-uk/api.themoviedb.org/search"
MOVIE = "test/resources/fixtures/enrichment-uk/www.flicks.co.uk/movie"

# `release download` hands over $STUB_ASSET when set, as a release holding it would, and fails as an unreachable
# release does when $STUB_UNREACHABLE is set; any other call is logged to $STUB_OTHER, since the release is the tree's
# only source.
GH = r"""#!/usr/bin/env bash
if [ "$1 $2" = "release download" ]; then
  [ -z "${STUB_UNREACHABLE:-}" ] || { echo "HTTP 502: Bad Gateway" >&2; exit 1; }
  [ ! -f "${STUB_BLIP:-/nonexistent}" ] || { rm -f "$STUB_BLIP"; echo "HTTP 500: Internal Server Error" >&2; exit 1; }
  [ -n "${STUB_ASSET:-}" ] || { echo "no assets match the file pattern" >&2; exit 1; }
  # STUB_ARCHIVE_ONLY: only the archive release (identity-recordings) holds it, as for a landing recording aged out
  [ -z "${STUB_ARCHIVE_ONLY:-}" ] || [ "$3" = identity-recordings ] || { echo "no assets match the file pattern" >&2; exit 1; }
  while [ "$#" -gt 0 ]; do case "$1" in --dir) dir="$2" ;; --pattern) pattern="$2" ;; esac; shift; done
  case "$pattern" in fill-*) cp "$STUB_FILL" "$dir/$pattern"; exit 0 ;; esac
  cp "$STUB_ASSET" "$dir/"
  exit 0
fi
echo "$*" >> "$STUB_OTHER"
[ "$1 $2" = "run list" ] && { echo 12345; exit 0; }
exit 1
"""


@unittest.skipUnless(shutil.which("zstd"), "zstd packs the trees")
class RestoreEnrichmentTreeTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        os.chmod(self.write(".fakes/bin/gh", GH), 0o755)
        self.write(f"src/{SEARCH}", "recorded\n")
        self.tree = self.zstd_tar("src", "enrichment-uk.tar.zst")
        self.write(f"fill/{MOVIE}", "filled\n")
        self.fill = self.zstd_tar("fill", "fill-uk-1-2.tar.zst")
        self.broken = self.write("enrichment-uk-broken.tar.zst", "not an archive")
        self.out = ""

    def zstd_tar(self, directory: str, name: str) -> str:
        """`directory`'s test/ tree as a zstd tarball named `name`."""
        tar = subprocess.run(["tar", "-cf", "-", "test"], cwd=self.path(directory), check=True, capture_output=True)
        with open(self.path(name), "wb") as f:
            f.write(subprocess.run(["zstd", "-q", "-c"], input=tar.stdout, check=True, capture_output=True).stdout)
        return self.path(name)

    def restore(self, label: str, mode: str, asset: str = "", **env: str) -> int:
        """The tool restoring uk's tree into stage-<label>; its exit status, its output in self.out."""
        done = self.run_tool("uk", mode, self.path(f"stage-{label}"), env={
            "STUB_ASSET": asset, "STUB_OTHER": self.path("other-calls"), "FIXTURE_RELEASE_TAG": "convergence-fixtures",
            "GH_DOWNLOAD_BACKOFF": "0", **env})
        self.out = done.output
        return done.code

    def staged(self, label: str, relative: str) -> str:
        path = self.path(f"stage-{label}", relative)
        if not os.path.exists(path):
            return ""
        with open(path) as f:
            return f.read().strip()

    def test_a_release_holding_the_tree_stages_it_under_its_own_paths(self) -> None:
        self.assertEqual(0, self.restore("pinned", "hermetic", self.tree), self.out)
        self.assertEqual("recorded", self.staged("pinned", SEARCH))
        self.assertIn("enrichment tree enrichment-uk.tar.* from release convergence-fixtures", self.out)

    def test_a_replays_tree_the_rolling_release_no_longer_holds_comes_from_the_archive_release(self) -> None:
        self.assertEqual(0, self.restore("archived", "hermetic", self.tree, STUB_ARCHIVE_ONLY="1"), self.out)
        self.assertIn("from release identity-recordings", self.out)

    def test_a_recordings_working_tree_never_comes_from_the_archive_release(self) -> None:
        self.assertEqual(0, self.restore("archived-record", "record", self.tree, STUB_ARCHIVE_ONLY="1"), self.out)
        self.assertNotIn("identity-recordings", self.out)

    def test_the_tree_asset_the_pair_names_is_the_one_downloaded(self) -> None:
        self.assertEqual(0, self.restore("named", "hermetic", self.tree,
                                         KINOWO_CONVERGENCE_TREE_ASSET="enrichment-uk-pinned.tar.zst"), self.out)
        self.assertIn("enrichment tree enrichment-uk-pinned.tar.zst from release", self.out)

    def test_a_hermetic_legs_pair_includes_the_fills_published_over_its_tree(self) -> None:
        self.assertEqual(0, self.restore("filled", "hermetic", self.tree, STUB_FILL=self.fill,
                                         KINOWO_CONVERGENCE_FILL_ASSETS="fill-uk-1-2.tar.zst"), self.out)
        self.assertEqual(("filled", "recorded"), (self.staged("filled", MOVIE), self.staged("filled", SEARCH)))

    def test_a_fill_the_pair_names_but_it_cannot_restore_fails_the_restore(self) -> None:
        status = self.restore("unfilled", "hermetic", self.tree, STUB_FILL=self.path("fill-uk-gone.tar.zst"),
                              KINOWO_CONVERGENCE_FILL_ASSETS="fill-uk-1-2.tar.zst", CONVERGENCE_FILL_LIST_BACKOFF="0")
        self.assertNotIn(status, (0, 3), self.out)
        self.assertIn("::error::fill fill-uk-1-2.tar.zst", self.out)

    def test_a_recording_never_lays_fills_over_its_tree(self) -> None:
        self.restore("recording-fill", "record", self.tree, STUB_FILL=self.fill,
                     KINOWO_CONVERGENCE_FILL_ASSETS="fill-uk-1-2.tar.zst")
        self.assertEqual("", self.staged("recording-fill", MOVIE))

    def test_a_replay_leg_whose_pinned_tree_is_gone_fails_distinctly_saying_why(self) -> None:
        self.assertEqual(3, self.restore("unpinned", "hermetic"), self.out)
        self.assertIn("a hermetic leg replays nothing else", self.out)

    def test_a_recording_with_no_tree_anywhere_goes_on_to_fetch_live_having_staged_nothing(self) -> None:
        self.assertEqual(0, self.restore("cold", "record"), self.out)
        self.assertEqual([], [f for _, _, files in os.walk(self.path("stage-cold")) for f in files])
        self.assertIn("no enrichment capture found for uk", self.out)
        # asked nothing but the release (no run artifacts publish a tree any more)
        self.assertFalse(os.path.exists(self.path("other-calls")))

    def test_a_recording_whose_release_cannot_be_read_fails_rather_than_fetching_everything_live(self) -> None:
        self.assertEqual(4, self.restore("unreachable", "record", STUB_UNREACHABLE="1"))
        self.assertIn("HTTP 502", self.out)

    def test_a_release_read_that_fails_once_is_asked_again(self) -> None:
        blip = self.write("blip", "")
        self.assertEqual(0, self.restore("blip", "hermetic", self.tree, STUB_BLIP=blip), self.out)
        self.assertEqual("recorded", self.staged("blip", SEARCH))

    def test_an_archive_that_will_not_unpack_fails_and_not_as_a_missing_pin(self) -> None:
        self.assertNotIn(self.restore("broken", "record", self.broken), (0, 3), self.out)

    def test_a_missing_argument_or_release_tag_is_refused(self) -> None:
        self.assertNotEqual(0, self.run_tool("uk", "hermetic", env={"FIXTURE_RELEASE_TAG": "x"}).code)
        done = self.run_tool("uk", "hermetic", self.path("stage-x"), env={"FIXTURE_RELEASE_TAG": ""})
        self.assertNotEqual(0, done.code)
        self.assertIn("FIXTURE_RELEASE_TAG", done.output)


if __name__ == "__main__":
    unittest.main()
