#!/usr/bin/env python3
"""No `actions/download-artifact` step downloads by `pattern` into a directory per artifact: that layout is not fixed.

download-artifact@v6 extracts a pattern matching ONE artifact straight into its `path`, two or more each into
`path/<artifact name>`, so a reader globbing `path/<name-pattern>/...` finds nothing when one row ran
(2026-11-07: Identity rule ablation #2's collect, run 42377038878, merged `shards/*/ablation/state.json` = no file;
identity-capture's `captures/identity-capture-*` the same for a one-country capture). A pattern download either merges
(`merge-multiple: true`: one flat layout) or goes through ./.github/actions/download-artifacts, whose
`gh run download -p` puts every artifact, the only one too, in a directory of its name."""

import os
import re
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from workflow_steps import FILES, ROOT, action_steps  # noqa: E402


class DownloadArtifactLayout(unittest.TestCase):
    def test_found_some(self):
        self.assertTrue(any(action_steps(open(f).read(), "actions/download-artifact") for f in FILES))

    def test_no_pattern_download_lays_artifacts_out_by_how_many_matched(self):
        varying = []
        for f in FILES:
            for line, block in action_steps(open(f).read(), "actions/download-artifact"):
                if re.search(r"^\s*pattern:", block, re.M) and not re.search(r"^\s*merge-multiple:\s*true\s*$", block, re.M):
                    varying.append("%s:%d" % (os.path.relpath(f, ROOT), line))
        self.assertEqual(varying, [], "download-artifact by pattern into per-artifact directories: one match lands "
                                      "elsewhere -- use ./.github/actions/download-artifacts")


if __name__ == "__main__":
    unittest.main()
