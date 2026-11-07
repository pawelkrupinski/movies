#!/usr/bin/env python3
"""Every `actions/upload-artifact` step in the workflows and composite actions sets `overwrite: true`.

A `gh run rerun --failed` re-runs a failed job inside the SAME run: its artifacts from the first attempt still exist,
and upload-artifact (v4+) refuses a name that exists unless `overwrite: true`
(2026-10-23: Record scrape fixtures #798's rerun died at leg-verdict with "an artifact named
'convergence-diagnostics-pl-convergence' already exists in this run"). Names stay fixed, so downloaders are unchanged."""

import os
import re
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from workflow_steps import FILES, ROOT, action_steps  # noqa: E402


def upload_steps(text):
    return action_steps(text, "actions/upload-artifact")


class UploadArtifactOverwrite(unittest.TestCase):
    def test_found_some(self):
        self.assertTrue(any(upload_steps(open(f).read()) for f in FILES))

    def test_every_upload_overwrites(self):
        missing = []
        for f in FILES:
            for line, block in upload_steps(open(f).read()):
                if not re.search(r"^\s*overwrite:\s*true\s*$", block, re.M):
                    missing.append("%s:%d" % (os.path.relpath(f, ROOT), line))
        self.assertEqual(missing, [], "upload-artifact without `overwrite: true` (a --failed rerun would fail)")


if __name__ == "__main__":
    unittest.main()
