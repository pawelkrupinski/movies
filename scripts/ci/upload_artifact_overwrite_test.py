#!/usr/bin/env python3
"""Every `actions/upload-artifact` step in the workflows and composite actions sets `overwrite: true`.

A `gh run rerun --failed` re-runs a failed job inside the SAME run: its artifacts from the first attempt still exist,
and upload-artifact (v4+) refuses a name that exists unless `overwrite: true`
(2026-10-23: Record scrape fixtures #798's rerun died at leg-verdict with "an artifact named
'convergence-diagnostics-pl-convergence' already exists in this run"). Names stay fixed, so downloaders are unchanged."""

import glob
import os
import re
import unittest

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
FILES = sorted(glob.glob(os.path.join(ROOT, ".github/workflows/*.yml"))
               + glob.glob(os.path.join(ROOT, ".github/actions/*/action.yml")))


def upload_steps(text):
    """(line number, the step's `with:` block) for each upload-artifact use."""
    lines = text.split("\n")
    for i, line in enumerate(lines):
        if line.lstrip().startswith("#") or not re.search(r"uses:\s*actions/upload-artifact@", line):
            continue
        j = i + 1
        while j < len(lines) and not re.match(r"\s*with:\s*$", lines[j]):
            j += 1
        w_indent = len(re.match(r"\s*", lines[j]).group())
        block = []
        j += 1
        while j < len(lines) and (not lines[j].strip() or len(re.match(r"\s*", lines[j]).group()) > w_indent):
            block.append(lines[j])
            j += 1
        yield i + 1, "\n".join(block)


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
