"""The `uses: <action>` steps of the workflows and composite actions, each with its `with:` block, for the lint tests
that hold every use of an action to one rule (upload_artifact_overwrite_test.py, download_artifact_layout_test.py)."""

import glob
import os
import re

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
FILES = sorted(glob.glob(os.path.join(ROOT, ".github/workflows/*.yml"))
               + glob.glob(os.path.join(ROOT, ".github/actions/*/action.yml")))


def action_steps(text, action):
    """(line number, the step's `with:` block) for each `uses: <action>@...` in text."""
    lines = text.split("\n")
    for i, line in enumerate(lines):
        if line.lstrip().startswith("#") or not re.search(r"uses:\s*" + re.escape(action) + "@", line):
            continue
        j = i + 1
        while j < len(lines) and not re.match(r"\s*with:\s*$", lines[j]):
            j += 1
        if j == len(lines):
            yield i + 1, ""
            continue
        w_indent = len(re.match(r"\s*", lines[j]).group())
        block = []
        j += 1
        while j < len(lines) and (not lines[j].strip() or len(re.match(r"\s*", lines[j]).group()) > w_indent):
            block.append(lines[j])
            j += 1
        yield i + 1, "\n".join(block)
