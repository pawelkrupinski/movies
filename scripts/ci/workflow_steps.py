"""The `uses: <action>` steps of the workflows and composite actions, each with its `with:` block, for the lint tests
that hold every use of an action to one rule (upload_artifact_overwrite_test.py, download_artifact_layout_test.py)."""

import glob
import os
import re

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))


def workflow_files():
    """Every workflow and composite action file of the checkout."""
    return sorted(glob.glob(os.path.join(ROOT, ".github/workflows/*.yml"))
                  + glob.glob(os.path.join(ROOT, ".github/actions/*/action.yml")))


def _indent(line: str) -> int:
    """The width of the whitespace a line starts with."""
    return len(line) - len(line.lstrip())


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
        w_indent = _indent(lines[j])
        block = []
        j += 1
        while j < len(lines) and (not lines[j].strip() or _indent(lines[j]) > w_indent):
            block.append(lines[j])
            j += 1
        yield i + 1, "\n".join(block)
