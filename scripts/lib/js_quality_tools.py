"""WHETHER scripts/js-quality-report CAN RUN HERE (scripts/lib; stdlib only): node on PATH, and its pinned tools either
installed already or installable because npm is here. run-script-tests' --linux container has node but no npm, so the
tests that run the real report (the end-to-end leg, the design rules' JS extractor) skip there and run on the Mac."""
from __future__ import annotations

import os
import shutil
import subprocess


def ready(tool: str) -> bool:
    """Whether `tool` (the js-quality-report script) has node, and its tools installed or npm to install them."""
    if shutil.which("node") is None:
        return False
    tools_dir = subprocess.run([tool, "--tools-dir"], capture_output=True, text=True, check=True).stdout.strip()
    return os.access(os.path.join(tools_dir, "node_modules", ".bin", "eslint"), os.X_OK) or shutil.which("npm") is not None
