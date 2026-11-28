"""WHETHER scripts/js-quality-report CAN RUN HERE (scripts/lib; stdlib only): node on PATH, and its pinned tools either
installed already or installable because npm is here. run-script-tests' --linux container has node but no npm, so the
tests that run the real report (the end-to-end leg, the design rules' JS extractor) skip there and run on the Mac."""
from __future__ import annotations

import os
import shutil
from typing import Mapping

import tool_process


def ready(tool: str, runner: tool_process.Runner, env: Mapping[str, str]) -> bool:
    """Whether `tool` (the js-quality-report script) has node, and its tools installed or npm to install them."""
    if shutil.which("node", path=env.get("PATH")) is None:
        return False
    done = runner.capture(tool_process.Command([tool, "--tools-dir"], env))
    if done.code != 0:
        raise RuntimeError(f"{tool} --tools-dir: exit {done.code}: {done.stderr.strip()}")
    tools_dir = done.stdout.strip()
    installed = os.access(os.path.join(tools_dir, "node_modules", ".bin", "eslint"), os.X_OK)
    return installed or shutil.which("npm", path=env.get("PATH")) is not None
