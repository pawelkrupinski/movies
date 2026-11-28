"""THE PYTHON PACKAGES A SELF-TEST NEEDS (scripts/run-script-tests): a test whose first 40 lines say
`# script-test-requires: <requirements.txt> <module>` runs under a python3 that imports <module> -- CI's own when it
does already, else the pinned venv scripts/python-venv builds once. A package that cannot be had REFUSES the run, naming
the module and the command that installs it: a test that skipped itself for want of numpy passed on the laptop and
failed in CI (round 6, 2026-11-28)."""
from __future__ import annotations

import os
import re
from dataclasses import dataclass
from typing import Callable, Dict, List, Optional, Sequence, Tuple

from script_tests_found import header

REQUIRES = re.compile(r"^#\s*script-test-requires:\s*(\S+)\s+(\S+)\s*$")


@dataclass(frozen=True)
class Requirement:
    requirements: str  # the pinned requirements file, relative to the root
    module: str


def declared_requirement(path: str) -> Optional[Requirement]:
    """What a test's `# script-test-requires:` line (first 40 lines) names; None for a test without one."""
    for line in header(path):
        declared = REQUIRES.match(line)
        if declared:
            return Requirement(declared.group(1), declared.group(2))
    return None


def provision(root: str, tests: Sequence[str], venv_tool: str,
              run: Callable[[Sequence[str]], Tuple[int, str]]) -> Tuple[Dict[str, str], List[str]]:
    """The directory to put first on each test's PATH (`run` is the venv tool's `argv -> (status, stdout)`), and one
    refusal a package that cannot be had."""
    bins: Dict[str, str] = {}
    refusals: List[str] = []
    for path in tests:
        need = declared_requirement(os.path.join(root, path))
        if need is None:
            continue
        lock = os.path.join(root, need.requirements)
        code, out = run([venv_tool, lock, need.module])
        if code == 0 and out.strip():
            bins[path] = out.strip().splitlines()[-1]
        else:
            refusals.append(f"run-script-tests: {path} needs the python module {need.module}, which python3 lacks: "
                            f"install it with `{venv_tool} {need.requirements} {need.module}` "
                            f"(a pinned venv, built once) and run again")
    return bins, refusals
