"""HOW A TOOL LAUNCHES A PYTHON CHILD (scripts/lib; stdlib only): isolated and writing no bytecode.

  isolated(script, *args) -> ["python3", "-I", "-B", script, *args]    `python=` names another interpreter
                                                                        (sys.executable)

-I keeps the caller's environment and the user's site-packages out of the child, but it implies -E, which drops
PYTHONDONTWRITEBYTECODE: a child run with -I alone writes the __pycache__ of every module of the tree it imports into
the checkout (identity-measure's scripts/ci/__pycache__, 3331008cd; run-script-tests' tree check fails it as POLLUTE).
-B puts that back. tool_python_test.py refuses, anywhere in scripts/, a shell `python3 -I` without -B and a Python tool
spelling "-I" itself."""
from __future__ import annotations

from typing import List

FLAGS = ("-I", "-B")


def isolated(*argv: str, python: str = "python3") -> List[str]:
    return [python, *FLAGS, *argv]
