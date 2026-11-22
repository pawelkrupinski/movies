"""A FUNCTION OF A SHELL LIBRARY NOT MIGRATED YET, called from a Python tool (scripts/lib): the library sourced in a
bash of its own and the function run with the arguments given, its status the call's -- so a Python tool keeps the one
implementation the shell tools still share (tool-conventions.sh's gh_download, identity-tools.sh's measure-branch push
and its machine-wide lock) until that library moves too. Its stdout is the answer; its stderr is passed on.

  shell_function(runner, env, "scripts/tool-conventions.sh", "gh_download", [...]) -> Captured
  shell_function(..., then='echo "$MEASURE_PUSH_ERROR"')  a line run after the function, the function's status kept"""
from __future__ import annotations

import os
import sys
from typing import Mapping, Sequence

import tool_process

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


def shell_function(runner: tool_process.Runner, env: Mapping[str, str], library: str, function: str,
                   args: Sequence[str], then: str = "") -> tool_process.Captured:
    script = f'. "$1" || exit 2; shift; {function} "$@"; rc=$?; {then or ":"}; exit "$rc"'
    done = runner.capture(tool_process.Command(["bash", "-c", script, function, os.path.join(ROOT, library), *args],
                                               env))
    sys.stderr.write(done.stderr)
    sys.stderr.flush()
    return done
