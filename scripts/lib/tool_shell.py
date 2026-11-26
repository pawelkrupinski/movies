"""A FUNCTION OF A SHELL LIBRARY NOT MIGRATED YET, called from a Python tool (scripts/lib): the library sourced in a
bash of its own and the function run with the arguments given, its status the call's -- so a Python tool keeps the one
implementation the shell tools still share (tool-conventions.sh's gh_download, identity-tools.sh's measure-branch push
and its machine-wide lock) until that library moves too. Its stdout is the answer; its stderr is passed on.

  shell_function(runner, env, "scripts/tool-conventions.sh", "gh_download", [...]) -> Captured
  shell_function(..., then='echo "$MEASURE_PUSH_ERROR"')  a line run after the function, the function's status kept
  shell_function(..., prelude='IDENTITY_TOOL=x')  a line run after sourcing, before the function (a variable, a log)
  shell_command(env, library, function, args, log=PATH)  the same call as a Command, for a runner's `run` (its output
                                                         into a log) or a process started in the background
  download_run(runner, env, timeout, run, pattern, directory) -> whether gh_download laid the run's artifacts
                                                                  matching `pattern` into `directory`"""
from __future__ import annotations

import os
from typing import Mapping, Optional, Sequence

import tool_process

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))


def shell_command(env: Mapping[str, str], library: str, function: str, args: Sequence[str], then: str = "",
                  prelude: str = "", log: Optional[str] = None) -> tool_process.Command:
    script = f'. "$1" || exit 2; shift; {prelude or ":"}; {function} "$@"; rc=$?; {then or ":"}; exit "$rc"'
    return tool_process.Command(["bash", "-c", script, function, os.path.join(ROOT, library), *args], env, log=log)


def shell_function(runner: tool_process.Runner, env: Mapping[str, str], library: str, function: str,
                   args: Sequence[str], then: str = "", prelude: str = "") -> tool_process.Captured:
    return tool_process.capture_loudly(runner, shell_command(env, library, function, args, then, prelude))


def download_run(runner: tool_process.Runner, env: Mapping[str, str], timeout: float, run: str, pattern: str,
                 directory: str) -> bool:
    """`gh run download <run> --pattern <pattern> --dir <directory>` through tool-conventions.sh's gh_download
    (bounded, a failed read asked again, absent told from failed)."""
    return shell_function(runner, env, "scripts/tool-conventions.sh", "gh_download", [
        str(int(timeout)), "run", "download", run, "--pattern", pattern, "--dir", directory]).code == 0
