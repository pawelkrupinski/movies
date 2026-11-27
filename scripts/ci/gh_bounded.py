"""GH, BOUNDED, for the CI tools a sparse checkout runs (scripts/ci, stdlib only: those jobs check out no scripts/lib,
where tool_github is): `$KINOWO_GH` (default `gh`) with its arguments, its stdout -- a gh that fails, or that is still
running after `timeout` seconds, raises CalledProcessError (124 for the time limit, its stderr saying so), so a hung API
call or artifact download is a failure the caller already handles, never a job that sits until its own limit.

  gh(["api", "repos/{owner}/{repo}/..."], input=None, cwd=None, timeout=None) -> stdout (None: LIMIT)
  gh([...], capture=False)  -> "" (its output goes to this process's: a download's progress stays visible)"""
from __future__ import annotations

import subprocess
from typing import List, Optional
import ci_machine  # noqa: E402

LIMIT = 300.0  # seconds: a paginated listing or an artifact download, past which gh is hung, not slow
TIMED_OUT = 124  # `timeout`'s and tool_process's code for a command the bound ended


def gh(args: List[str], input: Optional[str] = None, cwd: Optional[str] = None, timeout: Optional[float] = None,
       capture: bool = True) -> str:
    argv = [ci_machine.environment().get("KINOWO_GH") or "gh", *args]
    timeout = LIMIT if timeout is None else timeout
    try:
        done = subprocess.run(argv, input=input, capture_output=capture, text=True, cwd=cwd, timeout=timeout)
    except subprocess.TimeoutExpired as e:
        raise subprocess.CalledProcessError(TIMED_OUT, argv, "", f"gh timed out after {timeout:g}s") from e
    if done.returncode != 0:
        raise subprocess.CalledProcessError(done.returncode, argv, done.stdout, done.stderr)
    return done.stdout or ""
