"""THE COMMITS AND BRANCHES A CI MEASURE RUNS ON (scripts/lib; identity-tools.sh's identity_main_ci_over,
identity_measure_branch and identity_push_measure_branch).

  main_ci_over(env, remote, commit, prefix) -> a commit whose tree is <commit>'s with <remote>/main's .github/ and
        scripts/ci/ laid over it (child of <commit>), for a commit whose own workflows predate a dispatch input.
        <remote>/main must be fetched. Fails, saying nothing, when git does.
  measure_branch(commit, tag="") -> a fresh `measure/<user>/<short sha>[-<tag>]-<random>` branch name: the pre-push hook
        checks nothing on a measure/ ref, and the run it carries deletes it as its last job.
  MeasurePush(ctx, locks).push(remote, commit, branch) -> (git's status, "" or the remote's reason): <commit> pushed as
        refs/heads/<branch> (bounded), while no other measure/ push of this machine is under way; the reason ("remote rejected (unable to
        migrate objects to permanent storage)"). ONE AT A TIME (2026-10-23): identity-landing-check pushes one commit to a
        measure/ branch per spec group and one for its identity-measure, all at once. Concurrent receive-packs of the same
        new objects (100+, so kept as a pack) each write pack-<hash>.keep with their own pid in it; the second to move its
        quarantine in finds the first's .keep "differs in contents" and rejects the push. A push after the first finds the
        commit there and sends no objects."""
from __future__ import annotations

import os
import re
import secrets
import tempfile
from typing import MutableMapping, Optional, Tuple

from processes import machine
from processes import tool_python
from identity_tooling.base.identity_context import Context, Failed
from identity_tooling.base.identity_lock import Locks

PUSH_BOUND = 300
REJECTED = re.compile(r"^ ! \[(remote rejected|rejected)\] .* \((.*)\)$")


def main_ci_over(env: MutableMapping[str, str], remote: str, commit: str, prefix: str) -> Optional[str]:
    handle, index = tempfile.mkstemp()
    os.close(handle)
    try:
        return laid_over({**env, "GIT_INDEX_FILE": index}, remote, commit, prefix)
    finally:
        os.remove(index)


def laid_over(env: MutableMapping[str, str], remote: str, commit: str, prefix: str) -> Optional[str]:
    steps = [["read-tree", commit],
             ["rm", "-rqf", "--cached", "--ignore-unmatch", ".github", "scripts/ci"],
             ["read-tree", "--prefix=.github/", f"{remote}/main:.github"],
             ["read-tree", "--prefix=scripts/ci/", f"{remote}/main:scripts/ci"]]
    for step in steps:
        if machine.status(["git", *step], env=env) != 0:
            return None
    code, tree = machine.captured(["git", "write-tree"], env=env)
    if code != 0:
        return None
    code, out = machine.captured(["git", "commit-tree", tree.strip(), "-p", commit, "-m",
                                  f"{prefix}: {remote}/main's CI over {commit[:9]}"], env=env)
    return out.strip() if code == 0 else None


def measure_branch(commit: str, tag: str = "") -> str:
    _, user = machine.output(["id", "-un"])
    safe = re.sub(r"[^A-Za-z0-9._\n-]", "-", user.strip())
    return f"measure/{safe}/{commit[:9]}{'-' + tag if tag else ''}-{secrets.token_hex(3)}"


def reason(err: str, code: int) -> str:
    """The remote's words for a push that failed: its rejection, else git's last fatal or error line."""
    for line in err.splitlines():
        found = REJECTED.match(line)
        if found:
            return f"{found.group(1)} ({found.group(2)})"
    fatal = [line for line in err.splitlines() if re.match(r"^(fatal|error):", line)]
    return fatal[-1] if fatal else f"git push exited {code}"


class MeasurePush:
    def __init__(self, ctx: Context, locks: Locks) -> None:
        self.ctx, self.locks = ctx, locks

    def push(self, remote: str, commit: str, branch: str) -> Tuple[int, str]:
        """git's status and, when it failed, the remote's reason."""
        try:
            with self.locks.held("measure-push"):
                code, err = self.bounded_push(remote, commit, branch)
        except Failed as failed:
            return failed.code, reason("", failed.code)
        if err:
            self.ctx.say(err.rstrip("\n"))
        return code, "" if code == 0 else reason(err, code)

    def bounded_push(self, remote: str, commit: str, branch: str) -> Tuple[int, str]:
        tool = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "processes", "tool_bounded.py")
        return machine.errors(tool_python.isolated(tool, str(PUSH_BOUND), "git", "push", "-q", remote,
                                                   f"{commit}:refs/heads/{branch}"), env=self.ctx.env, out=2)
