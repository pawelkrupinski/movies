"""A COMMIT MEASURED ON CI, the steps the identity tools share (scripts/lib; identity-tools.sh's identity_measure_branch,
identity_main_ci_over and identity_push_measure_branch, and the retired identity_dispatch_measure and
identity_record_dispatch, whose rules these are): the commit pushed to a fresh `measure/<user>/<short sha>[-<tag>]-<random>` branch (the pre-push hook checks
nothing on a measure/ ref, and the run deletes it as its last job), a workflow dispatched on it, its run found by the
pushed commit and recorded as this machine's (agentctl dispatch record: a branch red reaches its owner).

  MeasureCi(runner, env, git, dispatcher)
    .branch(commit, tag)                 the branch name
    .main_ci_over(remote, commit, prefix) a child of <commit> whose tree has <remote>/main's .github/ and scripts/ci/
                                         laid over it, for a commit whose workflows predate a dispatch input; None when
                                         git fails
    .main_ci_fetched_over(remote, commit, prefix) the same after fetching <remote>/main; raises DispatchFailed
    .push(remote, commit, branch)        None pushed, else the remote's reason; one measure/ push of this machine at a
                                         time (identity-tools.sh's lock, which the shell tools share: kept there)
    .dispatch(workflow, branch, push, purpose, args) the run id, or raises DispatchFailed saying why"""
from __future__ import annotations

import pwd
import os
import re
import sys
import tempfile
from typing import Mapping, Optional, Sequence

from vcs import tool_dispatch
import tool_git
import tool_process
from processes import tool_shell

IDENTITY_TOOLS = "scripts/identity-tools.sh"


class DispatchFailed(Exception):
    """Why a dispatch has no run; a message starting `dispatched` means the run may exist (its branch is its own)."""


class MeasureCi:
    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str], git: tool_git.Git,
                 dispatcher: tool_dispatch.Dispatcher) -> None:
        self.runner = runner
        self.env = env
        self.git = git
        self.dispatcher = dispatcher

    @staticmethod
    def branch(commit: str, tag: str = "") -> str:
        user = re.sub(r"[^A-Za-z0-9._-]", "-", pwd.getpwuid(os.getuid()).pw_name)
        return f"measure/{user}/{commit[:9]}{'-' + tag if tag else ''}-{os.urandom(3).hex()}"

    def main_ci_over(self, remote: str, commit: str, prefix: str) -> Optional[str]:
        handle, index = tempfile.mkstemp()
        os.close(handle)
        try:
            env = {**self.env, "GIT_INDEX_FILE": index}
            git = tool_git.Git(self.runner, env, self.git.root)
            steps = (("read-tree", commit), ("rm", "-rqf", "--cached", "--ignore-unmatch", ".github", "scripts/ci"),
                     ("read-tree", "--prefix=.github/", f"{remote}/main:.github"),
                     ("read-tree", "--prefix=scripts/ci/", f"{remote}/main:scripts/ci"))
            if not all(git.ok(*step, quiet=True) for step in steps):
                return None
            tree = git.out("write-tree", quiet=True)
            message = f"{prefix}: {remote}/main's CI over {commit[:9]}"
            out = tree and git.out("commit-tree", tree.strip(), "-p", commit, "-m", message, quiet=True)
            return out.strip() if out else None
        finally:
            os.remove(index)

    def main_ci_fetched_over(self, remote: str, commit: str, prefix: str) -> str:
        """main_ci_over after fetching <remote>/main; raises DispatchFailed saying which step failed."""
        if not self.git.ok("fetch", "-q", remote, "main", timeout=120, quiet=True):
            raise DispatchFailed(f"could not fetch {remote} main")
        push = self.main_ci_over(remote, commit, prefix)
        if push is None:
            raise DispatchFailed(f"could not lay main's CI over {commit}")
        return push

    def push(self, remote: str, commit: str, branch: str) -> Optional[str]:
        done = tool_shell.shell_function(self.runner, self.env, IDENTITY_TOOLS, "identity_push_measure_branch",
                                         [remote, commit, branch], then='echo "$MEASURE_PUSH_ERROR"')
        return None if done.code == 0 else done.stdout.strip()

    def dispatch(self, workflow: str, branch: str, push: str, purpose: str, args: Sequence[str]) -> str:
        since = self.dispatcher.utc_stamp()
        if not self.dispatcher.dispatch(workflow, branch, args):
            raise DispatchFailed(f"could not dispatch {workflow} on {branch}")
        run = self.dispatcher.find_run(workflow, since, f'.headSha == "{push}"', ["--branch", branch])
        if run is None:
            raise DispatchFailed(f"dispatched, but no run of {push[:9]} on {branch} was found (the run deletes "
                                 f"{branch} when it ends)")
        self.record(workflow, branch, run, purpose)
        return run

    def record(self, workflow: str, branch: str, run: str, purpose: str) -> None:
        """Best effort, said when not. (A run of 2026-10-29 went unrecorded under a 30 s bound and showed as a branch
        red of NOBODY: 90 s, and said.)"""
        agentctl = os.path.join(self.env.get("HOME", ""), "bin", "agentctl")
        if not os.access(agentctl, os.X_OK):
            return
        name = workflow_name(os.path.join(tool_shell.ROOT, ".github", "workflows", workflow)) or workflow
        done = self.runner.capture(tool_process.Command(
            [agentctl, "dispatch", "record", "--branch", branch, "--run", run, "--workflow", name, "--purpose", purpose],
            self.env, timeout=90))
        if done.code != 0:
            print(f"[{self.env.get('IDENTITY_TOOL') or 'identity'}] could not record run {run}'s owner (agentctl "
                  "dispatch record): its red, if any, reaches nobody", file=sys.stderr)


def workflow_name(path: str) -> str:
    try:
        with open(path) as f:
            for line in f:
                if line.startswith("name:"):
                    return line[len("name:"):].strip()
    except OSError:
        pass
    return ""
