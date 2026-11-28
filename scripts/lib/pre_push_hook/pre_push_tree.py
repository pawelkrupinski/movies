"""WHERE THE HOOK RUNS (scripts/hooks/pre-push's header; the hook's shell entry hands a push over to the PUSHING
checkout's own copy before it gets here): core.hooksPath kept relative and proved unmoved at exit, the shared root
checkout's state said, and the tree the checks judge -- in place when the checkout is the pushed commit, else a
temporary worktree of it (and the same when a live sbt shares this checkout's target/)."""
from __future__ import annotations

import os
import re
from typing import List, Optional, Tuple

from processes import machine
from pre_push_hook.plan.pre_push_repo import Repo, Say


def real(path: str) -> str:
    return os.path.realpath(path)


def hooks_path_now(repo: Repo) -> str:
    return repo.git_out("config", "--local", "--get", "core.hooksPath").strip()


def hooks_path_is_own(repo: Repo, path: str) -> bool:
    """An absolute core.hooksPath naming a worktree of THIS clone's scripts/hooks."""
    if not repo.files.is_dir(path):
        return False
    worktrees = re.findall(r"(?m)^worktree (.*)$", repo.git_out("worktree", "list", "--porcelain"))
    return any(real(path) == f"{real(w)}/scripts/hooks" for w in worktrees)


def keep_hooks_path_relative(repo: Repo, say: Say) -> str:
    """Sets an absolute own core.hooksPath back to relative; the value the hook starts with."""
    now = hooks_path_now(repo)
    if now.startswith("/") and hooks_path_is_own(repo, now):
        say.warn(f"core.hooksPath was {now}: set back to the relative scripts/hooks")
        repo.git("config", "--local", "core.hooksPath", "scripts/hooks")
    return hooks_path_now(repo)


def root_warning(repo: Repo, say: Say, hook: str) -> None:
    check = f"{os.path.dirname(hook)}/../root-checkout-check"
    if os.access(check, os.X_OK):
        said = machine.captured(["sh", "-c", '"$0" "$1" 2>&1', check, repo.cwd], env=repo.env)[1].rstrip("\n")
        if said:
            say.warn(said)


def in_place(repo: Repo, head: str, changed: List[str]) -> bool:
    """The checkout IS the pushed commit: HEAD is it, and no changed path differs from it or is untracked there."""
    if repo.git_out("rev-parse", "HEAD").strip() != head:
        return False
    for path in changed:
        if repo.git("diff", "--quiet", "HEAD", "--", path)[0] != 0:
            return False
        if repo.git_out("ls-files", "--others", "--exclude-standard", "--", path).strip():
            return False
    return True


def live_sbt(repo: Repo) -> Optional[int]:
    """The first live sbt JVM whose working directory is this checkout (ONE sbt PER target/)."""
    here = real(repo.cwd)
    pids = machine.output(["pgrep", "-f", r"sbt-launch|xsbt\.boot\.Boot"])[1].split()
    for pid in pids:
        listed = machine.output(["lsof", "-a", "-p", pid, "-d", "cwd", "-Fn"])[1]
        cwd = next((line[1:] for line in listed.split("\n") if line.startswith("n")), "")
        if cwd and real(cwd) == here:
            return int(pid)
    return None


def isolation(repo: Repo, say: Say, head: str, changed: List[str], sbt_asked: bool) -> Tuple[str, bool]:
    """(why the checks need a temporary checkout or "", whether sbt runs through the checkout's kept server)."""
    if not in_place(repo, head, changed):
        return f"the working tree is not {head[:9]}", False
    pid = live_sbt(repo) if sbt_asked else None
    if pid is None:
        return "", False
    if "--detach-stdio" in machine.output(["ps", "-o", "args=", "-p", str(pid)])[1]:
        say.log(f"the sbt check runs through this checkout's kept sbt server (pid {pid}), warm")
        return "", True
    return f"a live sbt (pid {pid}) shares this checkout's target/", False


def temporary_tree(repo: Repo, say: Say, tmp: str, head: str, why: str) -> Optional[str]:
    tree = f"{tmp}/tree"
    if repo.git("worktree", "add", "-q", "--detach", tree, head)[0] != 0:
        say.warn(f"cannot check out {head[:9]} to check it")
        return None
    if repo.files.is_dir(f"{repo.cwd}/node_modules"):
        machine.output(["ln", "-s", f"{repo.cwd}/node_modules", f"{tree}/node_modules"])
    say.log(f"{why}: checking a temporary checkout of {head[:9]}")
    return tree


def on_exit(repo: Repo, say: Say, tree: Optional[str], area_tests: bool, at_start: str) -> None:
    """The temporary checkout removed (its kept sbt server stopped first), and core.hooksPath proved unmoved."""
    if tree:
        if area_tests and os.access(f"{tree}/scripts/sbt-warm", os.X_OK):
            machine.output([f"{tree}/scripts/sbt-warm", "--stop"], cwd=tree, env=repo.env)
        repo.git("worktree", "remove", "--force", tree)
        repo.git("worktree", "prune")
    now = hooks_path_now(repo)
    if now == at_start:
        return
    say.raw(f"\033[31m! pre-push\033[0m core.hooksPath moved from \"{at_start}\" to \"{now}\" WHILE this hook ran — "
            "restoring it. Candidates:\n")
    table = machine.output(["ps", "-axo", "pid,ppid,command"])[1].split("\n")
    say.raw("".join(line + "\n" for line in table if re.search(r"git( |-)config|install-hooks|hooksPath", line)
                    and "grep" not in line))
    if at_start:
        repo.git("config", "--local", "core.hooksPath", at_start)
    else:
        repo.git("config", "--local", "--unset", "core.hooksPath")
