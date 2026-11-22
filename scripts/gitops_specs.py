#!/usr/bin/env python3
"""Run movies' GitOps-reading specs against a movies-gitops tree before that tree reaches movies-gitops main.

  scripts/gitops-specs --plan                  print the sbt commands: the lint registry's `@gitops` rows, per module
  scripts/gitops-specs --manifests <dir>       run them, with <dir> (a movies-gitops tree) as infra/kubernetes

WHY (2026-11-17, Main #2564). The deploy specs read movies-gitops main through infra/bin/fetch-gitops, which tracks
main on purpose (the specs test what is deployed; a pin would go stale with every image-automation commit). So a
gitops push changed what movies main's specs saw with no movies check run: 5ec2eee added Flux's
notification-controller, NodeMemoryBudgetSpec went over its reserve, and main went red on a push to ANOTHER
repository. movies-gitops' pre-push hook (its scripts/hooks/pre-push) now calls this with the tree being pushed, so the
push is refused while it would turn movies main red -- the same local, pre-push shape as movies' own checks (no
pre-merge CI, the owner 2026-10-26).

It runs in its OWN detached worktree of movies at origin/main ($KINOWO_GITOPS_CHECK_WORKTREE, default
~/projects/movies-gitops-check), never the root checkout nor the caller's: the specs it runs are main's, and a gitops
change that needs a movies change lands the movies side first. sbt through scripts/sbt-warm (a JVM slot, released
when it ends). Exit: 0 green, 1 a spec failed (log named), 2 could not run. KINOWO_LINT_REGISTRY names another
registry for --plan (the test's); KINOWO_MOVIES_REPO (default ~/projects/movies) the repository whose origin it
fetches."""
from __future__ import annotations

import os
import re
import shutil
import sys
import tempfile
from typing import Dict, List, Mapping, Optional

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
import tool_process  # noqa: E402

USAGE = "\n".join(__doc__.split("\n")[:4]) if __doc__ else ""
FAILURE = re.compile(r"\*\*\* FAILED \*\*\*|^\[error\]")


class CannotRun(Exception):
    """Could not run (exit 2, or a failed git's own status): the message, if any, said on stderr."""

    def __init__(self, message: str, code: int = 2) -> None:
        super().__init__(message)
        self.code = code


def plan(registry: str) -> List[str]:
    """One `<module>/testOnly <spec> ...` line per module with an `@gitops` row, modules in first-seen order."""
    specs: Dict[str, List[str]] = {}
    with open(registry) as f:
        for line in f:
            fields = line.rstrip("\n").split("\t")
            if line.startswith("#") or len(fields) < 2 or "@gitops" not in fields[1].split(" "):
                continue
            module, _, spec = fields[0].partition(":")
            specs.setdefault(module, []).append(spec.split(":")[0])
    return [f"{module}/testOnly " + " ".join(names) for module, names in specs.items()]


class Check:
    """The tree under test, run through main's specs in the check worktree."""

    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str], movies: str, check: str) -> None:
        self.runner, self.env, self.movies, self.check = runner, dict(env), movies, check

    def git(self, *args: str) -> tool_process.Captured:
        done = self.runner.capture(tool_process.Command(["git", *args], self.env))
        sys.stderr.write(done.stderr)
        return done

    def worktree_at_main(self) -> None:
        if self.git("-C", self.movies, "fetch", "--quiet", "origin", "main").code != 0:
            raise CannotRun("gitops-specs: cannot fetch movies origin/main")
        if not os.path.exists(os.path.join(self.check, ".git")):
            done = self.git("-C", self.movies, "worktree", "add", "--quiet", "--detach", self.check, "origin/main")
        else:
            done = self.git("-C", self.check, "checkout", "--quiet", "--detach", "origin/main")
        if done.code != 0:
            raise CannotRun("", done.code)
        short = self.git("-C", self.check, "rev-parse", "--short", "HEAD").stdout.strip()
        print(f"gitops-specs: movies {short} (origin/main) in {self.check}", flush=True)

    def lay_manifests(self, manifests: str) -> None:
        """The tree under test, where fetch-gitops would have put main's (a copy: the specs never see its .git)."""
        target = os.path.join(self.check, "infra", "kubernetes")
        shutil.rmtree(target, ignore_errors=True)
        shutil.copytree(manifests, target, symlinks=True,
                        ignore=lambda directory, names: [".git"] if directory == manifests else [])

    def run_specs(self) -> int:
        commands = plan(os.path.join(self.check, "scripts", "hooks", "lint-registry.tsv"))
        if not commands:
            raise CannotRun("gitops-specs: the lint registry names no @gitops spec")
        handle, log = tempfile.mkstemp(prefix="gitops-specs.")
        os.close(handle)
        print(f"gitops-specs: {len(commands)} sbt line(s); log {log}", flush=True)
        sbt = os.path.join(self.check, "scripts", "sbt-warm")
        status = self.runner.run(tool_process.Command([sbt, *commands], self.env, log=log, cwd=self.check))
        # The server is stopped afterwards: a gitops push is rare, and a kept one would hold a JVM slot for nobody.
        self.runner.run(tool_process.Command([sbt, "--stop"], self.env, log=log, append=True, cwd=self.check))
        with open(log, errors="replace") as f:
            lines = f.read().split("\n")
        if status == 0:
            print("gitops-specs: GREEN (" + "".join(f"{line} " for line in lines if "Tests: succeeded" in line) + ")")
            return 0
        sys.stderr.write("".join(f"{line}\n" for line in [line for line in lines if FAILURE.search(line)][:20]))
        print(f"gitops-specs: RED -- this tree would turn movies main red; full log: {log}", file=sys.stderr)
        return 1


def manifests_run(manifests: str, env: Mapping[str, str]) -> int:
    if not os.path.isdir(manifests):
        raise CannotRun(f"gitops-specs: no directory {manifests}")
    home = env.get("HOME", "")
    movies = env.get("KINOWO_MOVIES_REPO") or os.path.join(home, "projects", "movies")
    check = Check(tool_process.SubprocessRunner(), env, movies,
                  env.get("KINOWO_GITOPS_CHECK_WORKTREE") or os.path.join(home, "projects", "movies-gitops-check"))
    check.worktree_at_main()
    check.lay_manifests(os.path.realpath(manifests))
    return check.run_specs()


def main(argv: List[str], env: Dict[str, str]) -> int:
    which: Optional[str] = argv[0] if argv else None
    if which == "--plan":
        # allow-fallback: this checkout's own registry; only the --manifests run (and the test) names another
        print("".join(f"{line}\n" for line in plan(
            env.get("KINOWO_LINT_REGISTRY") or os.path.join(os.path.dirname(HERE), "scripts/hooks/lint-registry.tsv"))),
            end="")
        return 0
    if which != "--manifests":
        print(USAGE, file=sys.stderr)
        return 2
    if len(argv) < 2 or not argv[1]:
        print("gitops-specs: --manifests <dir>", file=sys.stderr)
        return 2
    try:
        return manifests_run(argv[1], env)
    except CannotRun as cannot:
        if str(cannot):
            print(cannot, file=sys.stderr)
        return cannot.code


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(os.environ)))
