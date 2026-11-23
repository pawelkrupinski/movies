#!/usr/bin/env python3
"""THE LANDING CHECK'S SHARES (.github/workflows/precommit-check.yml): how one job of the check calls the pre-push hook,
and what the hook would run there -- so a job sets up sbt, Node and the tools only when its share has a check to run.

  precommit-share.sh env <share>     the job's hook environment, KEY=VALUE lines for $GITHUB_ENV
  precommit-share.sh checks <share>  the checks the hook runs for <share>, space-separated (none: an empty line)

<share> (SHARES): `rest` (every selected check but the other shares', Main's heavy rows among them: no job of their own,
which waited up to 17 min for a slot of its own, 2026-11-18), `identity`, or `plan` (every check of the jobs after it:
the plan job's). The mode is the branch's: check/fast-<id> is agentctl's batch check (--fast-only: the sbt check and the
touched files' linters, all in `rest`), check/verify-<full base sha>-<id> a change's combined-tree verification
(--verify-only, all in `rest` too; its range is `<that sha>..HEAD`, what landed since and the change together),
check/verifyareas-<full base sha>-<id> the same verification plus the area tests (agentctl's MODULE OVERLAP, the owner
2026-11-18: the area shares -- `identity` -- run the change's own checks, --checks-only, over that same range), every
other branch the change's own checks (--checks-only); a red- after check/ (a gate-red fix's: check/red-fast-<id>,
check/red-<id>) changes nothing here. The range is otherwise the change since it left main (`<merge-base with
origin/main>..HEAD`); origin/main is fetched before.

`checks` is the hook's own selection (PREPUSH_LIST=1, scripts/hooks/pre-push), under the very environment `env` gives
the hook step, so a job that sets nothing up for an empty share runs exactly what it ran when it always set up: and its
hook step still runs, so a check this missed fails there for want of sbt rather than going unchecked. The workflow runs
it by the name scripts/ci/precommit-share.sh; tested by precommit_share_test.py beside it (LandingCheckSplitSpec reads
SHARES and the rest's environment here). Standard library only: it runs beside the hook it lists, in whatever tree
holds the two (a test's scratch copy among them)."""
from __future__ import annotations

import os
import subprocess
import sys
from typing import Callable, List, Mapping, Optional, Sequence, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))

SHARES = ("rest", "identity", "plan")
CHECKS_ONLY = "--checks-only"
HOOK = os.path.join(os.path.dirname(HERE), "hooks", "pre-push")
USAGE = "usage: precommit-share.sh env|checks rest|identity|plan"


class Refused(Exception):
    def __init__(self, text: str, code: int) -> None:
        super().__init__(text)
        self.code = code


def mode_of(ref: str) -> Tuple[str, bool]:
    """The hook's mode the branch names, and whether its area shares run (a verifyareas- round)."""
    for prefix, mode, areas in (("fast-", "--fast-only", False), ("verify-", "--verify-only", False),
                                ("verifyareas-", "--verify-only", True)):
        if ref.startswith(prefix):
            return mode, areas
    return CHECKS_ONLY, False


def share_vars(share: str, mode: str, area_mode: str, range_: str) -> List[Tuple[str, str]]:
    """The job's hook environment."""
    found = [("PREPUSH_MODE", area_mode), ("PREPUSH_RANGE", range_)]
    if share == "rest":
        elsewhere = "identity" if mode == CHECKS_ONLY else ""
        return found + [("PREPUSH_HEAVY_ROWS", "1"), ("PREPUSH_ELSEWHERE", elsewhere)]
    return found if share == "plan" else found + [("PREPUSH_ONLY", share)]


Run = Callable[[Sequence[str], Mapping[str, str]], "subprocess.CompletedProcess[str]"]


def run(argv: Sequence[str], env: Mapping[str, str]) -> "subprocess.CompletedProcess[str]":
    """A command's stdout captured, its stderr passed on."""
    return subprocess.run(list(argv), env=dict(env), stdout=subprocess.PIPE, text=True)


class Share:
    def __init__(self, runner: Run, env: Mapping[str, str]) -> None:
        self.runner = runner
        self.env = env

    def git(self, *args: str) -> Optional[str]:
        done = self.runner(["git", *args], self.env)
        return done.stdout if done.returncode == 0 else None

    def ref(self) -> str:
        ref = self.env.get("GITHUB_REF_NAME") or (self.git("rev-parse", "--abbrev-ref", "HEAD") or "").strip()
        # a gate-red fix's check is check/red-<the rest>: the prefix names the runner lane, not the mode
        ref = ref[len("check/"):] if ref.startswith("check/") else ref
        return ref[len("red-"):] if ref.startswith("red-") else ref

    def range_of(self, ref: str, mode: str) -> str:
        if mode != "--verify-only":
            base = self.git("merge-base", "HEAD", "origin/main")
            if base is None:
                raise Refused("precommit-share.sh: no merge-base of HEAD and origin/main", 1)
            return f"{base.strip()}..HEAD"
        full = ref[len("verifyareas-"):] if ref.startswith("verifyareas-") else ref[len("verify-"):]
        full = full[:40]
        if self.git("rev-parse", "--verify", "-q", f"{full}^{{commit}}") is None:
            raise Refused(f"precommit-share.sh: {ref} names no commit ({full})", 1)
        return f"{full}..HEAD"

    def listed(self, variables: Sequence[Tuple[str, str]], mode: str, range_: str) -> List[str]:
        """The hook's own selection (PREPUSH_LIST=1) under `variables`."""
        env = {**self.env, **dict(variables), "PREPUSH_LIST": "1"}
        done = self.runner([HOOK, mode, range_], env)
        if done.returncode != 0:
            raise Refused("", done.returncode)
        return [line for line in done.stdout.split("\n") if line]

    def answer(self, what: str, share: str) -> str:
        ref = self.ref()
        mode, areas = mode_of(ref)
        range_ = self.range_of(ref, mode)
        # the area shares of a verifyareas- round: the change's own checks of the area, over the verification's range
        area_mode = CHECKS_ONLY if areas and share == "identity" else mode
        variables = share_vars(share, mode, area_mode, range_)
        if what == "env":
            return "".join(f"{k}={v}\n" for k, v in variables)
        # the fast-only and verify-only sets are the sbt check and the linters, all the rest's: nothing for another
        # share (a verifyareas- round's identity share aside: the area's own checks)
        if area_mode != CHECKS_ONLY and share not in ("rest", "plan"):
            return "\n"
        checks = self.listed(variables, area_mode, range_)
        if areas and share == "plan":  # the plan names the area the identity share runs
            checks += self.listed(share_vars("identity", mode, CHECKS_ONLY, range_), CHECKS_ONLY, range_)
        return " ".join(checks) + "\n"


def refusal(argv: Sequence[str]) -> Optional[str]:
    if len(argv) != 2 or argv[0] not in ("env", "checks") or argv[1] not in SHARES:
        return USAGE
    return None


def main(argv: Sequence[str], env: Mapping[str, str]) -> int:
    usage = refusal(argv)
    if usage:
        print(usage, file=sys.stderr)
        return 2
    try:
        sys.stdout.write(Share(run, env).answer(argv[0], argv[1]))
    except Refused as why:
        if str(why):
            print(why, file=sys.stderr)
        return why.code
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(os.environ)))
