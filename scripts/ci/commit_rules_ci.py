#!/usr/bin/env python3
"""THE DRIFT GATE'S COMMIT RULES, ON CI (scripts/ci/commit_rules.py: `allowlists` and `driftguards`). The pre-push hook
runs them on the laptop, but `git push --no-verify` or PREPUSH_SKIP skips the hook, and with it the allow-list ratchet,
the shrink-only known-flips baseline and the design-trailer rule. Here they run over every commit main gained since the
rules last held on main (a superseded or red run's commits stay in the range until a run passes with them), in Main's
CI and in the landing check (precommit-check.yml), whatever paths the push touched: a commit that breaks one turns the
run red.

THE BASE COMES FROM GIT (simplification-2026-10-31 item 5): refs/verified/commit-rules, the commit the rules last held
on (scripts/ci/verified_ref.py: a marker commit the ref points at), which a run on main ADVANCES when they hold (a
leased push of a new marker: a slower, older run never moves it back). It used to be "the newest green Main run on this
forge", which broke three ways in a week: 1,392 commits back after the forge move, a measurement run taken for main's,
and a red Main holding the range open for every other check.

usage: scripts/ci/commit-rules-ci.sh [--base <commit>]
  --base   the range's start (precommit-check: the merge-base with main); default refs/verified/commit-rules, else
           $BEFORE (the push's own, before the ref's first advance); none of them (a dispatch): nothing to judge, said
A run on main ($GITHUB_REF refs/heads/main, a push) whose rules hold advances the ref to HEAD; it failing to is an error
(exit 2), never a silent stale base. A NEW CHECK GATES ONLY ONCE IT HAS BEEN GREEN: the gating-checks registry is held
in step with the gate's workflows here too (scripts/ci/gating_checks.py lint).
Exit: 0 every rule holds, 1 a commit breaks one (each named), 2 the range could not be read. The workflows run it by the
name scripts/ci/commit-rules-ci.sh; tested by commit_rules_ci_test.py beside it."""
from __future__ import annotations

import os
import sys
from typing import List, Mapping, Optional, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(os.path.dirname(HERE), "lib"))
import tool_git  # noqa: E402
import tool_process  # noqa: E402

RULE_SETS = ("allowlists", "driftguards")
ZERO = "0" * 40
REF_NAME = "commit-rules"


class Usage(Exception):
    pass


def base_option(argv: Sequence[str]) -> str:
    base, rest = "", list(argv)
    while rest:
        arg = rest.pop(0)
        if arg != "--base":
            raise Usage(f"commit-rules-ci: unknown argument {arg}")
        if not rest or not rest[0]:
            raise Usage("commit-rules-ci: --base needs a commit")
        base = rest.pop(0)
    return base


class CommitRules:
    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str]) -> None:
        self.runner = runner
        self.env = env
        self.git = tool_git.Git(runner, env, ".")

    def python(self, *args: str) -> int:
        return self.runner.run(tool_process.Command(["python3", *args], self.env))

    def verified_ref(self, what: str) -> tool_process.Captured:
        return self.runner.capture(tool_process.Command(["python3", os.path.join(HERE, "verified_ref.py"), what,
                                                         REF_NAME], self.env))

    def base(self, named: str) -> Optional[str]:
        """The range's start: named, else the verified ref's, else $BEFORE; "" for none; None when unreadable."""
        if named:
            return named
        done = self.verified_ref("base")
        sys.stderr.write(done.stderr)
        if done.code != 0:
            return None
        before = self.env.get("BEFORE") or ZERO
        return done.stdout.strip() or ("" if before == ZERO else before)

    def reachable(self, base: str) -> Optional[int]:
        """A checkout that can read base..HEAD, else the exit; None when it can."""
        # A shallow checkout holds neither the range nor the lists' history the ratchet reads: the commits, not their
        # blobs (fetched as read), once.
        if (self.git.out("rev-parse", "--is-shallow-repository", quiet=True) or "").strip() == "true" and not self.git.ok(
                "fetch", "--quiet", "--no-tags", "--unshallow", "--filter=blob:none", "origin", timeout=300):
            print(f"::error title=commit rules::could not deepen the checkout to read {base}..HEAD")
            return 2
        if not self.git.ok("cat-file", "-e", f"{base}^{{commit}}", quiet=True) and not self.git.ok(
                "fetch", "--quiet", "--no-tags", "--filter=blob:none", "origin", base, timeout=120):
            print(f"::error title=commit rules::no commit {base} to judge from")
            return 2
        return None

    def fork_point(self, base: str) -> Optional[str]:
        """Where base meets HEAD: base itself when it is an ancestor, else the merge-base of a rewritten history (the
        commits since it are judged, never none); None when they share no history."""
        found = (self.git.out("merge-base", base, "HEAD", quiet=True) or "").strip()
        return found or None

    def rule_sets(self, base: str) -> int:
        rc = 0
        for rules in RULE_SETS:
            r = self.python(os.path.join(HERE, "commit_rules.py"), rules, "--range", f"{base}..HEAD")
            if r == 0:
                print(f"  {rules}: every commit holds", flush=True)
            elif r == 1:
                print(f"::error title=commit rules ({rules})::a commit breaks the drift gate's {rules} rule -- see the "
                      "step log", flush=True)
                rc = 1
            else:
                print(f"::error title=commit rules ({rules})::the range could not be read", flush=True)
                rc = rc if rc == 1 else 2
        return rc

    def gating_checks(self) -> int:
        if not os.path.isfile(".github/gating-checks.tsv"):
            return 0
        if self.python(os.path.join(HERE, "gating_checks.py"), "lint") == 0:
            print("  gating checks: every job, label and gate workflow registered", flush=True)
            return 0
        print("::error title=commit rules (gating checks)::.github/gating-checks.tsv is out of step with the gate's "
              "workflows -- see the step log", flush=True)
        return 1

    def advance(self) -> int:
        """The rules held on main: the verified ref moves to a marker of HEAD, only forward."""
        done = self.verified_ref("advance")
        sys.stdout.write(done.stdout)
        sys.stderr.write(done.stderr)
        return 0 if done.code == 0 else 2

    def check(self, named: str) -> int:
        base = self.base(named)
        if base is None:
            return 2
        if not base:
            print("commit-rules-ci: no green Main run and no push base: nothing to judge")
            return 0
        unreachable = self.reachable(base)
        if unreachable is not None:
            return unreachable
        fork = self.fork_point(base)
        if fork is None:
            print(f"::error title=commit rules::{base} and HEAD share no history: no range to judge")
            return 2
        if fork != base:
            print(f"commit-rules-ci: {base} is no ancestor of HEAD (history rewritten): judging from the merge-base "
                  f"{fork[:9]}")
            base = fork
        head = (self.git.out("rev-parse", "--short=9", "HEAD") or "").strip()
        count = (self.git.out("rev-list", "--count", f"{base}..HEAD") or "").strip()
        print(f"commit rules over {base[:9]}..{head} ({count} commit(s))", flush=True)
        rc = self.rule_sets(base)
        return self.finish(1 if self.gating_checks() else rc)

    def finish(self, rc: int) -> int:
        on_main = self.env.get("GITHUB_REF") == "refs/heads/main" and self.env.get("GITHUB_EVENT_NAME") == "push"
        return self.advance() if rc == 0 and on_main else rc


def main(argv: List[str], env: Mapping[str, str]) -> int:
    try:
        named = base_option(argv)
    except Usage as why:
        print(why, file=sys.stderr)
        return 2
    return CommitRules(tool_process.SubprocessRunner(), env).check(named)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], dict(os.environ)))
