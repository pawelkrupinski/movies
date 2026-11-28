#!/usr/bin/env python3
"""The unit tests of the modules a change touches, in this checkout's warm sbt server
The unit tests of the modules a change touches, in this checkout's warm sbt server -- the iteration loop between
edits, minutes where `sbt testUnit` is ~16 (2026-11-17, warm). The selection is the pre-push hook's own
(`scripts/hooks/pre-push --unit-plan`, what agentctl's combined-tree verification runs): `<module>/Test/test` for each
module with a changed source, the worker's rest row, and the identity-row specs the paths name.

  scripts/unit-changed [--base <ref>] [--plan]

The paths: everything committed since the merge base with <base> (default origin/main), plus the uncommitted and the
untracked. --plan prints the sbt commands and runs nothing. Nothing touched under a module's sources: says so, exit 0.

NOT the gate: a change that CLAUDE.md's table sends to `sbt testUnit` still runs it before it is reported done; this is
for the loop before that.
Tests: scripts/unit_changed_test.py."""
from __future__ import annotations

import os
import sys
from dataclasses import dataclass
from typing import List, Mapping, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from cli import tool_cli  # noqa: E402
from processes import machine  # noqa: E402
import tool_git  # noqa: E402
import tool_process  # noqa: E402

TOOL = "unit-changed"
USAGE = __doc__ or ""


@dataclass
class Options:
    base: str = "origin/main"
    plan: bool = False


def read_word(o: Options, argv: List[str], i: int) -> int:
    if argv[i] == "--base":
        o.base = tool_cli.value_at(argv, i)
        return i + 1
    if argv[i] == "--plan":
        o.plan = True
        return i
    raise tool_cli.UsageError(f"unknown argument {argv[i]}")


def die(line: str) -> int:
    print(f"{TOOL}: {line}", file=sys.stderr)
    return 1


class Checkout:
    """What the tool asks of the world: git over this checkout, the hook's plan and the warm sbt server (the leaves)."""

    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str]) -> None:
        self.runner, self.env, self.root = runner, env, "."

    def git(self) -> tool_git.Git:
        return tool_git.Git(self.runner, self.env, self.root)

    def top(self) -> Optional[str]:
        self.root = self.git().line("rev-parse", "--show-toplevel") or self.root
        return self.root if self.root != "." else None

    def merge_base(self, base: str) -> Optional[str]:
        return self.git().line("merge-base", base, "HEAD") or None

    def changed(self, merge_base: str) -> List[str]:
        git = self.git()
        return sorted(set(git.lines("diff", "--name-only", "--no-renames", merge_base)
                          + git.lines("ls-files", "--others", "--exclude-standard")))

    def plan(self, stdin: str) -> Optional[str]:
        """The hook's commands for the paths (its stdin), one a line; None when the hook failed."""
        hook = tool_process.Command([os.path.join(self.root, "scripts/hooks/pre-push"), "--unit-plan"], self.env,
                                    cwd=self.root, input=stdin, stderr_through=True)
        done = self.runner.capture(hook)
        return done.stdout if done.code == 0 else None

    def sbt_warm(self, commands: List[str]) -> int:
        return self.runner.run(tool_process.Command([os.path.join(self.root, "scripts/sbt-warm"), *commands],
                                                    self.env, cwd=self.root))


def paths_text(paths: List[str]) -> str:
    """The paths as the hook reads them on stdin: one a line."""
    return "".join(f"{p}\n" for p in paths)


def commands_of(plan: str) -> List[str]:
    return [line for line in plan.splitlines() if line]


def announce(options: Options, commands: List[str]) -> Optional[int]:
    """What ends the run before sbt: the nothing-to-run line, or --plan's commands; None when sbt runs."""
    if not commands:
        print(f"unit-changed: no module source changed since {options.base}: no unit tests to run")
        return 0
    if options.plan:
        print("\n".join(commands))
        return 0
    print(f"unit-changed: {len(commands)} command(s): {'; '.join(commands)}", flush=True)
    return None


def run(options: Options, checkout: Checkout) -> int:
    if checkout.top() is None:
        return 2
    merge_base = checkout.merge_base(options.base)
    if merge_base is None:
        return die(f"no merge base of {options.base} and HEAD")
    plan = checkout.plan(paths_text(checkout.changed(merge_base)))
    if plan is None:
        return die("scripts/hooks/pre-push --unit-plan failed")
    commands = commands_of(plan)
    ended = announce(options, commands)
    return checkout.sbt_warm(commands) if ended is None else ended


def main(argv: List[str], env: Mapping[str, str]) -> int:
    code, options = tool_cli.parse_call(TOOL, USAGE, lambda: tool_cli.read_options(Options(), argv, USAGE, read_word))
    if options is None:
        return code or 0
    return run(options, Checkout(tool_git.shared_runner(), env))


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], machine.environment()))
