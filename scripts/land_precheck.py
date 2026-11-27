#!/usr/bin/env python3
# Purpose: The fast precheck: before `agentctl land` queues a change, the cheap reasons the landing check would reject it, here
"""THE FAST PRECHECK: before `agentctl land` queues a change, the cheap reasons the landing check would reject it, here,
in seconds instead of after the queue (review 2026-11-06, P1: 68 of 160 rejections in two weeks were lint, commit-rule,
conflict or old-ref ones, and a rejected change waited a median 233 min instead of 25).

  scripts/land-precheck [--base <ref>] [--head <ref>]

Three steps, each printed `precheck <step> PASS|FAIL|UNCHECKED|SKIP (<n> s)`, a FAIL with the lines that say why:
  rebase  the change replayed onto <base> (default origin/main) with `git replay`, touching no ref and no file; a
          conflict names its files (docs and CLAUDE.md collide most)
  rules   the pre-push hook's own commit-rule and linter checks over <base>..<head>, alone (PREPUSH_ONLY): allowlists,
          driftguards (Fixes-Red / Drift-Slice / Drift-Harness-Refactor / Identity-Design), newchecks, judgements,
          the shellcheck and actionlint checks (the old refs compile after landing, on Main, since 2026-11-21);
          the stranded-trailer rule runs too. Not the red gate: a red main makes a change WAIT, not fail.
  lints   the lint specs the hook's registry (scripts/hooks/lint-registry.tsv) selects for the pushed paths, in one
          scripts/sbt-warm call (seconds on a warm checkout; a cold one compiles first), admitted by the governor as a
          landing check's JVM (KINOWO_GOV_CLASS=check, scripts/jvm-gate.sh)
A rebase or rules FAIL ends the precheck there: the lints are SKIPped (`refused already`), so a refusal the cheap
steps find comes back in seconds, never after the lints' wait on sbt.
NOT the script self-tests (dropped 2026-11-17): the landing check runs them on every change, on CI's Linux when it runs
remotely, so here they were the same tests a second time (148 s here beside 125 s in the check, 80 runs of 153 in
2026-11-14..17) on the laptop's CPU, for 3 early refusals.
Last line: `PRECHECK PASS (<n> s)`, `PRECHECK FAIL: <steps> (<n> s)` or `PRECHECK UNCHECKED: <steps> (<n> s)`.
Exit 0 pass, 1 a step refused the change, 2 a step could not run (sbt never came up, a bound ran out): agentctl land
queues the change anyway then, and the landing check judges it.

Bounds: PRECHECK_RULES_SECONDS (300), PRECHECK_LINT_SECONDS (300)."""
from __future__ import annotations

import os
import re
import sys
import tempfile
from dataclasses import dataclass, replace
from typing import Dict, List, Mapping, Sequence, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import machine  # noqa: E402
import tool_cli  # noqa: E402
import tool_git  # noqa: E402
import tool_process  # noqa: E402

TOOL = "land-precheck"
PURPOSE = "The fast precheck: before `agentctl land` queues a change, the cheap reasons the landing check would reject it, here"
RULE_CHECKS = "allowlists driftguards newchecks judgements shellcheck actionlint"
MODULES = ("common", "scrapers", "repochecks", "worker", "web")
BOUND_ENDED = (124, 137)
DETAIL_LINES = 40
CLUE_LINES = 20
REAL_FAILURE = re.compile(r"\*\*\* FAILED|Failed tests:|\[error\].*\.scala:[0-9]+")
SBT_NEVER_RAN = re.compile(r"failed to connect to server|sbt server did not start|server disconnected|JVM slot")
CLUE_LINE = re.compile(r"^\[info\]   +[^ ]")
STACK_FRAME = re.compile(r"^\[info\] +at ")
FAILED_TEST = re.compile(r"\*\*\* FAILED")
FAILED_SUMMARY = re.compile(r"Failed tests:|\[error\]")


class Died(Exception):
    """A failure of the tool itself (`die`: exit 1) or a command line it does not take (`usage_error`: exit 2)."""

    def __init__(self, message: str, status: int = 1) -> None:
        super().__init__(message)
        self.status = status


@dataclass(frozen=True)
class Options:
    base: str = "origin/main"
    head: str = "HEAD"
    help: bool = False


def parse(argv: Sequence[str]) -> Options:
    options, rest = Options(), list(argv)
    while rest:
        word = rest.pop(0)
        if word in ("--base", "--head"):
            if not rest:
                raise Died(f"{word} needs a value", 2)
            options = replace(options, **{word[2:]: rest.pop(0)})
        elif word in ("-h", "--help"):
            return Options(help=True)
        else:
            raise Died(f"unknown argument {word}", 2)
    return options


class FailureLines:
    """The failure's own lines of an sbt log: each failing test with its whole assertion message (the indented lines
    under it, up to its first stack frame, at most CLUE_LINES), the failed suites, sbt's errors -- never the specs that
    passed (2026-11-19: every passing `should` line was kept, the assertion dropped, and the NOT LANDED named two
    passes; 2026-11-23, b4be01: only the message's first line was kept, and a withClue's reason list under it was
    lost)."""

    def __init__(self) -> None:
        self.kept: List[str] = []
        self.clue = 0  # 0: not inside a failing test's message, else the lines of it kept so far, plus one

    def in_clue(self, line: str) -> bool:
        """`line` read as a line of the message of the failing test above it (kept while room is left)."""
        if not (self.clue and CLUE_LINE.search(line) and not STACK_FRAME.search(line)):
            self.clue = 0
            return False
        if self.clue <= CLUE_LINES:
            self.kept.append(line)
        self.clue += 1
        return True

    def feed(self, line: str) -> None:
        if self.in_clue(line):
            return
        if FAILED_TEST.search(line):
            self.kept.append(line)
            self.clue = 1
        elif FAILED_SUMMARY.search(line):
            self.kept.append(line)


def failure_lines(log: str) -> str:
    """The lines of `log` that say why it failed; the log whole when none of them is in it."""
    lines = FailureLines()
    for line in log.splitlines():
        lines.feed(line)
    return "\n".join(lines.kept) + "\n" if lines.kept else log


def lint_command(plan: str) -> str:
    """The one sbt-warm argument of a `module:spec` plan: a `testOnly` of each module's specs, `; ` between."""
    parts: List[str] = []
    for module in MODULES:
        specs = [line[len(module) + 1:] for line in plan.splitlines() if line.startswith(f"{module}:")]
        if specs:
            parts.append(f"{module}/Test/testOnly {' '.join(specs)}")
    return "; ".join(parts)


def sbt_never_ran(log: str) -> bool:
    return not REAL_FAILURE.search(log) and bool(SBT_NEVER_RAN.search(log))


class Precheck:
    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str], options: Options, tmp: str) -> None:
        self.runner, self.env, self.options, self.tmp = runner, dict(env), options, tmp
        self.git = tool_git.Git(runner, env, ".")
        self.started = machine.monotonic()
        self.failed = ""
        self.unchecked = ""

    def seconds(self, since: float) -> int:
        return int(machine.monotonic() - since)

    def report(self, step: str, status: str, since: float, detail: str = "") -> None:
        print(f"precheck {step} {status} ({self.seconds(since)} s)")
        if detail.strip():
            print("".join(f"    {line}\n" for line in detail.splitlines()[-DETAIL_LINES:]), end="")
        if status == "FAIL":
            self.failed += f" {step}"
        elif status == "UNCHECKED":
            self.unchecked += f" {step}"
        sys.stdout.flush()

    def bound(self, variable: str, default: str) -> float:
        return float(self.env.get(variable, default))

    def logged(self, name: str, argv: Sequence[str], env: Mapping[str, str], seconds: float) -> Tuple[int, str]:
        """`argv` with stdout and stderr in one file of the scratch directory, ended past `seconds`: its exit and text."""
        path = os.path.join(self.tmp, name)
        code = self.runner.run(tool_process.Command(list(argv), env, log=path, timeout=seconds))
        with open(path, errors="replace") as f:
            return code, f.read()

    def rebase(self, base_sha: str, head_sha: str, merge_base: str) -> None:
        """The change replayed onto the base in memory, never in the worktree."""
        since = machine.monotonic()
        if merge_base == base_sha:
            return self.report("rebase", "PASS", since)
        replay = self.git.call("replay", "--ref-action=print", "--onto", base_sha, f"{merge_base}..{head_sha}", quiet=True)
        if replay.code == 0:
            return self.report("rebase", "PASS", since)
        # merge-tree exits 1 on a conflict, and says the files then
        merged = self.git.call("merge-tree", "--write-tree", "--name-only", "--no-messages", base_sha, head_sha, quiet=True)
        conflicts = sorted(set(merged.stdout.splitlines()[1:]))
        if conflicts:
            why = (f"conflicts with {self.options.base} in: {''.join(c + ' ' for c in conflicts)}"
                   f"-- rebase onto {self.options.base} and resolve them before landing")
        else:
            why = replay.stderr
        self.report("rebase", "FAIL", since, why)

    def rules(self, base_sha: str, head_sha: str) -> None:
        """The hook's own checks, alone."""
        since = machine.monotonic()
        env = {**self.env, "PREPUSH_NO_REUSE": "1", "PREPUSH_ONLY": RULE_CHECKS}
        code, text = self.logged("rules", ["scripts/hooks/pre-push", "--checks-only", f"{base_sha}..{head_sha}"], env,
                                 self.bound("PRECHECK_RULES_SECONDS", "300"))
        self.report("rules", "PASS" if code == 0 else "UNCHECKED" if code in BOUND_ENDED else "FAIL", since,
                    "" if code == 0 else text)

    def with_gitops(self) -> Dict[str, str]:
        """The environment the lints run in. The specs reading the GitOps manifests (`@gitops` rows) run only over
        manifests infra/bin/fetch-gitops put in THIS checkout, as the hook does: an inherited PREPUSH_GITOPS=1 over a
        checkout without them failed every such spec and refused a sound change (2026-11-08). A fetch that fails leaves
        them to the hook, which fetches and blocks itself."""
        env = {k: v for k, v in self.env.items() if k != "PREPUSH_GITOPS"}
        fetch = "infra/bin/fetch-gitops"
        if os.access(fetch, os.X_OK):
            fetched = {**env, "KINOWO_GITOPS_DEST": os.path.join(os.getcwd(), "infra/kubernetes")}
            code, _ = self.logged("fetch-gitops", [fetch], fetched, self.bound("PRECHECK_GITOPS_SECONDS", "90"))
            if code == 0:
                env["PREPUSH_GITOPS"] = "1"
        return env

    def lints(self, base_sha: str, head_sha: str) -> None:
        """The registry's selection for the pushed paths, in the checkout's kept sbt server."""
        since = machine.monotonic()
        env = self.with_gitops()
        names = self.git.out("diff", "--name-only", "--no-renames", f"{base_sha}...{head_sha}") or ""
        planned = self.runner.capture(tool_process.Command(["scripts/hooks/pre-push", "--lint-plan"], env, input=names,
                                                           stderr_through=True))
        if not planned.stdout.strip():
            return self.report("lints", "SKIP", since)
        # a landing check's JVM to the governor, which evicts an idle kept server for it (scripts/jvm-gate.sh)
        env["KINOWO_GOV_CLASS"] = env.get("KINOWO_GOV_CLASS", "check")
        code, text = self.logged("lints", ["scripts/sbt-warm", lint_command(planned.stdout)], env,
                                 self.bound("PRECHECK_LINT_SECONDS", "300"))
        if code == 0:
            self.report("lints", "PASS", since)
        elif code in BOUND_ENDED or sbt_never_ran(text):
            self.report("lints", "UNCHECKED", since, text)
        else:
            self.report("lints", "FAIL", since, failure_lines(text))

    def finish(self) -> int:
        took = self.seconds(self.started)
        if self.failed:
            print(f"PRECHECK FAIL:{self.failed} ({took} s)")
            return 1
        if self.unchecked:
            print(f"PRECHECK UNCHECKED:{self.unchecked} ({took} s)")
            return 2
        print(f"PRECHECK PASS ({took} s)")
        return 0

    def commit(self, ref: str) -> str:
        sha = self.git.out("rev-parse", "--verify", "-q", f"{ref}^{{commit}}", quiet=True)
        if sha is None:
            raise Died(f"no commit {ref}")
        return sha.strip()

    def run(self) -> int:
        base_sha, head_sha = self.commit(self.options.base), self.commit(self.options.head)
        merge_base = self.git.out("merge-base", base_sha, head_sha, quiet=True)
        if merge_base is None:
            raise Died(f"no merge base of {self.options.base} and {self.options.head}")
        self.rebase(base_sha, head_sha, merge_base.strip())
        self.rules(base_sha, head_sha)
        # A change the rebase or the commit rules refuse comes back now, in seconds: the lints wait on sbt (and on a
        # JVM slot first), so a missing trailer came back 308 s after queueing (2026-11-17). Fix it and land again.
        if self.failed:
            print(f"precheck lints SKIP (0 s): refused already by{self.failed}")
            return self.finish()
        self.lints(base_sha, head_sha)
        return self.finish()


def precheck(argv: Sequence[str], environ: Mapping[str, str]) -> int:
    options = parse(argv)
    if options.help:
        print(f"Purpose: {PURPOSE}\n\n{__doc__}")
        return 0
    runner = tool_git.shared_runner()
    top = tool_git.Git(runner, environ, ".").out("rev-parse", "--show-toplevel")
    if top is None:
        return 2
    os.chdir(top.strip())
    with tempfile.TemporaryDirectory() as tmp:
        return Precheck(runner, environ, options, tmp).run()


def main(argv: Sequence[str], environ: Mapping[str, str]) -> int:
    tool_cli.raise_on_signals()
    try:
        return precheck(argv, environ)
    except Died as e:
        print(f"{TOOL}: {e}", file=sys.stderr)
        if e.status == 2:
            print(f"{TOOL}: see {TOOL} --help", file=sys.stderr)
        return e.status
    except tool_cli.Interrupted as e:
        return 128 + e.signum


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], machine.environment()))
