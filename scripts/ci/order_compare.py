#!/usr/bin/env python3
"""THE ORDER INDEPENDENCE VERDICT: a leg's red says an ORDER dependence only when the tests it failed pass in DEFAULT
order at the same commit. Of Order independence's ~66 failing jobs over runs #1-#100, ~31 were regressions (the same
failure in default order), ~18 flakes and ~12 the environment, against ONE real order dependence (2026-11-22
analysis), yet each read as an order red. So every leg runs through `leg`: on a failure it names the failing tests,
runs exactly those again in default order (the CONTROL PASS), and labels the leg
  regression  some fail in default order too: a change broke them, whatever ran before them;
  order       all pass in default order: an order dependence (or a flake -- a control pass is one sample);
  none        the leg failed naming no test (a build error, a timeout, the runner): no verdict;
  pass        the leg passed.
The control pass is a DIAGNOSIS, never a rerun to green: `leg` exits with the leg's own status whatever it found.

  order_compare.py leg <leg> <record dir> -- <command...>   run the leg's command; on a failure the control pass, its
                                                    record in <record dir>, the verdict as an annotation (agentctl
                                                    reads `REGRESSION, not an order dependence:`), the step summary
                                                    and `order-verdict=` in $GITHUB_OUTPUT. <leg>: scala-reverse,
                                                    scala-shuffled, android, android-instrumented, swift, ios.
  order_compare.py control-command <leg> <test>...  the default-order command `leg` runs for these tests (a dry run)
  order_compare.py failed-suites <sbt log>          the suites sbt names under `Failed tests:` / `Error during tests:`
  order_compare.py failed-junit <dir>               the classes of the failed test cases in <dir>'s JUnit XML reports
  order_compare.py failed-xctest <log>              the cases XCTest's log says failed, as <Module>.<Class>/<test>
  order_compare.py summary <dir> [<shuffled seed>]  every leg's verdict as markdown, and scala's reverse vs shuffled
  order_compare.py verdict <dir>                    the run's one word (regression > order > none > pass) and
                                                    <dir>/order-verdict.json, every leg's verdict and tests

A leg's record in <record dir>: <leg>.result (success|failure), <leg>.txt (the failing tests, one a line, sorted),
<leg>.control.txt (those failing in default order too), <leg>.verdict, <leg>.log (its output). A leg script that runs
each test alone writes its failing tests to $ORDER_FAILED_FILE (swift-test-shuffled.sh, devtest-shuffled.sh,
xcuitest-each-class-alone.sh); sbt's are read from its log and Gradle's from the JUnit reports.
ORDER_CONTROL_CMD, when set, runs the control pass instead as `$ORDER_CONTROL_CMD <leg> <test>...` (its test);
ORDER_CONTROL_EXTRA: words added to the android control command (the leg's own -P settings but the order seed);
ORDER_ANDROID_RESULTS stands in for the Gradle test-results directory. Runs on the macOS runners' python3 too (the swift
and ios legs; the 3.9 floor). (scripts/ci/order_compare_test.py; order_compare_verdicts.py: the summary and verdict)"""
from __future__ import annotations

import os
import shutil
import sys
from typing import Callable, Dict, List, Mapping, Sequence, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = [HERE, os.path.join(os.path.dirname(HERE), "lib")]
import order_compare_readers as readers  # noqa: E402
import order_compare_verdicts as verdicts  # noqa: E402
import ci_machine  # noqa: E402
from cli import tool_actions  # noqa: E402
from cli import tool_cli  # noqa: E402
import tool_process  # noqa: E402

LEGS = ("scala-reverse", "scala-shuffled", "android", "android-instrumented", "swift", "ios")
TOOL = "order_compare.py"


class Legs:
    """The control pass and the leg's verdict, over the injected runner, environment and step commands."""

    def __init__(self, root: str, env: Mapping[str, str], runner: tool_process.Runner,
                 actions: tool_actions.Actions) -> None:
        self.root = root
        self.env = env
        self.runner = runner
        self.actions = actions
        self.android_results = env.get("ORDER_ANDROID_RESULTS") or os.path.join(
            root, "android/app/build/test-results/testDebugUnitTest")

    def control_command(self, leg: str, tests: Sequence[str]) -> List[str]:
        """The default-order command for <leg>'s tests, a word an item (what the dry run prints a line each)."""
        return readers.control_command(leg, tests, self.root, self.env)

    def control_run(self, leg: str, directory: str, tests: Sequence[str]) -> List[str]:
        """Run <leg>'s tests in default order; those failing there too (a subset of the tests)."""
        log = os.path.join(directory, f"{leg}.control.log")
        open(log, "w").close()
        if self.env.get("ORDER_CONTROL_CMD"):
            status = self.runner.run(tool_process.Command([*self.env["ORDER_CONTROL_CMD"].split(), leg, *tests],
                                                          self.env, log=log, closed_stdin=True))
        elif leg == "android-instrumented":
            return self.each_alone(log, tests)
        else:
            status = self.default_order(leg, tests, log)
        if status == 0:
            return []
        named = readers.named_by(leg, log, self.android_results)
        asked = set(verdicts.lines_of(os.path.join(directory, f"{leg}.txt")))
        # only what was asked: a name the control run reports that the leg did not fail is no finding of this pass
        return sorted(asked & set(named))

    def each_alone(self, log: str, tests: Sequence[str]) -> List[str]:
        """Each class alone, as the instrumented leg ran it (a fresh install each), in name order; those failing."""
        devtest = os.path.join(self.root, "android/scripts/devtest.sh")
        return [t for t in tests if self.runner.run(tool_process.Command(
            [devtest, "--skip-live-youtube", t], self.env, log=log, append=True, closed_stdin=True)) != 0]

    def default_order(self, leg: str, tests: Sequence[str], log: str) -> int:
        if leg == "ios":
            shutil.rmtree(os.path.join(self.root, "build/alone/control.xcresult"), ignore_errors=True)
        if leg == "android":
            shutil.rmtree(self.android_results, ignore_errors=True)
        return self.runner.run(tool_process.Command(self.control_command(leg, tests), self.env, log=log,
                                                    closed_stdin=True))

    def leg(self, leg: str, directory: str, command: Sequence[str]) -> int:
        os.makedirs(directory, exist_ok=True)
        record = os.path.join(directory, leg)
        open(f"{record}.txt", "w").close()
        env = dict(self.env, ORDER_FAILED_FILE=f"{record}.txt")
        status = self.runner.run(tool_process.Command(command, env, log=f"{record}.log", tee=True)) if command else 0
        if status == 0:
            for suffix, text in ((".result", "success\n"), (".verdict", "pass\n"), (".txt", ""), (".control.txt", "")):
                verdicts.write(record + suffix, text)
            self.actions.output("order-verdict", "pass")
            return 0
        verdicts.write(f"{record}.result", "failure\n")
        failed = readers.failed_by(leg, f"{record}.log", f"{record}.txt", self.android_results)
        verdicts.write(f"{record}.txt", verdicts.lines(failed))
        verdict, line, control = self.judge(leg, directory, status, failed)
        verdicts.write(f"{record}.control.txt", verdicts.lines(control))
        verdicts.write(f"{record}.verdict", verdict + "\n")
        self.actions.output("order-verdict", verdict)
        self.actions.summary(f"### Order verdict ({leg}): {verdict.upper()}\n- {line}\n")
        return status

    def judge(self, leg: str, directory: str, status: int, failed: List[str]) -> Tuple[str, str, List[str]]:
        """The leg's verdict, the line saying it, and what failed in default order too."""
        title = f"Order verdict ({leg})"
        if not failed:
            line = f"NO ORDER VERDICT: the {leg} leg failed (exit {status}) naming no test -- a build error, a timeout or the runner"
            self.actions.annotate("warning", title, line)
            return "none", line, []
        self.actions.group(f"control pass: the {len(failed)} failing tests again, in DEFAULT order, at this commit")
        control = self.control_run(leg, directory, failed)
        self.actions.end_group()
        if control:
            verdict = "regression"
            line = (f"REGRESSION, not an order dependence: {verdicts.names(control)} fail in default order too -- a change"
                    " broke them; find it among the commits since the last green run")
        else:
            verdict = "order"
            replay = self.env.get("ORDER_REPLAY", "")
            line = (f"ORDER DEPENDENCE (or a flake): {verdicts.names(failed)} fail in the {leg} order but pass in default"
                    f" order{' -- replay: ' + replay if replay else ''}")
        self.actions.annotate("error", title, line)
        return verdict, line, control


def usage(line: str) -> tool_cli.UsageError:
    return tool_cli.UsageError(f"usage: {TOOL} {line}")


def leg_command(legs: Legs, args: List[str]) -> int:
    if len(args) < 3 or args[2] != "--":
        raise usage("leg <leg> <record dir> -- <command...>")
    if args[0] not in LEGS:
        raise tool_cli.UsageError(f"{TOOL}: unknown leg {args[0]}")
    return legs.leg(args[0], args[1], args[3:])


def control_command(legs: Legs, args: List[str]) -> int:
    if len(args) < 2:
        raise usage("control-command <leg> <test>...")
    if args[0] not in LEGS:
        raise tool_cli.UsageError(f"{TOOL}: unknown leg {args[0]}")
    print("\n".join(legs.control_command(args[0], args[1:])))
    return 0


READERS = {"failed-suites": (readers.failed_suites, "<sbt log>"), "failed-junit": (readers.failed_junit, "<dir>"),
           "failed-xctest": (readers.failed_xctest, "<log>")}


def reader_command(name: str, args: List[str]) -> int:
    read, what = READERS[name]
    if len(args) != 1:
        raise usage(f"{name} {what}")
    sys.stdout.write(verdicts.lines(read(args[0])))
    return 0


def summary_command(args: List[str]) -> int:
    if not args:
        raise usage("summary <dir> [<shuffled seed>]")
    print(verdicts.summary(args[0], args[1] if len(args) > 1 else ""), end="")
    return 0


def verdict_command(args: List[str]) -> int:
    if len(args) != 1:
        raise usage("verdict <dir>")
    print(verdicts.run_verdict(args[0]))
    return 0


def dispatch(legs: Legs, argv: List[str]) -> int:
    command, args = (argv[0], argv[1:]) if argv else ("", [])
    commands: Dict[str, Callable[[List[str]], int]] = {
        "leg": lambda a: leg_command(legs, a), "control-command": lambda a: control_command(legs, a),
        "summary": summary_command, "verdict": verdict_command,
        **{name: (lambda a, name=name: reader_command(name, a)) for name in READERS}}
    if command not in commands:
        raise usage("leg|control-command|failed-suites|failed-junit|failed-xctest|summary|verdict ...")
    return commands[command](args)


def main(argv: List[str]) -> int:
    """The composition point: the checkout, the environment, the real runner and step commands."""
    root = os.path.dirname(os.path.dirname(HERE))
    legs = Legs(root, ci_machine.environment(), tool_process.SubprocessRunner(), tool_actions.Actions(ci_machine.environment(), sys.stdout))
    return tool_cli.run_main(lambda: dispatch(legs, argv))


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
