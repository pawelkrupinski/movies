#!/usr/bin/env python3
"""tool_dispatch.py against a fake gh: a 5xx dispatch asked again after the pause, a spent rate limit waited out to its
reset, any other refusal failed at once; the dispatched run found by its predicate once it is listed (an unreadable list
no run, said), given up past the find bound; the ETA the median of the recent runs that ended on the branch prefix; the
run of a commit still queued or running found, and `tool_dispatch.py in-flight` saying it (convergence-daily.yml)."""
from __future__ import annotations

import json
import os
import subprocess
import sys
import time
import unittest
from typing import Callable, Dict, List, Sequence, Tuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import cli_harness  # noqa: E402
import tool_dispatch  # noqa: E402
import tool_process  # noqa: E402

Answer = Callable[[Sequence[str]], Tuple[int, str, str]]


class FakeGh:
    """gh answering each call from `answer(argv)`; every argv kept."""

    def __init__(self, answer: Answer) -> None:
        self.answer = answer
        self.calls: List[List[str]] = []

    def call(self, args: Sequence[str], timeout: float, quiet: bool = False) -> tool_process.Captured:
        self.calls.append(list(args))
        return tool_process.Captured(*self.answer(args))


class Clock:
    def __init__(self) -> None:
        self.now = 0.0
        self.slept: List[float] = []

    def sleep(self, seconds: float) -> None:
        self.slept.append(seconds)
        self.now += seconds


def dispatcher(answer: Answer, **settings: float) -> Tuple[tool_dispatch.Dispatcher, FakeGh, Clock]:
    gh, clock = FakeGh(answer), Clock()
    return tool_dispatch.Dispatcher(gh, tool_dispatch.Settings(**settings), lambda: clock.now, clock.sleep), gh, clock


def refusals(*errors: str) -> Answer:
    """`workflow run` refused with each error in turn, then accepted; the rate limit: core spent, reset in 100 s."""
    left = list(errors)

    def answer(args: Sequence[str]) -> Tuple[int, str, str]:
        if args[0] == "api":
            return 0, f"0 {int(time.time()) + 100}\n", ""
        return (1, "", left.pop(0)) if left else (0, "", "")
    return answer


class UtcStampTest(unittest.TestCase):
    def test_the_since_of_a_dispatch_is_the_injected_wall_clock_in_githubs_shape(self) -> None:
        gh = FakeGh(refusals())
        d = tool_dispatch.Dispatcher(gh, tool_dispatch.Settings(), wall=lambda: 1_700_000_000)
        self.assertEqual("2023-11-14T22:13:20Z", d.utc_stamp())

    def test_the_rate_limit_wait_reads_the_same_wall_clock(self) -> None:
        d = tool_dispatch.Dispatcher(FakeGh(lambda args: (0, "0 1000100\n", "")), tool_dispatch.Settings(rate_limit_pad=5),
                                     lambda: 0.0, lambda _s: None, wall=lambda: 1_000_000)
        self.assertEqual(105, d._rate_limit_wait())


class DispatchTest(unittest.TestCase):
    def test_a_5xx_is_asked_again_after_the_pause(self) -> None:
        d, gh, clock = dispatcher(refusals("HTTP 502: Bad Gateway\n"), retry=7)
        self.assertTrue(d.dispatch("w.yml", "measure/x", ["-f", "a=b"]))
        self.assertEqual(["workflow", "run", "w.yml", "--ref", "measure/x", "-f", "a=b"], gh.calls[-1])
        self.assertEqual([7], clock.slept)

    def test_a_spent_rate_limit_is_waited_out_to_its_reset(self) -> None:
        d, _, clock = dispatcher(refusals("API rate limit exceeded for user\n"), rate_limit_pad=5)
        self.assertTrue(d.dispatch("w.yml", "main"))
        self.assertEqual(1, len(clock.slept))
        self.assertTrue(100 <= clock.slept[0] <= 106, clock.slept)

    def test_a_reset_too_far_away_or_any_other_refusal_fails_at_once(self) -> None:
        for error, settings in (("rate limit\n", {"rate_limit_max_wait": 10.0}), ("HTTP 422: no such input\n", {})):
            d, gh, clock = dispatcher(refusals(error), **settings)
            self.assertFalse(d.dispatch("w.yml", "main"))
            self.assertEqual(([], 1), (clock.slept, len([c for c in gh.calls if c[0] == "workflow"])))

    def test_three_5xx_in_a_row_fail(self) -> None:
        d, _, clock = dispatcher(refusals(*["HTTP 500\n"] * 3))
        self.assertFalse(d.dispatch("w.yml", "main"))
        self.assertEqual([15, 15], clock.slept)


def listed(*rounds: object) -> Answer:
    """`run list` answering each round in turn (the last one from then on); None is an unreadable list."""
    left: List[object] = list(rounds)

    def answer(args: Sequence[str]) -> Tuple[int, str, str]:
        now = left.pop(0) if len(left) > 1 else left[0]
        if now is None:
            return 1, "", "HTTP 503\n"
        done = subprocess.run(["jq", "-r", args[args.index("--jq") + 1]], input=json.dumps(now), capture_output=True,
                              text=True)
        return done.returncode, done.stdout, done.stderr
    return answer


def run(run_id: int, sha: str, created: str, **more: str) -> Dict[str, object]:
    return {"databaseId": run_id, "headSha": sha, "createdAt": created, **more}


class FindRunTest(unittest.TestCase):
    def test_the_newest_matching_run_since_the_dispatch_once_listed(self) -> None:
        d, gh, clock = dispatcher(listed([run(1, "abc", "2026-01-01T00:00:00Z")],
                                         [run(1, "abc", "2026-01-01T00:00:00Z"), run(2, "def", "2026-02-01T00:00:01Z"),
                                          run(3, "abc", "2026-02-01T00:00:02Z"), run(4, "abc", "2026-02-01T00:00:03Z")]))
        self.assertEqual("4", d.find_run("w.yml", "2026-02-01T00:00:00Z", '.headSha == "abc"',
                                         ["--branch", "b"]))
        self.assertEqual(["run", "list", "--workflow", "w.yml", "--event", "workflow_dispatch", "--limit", "20",
                          "--branch", "b", "--json", "databaseId,headSha,createdAt,displayTitle"], gh.calls[0][:-2])
        self.assertEqual([5], clock.slept)

    def test_none_past_the_bound_or_when_the_list_cannot_be_read(self) -> None:
        d, _, clock = dispatcher(listed([]), find_seconds=12)
        self.assertIsNone(d.find_run("w.yml", "2026", "true"))
        self.assertEqual([5, 5, 5], clock.slept)
        d, _, clock = dispatcher(listed(None))
        self.assertIsNone(d.find_run("w.yml", "2026", "true"))
        self.assertEqual([], clock.slept)


class FindTitledTest(unittest.TestCase):
    def test_the_run_another_runs_job_dispatched_by_its_title_polled_until_listed(self) -> None:
        titled = [{"databaseId": 7, "displayTitle": "Identity takes (convergence run 5)"},
                  {"databaseId": 6, "displayTitle": "Identity takes (convergence run 4)"}]
        d, _, clock = dispatcher(listed([], titled))
        self.assertEqual("7", d.find_titled("t.yml", "Identity takes (convergence run 5)", 60, 30))
        self.assertEqual([30], clock.slept)
        d, _, clock = dispatcher(listed(titled))
        self.assertIsNone(d.find_titled("t.yml", "Identity takes (convergence run 9)", 60, 30))
        self.assertEqual([30, 30], clock.slept)


class EtaTest(unittest.TestCase):
    def test_the_median_of_the_runs_that_ended_on_the_prefix(self) -> None:
        def ended(minutes: int, branch: str = "measure/x", conclusion: str = "success") -> Dict[str, object]:
            return {"status": "completed", "conclusion": conclusion, "headBranch": branch,
                    "createdAt": "2026-01-01T00:00:00Z", "updatedAt": f"2026-01-01T00:{minutes:02d}:30Z"}
        runs = [ended(10), ended(20, conclusion="failure"), ended(30), ended(59, branch="main"),
                ended(59, conclusion="cancelled")]
        d, _, _ = dispatcher(listed(runs))
        self.assertEqual("~21 min (median of 3 recent runs)", d.eta("w.yml", "measure/"))
        self.assertIsNone(dispatcher(listed([ended(5, branch="main")]))[0].eta("w.yml", "measure/"))
        self.assertIsNone(dispatcher(listed(None))[0].eta("w.yml", "measure/"))

    def test_the_backoff(self) -> None:
        self.assertEqual([1, 1, 90, 300], [tool_dispatch.backoff_next(s, 300) for s in (0, 1, 60, 250)])


IN_FLIGHT = [{"databaseId": 9, "headSha": "abc", "status": "completed"},
             {"databaseId": 8, "headSha": "def", "status": "in_progress"},
             {"databaseId": 7, "headSha": "abc", "status": "queued"},
             {"databaseId": 6, "headSha": "abc", "status": "in_progress"}]


class InFlightTest(unittest.TestCase):
    def test_the_commits_run_still_queued_or_running_none_or_an_unreadable_list(self) -> None:
        d, gh, _ = dispatcher(listed(IN_FLIGHT))
        self.assertEqual("7", d.in_flight("c.yml", "main", "abc"))
        self.assertEqual(["run", "list", "--workflow", "c.yml", "--branch", "main", "--limit", "20",
                          "--json", "databaseId,headSha,status"], gh.calls[0][:-2])
        self.assertEqual("", dispatcher(listed(IN_FLIGHT))[0].in_flight("c.yml", "main", "fff"))
        self.assertIsNone(dispatcher(listed(None))[0].in_flight("c.yml", "main", "abc"))


class InFlightCliTest(cli_harness.ToolCase):
    """`python3 scripts/lib/tool_dispatch.py in-flight <workflow> <ref> <sha>`, as convergence-daily.yml runs it."""
    TOOL = sys.executable
    MODULE = os.path.join(os.path.dirname(os.path.abspath(__file__)), "tool_dispatch.py")

    def gh_listing(self, runs: object, code: int = 0) -> None:
        self.write("runs.json", json.dumps(runs))
        self.fakes.script("gh", f'[ {code} = 0 ] || {{ echo "HTTP 503" >&2; exit {code}; }}\n'
                                f'while [ "$1" != --jq ]; do shift; done; jq -r "$2" "{self.path("runs.json")}"')

    def test_it_prints_the_run_in_flight_or_nothing(self) -> None:
        self.gh_listing(IN_FLIGHT)
        done = self.run_tool(self.MODULE, "in-flight", "c.yml", "main", "abc")
        self.assertEqual((0, "7\n"), (done.code, done.stdout), done.output)
        self.assertEqual(["run", "list", "--workflow", "c.yml", "--branch", "main"], self.fakes.calls("gh")[0][:6])
        done = self.run_tool(self.MODULE, "in-flight", "c.yml", "main", "fff")
        self.assertEqual((0, ""), (done.code, done.stdout), done.output)

    def test_an_unreadable_list_fails_said_and_a_wrong_command_line_is_a_usage_error(self) -> None:
        self.gh_listing([], code=1)
        done = self.run_tool(self.MODULE, "in-flight", "c.yml", "main", "abc")
        self.assertEqual((1, ""), (done.code, done.stdout), done.output)
        self.assertIn("could not list c.yml's runs on main", done.stderr)
        self.assertEqual(64, self.run_tool(self.MODULE, "in-flight", "c.yml").code)


if __name__ == "__main__":
    unittest.main()
