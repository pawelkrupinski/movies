#!/usr/bin/env python3
"""Unit tests for ci_wait.py (scripts/ci-wait): runs waited on to their conclusion through null and partial answers,
a run that never appears, cancelled runs followed to the run that superseded them (or not, when nothing did), a set
of workflows, --or-later / --until-green, the timeout -- every gh call answered by a fake, the clock a fake one. The
last test runs scripts/ci-wait itself against a stub `gh` on the PATH.

    python3 scripts/ci_wait_test.py
"""
import io
import json
import os
import subprocess
import sys
import tempfile
import unittest
from contextlib import redirect_stderr, redirect_stdout

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import ci_wait  # noqa: E402

A, B, C = "a" * 40, "b" * 40, "c" * 40  # three commits on main, in order: A, then B, then C
DESCENDS = {(A, B): True, (A, C): True, (B, C): True, (B, A): False, (C, A): False, (C, B): False}


def run(id, sha=A, status="completed", conclusion="success", workflow="Main", created="2026-10-15T10:00:00Z",
        updated=None):
    return {"databaseId": id, "headSha": sha, "headBranch": "main", "status": status,
            "conclusion": conclusion if status == "completed" else "", "workflowName": workflow,
            "createdAt": created, "updatedAt": updated or created}


class FakeClock:
    def __init__(self):
        self.t = 0.0

    def now(self):
        return self.t

    def wall(self):
        return f"t+{self.t:g}"

    def sleep(self, seconds):
        self.t += max(seconds, 1)


class Gh:
    """Answers `run view` / `run list` from a script: per kind, a list of answers consumed one per call, the last
    repeated. An answer is a Python value (dumped as JSON), a raw string (printed as is), or ("fail", status)."""

    def __init__(self, view=None, lists=None):
        self.view, self.lists, self.calls = list(view or []), dict(lists or {}), []

    def _next(self, answers):
        return answers.pop(0) if len(answers) > 1 else answers[0]

    def __call__(self, argv):
        self.calls.append(argv)
        if argv[:2] == ["run", "view"]:
            answer = self._next(self.view)
        elif argv[:2] == ["run", "list"]:
            answer = self._next(self.lists[argv[argv.index("--workflow") + 1]])
        else:
            return 1, ""
        if isinstance(answer, tuple):
            return answer[1], ""
        return 0, answer if isinstance(answer, str) else json.dumps(answer)


def wait(argv, gh, git=lambda a, b: DESCENDS.get((a, b))):
    out, err = io.StringIO(), io.StringIO()
    with redirect_stdout(out), redirect_stderr(err):
        status = ci_wait.main(argv + ["--interval", "30s"], gh=gh, git=git, clock=FakeClock())
    return status, out.getvalue().strip().split("\t") if out.getvalue().strip() else [], err.getvalue()


class RunTest(unittest.TestCase):
    def test_a_run_is_waited_on_to_its_success(self):
        gh = Gh(view=[run(7, status="queued"), run(7, status="in_progress"), run(7)])
        status, final, err = wait(["--run", "7"], gh)
        self.assertEqual(status, 0)
        self.assertEqual(final[:3], ["7", A, "success"])
        self.assertEqual(err.count("run 7 at aaaaaaaaa:"), 3)  # one line per state change: queued, in_progress, done

    def test_a_failure_is_exit_1(self):
        status, final, _ = wait(["--run", "7"], Gh(view=[run(7, conclusion="failure")]))
        self.assertEqual((status, final[2]), (1, "failure"))

    def test_another_conclusion_is_a_failure_too(self):
        status, final, _ = wait(["--run", "7"], Gh(view=[run(7, conclusion="startup_failure")]))
        self.assertEqual((status, final[2]), (1, "startup_failure"))

    def test_null_and_partial_answers_are_never_a_verdict(self):
        nulls = ["null", "", "{}", {"databaseId": None, "headSha": None, "status": None},
                 {"databaseId": 7, "headSha": A, "status": "completed", "conclusion": None}, "not json", ("fail", 1),
                 ("fail", 124)]
        gh = Gh(view=nulls + [run(7)])
        status, final, _ = wait(["--run", "7"], gh)
        self.assertEqual((status, final[:3]), (0, ["7", A, "success"]))
        self.assertEqual(len(gh.calls), len(nulls) + 1)

    def test_unreadable_reads_in_a_row_end_the_wait_with_no_verdict(self):
        gh = Gh(view=["null"])
        status, final, err = wait(["--run", "7"], gh)
        self.assertEqual((status, final[0], final[2]), (2, "-", "unreadable"))
        self.assertEqual(len(gh.calls), 10)
        self.assertIn("unreadable reads in a row", err)

    def test_a_run_not_done_by_the_timeout_is_exit_2(self):
        status, final, err = wait(["--run", "7", "--timeout", "5m"], Gh(view=[run(7, status="in_progress")]))
        self.assertEqual((status, final[:3]), (2, ["7", A, "timeout"]))
        self.assertIn("timed out after 5 min", err)

    def test_a_wait_whose_caller_is_gone_ends_itself(self):
        github = ci_wait.GitHub(Gh(view=[run(7, status="in_progress")]), git=lambda a, b: None)
        alive = [True, False]
        with redirect_stdout(io.StringIO()) as out, redirect_stderr(io.StringIO()):
            status = ci_wait.wait([ci_wait.Target(run_id=7)], github, FakeClock(), 3600, 30, 10, lambda: alive.pop(0))
        self.assertEqual((status, out.getvalue().split("\t")[2]), (2, "orphaned"))

    def test_a_cancelled_run_is_followed_to_the_one_that_superseded_it(self):
        gh = Gh(view=[run(7, conclusion="cancelled")], lists={"Main": [
            [run(8, sha=B, status="in_progress", created="2026-10-15T10:05:00Z"), run(7, conclusion="cancelled")],
            [run(8, sha=B, conclusion="success", created="2026-10-15T10:05:00Z"), run(7, conclusion="cancelled")]]})
        status, final, err = wait(["--run", "7"], gh)
        self.assertEqual((status, final[:3]), (0, ["8", B, "success"]))
        self.assertIn("superseded run 7", final[4])
        self.assertIn("waiting on run 8", err)

    def test_a_cancelled_run_nothing_superseded_is_exit_3(self):
        gh = Gh(view=[run(7, conclusion="cancelled")], lists={"Main": [[run(7, conclusion="cancelled")]]})
        status, final, _ = wait(["--run", "7"], gh)
        self.assertEqual((status, final[2]), (3, "cancelled"))
        self.assertIn("nothing superseded it", final[4])

    def test_a_run_on_another_line_of_history_supersedes_nothing(self):
        gh = Gh(view=[run(7, sha=B, conclusion="cancelled")],
                lists={"Main": [[run(9, sha=A, created="2026-10-15T11:00:00Z"), run(7, sha=B, conclusion="cancelled")]]})
        status, _, _ = wait(["--run", "7"], gh)
        self.assertEqual(status, 3)

    def test_no_follow_takes_a_cancelled_run_as_the_answer(self):
        gh = Gh(view=[run(7, conclusion="cancelled")], lists={"Main": [[run(8, sha=B)]]})
        status, final, _ = wait(["--run", "7", "--no-follow"], gh)
        self.assertEqual((status, final[:3]), (3, ["7", A, "cancelled"]))
        self.assertFalse([c for c in gh.calls if c[:2] == ["run", "list"]])


class ShaTest(unittest.TestCase):
    def test_a_run_for_the_commit_is_waited_for_to_appear_then_to_complete(self):
        gh = Gh(lists={"main.yml": [[], [run(8, sha=B)], [run(9, sha=A, status="queued"), run(8, sha=B)],
                                    [run(9, sha=A), run(8, sha=B)]]})
        status, final, err = wait(["--workflow", "main.yml", "--sha", A], gh)
        self.assertEqual((status, final[:3]), (0, ["9", A, "success"]))
        self.assertIn("no run at aaaaaaaaa yet", err)

    def test_a_run_that_never_appears_is_a_timeout(self):
        status, final, _ = wait(["--workflow", "main.yml", "--sha", A, "--timeout", "3m"], Gh(lists={"main.yml": [[]]}))
        self.assertEqual((status, final[:3]), (2, ["-", A, "timeout"]))

    def test_an_unreadable_run_in_a_listing_makes_the_listing_unreadable(self):
        gh = Gh(lists={"main.yml": [[run(9, sha=A, status="queued"), {"databaseId": None}], [run(9, sha=A)]]})
        status, _, err = wait(["--workflow", "main.yml", "--sha", A], gh)
        self.assertEqual(status, 0)
        self.assertNotIn("queued", err)

    def test_the_commits_cancelled_run_is_followed_and_named_superseded(self):
        gh = Gh(lists={"main.yml": [[run(10, sha=C, created="2026-10-15T10:09:00Z"),
                                     run(9, sha=A, conclusion="cancelled", created="2026-10-15T10:00:00Z")]]})
        status, final, _ = wait(["--workflow", "main.yml", "--sha", A], gh)
        self.assertEqual((status, final[0], final[2]), (0, "10", "success"))
        self.assertIn("superseded run 9 at aaaaaaaaa", final[4])

    def test_the_first_successor_to_complete_answers(self):
        gh = Gh(lists={"main.yml": [[run(11, sha=C, status="in_progress", created="2026-10-15T10:09:00Z"),
                                     run(10, sha=B, conclusion="failure", created="2026-10-15T10:05:00Z",
                                         updated="2026-10-15T10:20:00Z"),
                                     run(9, sha=A, conclusion="cancelled")]]})
        status, final, _ = wait(["--workflow", "main.yml", "--sha", A], gh)
        self.assertEqual((status, final[0], final[2]), (1, "10", "failure"))

    def test_or_later_takes_a_later_commits_run_when_the_commit_has_none(self):
        gh = Gh(lists={"android.yml": [[run(10, sha=B, workflow="Android")]]})
        status, final, _ = wait(["--workflow", "android.yml", "--sha", A, "--or-later"], gh)
        self.assertEqual((status, final[0]), (0, "10"))
        self.assertIn("no run at aaaaaaaaa", final[4])

    def test_until_green_waits_through_a_failure_for_a_green_run(self):
        gh = Gh(lists={"main.yml": [[run(9, sha=A, conclusion="failure")],
                                    [run(10, sha=B, status="in_progress"), run(9, sha=A, conclusion="failure")],
                                    [run(10, sha=B), run(9, sha=A, conclusion="failure")]]})
        status, final, _ = wait(["--workflow", "main.yml", "--sha", A, "--until-green"], gh)
        self.assertEqual((status, final[:3]), (0, ["10", B, "success"]))

    def test_until_green_never_green_is_a_timeout(self):
        gh = Gh(lists={"main.yml": [[run(9, sha=A, conclusion="failure")]]})
        status, final, _ = wait(["--workflow", "main.yml", "--sha", A, "--until-green", "--timeout", "2m"], gh)
        self.assertEqual((status, final[2]), (2, "timeout"))

    def test_an_unknown_ancestry_is_asked_again_not_taken_for_no(self):
        answers = [None, True]
        gh = Gh(lists={"main.yml": [[run(10, sha=B), run(9, sha=A, conclusion="cancelled")]]})
        status, final, _ = wait(["--workflow", "main.yml", "--sha", A], gh, git=lambda a, b: answers.pop(0))
        self.assertEqual((status, final[0]), (0, "10"))


class SetTest(unittest.TestCase):
    def test_a_set_is_green_once_every_workflow_is(self):
        gh = Gh(lists={"main.yml": [[run(9, sha=A, status="in_progress")], [run(9, sha=A)]],
                       "conv.yml": [[], [], [run(20, sha=A, status="queued")], [run(20, sha=A, workflow="Conv")]]})
        status, out, _ = self.wait_set(["--workflow", "main.yml", "--workflow", "conv.yml", "--sha", A], gh)
        self.assertEqual(status, 0)
        self.assertEqual([line.split("\t")[:3] for line in out], [["9", A, "success"], ["20", A, "success"]])

    def test_a_set_fails_at_its_first_failure_and_names_the_rest_pending(self):
        gh = Gh(lists={"main.yml": [[run(9, sha=A, conclusion="failure")]],
                       "conv.yml": [[run(20, sha=A, status="in_progress")]]})
        status, out, _ = self.wait_set(["--workflow", "main.yml", "--workflow", "conv.yml", "--sha", A], gh)
        self.assertEqual(status, 1)
        self.assertEqual([line.split("\t")[2] for line in out], ["failure", "pending"])

    def test_any_completed_is_the_newest_uncancelled_verdict_of_each_a_red_one_at_once(self):
        gh = Gh(lists={"main.yml": [[run(11, sha=C, status="in_progress", created="2026-10-15T10:09:00Z"),
                                     run(10, sha=B, conclusion="cancelled", created="2026-10-15T10:05:00Z"),
                                     run(9, sha=A, conclusion="failure")]],
                       "conv.yml": [[], [run(20, sha=B)]]})
        status, out, _ = self.wait_set(["--workflow", "main.yml", "--workflow", "conv.yml", "--any-completed"], gh)
        self.assertEqual(status, 1)
        self.assertEqual([line.split("\t")[:3] for line in out], [["9", A, "failure"], ["-", "-", "pending"]])
        gh = Gh(lists={"main.yml": [[run(9, sha=A)]], "conv.yml": [[], [run(20, sha=B)]]})
        status, out, _ = self.wait_set(["--workflow", "main.yml", "--workflow", "conv.yml", "--any-completed"], gh)
        self.assertEqual(status, 0)
        self.assertEqual([line.split("\t")[:3] for line in out], [["9", A, "success"], ["20", B, "success"]])

    def wait_set(self, argv, gh):
        out, err = io.StringIO(), io.StringIO()
        with redirect_stdout(out), redirect_stderr(err):
            status = ci_wait.main(argv + ["--interval", "1"], gh=gh, git=lambda a, b: DESCENDS.get((a, b)),
                                  clock=FakeClock())
        return status, out.getvalue().strip().splitlines(), err.getvalue()


class UsageTest(unittest.TestCase):
    def test_a_workflow_needs_a_question(self):
        with redirect_stderr(io.StringIO()):
            self.assertEqual(ci_wait.main(["--workflow", "main.yml"], gh=Gh(), clock=FakeClock()), 2)

    def test_durations(self):
        self.assertEqual([ci_wait.duration(t, "m") for t in ("90s", "20m", "2h", "5")], [90, 1200, 7200, 300])


class ToolTest(unittest.TestCase):
    """scripts/ci-wait itself, its gh a stub on the PATH: the subprocess wiring, gh's output read as gh prints it."""

    def test_the_tool_waits_through_a_null_answer_to_a_conclusion(self):
        with tempfile.TemporaryDirectory() as work:
            answers = [json.dumps({"databaseId": None, "headSha": None, "status": None}),
                       json.dumps(run(7, status="in_progress")), json.dumps(run(7, conclusion="failure"))]
            for n, answer in enumerate(answers, 1):
                with open(os.path.join(work, str(n)), "w") as f:
                    f.write(answer)
            gh = os.path.join(work, "gh")
            with open(gh, "w") as f:
                f.write('#!/usr/bin/env bash\necho "$*" >> "$WORK/calls"\nn=$(wc -l < "$WORK/calls" | tr -d " ")\n'
                        '[ -f "$WORK/$n" ] || n=3\ncat "$WORK/$n"\n')
            os.chmod(gh, 0o755)
            env = dict(os.environ, PATH=work + os.pathsep + os.environ["PATH"], WORK=work, CI_WAIT_POLL_SECONDS="0")
            done = subprocess.run([os.path.join(HERE, "ci-wait"), "--run", "7"], capture_output=True, text=True,
                                  env=env, timeout=60)
            self.assertEqual(done.returncode, 1, done.stderr)
            self.assertEqual(done.stdout.split("\t")[:3], ["7", A, "failure"])
            with open(os.path.join(work, "calls")) as f:
                self.assertEqual(f.readline().split()[:3], ["run", "view", "7"])


if __name__ == "__main__":
    unittest.main()
