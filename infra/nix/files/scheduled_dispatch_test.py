#!/usr/bin/env python3
"""scheduled_dispatch.py: the crons -> OnCalendar generator, the repository's drift check, and the dispatcher's
decision against recorded GitHub answers (scheduled_dispatch_fixtures/, captured with `gh api` on 2026-11-08:
the drift explorer's late-but-run 2026-11-07 slot, and the family explorer's dropped 2026-11-08 slot, whose only
run since is a person's workflow_dispatch). No live HTTP: the GitHub seam is replaced, the decision code is real."""
import copy
import datetime
import json
import os
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.abspath(os.path.join(HERE, "..", "..", ".."))
sys.path.insert(0, HERE)
import scheduled_dispatch as sd  # noqa: E402

UTC = datetime.timezone.utc
FIXTURES = os.path.join(HERE, "scheduled_dispatch_fixtures")


def at(text):
    return datetime.datetime.strptime(text, "%Y-%m-%dT%H:%M").replace(tzinfo=UTC)


def recorded(name):
    with open(os.path.join(FIXTURES, name)) as f:
        return json.load(f)["workflow_runs"]


class Clock:
    """A clock that `sleep` advances: the dispatcher's waits take no wall time."""

    def __init__(self, now):
        self.now = now
        self.slept = []

    def __call__(self):
        return self.now

    def sleep(self, seconds):
        self.slept.append(seconds)
        self.now += datetime.timedelta(seconds=seconds)


class FakeGitHub:
    """The GitHub seam: `answers` is a function of the clock -> the runs list GitHub would answer then (or an
    ApiError to raise); dispatches and cancels are recorded and, once made, show in later answers."""

    def __init__(self, clock, answers, dispatch_errors=0):
        self.clock, self.answers, self.dispatch_errors = clock, answers, dispatch_errors
        self.dispatched, self.cancelled, self.queries = [], [], 0

    def runs_since(self, workflow_file, since):
        self.queries += 1
        answer = self.answers(self.clock())
        if isinstance(answer, Exception):
            raise answer
        runs = [r for r in copy.deepcopy(answer) if sd._created(r) >= since]
        runs += [{"id": 9000 + i, "event": "repository_dispatch", "status": "in_progress",
                  "created_at": sd.iso(when)} for i, (_, _, when) in enumerate(self.dispatched)]
        for r in runs:
            if r["id"] in self.cancelled:
                r["status"] = "completed"
        return runs

    def dispatch(self, event_type, payload):
        if self.dispatch_errors:
            self.dispatch_errors -= 1
            raise sd.ApiError("POST repos/x/dispatches: HTTP 502 Bad Gateway")
        self.dispatched.append((event_type, payload, self.clock()))

    def cancel(self, run_id):
        self.cancelled.append(run_id)


def settle(github, clock, workflow_file, crons, guard_hours=14):
    lines = []
    status = sd.settle(github, workflow_file, crons, clock(), 15, guard_hours, lines.append, clock, clock.sleep)
    return status, lines


class CronToOnCalendar(unittest.TestCase):
    def test_daily(self):
        self.assertEqual(sd.on_calendar("23 3 * * *"), ["*-*-* 03:23:00 UTC"])

    def test_day_of_week_sunday_is_0_and_7(self):
        self.assertEqual(sd.on_calendar("7 2 * * 0"), ["Sun *-*-* 02:07:00 UTC"])
        self.assertEqual(sd.on_calendar("7 2 * * 7"), ["Sun *-*-* 02:07:00 UTC"])

    def test_day_of_week_lists_and_ranges(self):
        self.assertEqual(sd.on_calendar("0 5 * * 1-3,5"), ["Mon,Tue,Wed,Fri *-*-* 05:00:00 UTC"])

    def test_steps_and_lists_spell_out(self):
        self.assertEqual(sd.on_calendar("7,37 */6 * * *"), ["*-*-* 00,06,12,18:07,37:00 UTC"])

    def test_day_of_month_and_week_are_ored_as_cron_does(self):
        self.assertEqual(sd.on_calendar("0 4 1 * 1"), ["*-*-01 04:00:00 UTC", "Mon *-*-* 04:00:00 UTC"])

    def test_months(self):
        self.assertEqual(sd.on_calendar("0 0 15 1,7 *"), ["*-01,07-15 00:00:00 UTC"])

    def test_rejects_out_of_range(self):
        with self.assertRaises(ValueError):
            sd.parse_cron("61 0 * * *")

    def test_matches_agrees_with_on_calendar_for_day_of_week(self):
        # 2026-11-08 is a Sunday.
        self.assertTrue(sd.matches("7 2 * * 0", at("2026-11-08T02:07")))
        self.assertFalse(sd.matches("7 2 * * 0", at("2026-11-09T02:07")))
        self.assertTrue(sd.matches("0 4 1 * 1", at("2026-11-09T04:00")))  # a Monday, not the 1st: cron ORs
        self.assertTrue(sd.matches("0 4 1 * 1", at("2026-12-01T04:00")))  # the 1st, a Tuesday

    def test_latest_and_next_slot(self):
        self.assertEqual(sd.latest_slot(["7 2 * * 0"], at("2026-11-08T02:30")), at("2026-11-08T02:07"))
        self.assertEqual(sd.latest_slot(["7 2 * * 0"], at("2026-11-08T02:06")), at("2026-11-01T02:07"))
        self.assertEqual(sd.next_slot(["41 0 * * *"], at("2026-11-07T00:41")), at("2026-11-08T00:41"))


WORKFLOW = """name: x
on:
    schedule:
        # a comment
        - cron: '7 2 * * 0'   # Sundays
        - cron: "0 5 * * 1"
    repository_dispatch:
        types: [scheduled-x, other]
    workflow_dispatch:
jobs:
    a:
        if: github.event_name == 'schedule'
"""


class Workflows(unittest.TestCase):
    def test_reads_crons_and_dispatch_types_without_yaml(self):
        self.assertEqual(sd.workflow_crons(WORKFLOW), ["7 2 * * 0", "0 5 * * 1"])
        self.assertEqual(sd.dispatch_types(WORKFLOW), ["scheduled-x", "other"])

    def test_generate_covers_daily_and_rarer_only(self):
        document = sd.generate({"x.yml": WORKFLOW, "often.yml": "on:\n  schedule:\n    - cron: '7,37 * * * *'\n",
                                "none.yml": "on:\n  push:\n"})
        self.assertEqual(list(document["workflows"]), ["x"])
        self.assertEqual(document["workflows"]["x"]["onCalendar"],
                         ["Sun *-*-* 02:07:00 UTC", "Mon *-*-* 05:00:00 UTC"])
        self.assertEqual(list(document["excluded"]), ["often.yml"])

    def check(self, workflow_text, with_json=True):
        with tempfile.TemporaryDirectory() as root:
            os.makedirs(os.path.join(root, ".github", "workflows"))
            os.makedirs(os.path.join(root, "infra", "nix", "files"))
            with open(os.path.join(root, ".github", "workflows", "x.yml"), "w") as f:
                f.write(workflow_text)
            if with_json:
                with open(os.path.join(root, sd.JSON_PATH), "w") as f:
                    f.write(sd.render(sd.generate({"x.yml": workflow_text})))
            return sd.problems(root)

    def test_check_flags_a_bare_schedule_condition_and_a_stale_file(self):
        found = self.check(WORKFLOW, with_json=False)
        self.assertTrue(any("is not what the workflows' crons generate" in p for p in found), found)
        self.assertTrue(any("misses the external scheduler's runs" in p for p in found), found)

    def test_check_passes_the_wired_workflow(self):
        wired = WORKFLOW.replace("github.event_name == 'schedule'", sd.DISPATCH_EQ)
        self.assertEqual(self.check(wired), [])

    def test_check_wants_the_workflows_own_event_type(self):
        found = self.check(WORKFLOW.replace("scheduled-x", "scheduled-y").replace(
            "github.event_name == 'schedule'", sd.DISPATCH_EQ))
        self.assertEqual(len(found), 1)
        self.assertIn("[scheduled-x]", found[0])

    def test_the_repository_is_in_sync(self):
        self.assertEqual(sd.problems(ROOT), [])


class Decision(unittest.TestCase):
    def test_already_ran_no_dispatch(self):
        # The drift explorer's 2026-11-07 00:41 slot: GitHub's schedule run, created 06:36. Checked at 07:00.
        clock = Clock(at("2026-11-07T07:00"))
        github = FakeGitHub(clock, lambda now: recorded("drift-explorer-runs-since-2026-11-07T0041.json"))
        status, lines = settle(github, clock, "identity-drift-explorer.yml", ["41 0 * * *"])
        self.assertEqual(status, 0)
        self.assertEqual(github.dispatched, [])
        self.assertIn("outcome=ran run=42347764855 event=schedule late_minutes=355", lines[-1])

    def test_missing_dispatches_after_the_grace(self):
        # The family explorer's dropped 2026-11-08 02:07 slot: the timer fires on it; the only run since is a person's
        # workflow_dispatch, which is not the slot's.
        clock = Clock(at("2026-11-08T02:07"))
        github = FakeGitHub(clock, lambda now: recorded("family-explorer-runs-since-2026-11-08T0207.json"))
        status, lines = settle(github, clock, "identity-family-explorer.yml", ["7 2 * * 0"])
        self.assertEqual(status, 0)
        self.assertEqual(clock.slept[0], 15 * 60)
        self.assertEqual([(e, p) for e, p, _ in github.dispatched],
                         [("scheduled-identity-family-explorer",
                           {"slot": "2026-11-08T02:07:00Z", "source": "scheduled-dispatch"})])
        self.assertEqual(github.dispatched[0][2], at("2026-11-08T02:22"))
        self.assertTrue(any("outcome=missed-dispatched" in line for line in lines), lines)

    def test_github_late_run_is_cancelled_once_ours_runs(self):
        # The drift explorer's recorded run, shifted to the slot after it and still in progress: GitHub starting the
        # 2026-11-08 00:41 slot at 06:36, after our 00:56 dispatch.
        late = recorded("drift-explorer-runs-since-2026-11-07T0041.json")[0]
        late.update(created_at="2026-11-08T06:36:27Z", status="in_progress", conclusion=None)
        clock = Clock(at("2026-11-08T00:41"))
        github = FakeGitHub(clock, lambda now: [late] if now >= at("2026-11-08T06:36") else [])
        status, lines = settle(github, clock, "identity-drift-explorer.yml", ["41 0 * * *"])
        self.assertEqual(status, 0)
        self.assertEqual(len(github.dispatched), 1)
        self.assertEqual(github.cancelled, [42347764855])
        self.assertTrue(any("outcome=late-duplicate-cancelled run=42347764855 late_minutes=355" in line
                            for line in lines), lines)
        self.assertLessEqual(clock.now, at("2026-11-08T14:45"))  # the guard ends 14 h after the slot

    def test_github_on_time_run_is_never_cancelled(self):
        on_time = recorded("drift-explorer-runs-since-2026-11-07T0041.json")[0]
        on_time.update(created_at="2026-11-07T00:50:00Z", status="in_progress")
        clock = Clock(at("2026-11-07T00:41"))
        github = FakeGitHub(clock, lambda now: [on_time])
        status, _ = settle(github, clock, "identity-drift-explorer.yml", ["41 0 * * *"])
        self.assertEqual((status, github.dispatched, github.cancelled), (0, [], []))

    def test_restarted_after_its_own_dispatch_does_not_dispatch_again(self):
        clock = Clock(at("2026-11-08T03:00"))
        github = FakeGitHub(clock, lambda now: [])
        github.dispatched.append(("scheduled-identity-family-explorer", {}, at("2026-11-08T02:22")))
        _, lines = settle(github, clock, "identity-family-explorer.yml", ["7 2 * * 0"], guard_hours=1)
        self.assertEqual(len(github.dispatched), 1)
        self.assertIn("outcome=dispatched-earlier", " ".join(lines))

    def test_api_down_retries_then_fails_the_unit(self):
        clock = Clock(at("2026-11-08T02:22"))
        github = FakeGitHub(clock, lambda now: sd.ApiError("GET runs: <urlopen error [Errno 101] unreachable>"))
        status, lines = settle(github, clock, "identity-family-explorer.yml", ["7 2 * * 0"])
        self.assertEqual(status, 1)
        self.assertEqual(github.queries, 4)
        self.assertEqual(github.dispatched, [])
        self.assertIn("outcome=dispatch-failed", lines[-1])

    def test_api_back_after_a_retry_dispatches(self):
        clock = Clock(at("2026-11-08T02:22"))
        github = FakeGitHub(clock, lambda now: [], dispatch_errors=1)
        status, lines = settle(github, clock, "identity-family-explorer.yml", ["7 2 * * 0"], guard_hours=1)
        self.assertEqual(status, 0)
        self.assertEqual(len(github.dispatched), 1)
        self.assertIn("api-error=POST repos/x/dispatches: HTTP 502", " ".join(lines))


if __name__ == "__main__":
    unittest.main()
