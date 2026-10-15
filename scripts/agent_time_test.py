#!/usr/bin/env python3
"""Unit tests for agent_time.py over a small synthetic session: a tasks directory holding two subagent transcripts, a
background command's log and the session's own transcript -- each second of an agent's time in exactly one bucket.

    python3 scripts/agent_time_test.py
"""
import io
import json
import os
import sys
import tempfile
import unittest
import shutil
from contextlib import redirect_stderr, redirect_stdout

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import agent_time  # noqa: E402

T0 = 1_791_000_000  # an epoch second; every event below is T0 + its offset


def at(offset):
    import datetime
    return datetime.datetime.fromtimestamp(T0 + offset, datetime.timezone.utc).isoformat().replace("+00:00", "Z")


def assistant(offset, *blocks):
    return {"type": "assistant", "agentId": "x", "timestamp": at(offset), "message": {"role": "assistant", "content": list(blocks)}}


def user(offset, content):
    return {"type": "user", "agentId": "x", "timestamp": at(offset), "message": {"role": "user", "content": content}}


def use(tool_id, name, **given):
    return {"type": "tool_use", "id": tool_id, "name": name, "input": given}


def result(tool_id, text=""):
    return {"type": "tool_result", "tool_use_id": tool_id, "content": text}


def notification(task):
    return (f"<task-notification>\n<task-id>{task}</task-id>\n<status>completed</status>\n"
            f"<summary>Background command done</summary>\n</task-notification>")


LAUNCHED = "Command running in background with ID: {}. Output is being written to: /x/tasks/{}.output."

# Agent one: a prompt; 60 s of model; a measurement launched in the background (returns at once); a polling loop on its
# output, 600 s; 30 s of model; an sbt testOnly, 120 s, beside a parallel 10 s Read (counted once, as the sbt run); then
# 400 s of nothing (an idle gap); a slice launched in the background, and the agent waits 900 s for its notification;
# 20 s of model; `git push` (the pre-push hook) 50 s.
AGENT_ONE = [
    user(0, "Measure the change."),
    assistant(60, use("t1", "Bash", command="scripts/identity-measure HEAD", run_in_background=True)),
    user(61, [result("t1", LAUNCHED.format("bmeasure1", "bmeasure1"))]),
    assistant(62, use("t2", "Bash", command="until grep -q LANDABLE /x/tasks/bmeasure1.output; do sleep 30; done")),
    user(662, [result("t2", "LANDABLE")]),
    assistant(692, use("t3", "Bash", command='scripts/sbt-warm "worker/testOnly services.FooSpec"'),
              use("t4", "Read", file_path="/x/y.scala")),
    user(702, [result("t4", "...")]),
    user(812, [result("t3", "[success]")]),
    assistant(1212, use("t5", "Bash", command="scripts/identity-slice de --title x", run_in_background=True)),
    user(1213, [result("t5", LAUNCHED.format("bslice001", "bslice001"))]),
    assistant(1214, {"type": "text", "text": "Waiting for the slice."}),
    user(2114, notification("bslice001")),
    assistant(2134, use("t6", "Bash", command="git push origin HEAD:main")),
    user(2184, [result("t6", "pushed")]),
]
# Agent two waits on a convergence leg the SESSION launched (its transcript is not a subagent's): 300 s polling the log
# the launch writes, then waits on a CI run by id for 200 s.
SESSION = [
    assistant(0, use("s1", "Bash", command="bash /x/scratchpad/legs.sh pl > /x/scratchpad/legs.out 2>&1",
                     description="Run the convergence legs", run_in_background=True)),
    user(1, [result("s1", LAUNCHED.format("blegs0001", "blegs0001"))]),
]
AGENT_TWO = [
    user(0, "Wait for the legs."),
    assistant(5, use("u1", "Bash", command='until grep -q "=== pl exit" /x/scratchpad/legs.out; do sleep 30; done')),
    user(305, [result("u1", "")]),
    assistant(310, use("u2", "Bash", command="while gh run view 38735006233 --json status | grep -q progress; do sleep 20; done")),
    user(510, [result("u2", "")]),
]


class AgentTimeTest(unittest.TestCase):
    def setUp(self):
        self.root = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, self.root)
        session = os.path.join(self.root, "project", "s1")
        os.makedirs(os.path.join(session, "subagents"))
        self.tasks = os.path.join(self.root, "tasks")
        os.makedirs(self.tasks)
        for name, records in (("aone000000000001", AGENT_ONE), ("atwo000000000002", AGENT_TWO)):
            transcript = os.path.join(session, "subagents", f"agent-{name}.jsonl")
            with open(transcript, "w") as f:
                f.writelines(json.dumps(r) + "\n" for r in records)
            os.symlink(transcript, os.path.join(self.tasks, f"{name}.output"))  # as Claude Code links them
        with open(os.path.join(self.root, "project", "s1.jsonl"), "w") as f:
            f.writelines(json.dumps(r) + "\n" for r in SESSION)
        with open(os.path.join(self.tasks, "bmeasure1.output"), "w") as f:
            f.write("identity-measure: LANDABLE\n")  # a background command's log: no transcript
        self.agents = {a.id: a for a in agent_time.load(self.tasks)}

    def test_only_transcripts_are_agents(self):
        self.assertEqual(sorted(self.agents), ["aone000000000001", "atwo000000000002"])

    def test_every_second_in_one_bucket(self):
        one = self.agents["aone000000000001"]
        self.assertEqual(dict(one.buckets), {
            "model": 60 + 1 + 1 + 30 + 1 + 1 + 20,
            "poll: measure": 600,
            "sbt: testOnly": 120,
            "idle gap >3m": 400,
            "bg wait: slice": 900,
            "pre-push (git push)": 50,
        })
        self.assertEqual(sum(one.buckets.values()), one.span)

    def test_waits_on_what_another_transcript_launched(self):
        self.assertEqual(dict(self.agents["atwo000000000002"].buckets),
                         {"model": 10, "poll: local convergence": 300, "poll: CI wait": 200})

    def test_report_totals_and_longest_agents(self):
        out = agent_time.report(list(self.agents.values()), self.tasks)
        self.assertIn("2 agents, 0.7 agent-hours", out)
        self.assertRegex(out, r"bg wait: slice\s+0\.2\s+")
        longest = out.split("longest agents:")[1].strip().splitlines()
        self.assertTrue(longest[0].lstrip().startswith("aone000000000001"))
        self.assertIn("bg wait: slice 15m", longest[0])

    def test_one_agents_slowest_steps_in_order(self):
        out = io.StringIO()
        with redirect_stdout(out):
            self.assertEqual(agent_time.main([self.tasks, "--agent", "aone", "--top", "3"]), 0)
        steps = out.getvalue().splitlines()[2:]
        self.assertEqual([s.split()[3] if "poll" not in s else "poll" for s in steps], ["poll", "idle", "bg"])
        self.assertIn("10m", steps[0])

    def test_an_unknown_agent_and_a_missing_directory(self):
        with redirect_stdout(io.StringIO()), redirect_stderr(io.StringIO()):
            self.assertEqual(agent_time.main([self.tasks, "--agent", "zzz"]), 2)
            self.assertEqual(agent_time.main([os.path.join(self.root, "nowhere")]), 2)


if __name__ == "__main__":
    unittest.main()
