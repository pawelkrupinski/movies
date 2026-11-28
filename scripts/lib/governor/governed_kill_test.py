#!/usr/bin/env python3
"""governed_kill.py and scripts/claude-hooks/governed-kill-guard: an agent's kill of a governed run-script-tests / sbt-warm
run is refused while the governor still lists it and it is younger than its own bound; past the bound, or a run the
governor does not list, or any other process, is allowed."""
from __future__ import annotations

import json
import os
import stat
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))
from governor import governed_kill  # noqa: E402

HOOK = os.path.join(os.path.dirname(os.path.dirname(HERE)), "claude-hooks", "governed-kill-guard")
STATUS = """governor: 5 admitted, 15/18 cores charged
  agent    2.0 GB    5 cores  script-tests run-script-tests  pid 81397  since 2026-11-27T09:57:04Z  /w
  WAITING agent    2.0 GB    8 cores  run-script-tests  pid 88751
"""
RUNS = {88751: ("python3 /w/scripts/run-script-tests --changed origin/main", "07:30"),
        81397: ("python3 /w/scripts/run-script-tests --timeout 600", "15:00"),
        555: ("vim notes.txt", "01:00:00")}


def ps(args):
    if args[:2] == ["-axo", "pid=,command="]:
        return "".join(f"{pid} {cmd}\n" for pid, (cmd, _age) in RUNS.items())
    pid = int(args[-1])
    return RUNS[pid][0] if "command=" in args else RUNS[pid][1]


class JudgementTest(unittest.TestCase):
    def refusal(self, *words, status=STATUS):
        return governed_kill.refusal([list(words)], ps, status)

    def test_a_queued_run_is_not_killed(self) -> None:
        said = self.refusal("kill", "-9", "88751")
        self.assertIn("pid 88751 is a governed run", said or "")
        self.assertIn("may be WAITING", said or "")

    def test_a_run_past_its_own_timeout_may_be_killed(self) -> None:
        self.assertIsNone(self.refusal("kill", "81397"))  # 15 min in, --timeout 600

    def test_a_run_the_governor_does_not_list_or_another_process_is_not_protected(self) -> None:
        self.assertIsNone(self.refusal("kill", "88751", status="governor: 0 admitted\n"))
        self.assertIsNone(self.refusal("kill", "555"))

    def test_pkill_naming_a_governed_tool_is_refused_for_the_runs_it_would_match(self) -> None:
        self.assertIsNotNone(self.refusal("pkill", "-f", "run-script-tests"))
        self.assertIsNone(self.refusal("pkill", "vim"))

    def test_elapsed_time_in_ps_form(self) -> None:
        self.assertEqual([62, 3725, 90061], [governed_kill.elapsed_seconds(t) for t in ("01:02", "1:02:05", "1-01:01:01")])


class HookTest(unittest.TestCase):
    """The hook itself, a fake ps and a fake agentctl first on the PATH / in KINOWO_GOVERNOR_BIN."""

    def setUp(self) -> None:
        self.dir = tempfile.TemporaryDirectory()
        self.addCleanup(self.dir.cleanup)
        self.write("bin/ps", '#!/bin/sh\ncase "$*" in\n  "-axo pid=,command=") echo "88751 python3 /w/scripts/run-script-tests";;\n'
                   '  *"command="*) echo "python3 /w/scripts/run-script-tests --changed x";;\n  *"etime="*) echo "07:30";;\nesac\n')
        self.write("agentctl", "#!/bin/sh\ncat <<'EOF'\n" + STATUS + "EOF\n")

    def write(self, name: str, text: str) -> str:
        path = os.path.join(self.dir.name, name)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, "w") as f:
            f.write(text)
        os.chmod(path, os.stat(path).st_mode | stat.S_IXUSR)
        return path

    def hook(self, command: str) -> subprocess.CompletedProcess:
        env = dict(os.environ, PATH=os.path.join(self.dir.name, "bin") + os.pathsep + os.environ["PATH"],
                   KINOWO_GOVERNOR_BIN=os.path.join(self.dir.name, "agentctl"))
        return subprocess.run([sys.executable, HOOK], input=json.dumps({"tool_input": {"command": command}}),
                              capture_output=True, text=True, env=env)

    def test_the_kill_of_a_queued_run_is_denied(self) -> None:
        done = self.hook("kill 88751")
        self.assertEqual(2, done.returncode, done.stderr)
        self.assertIn("governed run", done.stderr)

    def test_the_kill_in_a_longer_command_is_denied_and_other_commands_pass(self) -> None:
        self.assertEqual(2, self.hook("echo go; kill -TERM 88751 && echo done").returncode)
        self.assertEqual(0, self.hook("kill 4242").returncode)
        self.assertEqual(0, self.hook("echo 'kill 88751'").returncode)
        self.assertEqual(0, self.hook("ls").returncode)


if __name__ == "__main__":
    unittest.main()
