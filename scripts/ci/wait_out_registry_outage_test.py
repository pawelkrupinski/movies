#!/usr/bin/env python3
"""wait_out_registry_outage.py around a fake `docker` (logs in on cue, or meets the registry gone the way Main #2906's
publish did) and a fake `curl` (the registry's /v2/ answers, one per probe, the last repeating)."""
from __future__ import annotations

import os
import sys
import unittest
from typing import Dict, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/wait_out_registry_outage.py")
HOST = "registry.example"
COMMAND = ["docker", "login", HOST, "-u", "ci", "--password-stdin"]
# FAKE_DOCKER: `ok`; `refused-once` -- the first call meets Caddy restarting (Main #2906), later ones succeed;
# `refused` -- every call does; `other-host` -- refused by some other host; `broken` -- fails on its own.
DOCKER = """
import os
mode = env["FAKE_DOCKER"]
first = not os.path.exists("called.once")
open("called.once", "w").close()
refused = 'Error response from daemon: Get "https://{}/v2/": dial tcp 2.28.47.31:443: connect: connection refused'
if mode == "refused" or (mode == "refused-once" and first):
    err(refused.format("registry.example"))
    exit(1)
if mode == "other-host":
    err(refused.format("mirror.gcr.io"))
    exit(1)
if mode == "broken":
    err("ERROR: registry.example/movies-web:abc123: not found")
    exit(1)
out("Login Succeeded")
"""
CURL = """
import os
answers = env["FAKE_ANSWERS"].split()
n = int(open("probes").read()) if os.path.exists("probes") else 0
open("probes", "w").write(str(n + 1))
out(answers[min(n, len(answers) - 1)], end="")
"""
ENV: Dict[str, Optional[str]] = {"IMAGE_PUSH_WAIT_SECONDS": "5", "IMAGE_PUSH_POLL_SECONDS": "0"}


class WaitOutRegistryOutageTest(cli_harness.ToolCase):
    TOOL = TOOL

    def wrapped(self, docker: str, answers: str = "200") -> cli_harness.Done:
        self.fakes.program("docker", DOCKER)
        self.fakes.program("curl", CURL)
        return self.run_tool(HOST, "--", *COMMAND, env={**ENV, "FAKE_DOCKER": docker, "FAKE_ANSWERS": answers})

    def test_a_command_that_succeeds_runs_once_and_probes_nothing(self) -> None:
        done = self.wrapped("ok")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([COMMAND[1:]], self.fakes.calls("docker"))
        self.assertIn("Login Succeeded", done.stdout)
        self.assertEqual([], self.fakes.calls("curl"))

    def test_a_command_that_met_the_registry_restarting_waits_for_it_and_runs_once_more(self) -> None:
        done = self.wrapped("refused-once", answers="000 000 401")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([COMMAND[1:], COMMAND[1:]], self.fakes.calls("docker"))
        self.assertEqual(3, len(self.fakes.calls("curl")))
        self.assertIn(f"https://{HOST}/v2/", self.fakes.calls("curl")[0])
        self.assertIn(f"lost {HOST} (connection refused): it answers 000, waiting for it", done.stdout)
        self.assertIn(f"::warning::lost {HOST}: connection refused -- the image registry was down or restarting",
                      done.stdout)

    def test_a_registry_still_gone_at_the_deadline_fails_naming_it(self) -> None:
        done = self.wrapped("refused", answers="000")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(1, len(self.fakes.calls("docker")))
        self.assertIn(f"::error::lost {HOST}: connection refused, and it still answered 000 after 5 s", done.stdout)

    def test_any_other_failure_is_the_verdict_as_it_stands(self) -> None:
        for mode in ("broken", "other-host"):
            with self.subTest(mode):
                self.fakes.clear()
                if os.path.exists(os.path.join(self.work, "called.once")):
                    os.remove(os.path.join(self.work, "called.once"))
                done = self.wrapped(mode)
                self.assertEqual(1, done.code, done.output)
                self.assertEqual(1, len(self.fakes.calls("docker")))
                self.assertEqual([], self.fakes.calls("curl"))


if __name__ == "__main__":
    unittest.main()
