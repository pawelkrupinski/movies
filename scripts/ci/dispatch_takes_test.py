#!/usr/bin/env python3
"""scripts/ci/dispatch-takes.sh, black-box, against a fake gh: convergence's preflight dispatches Identity takes on its
own ref with the takes-meaningful inputs it was given and its run id as parent-run, passes no empty input, and fails
when GitHub refuses it; every input it forwards is one both workflows declare (handed every input convergence
declares, it forwards only those). Ported from dispatch-takes-test.sh; the same cases hold the shell tool and its
Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import re
import sys
import unittest
from typing import Dict, List

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/ci/dispatch_takes.py", "scripts/ci/dispatch-takes.sh")
WORKFLOWS = os.path.join(cli_harness.ROOT, ".github", "workflows")


def declared(workflow: str) -> List[str]:
    """The workflow_dispatch inputs a workflow declares (each at its 12-space indent)."""
    with open(os.path.join(WORKFLOWS, workflow)) as f:
        return re.findall(r"^            ([a-z-]+):$", f.read(), re.M)


class DispatchTakesTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.script("gh", '[ -z "${FAIL:-}" ] || { echo "$FAIL" >&2; exit 1; }')

    def dispatch(self, env: Dict[str, str]) -> cli_harness.Done:
        self.fakes.clear()
        return self.run_tool("main", "42", env={"DISPATCH_RETRY_SECONDS": "0", **env})

    def test_a_dispatch_naming_nothing_hands_only_parent_run_on_the_runs_ref(self) -> None:
        self.assertEqual(0, self.dispatch({}).code)
        self.assertEqual([["workflow", "run", "identity-takes.yml", "--ref", "main", "-f", "parent-run=42"]],
                         self.fakes.calls("gh"))

    def test_the_takes_inputs_are_forwarded_each_as_identity_takes_names_it_empty_ones_left_out(self) -> None:
        done = self.dispatch({"INPUT_TAKES_BASE": "abc", "INPUT_TAKES_SKIP": "shadow", "INPUT_DRIFT_TWICE": "true",
                              "INPUT_REUSE_RUN": "7", "INPUT_JVM_OPTIONS": "", "INPUT_RECORDING": "99"})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([["workflow", "run", "identity-takes.yml", "--ref", "main", "-f", "parent-run=42",
                           "-f", "takes-base=abc", "-f", "recording=99", "-f", "takes-skip=shadow",
                           "-f", "drift-twice=true", "-f", "reuse-run=7"]], self.fakes.calls("gh"))
        self.assertIn("dispatched Identity takes on main for convergence run 42", done.stdout)

    def test_a_refusal_fails_the_step_saying_the_run_has_no_takes_run_beside_it(self) -> None:
        done = self.dispatch({"FAIL": "HTTP 403: Resource not accessible"})
        self.assertEqual(1, done.code, done.output)
        self.assertIn("no takes run", done.output)

    def test_every_forwarded_input_is_one_both_workflows_declare(self) -> None:
        convergence, takes = declared("identity-model-convergence.yml"), declared("identity-takes.yml")
        every = {"INPUT_" + name.upper().replace("-", "_"): "x" for name in convergence}
        self.assertEqual(0, self.dispatch(every).code)
        args = self.fakes.calls("gh")[0]
        forwarded = [arg.split("=")[0] for flag, arg in zip(args, args[1:]) if flag == "-f"]
        self.assertGreater(len(forwarded), 5, args)
        self.assertEqual([], [name for name in forwarded if name != "parent-run" and
                              (name not in convergence or name not in takes)])


if __name__ == "__main__":
    unittest.main()
