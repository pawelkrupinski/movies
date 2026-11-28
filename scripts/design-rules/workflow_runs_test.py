#!/usr/bin/env python3
"""THE WORKFLOWS' `run:` BLOCKS' tests (workflow_runs.py): each step's run, keyed by job and step name, block scalars
dedented, expressions neutralised, `with: run:` inputs and `defaults: run:` left out, a composite action's steps read."""
from __future__ import annotations

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import workflow_runs  # noqa: E402

WORKFLOW = """name: x
defaults:
  run:
    shell: bash
jobs:
  build:
    runs-on: ubuntu
    steps:
      - uses: actions/checkout@v5
      - name: Compile ${{ matrix.cc }}: now
        run: |
          set -e
          if [ "${{ a && b }}" ]; then echo hi; fi
      - run: ./one-liner.sh
      - name: Twice
        run: a
      - name: Twice
        run: b
        with:
          run: not-a-step
  test:
    steps:
      - name: Other
        run: >-
          folded
"""


class WorkflowRunsTest(unittest.TestCase):
    def test_runs_are_keyed_by_job_and_step_name(self) -> None:
        runs = workflow_runs.run_blocks(WORKFLOW)
        self.assertEqual(["build/Compile-X-now", "build/step3", "build/Twice", "build/Twice#2", "test/Other"], list(runs))

    def test_a_block_scalar_is_dedented_and_an_expression_is_no_shell_decision(self) -> None:
        self.assertEqual('set -e\nif [ "X" ]; then echo hi; fi'
                         , workflow_runs.run_blocks(WORKFLOW)["build/Compile-X-now"])

    def test_a_one_line_run_is_taken_as_written(self) -> None:
        self.assertEqual("./one-liner.sh", workflow_runs.run_blocks(WORKFLOW)["build/step3"])

    def test_a_composite_actions_steps_are_read(self) -> None:
        action = "runs:\n  using: composite\n  steps:\n    - id: a\n      shell: bash\n      run: |\n        echo hi\n"
        self.assertEqual({"action/step1": "echo hi"}, workflow_runs.run_blocks(action))

    def test_only_workflows_and_actions_hold_runs(self) -> None:
        self.assertTrue(workflow_runs.holds_runs(".github/workflows/ci.yml"))
        self.assertTrue(workflow_runs.holds_runs(".github/actions/x/action.yml"))
        self.assertFalse(workflow_runs.holds_runs(".github/dependabot.yml"))


if __name__ == "__main__":
    unittest.main()
