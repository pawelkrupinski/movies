#!/usr/bin/env python3
"""reusable_workflow_inputs.py: a reusable workflow's caller is held to the inputs its `on.workflow_call` declares."""

import os
import shutil
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import reusable_workflow_inputs as check  # noqa: E402

ROOT = check.ROOT

CALLEE = """name: Leg
on:
    workflow_call:
        inputs:
            code:
                required: true
                type: string
            command:   # the alias
                required: true
                type: string
                default: convergence
            job-timeout-minutes:
                required: false
                type: number
                default: 155
            order-split:
                type: boolean
                default: false
jobs:
    run:
        runs-on: ubuntu-26.04
        steps:
            - run: echo
"""


def caller(with_block):
    return f"""name: Caller
on: [ workflow_dispatch ]
jobs:
    leg:
        uses: ./.github/workflows/leg.yml
        with:
{with_block}        secrets: inherit
"""


class ReusableWorkflowInputsTest(unittest.TestCase):

    def problems(self, with_block, callee=CALLEE):
        d = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, d)
        with open(os.path.join(d, "leg.yml"), "w") as f:
            f.write(callee)
        with open(os.path.join(d, "caller.yml"), "w") as f:
            f.write(caller(with_block))
        return check.problems(d)

    def test_a_call_handing_every_input_its_declared_type_passes(self):
        self.assertEqual(self.problems("            code: pl # the country\n"
                                       "            job-timeout-minutes: 55\n"
                                       "            order-split:  ${{ matrix.order-split == true }}\n"), [])

    def test_an_input_the_callee_does_not_declare_fails(self):
        # a caller still passing an input a refactor removed (the runner scheduler's `row-runners`)
        self.assertEqual(self.problems("            code: pl\n            row-runners: 3\n"),
                         ["caller.yml: job leg calls leg.yml with `row-runners`, an input it does not declare"])

    def test_a_required_input_left_out_fails_unless_it_has_a_default(self):
        self.assertEqual(self.problems("            job-timeout-minutes: 55\n"),
                         ["caller.yml: job leg calls leg.yml without `code`, which it requires"])

    def test_a_string_to_a_number_or_boolean_input_fails(self):
        self.assertEqual(self.problems("            code: pl\n"
                                       "            job-timeout-minutes: '55'\n"
                                       "            order-split: ${{ 'true' }}\n"),
                         ["caller.yml: job leg calls leg.yml passing `job-timeout-minutes` \"'55'\" (a string) to a number input",
                          "caller.yml: job leg calls leg.yml passing `order-split` a quoted literal 'true' to a boolean input"])

    def test_a_call_of_a_missing_or_unreusable_workflow_fails(self):
        self.assertEqual(self.problems("            code: pl\n", callee="name: Leg\non: [ push ]\njobs: {}\n"),
                         ["caller.yml: job leg calls leg.yml, which declares no `on: workflow_call`"])

    def test_a_planned_matrix_of_json_strings_fails_where_a_number_or_boolean_is_taken(self):
        # Identity takes #1 (run 42515079814): the preflight's rows held "55" and "true"
        declared = check.declared_inputs(CALLEE)
        given = {"job-timeout-minutes": "${{ matrix.job }}", "order-split": "${{ matrix.order-split }}", "code": "${{ matrix.code }}"}
        self.assertEqual(check.matrix_problems(given, declared, [{"code": "pl", "job": 55, "order-split": False}]), [])
        self.assertEqual(check.matrix_problems(given, declared, [{"code": "pl", "job": "55", "order-split": "true"}]),
                         ["row pl: `job-timeout-minutes` takes matrix.job = '55', not a number",
                          "row pl: `order-split` takes matrix.order-split = 'true', not a boolean"])

    def test_this_repositorys_workflows_call_their_reusable_workflows_as_declared(self):
        res = subprocess.run([sys.executable, os.path.join(ROOT, "scripts/ci/reusable_workflow_inputs.py")],
                             capture_output=True, text=True)
        self.assertEqual(res.returncode, 0, res.stdout + res.stderr)
        texts = [check.read(os.path.join(ROOT, ".github/workflows", f))
                 for f in ("identity-model-convergence.yml", "identity-takes.yml", "verify-scrape-recording.yml")]
        self.assertTrue(all(any(c[1] == "country-convergence-leg.yml" for c in check.calls(t)) for t in texts))


if __name__ == "__main__":
    unittest.main()
