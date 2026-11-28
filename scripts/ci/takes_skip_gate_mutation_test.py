#!/usr/bin/env python3
# script-test-width: 4 (its mutants run 4 at a time: scripts/run-script-tests charges the run its extra cores)
"""THE GATE FAILS CLOSED, proved by mutation: each mutant below is takes_skip_gate.py with ONE check taken out, and
takes_skip_gate_test.py must FAIL against every one -- a check whose removal no test notices is a check that could
silently stop working and let a takes row be skipped that should run. Each mutant is applied by exact text; a mutant
whose text is not in the gate exactly once fails this test too (the gate changed: re-aim the mutant at the new check).
Each mutant is written into a sandbox of its own beside the adapters the gate imports (sandbox_closure.py)."""
from __future__ import annotations

import os
import subprocess
import sys
import unittest
from concurrent.futures import ThreadPoolExecutor
from typing import Dict, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(os.path.dirname(HERE), "lib"))
import sandbox_closure  # noqa: E402
import takes_skip_gate_test  # noqa: E402
from tool_files import Files  # noqa: E402
from tool_pool import governed_workers  # noqa: E402  (THE GOVERNED FAN-OUT: the mutants' pool under a governor ticket)

ROOT = os.path.dirname(os.path.dirname(HERE))
GATE_PATH = "scripts/ci/takes_skip_gate.py"
GATE = os.path.join(ROOT, GATE_PATH)
TEST = os.path.join(HERE, "takes_skip_gate_test.py")
# name -> (exact text in the gate, what replaces it)
MUTANTS: Dict[str, Tuple[str, str]] = {
    "the mode is not checked": ('require(mode in ("shadow", "enforce"),', "require(True,"),
    "the proof command is not required": ('require(len(argv) >= 5 and argv[3] == "--",', "require(True,"),
    "commit names are not checked": ("require(bool(FULL_NAME.match(base) and FULL_NAME.match(head)),", "require(True,"),
    "one commit is allowed": ("require(base != head,", "require(True,"),
    "a failed diff reads as no change": ('require(diff.code == 0, f"git could not diff {base}',
                                         'require(True, f"git could not diff {base}'),
    "an empty change is allowed": ("require(bool(changed),", "require(True,"),
    "any file is allowed": ("require(inert(f),", "require(True,"),
    "the placeholders count as a title rule": ("if path == PLACEHOLDERS:\n        return False",
                                               "if path == PLACEHOLDERS:\n        return True"),
    "no rule change is needed": ("require(rules > 0,", "require(True,"),
    "identity code is not denied": ("require(not denied,", "require(True,"),
    "changed tooling is allowed": ("require(not diff.stdout.strip(),", "require(True,"),
    "a missing trace is allowed": ("require(self.files.is_file(trace) and self.files.size(trace) > 0,", "require(True,"),
    "a broken trace is allowed": ("require(lines is not None,", "lines = lines or 1\n        require(True,"),
    "an empty trace is allowed": ("require(bool(lines),", "require(True,"),
    "the proof exit is not read": ("require(rc == 0,", "require(True,"),
    "the proof line is not read": ("require(matched is not None,", "require(True,"),
    "a proof of nothing is allowed": ("require(readings > 0,", "require(True,"),
    "a differ row is ignored": ("require(not self.files.read_text_or(differ, \"\"),", "require(True,"),
    "the proof is unbounded": ("timeout=proof_timeout(self.env)))", "timeout=None))"),
    "an ended gate strands its proof": ("runner = tool_process.SubprocessRunner()",
                                        'runner = tool_process.SubprocessRunner(groups=type("Deaf", (), {"guard": lambda self, timeout: tool_process.Bounded(None)})())'),
}


def run_test(gate: str, template: str, failfast: Tuple[str, ...] = ()) -> Tuple[int, str]:
    """takes_skip_gate_test.py against `gate`, its scratch repository cloned from `template` -> its exit and output;
    `("-f",)` stops at its first failure (one kills a mutant)."""
    env = {**os.environ, "CLI_TOOL_UNDER_TEST": gate, "SPEC_HANG_BOUND": "20",
           takes_skip_gate_test.TEMPLATE_ENV: template}
    done = subprocess.run([sys.executable, TEST, *failfast], env=env, capture_output=True, text=True, timeout=600)
    return done.returncode, done.stdout + done.stderr


class TakesSkipGateMutationTest(unittest.TestCase):
    def test_the_gate_test_fails_against_every_mutant_and_passes_against_the_gate(self) -> None:
        files = Files.here()
        source = files.read_text(GATE)
        with files.scratch("takes-skip-mutants-") as work:
            paths: Dict[str, str] = {}
            for i, (name, (frm, to)) in enumerate(MUTANTS.items()):
                with self.subTest(aimed=name):
                    self.assertEqual(1, source.count(frm), f"the mutant '{name}' is not aimed at exactly one check")
                sandbox_closure.copy(files, ROOT, f"{work}/mutant-{i}", [GATE_PATH])
                paths[name] = f"{work}/mutant-{i}/{GATE_PATH}"
                files.replace_text(paths[name], source.replace(frm, to, 1), 0o755)
            # the scratch repository every run reads, built ONCE here and cloned per run; the unmutated gate's run goes
            # in the same pool (it is the slowest of all, and has no mutant to wait for)
            template = takes_skip_gate_test.Shared(f"{work}/template").dir
            with governed_workers("mutation-tests", 4) as width, ThreadPoolExecutor(max_workers=width) as pool:
                gate_run = pool.submit(run_test, GATE, template)
                results = dict(zip(paths, pool.map(lambda path: run_test(path, template, ("-f",)), paths.values())))
                gate_code, gate_output = gate_run.result()
        for name, (code, output) in results.items():
            with self.subTest(mutant=name):
                self.assertNotEqual(0, code, f"takes_skip_gate_test.py passed against the mutant '{name}':\n{output}")
                # one failing test kills a mutant, and its run stops there (-f): the job stays inside its timeout
                self.assertRegex(output, r"(?m)^FAILED \((failures|errors)=1\)$", output)
        self.assertEqual(0, gate_code, f"takes_skip_gate_test.py fails against the gate itself:\n{gate_output}")


if __name__ == "__main__":
    unittest.main()
