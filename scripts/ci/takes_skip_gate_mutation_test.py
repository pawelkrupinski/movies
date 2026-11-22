#!/usr/bin/env python3
"""THE GATE FAILS CLOSED, proved by mutation: each mutant below is takes_skip_gate.py with ONE check taken out, and
takes_skip_gate_test.py must FAIL against every one -- a check whose removal no test notices is a check that could
silently stop working and let a takes row be skipped that should run. Each mutant is applied by exact text; a mutant
whose text is not in the gate exactly once fails this test too (the gate changed: re-aim the mutant at the new check).
The gate is standard library only, so a mutant runs from the scratch directory it is written to."""
from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest
from concurrent.futures import ThreadPoolExecutor
from typing import Dict, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
GATE = os.path.join(HERE, "takes_skip_gate.py")
TEST = os.path.join(HERE, "takes_skip_gate_test.py")
# name -> (exact text in the gate, what replaces it)
MUTANTS: Dict[str, Tuple[str, str]] = {
    "the mode is not checked": ('require(mode in ("shadow", "enforce"),', "require(True,"),
    "the proof command is not required": ('require(len(argv) >= 5 and argv[3] == "--",', "require(True,"),
    "commit names are not checked": ("require(bool(FULL_NAME.match(base) and FULL_NAME.match(head)),", "require(True,"),
    "one commit is allowed": ("require(base != head,", "require(True,"),
    "a failed diff reads as no change": ('require(diff.returncode == 0, f"git could not diff {base}',
                                         'require(True, f"git could not diff {base}'),
    "an empty change is allowed": ("require(bool(changed),", "require(True,"),
    "any file is allowed": ("require(inert(f),", "require(True,"),
    "the placeholders count as a title rule": ("if path == PLACEHOLDERS:\n        return False",
                                               "if path == PLACEHOLDERS:\n        return True"),
    "no rule change is needed": ("require(rules > 0,", "require(True,"),
    "identity code is not denied": ("require(not denied,", "require(True,"),
    "changed tooling is allowed": ("require(not diff.stdout.strip(),", "require(True,"),
    "a missing trace is allowed": ("require(os.path.isfile(trace) and os.path.getsize(trace) > 0,", "require(True,"),
    "a broken trace is allowed": ('raise Run(f"the base trace {trace} is not a whole gzip") from None', "lines = 1"),
    "an empty trace is allowed": ("require(lines > 0,", "require(True,"),
    "the proof exit is not read": ("require(rc == 0,", "require(True,"),
    "the proof line is not read": ("require(matched is not None,", "require(True,"),
    "a proof of nothing is allowed": ("require(readings > 0,", "require(True,"),
    "a differ row is ignored": ("require(not (os.path.isfile(differ)", "require(True or (os.path.isfile(differ)"),
    "the proof is unbounded": ("return proc.wait(timeout=timeout)", "return proc.wait()"),
}


def run_test(gate: str) -> Tuple[int, str]:
    """takes_skip_gate_test.py against `gate` -> its exit and output."""
    env = {**os.environ, "CLI_TOOL_UNDER_TEST": gate, "SPEC_HANG_BOUND": "20"}
    done = subprocess.run([sys.executable, TEST], env=env, capture_output=True, text=True, timeout=600)
    return done.returncode, done.stdout + done.stderr


class TakesSkipGateMutationTest(unittest.TestCase):
    def test_the_gate_test_fails_against_every_mutant_and_passes_against_the_gate(self) -> None:
        with open(GATE) as f:
            source = f.read()
        with tempfile.TemporaryDirectory(prefix="takes-skip-mutants-") as work:
            paths: Dict[str, str] = {}
            for i, (name, (frm, to)) in enumerate(MUTANTS.items()):
                with self.subTest(aimed=name):
                    self.assertEqual(1, source.count(frm), f"the mutant '{name}' is not aimed at exactly one check")
                paths[name] = os.path.join(work, f"mutant-{i}.py")
                with open(paths[name], "w") as f:
                    f.write(source.replace(frm, to, 1))
                os.chmod(paths[name], 0o755)
            with ThreadPoolExecutor(max_workers=4) as pool:
                results = dict(zip(paths, pool.map(run_test, paths.values())))
        for name, (code, output) in results.items():
            with self.subTest(mutant=name):
                self.assertNotEqual(0, code, f"takes_skip_gate_test.py passed against the mutant '{name}':\n{output}")
                self.assertRegex(output, r"FAILED \(", output)
        code, output = run_test(GATE)
        self.assertEqual(0, code, f"takes_skip_gate_test.py fails against the gate itself:\n{output}")


if __name__ == "__main__":
    unittest.main()
