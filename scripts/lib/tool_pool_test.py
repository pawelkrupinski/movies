#!/usr/bin/env python3
"""tool_pool.governed_workers (THE GOVERNED FAN-OUT for a Python tool) against a stub governor: the pool's ticket is
taken charged its width and held for exactly the block, its workers see one core each, a pool under a ticket already
takes none, and a refusing governor leaves it one wide."""
from __future__ import annotations

import os
import subprocess
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import tool_pool  # noqa: E402

STUB = """#!/usr/bin/env bash
[ "$1 $2" = "gov probe" ] && { echo "agentctl governor: admit cores route width"; exit 0; }
printf '%s\\n' "$*" >> "$GOV_CALLS"
[ -n "${GOV_REFUSE:-}" ] && exit 1
exec python3 -I -B -c 'import fcntl; fcntl.flock(7, fcntl.LOCK_EX | fcntl.LOCK_NB)'
"""
KEEP_OUT = ("AGENTCTL_GOV_WIDTH", "KINOWO_GOVERNOR_FEATURES", "KINOWO_JVM_SLOT_HELD", "KINOWO_GOV_CPU_HELD",
            "KINOWO_GOV_CLASS")


def lock_held(path: str) -> bool:
    import fcntl
    with open(path, "a") as f:
        try:
            fcntl.flock(f, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except OSError:
            return True
        return False


class GovernedWorkersTest(unittest.TestCase):
    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        self.d = self.tmp.name
        self.calls = os.path.join(self.d, "calls")
        gov = os.path.join(self.d, "agentctl")
        with open(gov, "w") as f:
            f.write(STUB)
        os.chmod(gov, 0o755)
        self.saved = dict(os.environ)
        for k in KEEP_OUT:
            os.environ.pop(k, None)
        os.environ.update(KINOWO_JVM_GATE_FOLLOW_MAIN="0", KINOWO_GOVERNOR="1", KINOWO_GOVERNOR_BIN=gov,
                          KINOWO_JVM_GATE_OS="Darwin",
                          AGENTCTL_GOV_DIR=os.path.join(self.d, "gov"), GOV_CALLS=self.calls)

    def tearDown(self) -> None:
        os.environ.clear()
        os.environ.update(self.saved)
        self.tmp.cleanup()

    def tickets(self) -> list[str]:
        d = os.path.join(self.d, "gov")
        return [os.path.join(d, p) for p in os.listdir(d) if p.endswith(".lock")] if os.path.isdir(d) else []

    def calls_made(self) -> list[str]:
        return open(self.calls).read().splitlines() if os.path.exists(self.calls) else []

    def test_a_pool_holds_a_ticket_charged_its_width_for_exactly_its_block(self) -> None:
        with tool_pool.governed_workers("mutation-tests", 4) as width:
            self.assertEqual(width, 4)
            [ticket] = self.tickets()
            self.assertTrue(lock_held(ticket), "the ticket is held while the pool runs")
            seen = subprocess.run(["sh", "-c", "echo $AGENTCTL_GOV_WIDTH"], capture_output=True, text=True).stdout
            self.assertEqual(seen.strip(), "1", "each worker one core of it")
        self.assertFalse(lock_held(ticket), "given back once the pool ends")
        self.assertNotIn("AGENTCTL_GOV_WIDTH", os.environ, "the environment restored")
        [call] = [c for c in self.calls_made() if c.startswith("gov admit")]
        self.assertIn("--job mutation-tests --cores 4", call)

    def test_a_pool_whose_children_burn_cores_records_them_not_its_width(self) -> None:
        os.environ["KINOWO_CPU_METER_POLL"] = "0.1"
        burn = "import time; t = time.process_time()\nwhile time.process_time() - t < 1.0: pass"
        with tool_pool.governed_workers("mutation-tests", 1):
            workers = [subprocess.Popen([sys.executable, "-c", burn]) for _ in range(3)]
            for worker in workers:
                worker.wait()
        [usage] = [c.split() for c in self.calls_made() if c.startswith("gov usage")]
        self.assertEqual(["--job", "mutation-tests"], usage[2:4])
        cores = int(usage[usage.index("--cpu") + 1]) / max(1, int(usage[usage.index("--wall") + 1]))
        self.assertGreaterEqual(cores, 2.0, "three busy children: about 3 cores, though it asked for 1")
        self.assertEqual("tree", usage[usage.index("--meter") + 1])

    def test_under_a_ticket_it_takes_none_and_runs_at_most_its_width(self) -> None:
        os.environ["AGENTCTL_GOV_WIDTH"] = "2"
        with tool_pool.governed_workers("mutation-tests", 4) as width:
            self.assertEqual(width, 2)
        self.assertEqual(self.calls_made(), [])
        self.assertEqual(os.environ["AGENTCTL_GOV_WIDTH"], "2", "the caller's width restored")

    def test_a_refusing_governor_leaves_it_one_wide(self) -> None:
        os.environ["GOV_REFUSE"] = "1"
        with tool_pool.governed_workers("mutation-tests", 4) as width:
            self.assertEqual(width, 1)

    def test_no_governor_keeps_its_width(self) -> None:
        os.environ["KINOWO_GOVERNOR"] = "0"
        with tool_pool.governed_workers("mutation-tests", 4) as width:
            self.assertEqual(width, 4)
        self.assertEqual(self.calls_made(), [])


if __name__ == "__main__":
    unittest.main()
