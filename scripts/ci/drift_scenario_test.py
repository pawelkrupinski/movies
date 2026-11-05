#!/usr/bin/env python3
"""drift_scenario.py: head and base meeting the same scenario on a seed pass; a coupled harness -- one random stream
drawing every fill, so the head's one extra question re-deals every later draw (the harness before 3e97bde52) -- is a
HARNESS ERROR, never flips; a question only one side asks is the change's own; listings dealt otherwise leave the
arrangement uncompared; a side with no scenario (a base from before the guard) is not compared.

    python3 scripts/ci/drift_scenario_test.py
"""
import hashlib
import os
import random
import subprocess
import sys
import tempfile
import unittest

SCRIPT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "drift_scenario.py")
HEADER = "seed\tkind\tkey\tvalue\n"
ARRANGEMENT = {"inputs": "aa11", "venues": "bb22", "boot": "cc33", "half-1": "dd44"}


def tsv(rows, header=HEADER):
    path = os.path.join(tempfile.mkdtemp(), "scenario-de.tsv")
    with open(path, "w") as f:
        f.write(header + "".join("\t".join(r) + "\n" for r in rows))
    return path


def keyed(seed, round_, qid, fraction=0.5):
    """Share.filed: the question's own draw."""
    h = int(hashlib.sha256(f"{seed}:{round_}:{qid}".encode()).hexdigest(), 16)
    return h % 1000000 < fraction * 1000000


def scenario(questions, seed=8, coupled=False, arrangement=ARRANGEMENT):
    """A seed's rows as the harness writes them: the arrangement, then one fill round over the questions asked, sorted --
    each drawn by its own id, or (coupled) from one stream shared by every question, the old harness's."""
    rows = [[str(seed), "arrangement", part, digest] for part, digest in arrangement.items()]
    stream = random.Random(seed)
    for q in sorted(questions):
        filed = stream.random() < 0.5 if coupled else keyed(seed, 1, q)
        rows.append([str(seed), "fill", f"1:{q}", "1" if filed else "0"])
    return rows


class ScenarioTest(unittest.TestCase):
    def compare(self, head, base, seed="8"):
        r = subprocess.run([sys.executable, SCRIPT, "compare", "--seed", seed, "--head", head, "--base", base],
                           capture_output=True, text=True)
        return r.returncode, r.stdout

    asked = [f"q:film {n:03d}" for n in range(1, 120)]

    def test_the_same_scenario_passes(self):
        code, out = self.compare(tsv(scenario(self.asked)), tsv(scenario(self.asked)))
        self.assertEqual(code, 0, out)
        self.assertIn("scenario: the same on seed 8", out)

    def test_a_question_only_the_head_asks_is_the_changes_own(self):
        code, out = self.compare(tsv(scenario(["q:aaa new search"] + self.asked)), tsv(scenario(self.asked)))
        self.assertEqual(code, 0, out)
        self.assertIn("119 common fill draw(s), 1 head-only, 0 base-only", out)

    def test_a_coupled_harness_is_a_harness_error_never_flips(self):
        # the old harness: one stream for every draw -- the head's one extra question, sorted first, re-deals the rest
        code, out = self.compare(tsv(scenario(["q:aaa new search"] + self.asked, coupled=True)), tsv(scenario(self.asked, coupled=True)))
        self.assertEqual(code, 1, out)
        self.assertIn("HARNESS ERROR on seed 8", out)
        self.assertIn("fill draw(s) both sides made differ", out)
        self.assertNotIn("flip", out.replace("their flips are not comparable", ""))

    def test_an_arrangement_that_differs_over_the_same_listings_is_a_harness_error(self):
        moved = dict(ARRANGEMENT, **{"half-1": "ee55"})
        code, out = self.compare(tsv(scenario(self.asked, arrangement=moved)), tsv(scenario(self.asked)))
        self.assertEqual(code, 1, out)
        self.assertIn("arrangement `half-1` differs: head ee55, base dd44", out)

    def test_listings_dealt_otherwise_leave_the_arrangement_uncompared(self):
        other = dict(ARRANGEMENT, inputs="ff66", venues="ff77")
        code, out = self.compare(tsv(scenario(self.asked, arrangement=other)), tsv(scenario(self.asked)))
        self.assertEqual(code, 0, out)
        self.assertIn("different listings (inputs): the arrangement is not compared", out)

    def test_a_base_from_before_the_guard_is_not_compared(self):
        code, out = self.compare(tsv(scenario(self.asked)), os.path.join(tempfile.mkdtemp(), "absent.tsv"))
        self.assertEqual(code, 0, out)
        self.assertIn("the base wrote no scenario", out)

    def test_only_the_seed_asked_is_compared(self):
        code, out = self.compare(tsv(scenario(self.asked, seed=9, coupled=True)), tsv(scenario(self.asked)), seed="8")
        self.assertEqual(code, 0, out)

    def test_an_unreadable_scenario_is_no_verdict(self):
        code, out = self.compare(tsv([["8", "fill"]]), tsv(scenario(self.asked)))
        self.assertEqual(code, 2, out)


if __name__ == "__main__":
    unittest.main()
