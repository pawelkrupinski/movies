#!/usr/bin/env python3
"""scripts/ci/lane-policy.tsv: the ci-runner lane policy -- where a job's runner lane ranks -- is well-formed, obeys its
own rules (classes in order, a consumer just below its producer, no priority inversion), and runner_scheduler.py serves
its lanes in the policy's order. A lane added to the scheduler without its policy row, or out of the policy's order,
fails here.

    python3 scripts/ci/lane_policy_test.py
"""
# script-test-reads: scripts/ci/lane-policy.tsv scripts/ci/runner_scheduler.py
import csv
import os
import sys
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import runner_scheduler as rs  # noqa: E402

POLICY = os.path.join(HERE, "lane-policy.tsv")
CLASSES = ("red-fix", "landing-check", "deploy-feeding", "optional-measurement", "consumer", "exploration")
COLUMNS = ["runner", "rank", "lane", "class", "producer", "yields_to_landing", "decided"]


def policy_rows():
    with open(POLICY) as f:
        rows = list(csv.reader((line for line in f if line.strip() and not line.startswith("#")), delimiter="\t"))
    assert rows[0] == COLUMNS, rows[0]
    return [dict(zip(COLUMNS, r)) for r in rows[1:]]


class LanePolicyTest(unittest.TestCase):
    def setUp(self):
        self.rows = policy_rows()
        self.by_lane = {r["lane"]: r for r in self.rows}

    def runner(self, runner):
        return sorted((r for r in self.rows if r["runner"] == runner), key=lambda r: int(r["rank"]))

    def test_every_row_is_well_formed(self):
        self.assertEqual(len(self.by_lane), len(self.rows), "a lane is listed twice")
        for r in self.rows:
            self.assertEqual(len(r), len(COLUMNS), r)
            self.assertIn(r["runner"], rs.LANES, r)
            self.assertIn(r["class"], CLASSES, r)
            self.assertIn(r["yields_to_landing"], ("yes", "no"), r)
            self.assertTrue(r["decided"].strip(), f"{r['lane']}: every row says who decided it, when")
            self.assertEqual(r["producer"] != "-", r["class"] == "consumer", f"{r['lane']}: only a consumer names a producer")
        for runner in rs.LANES:
            self.assertEqual([int(r["rank"]) for r in self.runner(runner)], list(range(1, len(self.runner(runner)) + 1)))

    def test_classes_rank_in_the_policys_order_and_a_consumer_sits_just_below_its_producer(self):
        for runner in rs.LANES:
            rows = self.runner(runner)
            order = []
            for i, r in enumerate(rows):
                if r["class"] == "consumer":
                    producer = self.by_lane.get(r["producer"])
                    self.assertIsNotNone(producer, f"{r['lane']}: its producer {r['producer']} has no row")
                    self.assertEqual(producer["runner"], runner)
                    self.assertEqual(int(r["rank"]), int(producer["rank"]) + 1,
                                     f"{r['lane']}: a consumer ranks JUST below its producer {producer['lane']}")
                    order.append(CLASSES.index(producer["class"]))
                else:
                    order.append(CLASSES.index(r["class"]))
            self.assertEqual(order, sorted(order), f"{runner}: classes out of the policy's order")

    def test_no_priority_inversion(self):
        for r in self.rows:
            if r["class"] == "consumer" and r["yields_to_landing"] == "yes":
                self.assertEqual(self.by_lane[r["producer"]]["yields_to_landing"], "yes",
                                 f"{r['lane']} cannot start while a landing check waits, but its producer can")

    def test_the_scheduler_serves_the_policys_lanes_in_its_order(self):
        """Every lane the scheduler serves has its policy row, and the scheduler's order is the policy's (a policy row
        whose lane the scheduler does not serve yet is a decided placement on its way)."""
        for runner, lanes in rs.LANES.items():
            policy = [r["lane"] for r in self.runner(runner)]
            for lane in lanes:
                self.assertIn(lane, policy, f"{runner} lane {lane} has no row in lane-policy.tsv")
            self.assertEqual(list(lanes), [x for x in policy if x in lanes], f"{runner}: the scheduler's order")
        self.assertEqual(self.by_lane[rs.EXPLORE_LANE]["class"], "exploration")


if __name__ == "__main__":
    unittest.main()
