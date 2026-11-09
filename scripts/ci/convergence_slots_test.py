#!/usr/bin/env python3
"""convergence_slots.py: main's convergence and takes rows fit in 20 runners together -- each leg's rows longest first
under a `max-parallel` its share of the 20, the shares chosen so the slowest leg of either suite ends as early as it can --
and every row the workflows run has a measured cost.

    python3 scripts/ci/convergence_slots_test.py
"""
import json
import os
import sys
import tempfile
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import convergence_reuse as reuse  # noqa: E402
import convergence_slots as slots  # noqa: E402

with open(os.path.join(slots.ROOT, reuse.WORKFLOW)) as f:
    WORKFLOW = f.read()


class ScheduleTest(unittest.TestCase):
    def test_rows_start_longest_first(self):
        costs = {("xx", "a"): 1.0, ("xx", "b"): 9.0, ("xx", "c"): 5.0}
        self.assertEqual(slots.longest_first("xx", ["a", "b", "c"], costs), ["b", "c", "a"])

    def test_equal_costs_keep_the_workflows_order(self):
        costs = {("xx", "a"): 2.0, ("xx", "b"): 2.0}
        self.assertEqual(slots.longest_first("xx", ["b", "a"], costs), ["b", "a"])

    def test_the_takes_base_row_starts_just_before_the_takes_row_that_waits_for_it(self):
        costs = {("xx", "takes"): 8.0, ("xx", "takes-base"): 1.0, ("xx", "a"): 9.0, ("xx", "b"): 5.0}
        self.assertEqual(slots.longest_first("xx", ["a", "takes", "takes-base", "b"], costs),
                         ["a", "takes-base", "takes", "b"])
        # a run without the takes row orders the base row by its own cost
        self.assertEqual(slots.longest_first("xx", ["takes-base", "b"], costs), ["b", "takes-base"])

    def test_a_row_starts_on_the_first_slot_free_as_max_parallel_releases_it(self):
        # two slots: 9 | 5 then 4 (at 5) then 1 (at 9) -> ends at 10
        self.assertEqual(slots.makespan([9.0, 5.0, 4.0, 1.0], 2), 10.0)
        self.assertEqual(slots.makespan([9.0, 5.0, 4.0, 1.0], 1), 19.0)
        self.assertEqual(slots.makespan([9.0, 5.0], 8), 9.0)

    def test_an_unmeasured_row_weighs_the_default(self):
        self.assertEqual(slots.cost("xx", "new-row", {}), slots.UNMEASURED_MINUTES)


class ShareTest(unittest.TestCase):
    COSTS = {("aa", p): 10.0 for p in "pqrs"} | {("bb", "p"): 3.0, ("bb", "q"): 3.0}

    def test_the_slowest_country_gets_the_next_slot_and_the_total_never_passes_the_cap(self):
        plan = slots.plan({"aa": ["p", "q", "r", "s"], "bb": ["p", "q"]}, self.COSTS, total=3)
        self.assertEqual((plan["aa"]["max-parallel"], plan["bb"]["max-parallel"]), (2, 1))
        self.assertEqual(plan["aa"]["minutes"], 20.0)
        self.assertEqual(plan["bb"]["minutes"], 6.0)

    def test_no_country_holds_more_slots_than_rows(self):
        plan = slots.plan({"aa": ["p", "q", "r", "s"], "bb": ["p", "q"]}, self.COSTS, total=20)
        self.assertEqual((plan["aa"]["max-parallel"], plan["bb"]["max-parallel"]), (4, 2))

    def test_a_country_after_its_rows_adds_its_tail(self):
        # the order comparison waits for every row of its country
        plan = slots.plan({"aa": ["p"]}, {("aa", "p"): 4.0}, total=2, tails={"aa": 1.5})
        self.assertEqual(plan["aa"]["minutes"], 5.5)

    def test_suites_side_by_side_share_one_pool_each_slot_to_the_leg_that_ends_last(self):
        costs = {("pl", "a"): 10.0, ("pl", "b"): 10.0, ("pl", "c"): 10.0, ("pl", "t"): 4.0, ("pl", "u"): 4.0}
        plans = slots.plan_suites({"convergence": {"pl": ["a", "b", "c"]}, "takes": {"pl": ["t", "u"]}}, costs, total=4)
        self.assertEqual((plans["convergence"]["pl"]["max-parallel"], plans["takes"]["pl"]["max-parallel"]), (3, 1))
        self.assertEqual(slots.plan({"pl": ["a", "b", "c"]}, costs, total=3)["pl"]["max-parallel"], 3)

    def test_every_country_gets_a_slot_even_over_the_cap(self):
        plan = slots.plan({c: ["p"] for c in ("aa", "bb", "cc")}, {}, total=2)
        self.assertEqual([plan[c]["max-parallel"] for c in ("aa", "bb", "cc")], [1, 1, 1])


class MainsRunTest(unittest.TestCase):
    """The plan main's two suites hand their legs (convergence_reuse.py legs): the same rows the leg's expression expands
    to, both suites together in 20 runners (the owner, 2026-11-08: Identity takes split from Identity model convergence,
    side by side on one account)."""

    def matrix(self, inputs, order_phases="both", suite="convergence", harness_aa=False):
        return reuse.planned_matrix(reuse.matrix_rows(WORKFLOW), inputs, order_phases, harness_aa, suite)

    def runners(self, *matrices):
        return sum(r["max-parallel"] for m in matrices for r in m["include"])

    def test_main_hands_every_leg_of_both_suites_its_rows_longest_first_within_20_runners_together(self):
        costs = slots.read_costs()
        compares = {"order-independence-p01", "order-independence-p12"}
        matrices = {}
        for suite in reuse.SUITES:
            matrices[suite] = self.matrix(reuse.leg_inputs({}), suite=suite)
            rows = []
            for r in matrices[suite]["include"]:
                phases = json.loads(r["phases"])
                self.assertEqual(phases, slots.longest_first(r["code"], phases, costs))
                rows += [f"{p} ({r['country']})" for p in phases]
            self.assertEqual(sorted(rows), sorted(r for r in reuse.expected_rows(WORKFLOW, suite)
                                                  if r.split(" (")[0] not in compares))
        self.assertEqual(self.runners(*matrices.values()), slots.RUNNERS)

    def test_the_two_suites_share_one_pool_not_a_fixed_split(self):
        """A fixed 10 and 10 ends convergence at ~26 min; one pool of 20 legs no later, each slot to the leg that ends last."""
        costs = slots.read_costs()
        rows = reuse.matrix_rows(WORKFLOW)
        plans = reuse.main_plan(rows, reuse.leg_inputs({}))
        joint = max(slots.ends_at(p) for p in plans.values())
        phases = {s: {r["code"]: reuse.matrix_phases(r, suite=s) for r in rows} for s in reuse.SUITES}
        for n in range(len(rows), slots.RUNNERS - len(rows) + 1):
            fixed = max(slots.ends_at(slots.plan(phases["convergence"], costs, total=n,
                                                 tails=reuse.suite_tails(rows, "convergence", costs=costs))),
                        slots.ends_at(slots.plan(phases["takes"], costs, total=slots.RUNNERS - n)))
            self.assertLessEqual(joint, fixed, f"a fixed split {n}/{slots.RUNNERS - n} ends sooner")

    def test_spain_splits_its_order_rows_like_the_other_countries(self):
        # owner 2026-11-08: Spain's order passes ran inside its boot, so the leg's `order-compare` job was created and skipped,
        # a skipped matrix job rendering its `name:` template raw (`${{ matrix.phase }}`)
        matrix = self.matrix(reuse.leg_inputs({}))
        es = next(r for r in matrix["include"] if r["code"] == "es")
        self.assertIn(es["order-split"], (True, "true"))
        self.assertEqual(es["cmd"], "convergenceSpainWithoutOrder")
        self.assertEqual(es["order"], "convergenceSpainOrder")
        self.assertLessEqual({"order-independence-p01-pass0", "order-independence-p01-pass1",
                              "order-independence-p12-pass1", "order-independence-p12-pass2"}, set(json.loads(es["phases"])))

    def test_one_order_phase_hands_only_the_p01_passes(self):
        matrix = self.matrix(reuse.leg_inputs({}), order_phases="one")
        de = next(r for r in matrix["include"] if r["code"] == "de")
        self.assertNotIn("order-independence-p12-pass1", json.loads(de["phases"]))
        self.assertIn("order-independence-p01-pass1", json.loads(de["phases"]))

    def test_the_harness_a_a_rows_are_the_takes_suites_planned_when_the_preflight_decided_them(self):
        takes = self.matrix(reuse.leg_inputs({}), suite="takes", harness_aa=True)
        conv = self.matrix(reuse.leg_inputs({}), suite="convergence", harness_aa=True)
        for r in takes["include"]:
            self.assertLessEqual({"takes-drift-aa-pass0", "takes-drift-aa-pass1", "takes-aa"}, set(json.loads(r["phases"])))
            self.assertNotIn("takes-drift-aa", json.loads(r["phases"]), "the comparison is its own job's, after the rows")
        for r in conv["include"]:
            self.assertFalse([p for p in json.loads(r["phases"]) if p.startswith("takes")])
        self.assertLessEqual(self.runners(takes, conv), slots.RUNNERS)
        self.assertEqual(slots.cost("xx", "takes-drift-aa-pass1", {("xx", "takes-drift-1"): 10.0}), 10.0)

    def test_the_a_a_comparison_after_the_rows_is_every_takes_legs_tail(self):
        plan = reuse.leg_plan(reuse.matrix_rows(WORKFLOW), reuse.leg_inputs({}), harness_aa=True, suite="takes")
        costs = slots.read_costs()
        es = [slots.cost("es", p, costs) for p in plan["es"]["phases"]]
        self.assertEqual(plan["es"]["minutes"],
                         round(slots.makespan(es, plan["es"]["max-parallel"]) + slots.AA_COMPARE_MINUTES, 1))

    def test_a_takes_only_run_keeps_the_legs_own_expression_and_no_cap(self):
        matrix = self.matrix(reuse.leg_inputs({"INPUT_TAKES_ONLY": "true"}), suite="takes")
        self.assertEqual({(r["phases"], r["max-parallel"]) for r in matrix["include"]}, {("", slots.UNCAPPED)})

    def test_a_drift_only_run_plans_every_drift_row_a_country_has_and_a_pinned_one_its_pinned_rows(self):
        matrix = self.matrix(reuse.leg_inputs({"INPUT_DRIFT_ONLY": "true"}), suite="takes")
        for r in matrix["include"]:
            self.assertEqual(sorted(json.loads(r["phases"])),
                             sorted(f"takes-drift-{n}" for n in reuse.drift_seeds.rows_of(r["code"])))
        self.assertLessEqual(self.runners(matrix), slots.RUNNERS)
        pinned = dict(reuse.leg_inputs({"INPUT_DRIFT_ONLY": "true"}), **{"drift-seed-1": "", "drift-seed-2": "40"})
        self.assertEqual({r["phases"] for r in self.matrix(pinned, suite="takes")["include"]}, {'["takes-drift-2"]'})

    def test_no_drift_row_of_main_is_slower_than_the_other_rows_end(self):
        """Germany's seeds are spread over rows so its slowest drift row ends no later than the rest of the run."""
        costs = slots.read_costs()
        plans = reuse.main_plan(reuse.matrix_rows(WORKFLOW), reuse.leg_inputs({}))
        drift = max(slots.cost(c, p, costs) for c, s in plans["takes"].items() for p in s["phases"] if p.startswith("takes-drift-"))
        others = max(slots.cost(c, p, costs) for plan in plans.values() for c, s in plan.items() for p in s["phases"]
                     if not p.startswith("takes-drift-"))
        self.assertLessEqual(drift, max(others, 20.0))

    def test_every_row_main_runs_has_a_measured_cost(self):
        costs = slots.read_costs()
        codes = dict(reuse.countries(WORKFLOW))
        missing = [r for suite in reuse.SUITES for r in reuse.expected_rows(WORKFLOW, suite)
                   if (codes[r.split(" (")[1][:-1]], r.split(" (")[0]) not in costs]
        self.assertEqual(missing, [])


class CostsTest(unittest.TestCase):
    def test_costs_are_each_rows_median_minutes_over_the_runs_jobs(self):
        def job(name, start, end, conclusion="success"):
            return {"name": f"leg (poland) / {name}", "conclusion": conclusion,
                    "started_at": f"2026-11-06T10:{start:02d}:00Z", "completed_at": f"2026-11-06T10:{end:02d}:00Z"}
        runs = [[job("takes (poland)", 0, 4), job("convergence (poland)", 0, 6)],
                [job("takes (poland)", 0, 6), job("convergence (poland)", 0, 6, "skipped")],
                [job("takes (poland)", 0, 10)]]
        self.assertEqual(slots.costs_of(runs, {"poland": "pl"}),
                         {("pl", "takes"): (6.0, 3), ("pl", "convergence"): (6.0, 1)})

    def test_the_costs_file_round_trips(self):
        path = os.path.join(tempfile.mkdtemp(), "c.tsv")
        slots.write_costs(path, {("pl", "takes"): (6.0, 3)}, ["1", "2"])
        self.assertEqual(slots.read_costs(path), {("pl", "takes"): 6.0})


if __name__ == "__main__":
    unittest.main()
