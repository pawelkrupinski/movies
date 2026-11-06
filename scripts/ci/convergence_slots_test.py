#!/usr/bin/env python3
"""convergence_slots.py: main's convergence rows fit in 20 runners -- each country's rows longest first under a
`max-parallel` its share of the 20, the shares chosen so the slowest country ends as early as it can -- and every row the
workflow runs has a measured cost.

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

    def test_every_country_gets_a_slot_even_over_the_cap(self):
        plan = slots.plan({c: ["p"] for c in ("aa", "bb", "cc")}, {}, total=2)
        self.assertEqual([plan[c]["max-parallel"] for c in ("aa", "bb", "cc")], [1, 1, 1])


class MainsRunTest(unittest.TestCase):
    """The plan main's run hands the legs (convergence_reuse.py legs): the same rows the leg's expression expands to, in
    20 runners."""

    def matrix(self, inputs, order_phases="both"):
        return reuse.planned_matrix(reuse.matrix_rows(WORKFLOW), inputs, order_phases)

    def test_main_hands_every_leg_its_rows_longest_first_within_20_runners(self):
        matrix = self.matrix(reuse.leg_inputs({}))
        costs = slots.read_costs()
        self.assertLessEqual(sum(r["max-parallel"] for r in matrix["include"]), slots.RUNNERS)
        rows = []
        for r in matrix["include"]:
            phases = json.loads(r["phases"])
            self.assertEqual(phases, slots.longest_first(r["code"], phases, costs))
            rows += [f"{p} ({r['country']})" for p in phases]
        compares = {"order-independence-p01", "order-independence-p12"}
        self.assertEqual(sorted(rows), sorted(r for r in reuse.expected_rows(WORKFLOW) if r.split(" (")[0] not in compares))

    def test_one_order_phase_hands_only_the_p01_passes(self):
        matrix = self.matrix(reuse.leg_inputs({}), order_phases="one")
        de = next(r for r in matrix["include"] if r["code"] == "de")
        self.assertNotIn("order-independence-p12-pass1", json.loads(de["phases"]))
        self.assertIn("order-independence-p01-pass1", json.loads(de["phases"]))

    def test_the_harness_a_a_rows_are_planned_when_the_preflight_decided_them_each_drift_pass_one_replay(self):
        matrix = reuse.planned_matrix(reuse.matrix_rows(WORKFLOW), reuse.leg_inputs({}), "both", harness_aa=True)
        for r in matrix["include"]:
            self.assertLessEqual({"takes-drift-aa-pass0", "takes-drift-aa-pass1", "takes-aa"}, set(json.loads(r["phases"])))
            self.assertNotIn("takes-drift-aa", json.loads(r["phases"]), "the comparison is order-compare's, after the rows")
        self.assertLessEqual(sum(r["max-parallel"] for r in matrix["include"]), slots.RUNNERS)
        self.assertEqual(slots.cost("xx", "takes-drift-aa-pass1", {("xx", "takes-drift-1"): 10.0}), 10.0)

    def test_the_a_a_comparison_after_the_rows_is_every_a_a_countrys_tail(self):
        # Spain splits no order row: the A/A comparison is its only tail
        plan = reuse.leg_plan(reuse.matrix_rows(WORKFLOW), reuse.leg_inputs({}), harness_aa=True)
        bare = reuse.leg_plan(reuse.matrix_rows(WORKFLOW), reuse.leg_inputs({}))
        costs = slots.read_costs()
        es = [slots.cost("es", p, costs) for p in plan["es"]["phases"]]
        self.assertEqual(plan["es"]["minutes"],
                         round(slots.makespan(es, plan["es"]["max-parallel"]) + slots.AA_COMPARE_MINUTES, 1))
        self.assertIn("es", bare)

    def test_a_takes_only_run_keeps_the_legs_own_expression_and_no_cap(self):
        matrix = self.matrix(reuse.leg_inputs({"INPUT_TAKES_ONLY": "true"}))
        self.assertEqual({(r["phases"], r["max-parallel"]) for r in matrix["include"]}, {("", slots.UNCAPPED)})

    def test_a_drift_only_run_plans_every_drift_row_a_country_has_and_a_pinned_one_its_pinned_rows(self):
        matrix = self.matrix(reuse.leg_inputs({"INPUT_DRIFT_ONLY": "true"}))
        for r in matrix["include"]:
            self.assertEqual(sorted(json.loads(r["phases"])),
                             sorted(f"takes-drift-{n}" for n in reuse.drift_seeds.rows_of(r["code"])))
        pinned = dict(reuse.leg_inputs({"INPUT_DRIFT_ONLY": "true"}), **{"drift-seed-1": "", "drift-seed-2": "40"})
        self.assertEqual({r["phases"] for r in self.matrix(pinned)["include"]}, {'["takes-drift-2"]'})

    def test_no_drift_row_of_main_is_slower_than_the_other_countries_end(self):
        """Germany's seeds are spread over rows so its slowest drift row ends no later than the rest of the run."""
        costs = slots.read_costs()
        plan = reuse.leg_plan(reuse.matrix_rows(WORKFLOW), reuse.leg_inputs({}))
        drift = max(slots.cost(c, p, costs) for c, s in plan.items() for p in s["phases"] if p.startswith("takes-drift-"))
        others = max(slots.cost(c, p, costs) for c, s in plan.items() for p in s["phases"]
                     if not p.startswith("takes-drift-"))
        self.assertLessEqual(drift, max(others, 20.0))

    def test_every_row_main_runs_has_a_measured_cost(self):
        costs = slots.read_costs()
        codes = dict(reuse.countries(WORKFLOW))
        missing = [r for r in reuse.expected_rows(WORKFLOW)
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
