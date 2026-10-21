#!/usr/bin/env python3
"""Unit tests for identity_rule_ablation.py: which rules are ablated, how they are batched, and how a round's outcomes
settle rules, split movers and name interactions.

    python3 scripts/identity_rule_ablation_test.py
"""
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import identity_rule_ablation as ablation  # noqa: E402


def outcome(moved=0, **buckets):
    o = ablation.empty_outcome()
    o["moved"] = moved
    o.update(buckets)
    return o


class RulesTest(unittest.TestCase):
    def test_a_veto_naming_a_film_or_feed_is_ablated_as_its_family(self):
        self.assertEqual(ablation.family("veto:vetoed-not-released-in-gb-as-namesake-1382513-is"),
                         "veto:vetoed-not-released-in-gb-as-namesake-*")
        self.assertEqual(ablation.family("veto:named-only-by-the-title-its-feed-p-https-www-kinoheld-de-film-x-copied"),
                         "veto:named-only-by-the-title-its-feed-*")
        self.assertEqual(ablation.family("veto:seasonsapart"), "veto:seasonsapart")

    def test_identity_rules_leave_out_learned_rules_and_reasons_and_read_a_pooled_rule_as_its_acceptance(self):
        rules = ablation.identity_rules({"accept:sole-work": 3, "pooled:unrivalled-calibrated": 2, "veto:learned-runtime-gap-36": 9,
                                         "apart:learned-x": 1, "refused:sole-work:none": 5, "format:2D": 4, "title:x": 1,
                                         "veto:vetoed-not-released-in-gb-as-namesake-1-is": 1,
                                         "veto:vetoed-not-released-in-gb-as-namesake-2-is": 2})
        self.assertEqual(rules, {"accept:sole-work": 3, "accept:unrivalled-calibrated": 2,
                                 "veto:vetoed-not-released-in-gb-as-namesake-*": 3})


class BatchesTest(unittest.TestCase):
    def test_rules_are_batched_by_kind_countries_and_whether_they_fire(self):
        countries = {"title:a": {"pl"}, "title:b": {"pl"}, "title:c": {"pl"}, "title:d": None, "veto:x": None}
        out = ablation.batches(countries, countries.get, {"title:a": 1, "title:b": 4}, hot_size=1, cold_size=10)
        self.assertEqual([(c["rules"], c["countries"]) for c in out],
                         [(["title:d"], None), (["title:c"], ["pl"]), (["title:a"], ["pl"]), (["title:b"], ["pl"]), (["veto:x"], None)])
        self.assertEqual(len({c["id"] for c in out}), len(out))


class NextRoundTest(unittest.TestCase):
    def test_a_batch_moving_nothing_settles_every_rule_and_a_mover_splits_until_one_rule(self):
        configs = [{"id": "b001", "rules": ["r1", "r2"], "countries": None},
                   {"id": "b002", "rules": ["r3", "r4", "r5"], "countries": ["pl"]},
                   {"id": "b003", "rules": ["r6"], "countries": None}]
        results = {"b001": outcome(), "b002": outcome(2), "b003": outcome(1, cure=1)}
        nxt, settled, interactions = ablation.next_round(results, configs)
        self.assertEqual(sorted(settled), ["r1", "r2", "r6"])
        self.assertEqual(settled["r6"]["cure"], 1)
        self.assertEqual([(c["id"], c["rules"], c["countries"]) for c in nxt],
                         [("b002a", ["r3"], ["pl"]), ("b002b", ["r4", "r5"], ["pl"])])
        self.assertEqual(interactions, [])

    def test_two_halves_moving_nothing_under_a_moving_parent_are_an_interaction(self):
        parent = outcome(3)
        configs = [{"id": "b002a", "rules": ["r3"], "countries": None, "parentMoved": parent},
                   {"id": "b002b", "rules": ["r4"], "countries": None, "parentMoved": parent}]
        _, settled, interactions = ablation.next_round({"b002a": outcome(), "b002b": outcome()}, configs)
        self.assertEqual(interactions, [(["r3", "r4"], parent)])
        self.assertEqual(sorted(settled), ["r3", "r4"])


class GateTest(unittest.TestCase):
    def test_an_executable_runs_the_command_and_any_other_path_is_a_lockf_file(self):
        import tempfile
        with tempfile.TemporaryDirectory() as d:
            runner, lock = os.path.join(d, "heavy-run"), os.path.join(d, "heavy.lock")
            open(runner, "w").close()
            os.chmod(runner, 0o755)
            self.assertEqual(ablation.gate(runner), [runner])
            self.assertEqual(ablation.gate(lock), ["lockf", "-t", "7200", lock])
            self.assertEqual(ablation.gate(""), [])


class ShardAndMergeTest(unittest.TestCase):
    def test_shards_split_the_first_round_and_merge_back_whole(self):
        configs = [{"id": f"b{i}", "rules": [f"r{i}"]} for i in range(5)]
        shards = [ablation.shard_of(configs, f"{i}/2") for i in range(2)]
        self.assertEqual([[c["id"] for c in s] for s in shards], [["b0", "b2", "b4"], ["b1", "b3"]])
        self.assertEqual(ablation.shard_of(configs, ""), configs)
        merged = ablation.merge([{"round": 3, "settled": {"r0": outcome()}, "interactions": [], "fired": {"r0": 2}, "pending": []},
                                 {"round": 2, "settled": {"r1": outcome(1)}, "interactions": [{"rules": ["a"]}], "fired": {}, "pending": []}])
        self.assertEqual(sorted(merged["settled"]), ["r0", "r1"])
        self.assertEqual((merged["round"], merged["interactions"], merged["fired"]), (3, [{"rules": ["a"]}], {"r0": 2}))

    def test_sbt_runs_against_ci_s_mongodb_when_one_is_named(self):
        old = os.environ.pop("MONGODB_URI", None)
        try:
            self.assertTrue(ablation.sbt_argv("/r", "x")[-2].endswith("scripts/it-local.sh"))
            os.environ["MONGODB_URI"] = "mongodb://127.0.0.1:27017"
            self.assertEqual(ablation.sbt_argv("/r", "x"), ["nice", "-n", "10", "sbt", "x"])
        finally:
            os.environ.pop("MONGODB_URI", None)
            if old is not None:
                os.environ["MONGODB_URI"] = old


class HarmOnlyTest(unittest.TestCase):
    def test_a_rule_does_only_harm_when_removing_it_cures_and_costs_no_right_take(self):
        self.assertTrue(ablation.harm_only(outcome(2, cure=1, unjudged=1)))
        self.assertFalse(ablation.harm_only(outcome(2, cure=1, **{"right-lost": 1})))
        self.assertFalse(ablation.harm_only(outcome(1, **{"gained-right": 1})))


if __name__ == "__main__":
    unittest.main()
