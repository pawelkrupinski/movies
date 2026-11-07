"""identity_run_shards: identity-run.yml's plan of jobs -- one a country for a command naming {cc}, one a line of a
command of several lines, and the artifact layout of the one-job run they replace.

Run: python3 scripts/ci/identity_run_shards_test.py
"""
import json
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import identity_run_shards as shards  # noqa: E402


class PlanTest(unittest.TestCase):
    def test_a_command_naming_its_country_runs_once_a_country_side_by_side(self):
        plan = shards.plan("worker/IntegrationTest/runMain x --out {out} {cc}", "de,uk")
        self.assertEqual([(s["name"], s["cc"], s["out"], s["log"]) for s in plan],
                         [("de", "de", "de", "sbt-de.log"), ("uk", "uk", "uk", "sbt-uk.log")])
        self.assertEqual({s["command"] for s in plan}, {"worker/IntegrationTest/runMain x --out {out} {cc}"})

    def test_a_command_naming_no_country_runs_once_as_before(self):
        self.assertEqual(shards.plan("worker/testOnly deploy.*", "pl,de,uk,us,es"),
                         [{"name": "all", "cc": "", "out": "", "log": "sbt.log", "command": "worker/testOnly deploy.*"}])

    def test_each_line_of_a_command_is_a_job_of_its_own(self):
        # run 42370669779: `runMain KeptListingsBench de ...; runMain KeptListingsBench uk ...` ran one after the other
        plan = shards.plan("runMain Bench de --older a\n\n  runMain Bench uk --older b  \n", "de,uk")
        self.assertEqual([(s["name"], s["cc"], s["out"], s["log"], s["command"]) for s in plan],
                         [("1", "", "", "sbt-1.log", "runMain Bench de --older a"),
                          ("2", "", "", "sbt-2.log", "runMain Bench uk --older b")])

    def test_lines_naming_their_country_shard_each_country_of_each_line(self):
        plan = shards.plan("a {cc}\nb", "de,uk")
        self.assertEqual([(s["name"], s["cc"], s["out"], s["log"]) for s in plan],
                         [("1-de", "de", "1-de", "sbt-1-de.log"), ("1-uk", "uk", "1-uk", "sbt-1-uk.log"),
                          ("2", "", "", "sbt-2.log")])

    def test_countries_tolerate_spaces_and_empty_entries(self):
        self.assertEqual([s["cc"] for s in shards.plan("x {cc}", " de, ,uk,")], ["de", "uk"])

    def test_no_command_is_no_plan(self):
        with self.assertRaises(ValueError):
            shards.plan(" \n ", "de")

    def test_a_country_command_with_no_country_is_no_plan(self):
        with self.assertRaises(ValueError):
            shards.plan("x {cc}", " , ")

    def test_the_cli_prints_the_matrix_as_one_json_line(self):
        out = shards.main(["--command", "x {cc}", "--countries", "de"])
        self.assertEqual(json.loads(out), [{"name": "de", "cc": "de", "out": "de", "log": "sbt-de.log", "command": "x {cc}"}])
        self.assertNotIn("\n", out)


if __name__ == "__main__":
    unittest.main()
