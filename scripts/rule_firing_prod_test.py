#!/usr/bin/env python3
"""Unit tests for rule_firing_prod.py: title-rules.tsv and the consolidation inventory read, Prometheus' rule firings
classified into dead in prod / dead in the recordings, every outside call answered by a fake.

    python3 scripts/rule_firing_prod_test.py
"""
import io
import json
import os
import sys
import unittest
from contextlib import redirect_stdout

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import rule_firing_prod as rfp  # noqa: E402

TSV = "\n".join([
    "# a comment",
    "",
    "search-programme-prefix\tbanner\t\t\tseed\tbanner\t\t(?i)^x\t\tnote",
    "xtra-pp-lato-cycle\tbanner\t\t\t\tbanner\tjun-sep\t(?i)^lato\t\tnote",
    "xtra-pp-konesera\tbanner\t\t\t\tbanner\t\t(?i)^k\t\tnote",
    "xtra-new-row\tquery\t\t\t\tformat\t\t(?i)x\t\tnote",
    "xtra-recorded-only\tquery\t\t\t\tformat\t\t(?i)y\t\tnote",
])
INVENTORY = "\n".join([
    "| row (before) | kind | effect | words | season | prod fires | recordings (by country) | corpus fires | BillingParse reads | after |",
    "|---|---|---|---|---|---:|---|---:|---|---|",
    "| `search-programme-prefix` | banner | banner | pl |  | 19 | pl:92 | 61 | 58/61 piece |  |",
    "| `xtra-pp-lato-cycle` | banner | banner | pl | jun-sep | 0 |  | 0 |  |  |",
    "| `xtra-pp-konesera` | banner | banner | pl |  | 4 | pl:10, uk:0 | 6 | 6/6 piece |  |",
    "| `xtra-recorded-only` | query | format | any |  | 0 | de:3 | 1 |  |  |",
    "| `xtra-pp-family-banners` | 8 | 92 | 692 | `a`, `b` |",
])


def prom(result):
    return json.dumps({"status": "success", "data": {"resultType": "vector", "result": result}})


FIRES = prom([{"metric": {"rule": "title:search-programme-prefix"}, "value": [0, "420"]},
              {"metric": {"rule": "title:xtra-new-row"}, "value": [0, "3"]},
              {"metric": {"rule": "title:xtra-pp-konesera"}, "value": [0, "0"]},
              {"metric": {"rule": "accept:sole-work"}, "value": [0, "9000"]},
              {"metric": {"rule": "veto:learned-x"}, "value": [0, "0"]}])
COUNTED = prom([{"metric": {}, "value": [0, "120000"]}])


def fake_ask(answers):
    asked = []

    def ask(query):
        asked.append(query)
        return answers[len(asked) - 1]
    return ask, asked


def read(path):
    return TSV if path.endswith(".tsv") else INVENTORY


class RuleFiringProdTest(unittest.TestCase):
    def test_reads_the_rules_and_the_inventory_rows_only(self):
        self.assertEqual(rfp.title_rules(TSV)["xtra-pp-lato-cycle"], ("banner", "jun-sep"))
        self.assertEqual(len(rfp.title_rules(TSV)), 5)
        recorded = rfp.inventory(INVENTORY)
        self.assertEqual(recorded, {"search-programme-prefix": "pl:92", "xtra-pp-lato-cycle": "",
                                    "xtra-pp-konesera": "pl:10, uk:0", "xtra-recorded-only": "de:3"})

    def test_classifies_each_rule_by_prod_and_recordings(self):
        rows = {r[1]: r[0] for r in rfp.classify(rfp.title_rules(TSV), rfp.inventory(INVENTORY), rfp.vector(FIRES))}
        self.assertEqual(rows, {"search-programme-prefix": "LIVE IN BOTH", "xtra-pp-lato-cycle": "DEAD IN BOTH",
                                "xtra-pp-konesera": "DEAD IN PROD ONLY", "xtra-new-row": "NOT IN INVENTORY (live in prod)",
                                "xtra-recorded-only": "DEAD IN PROD ONLY"})

    def test_prints_the_classes_and_queries_one_country(self):
        ask, asked = fake_ask([FIRES, COUNTED])
        out = io.StringIO()
        with redirect_stdout(out):
            self.assertEqual(rfp.main(["--country", "pl", "--window", "3d", "--all-kinds"], ask=ask, read=read), 0)
        self.assertIn('kinowo_worker_identity_rule_fires_total{country="pl"}[3d]', asked[0])
        text = out.getvalue()
        self.assertIn("120000 listings counted", text)
        self.assertIn("DEAD IN BOTH (1)\n  xtra-pp-lato-cycle  [banner, season jun-sep]  prod 0  recordings -", text)
        self.assertIn("  veto:learned-x  prod 0\n  accept:sole-work  prod 9000", text)

    def test_refuses_a_window_with_nothing_counted(self):
        ask, _ = fake_ask([prom([]), prom([])])
        with redirect_stdout(io.StringIO()):
            self.assertEqual(rfp.main([], ask=ask, read=read), 2)


if __name__ == "__main__":
    unittest.main()
