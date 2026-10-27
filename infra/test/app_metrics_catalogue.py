"""The application metric catalogue: every worker and web metric family as the code declares it at boot.

Written by the tiers' MetricsAtBootSpec (services.metrics.MetricCatalogue) to app-metrics-catalogue/<tier>.json from
the registry as it stands before the first event -- name, type, the sample name a query spells, every label with each
value seeded at boot, and what it reads at boot (`0`, `NaN` for a reading not taken yet, `none` for a family whose
series only a wiring names). It is the ONE declaration of a metric the infra checks read:

- test_label_shapes.py accepts a value the code declares, so a new metric needs no "NOT PUBLISHED YET" exemption
  while the fleet snapshot has not seen it;
- app_metrics_absent.py takes each family's scrape job from its tier;
- test_alert_rule_coverage.py reads which families boot at NaN;
- test_dashboards.py requires each family on a panel.

`country` is not listed: the scrape's target label replaces it. Regenerate with
`KINOWO_WRITE_METRIC_CATALOGUE=1 scripts/sbt-warm "worker/testOnly deploy.WorkerMetricsAtBootSpec"
"web/testOnly deploy.WebMetricsAtBootSpec"`.
"""

import glob
import json
import os
import re

HERE = os.path.dirname(os.path.abspath(__file__))
CATALOGUE = os.path.join(HERE, "app-metrics-catalogue")
JOBS = {"worker": "kinowo-worker", "web": "kinowo-web"}
_SUFFIX = re.compile(r"_(total|bucket|count|sum|created)$")


def families():
    """{family name: entry} over every tier's file."""
    found = {}
    for path in sorted(glob.glob(os.path.join(CATALOGUE, "*.json"))):
        with open(path, encoding="utf-8") as handle:
            for entry in json.load(handle)["families"]:
                found[entry["name"]] = entry
    return found


def family_of(sample):
    """The catalogue entry a sample name (`x_total`, `x_bucket`, `x`) belongs to, or None."""
    every = families()
    return every.get(sample) or every.get(_SUFFIX.sub("", sample))


def sample_names(entry):
    """Every name a query can spell the family by."""
    name = entry["name"]
    if entry["type"] == "counter":
        return {name + "_total", name + "_created"}
    if entry["type"] in ("histogram", "summary"):
        return {name + "_bucket", name + "_count", name + "_sum", name + "_created"}
    return {name}


def declared_values():
    """{sample name: {label: [values]}} -- each family's seeded label values, plus `job` from its tier."""
    found = {}
    for entry in families().values():
        labels = dict(entry["labels"])
        labels["job"] = [JOBS[entry["tier"]]]
        for name in sample_names(entry):
            found[name] = labels
    return found


def unmeasured_at_boot():
    """The families seeded NaN at boot -- a reading nothing has taken yet."""
    return {name for name, entry in families().items() if entry["boot"] == "NaN"}
