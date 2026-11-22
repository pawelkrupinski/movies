#!/usr/bin/env python3
"""Every `kinowo_*` metric family and every recording rule is read by an alert, or says why it is not.

WHY. On 2026-11-17 `kinowo_web_readmodel_stream_live` was exported, charted, and read by no alert: a web whose
read-model change stream had died would have served stale showtimes behind a green board until someone looked at
the panel (WebReadModelStreamDown came after). A metric with no alert is a decision -- "dashboard only", "debug",
"an input to a panel's ratio" -- and this makes it one somebody wrote down, instead of the default nobody chose.

WHAT COUNTS AS EXPORTED. The code's own declarations, the same three the dashboards' coverage checks read:
  - the metric catalogue (app_metrics_catalogue.py): every worker and web family as the registries hold it at boot,
    kept current by the tiers' MetricsAtBootSpec, so a new registration changes it in the same commit;
  - the families the web renders as exposition text by hand (a `"# TYPE "` writer under */src/main), which no
    registry declares (kinowo_web_movies_served, the uptime and fallback gauges);
  - the fleet's families: the `# TYPE kinowo_... <type>` header a textfile script (infra/nix) or a CI pusher
    (scripts/ci) echoes;
plus every recording rule in nix/files/monitoring/rules.

WHAT COUNTS AS READ. A Prometheus alerting rule or a Grafana-managed alert whose expression names the family (any
sample of it: `_total`, `_bucket`, ...; a `{__name__=~...}` regex counts for each name it matches), directly or
through a chain of recording rules an alert reads. The generated AppMetricAbsent rule does NOT count: it is written
from the alerts themselves (app_metrics_absent.py), so it covers only what another alert already reads.

OTHERWISE. A row in test/resources/metrics-without-alerts.tsv: `metric<TAB>reason<TAB>owner`. A row whose metric is
now alerted, or is no longer exported, fails as stale -- the file lists today's decisions, not yesterday's.

Run: python3 infra/test/test_metric_alert_coverage.py   (pre-push's `metricalerts` check)
"""

import glob
import os
import re
import sys
import unittest

import yaml

HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(os.path.dirname(HERE))
sys.dont_write_bytecode = True  # `python3 -I` ignores PYTHONDONTWRITEBYTECODE: no __pycache__ in the tree
sys.path.insert(0, HERE)
import app_metrics_absent  # noqa: E402
import app_metrics_catalogue  # noqa: E402
import promql_selectors  # noqa: E402

EXEMPTIONS = os.path.join(REPO, "test", "resources", "metrics-without-alerts.tsv")
HEADER = ["metric", "reason", "owner"]
_FLEET_TYPE = re.compile(r"# TYPE (kinowo_[a-z0-9_]+) ")
_LITERAL = re.compile(r'"(kinowo_[a-z0-9_]+)"')
_SUFFIX = re.compile(r"_(total|bucket|count|sum|created)$")


# ---- what is exported ----------------------------------------------------------------------------

def _files(root, suffixes):
    for directory, _, names in os.walk(os.path.join(REPO, root)):
        if "/target" in directory or "/node_modules" in directory:
            continue
        for name in sorted(names):
            if name.endswith(suffixes):
                yield os.path.join(directory, name)


def _read(path):
    with open(path, encoding="utf-8", errors="replace") as handle:
        return handle.read()


def exported_families():
    """{family name: where it is declared} -- the catalogue, the hand-rendered web families, the fleet's."""
    found = {name: "catalogue (%s)" % entry["tier"]
             for name, entry in app_metrics_catalogue.families().items() if name.startswith("kinowo_")}
    for module in ("web", "worker", "common", "scrapers", "identity", "geodata"):
        for path in _files(os.path.join(module, "src", "main"), (".scala",)):
            text = _read(path)
            if '"# TYPE "' in text:
                for name in _LITERAL.findall(text):
                    found.setdefault(name, os.path.relpath(path, REPO))
    for root, suffixes in (("infra/nix", (".sh", ".nix")), ("scripts/ci", (".py",))):
        for path in _files(root, suffixes):
            for name in _FLEET_TYPE.findall(_read(path)):
                found.setdefault(_SUFFIX.sub("", name) if name.endswith("_total") else name, os.path.relpath(path, REPO))
    return found


def sample_names(family):
    """Every name a query can spell `family` by, whatever type it turns out to be."""
    entry = app_metrics_catalogue.families().get(family)
    if entry is not None:
        return app_metrics_catalogue.sample_names(entry) | {family}
    return {family} | {family + s for s in ("_total", "_bucket", "_count", "_sum", "_created")}


# ---- what is read ----------------------------------------------------------------------------------

def rules():
    """(alerts, recordings): alerts = [(where, expr)]; recordings = {record name: [expr]}. The generated
    AppMetricAbsent rule is left out (see the module docstring)."""
    alerts, recordings = [], {}
    for path in promql_selectors.RULE_FILES:
        if os.path.abspath(path) == os.path.abspath(app_metrics_absent.RULES_OUT):
            continue
        with open(path, encoding="utf-8") as handle:
            document = yaml.safe_load(handle)
        for group in document["groups"]:
            for rule in group["rules"]:
                if "alert" in rule:
                    alerts.append(("%s %s" % (os.path.basename(path), rule["alert"]), rule["expr"]))
                else:
                    recordings.setdefault(rule["record"], []).append(rule["expr"])
    alerts.extend(promql_selectors.grafana_rule_expressions())
    return alerts, recordings


def _reads(expr, candidates):
    """The candidate names `expr` reads: by name, or by a `{__name__=~...}` regex."""
    named = promql_selectors.metric_names(expr)
    hit = {c for c in candidates if c in named}
    for pattern in promql_selectors.name_patterns(expr):
        hit |= {c for c in candidates if pattern.matches(c)}
    return hit


def alerted_names(alerts, recordings, candidates):
    """Every name in `candidates` or `recordings` an alert reads, directly or through recording rules."""
    universe = set(candidates) | set(recordings)
    read = set()
    frontier = [expr for _, expr in alerts]
    while frontier:
        expr = frontier.pop()
        for name in _reads(expr, universe) - read:
            read.add(name)
            frontier.extend(recordings.get(name, []))
    return read


# ---- the verdict ---------------------------------------------------------------------------------

def load_exemptions(path=EXEMPTIONS):
    """{metric: (reason, owner)}; raises ValueError on a malformed file."""
    rows = {}
    with open(path, encoding="utf-8") as handle:
        lines = [line.rstrip("\n") for line in handle if line.strip() and not line.startswith("#")]
    if not lines or lines[0].split("\t") != HEADER:
        raise ValueError("%s must start with the header %s" % (path, "\\t".join(HEADER)))
    for number, line in enumerate(lines[1:], start=2):
        cells = line.split("\t")
        if len(cells) != 3 or not all(c.strip() for c in cells):
            raise ValueError("%s row %d needs three non-empty tab-separated cells: %r" % (path, number, line))
        if cells[0] in rows:
            raise ValueError("%s lists %s twice" % (path, cells[0]))
        rows[cells[0]] = (cells[1], cells[2])
    return rows


def verdict(families, alerts, recordings, exemptions):
    """(unalerted, stale): names with neither an alert nor a row; rows naming an alerted or unexported name."""
    by_sample = {}
    for family in families:
        for sample in sample_names(family):
            by_sample[sample] = family
    read = alerted_names(alerts, recordings, by_sample)
    alerted = {by_sample.get(name, name) for name in read}
    subjects = set(families) | set(recordings)
    unalerted = sorted(subjects - alerted - set(exemptions))
    stale = sorted(m for m in exemptions if m in alerted or m not in subjects)
    return unalerted, stale


def report(unalerted, stale, families):
    lines = []
    if unalerted:
        lines.append("read by no alert and not in %s -- alert on it, or add a row (metric, reason, owner):"
                     % os.path.relpath(EXEMPTIONS, REPO))
        lines += ["  %s\t(%s)" % (m, families.get(m, "recording rule")) for m in unalerted]
    if stale:
        lines.append("stale rows in %s (now alerted, or no longer exported) -- strike them:"
                     % os.path.relpath(EXEMPTIONS, REPO))
        lines += ["  " + m for m in stale]
    return "\n".join(lines)


class EveryMetricHasAnAlertOrAReason(unittest.TestCase):

    def test_the_tree(self):
        families = exported_families()
        self.assertGreater(len(families), 150)  # the catalogue was read
        alerts, recordings = rules()
        unalerted, stale = verdict(families, alerts, recordings, load_exemptions())
        self.assertEqual(([], []), (unalerted, stale), "\n" + report(unalerted, stale, families))


class TheCheckItself(unittest.TestCase):
    """The verdict over a fixture: a metric with no alert and no row fails, and passes once the row is added."""

    FAMILIES = {"kinowo_fixture_alerted": "fixture", "kinowo_fixture_orphan": "fixture",
                "kinowo_fixture_via_recording_total": "fixture"}
    ALERTS = [("fixture FixtureAlert", "kinowo_fixture_alerted > 0"),
              ("fixture FixtureRecorded", "fixture:rate5m > 1")]
    RECORDINGS = {"fixture:rate5m": ["rate(kinowo_fixture_via_recording_total[5m])"],
                  "fixture:unread": ["sum(kinowo_fixture_alerted)"]}

    def test_a_metric_with_no_alert_and_no_row_fails(self):
        unalerted, _ = verdict(self.FAMILIES, self.ALERTS, self.RECORDINGS, {})
        self.assertEqual(["fixture:unread", "kinowo_fixture_orphan"], unalerted)

    def test_it_passes_once_the_row_is_added(self):
        rows = {"kinowo_fixture_orphan": ("debug", "owner"), "fixture:unread": ("dashboard only", "owner")}
        self.assertEqual(([], []), verdict(self.FAMILIES, self.ALERTS, self.RECORDINGS, rows))

    def test_a_row_for_an_alerted_or_unknown_metric_is_stale(self):
        rows = {"kinowo_fixture_orphan": ("debug", "o"), "fixture:unread": ("d", "o"),
                "kinowo_fixture_alerted": ("d", "o"), "kinowo_gone": ("d", "o")}
        self.assertEqual(["kinowo_fixture_alerted", "kinowo_gone"],
                         verdict(self.FAMILIES, self.ALERTS, self.RECORDINGS, rows)[1])

    def test_a_name_regex_reads_every_family_it_matches(self):
        alerts = [("fixture Regex", 'sum({__name__=~"kinowo_fixture_(orphan|alerted)"}) > 0')]
        self.assertEqual(["kinowo_fixture_via_recording_total"],
                         verdict(self.FAMILIES, alerts, {}, {})[0])

    def test_a_malformed_file_is_refused(self):
        import tempfile
        with tempfile.NamedTemporaryFile("w", suffix=".tsv", delete=False) as handle:
            handle.write("metric\treason\towner\nkinowo_x\tdebug\n")
        try:
            with self.assertRaises(ValueError):
                load_exemptions(handle.name)
        finally:
            os.unlink(handle.name)


if __name__ == "__main__":
    unittest.main(verbosity=2)
