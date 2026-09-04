#!/usr/bin/env python3
"""The provisioned Grafana dashboards, checked without a Grafana.

WHY THIS EXISTS. A dashboard is 100KB of hand-edited JSON that nothing else in this tree looks at:
`nix eval` type-checks the module that COPIES the directory, never the documents inside it, and
Grafana's own reaction to a bad one is to log a line at startup and carry on with the dashboards it
could read. So the failure mode is a page that is quietly missing a panel -- or, worse, one whose
panels are stacked on top of each other because a `gridPos.y` was not bumped -- and the only
witness is somebody opening the page during an incident, which is the worst possible moment to find
out.

Everything here is a structural invariant that a hand-edit breaks and a human review misses:
duplicate panel ids, overlapping tiles, a target with no query, a datasource uid that no
provisioning file defines. It says nothing about whether a PromQL expression is TRUE -- that needs
a Prometheus with the fleet's series in it, which this deliberately does not have.

Run: python3 infra/test/test_dashboards.py   (also run by infra/bin/check)
"""

import json
import os
import re
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
DASHBOARD_DIR = os.path.join(HERE, "..", "nix", "files", "monitoring", "grafana", "dashboards")
GRAFANA_ROLE = os.path.join(HERE, "..", "nix", "modules", "roles", "grafana.nix")
PROMETHEUS_ROLE = os.path.join(HERE, "..", "nix", "modules", "roles", "prometheus.nix")
HOSTS_DIR = os.path.join(HERE, "..", "nix", "hosts")
MONITORING_HOST = os.path.join(HOSTS_DIR, "monitoring-1", "default.nix")
MONITORING_FILES = os.path.join(HERE, "..", "nix", "files", "monitoring")

# Everywhere a Prometheus job name is defined: the static config, the app scrape file, and the
# scrape.d generators that build a job out of a NixOS option.
JOB_NAME_SOURCES = [
    os.path.join(MONITORING_FILES, "prometheus.yaml"),
    os.path.join(MONITORING_FILES, "scrape-kinowo-apps.yaml"),
    PROMETHEUS_ROLE,
]

# The uids roles/grafana.nix provisions. A panel pointing at anything else renders "Datasource not
# found" -- which reads exactly like a broken query, so it is worth failing here instead.
PROVISIONED_DATASOURCE_UIDS = {"local-prometheus", "victorialogs"}

# Panels that carry no query and so need no datasource or targets.
PROSE_PANEL_TYPES = {"row", "text"}


def dashboards():
    """Every provisioned dashboard, as (relative path, parsed document)."""
    found = []
    for folder in sorted(os.listdir(DASHBOARD_DIR)):
        folder_path = os.path.join(DASHBOARD_DIR, folder)
        if not os.path.isdir(folder_path):
            continue
        for name in sorted(os.listdir(folder_path)):
            if not name.endswith(".json"):
                continue
            path = os.path.join(folder_path, name)
            with open(path, encoding="utf-8") as handle:
                found.append(("%s/%s" % (folder, name), json.load(handle)))
    return found


def query_panels(document):
    """The panels that ask Prometheus something -- rows and text banners excluded.

    DESCENDS INTO ROWS, and that is the whole point of the function. A COLLAPSED row
    keeps its children inside its own `panels` list instead of at the top level, so a
    helper reading only `document["panels"]` sees whichever rows happen to be expanded
    and silently skips every panel in the rest.

    On apps/fly-overview.json that was 49 of 61 query panels. The coverage tests below
    reported four panels missing -- the mongod internals, both machines' host
    resources, the answering-vs-configured target counts -- every one of which had
    been on the dashboard the whole time, inside a collapsed row. Four red assertions
    naming panels that exist is the loud half; the quiet half is that the structure
    tests were checking a fifth of the dashboard and passing.
    """
    def walk(panels):
        for panel in panels:
            if panel.get("type") == "row":
                yield from walk(panel.get("panels", []))
            elif panel.get("type") not in PROSE_PANEL_TYPES:
                yield panel

    return list(walk(document["panels"]))


class DashboardStructure(unittest.TestCase):
    """Invariants every dashboard in the tree has to hold, whatever it is about."""

    def setUp(self):
        self.dashboards = dashboards()
        self.assertTrue(self.dashboards, "no dashboards found under %s" % DASHBOARD_DIR)

    def test_every_dashboard_parses_and_has_a_uid(self):
        # The uid is what a bookmark and every alert notification's link is built from, so a
        # dashboard that loses one is reachable only by searching for its title.
        seen = {}
        for path, document in self.dashboards:
            uid = document.get("uid")
            self.assertTrue(uid, "%s has no uid" % path)
            self.assertNotIn(uid, seen, "%s and %s share uid %r" % (path, seen.get(uid), uid))
            seen[uid] = path
            self.assertTrue(document.get("title"), "%s has no title" % path)
            self.assertIsInstance(document.get("panels"), list, "%s has no panel list" % path)

    def test_panel_ids_are_unique_within_a_dashboard(self):
        # Grafana keys panel links, and its own edit/duplicate machinery, on the id. Two panels
        # sharing one is not an error at load time -- it is a page where the wrong panel opens.
        for path, document in self.dashboards:
            ids = [p.get("id") for p in document["panels"]]
            self.assertNotIn(None, ids, "%s has a panel with no id" % path)
            duplicates = sorted({i for i in ids if ids.count(i) > 1})
            self.assertEqual([], duplicates, "%s reuses panel id(s) %s" % (path, duplicates))

    def test_no_two_panels_occupy_the_same_grid_cell(self):
        # THE BUG THIS FILE WAS WRITTEN FOR. Appending a panel means picking a `y` below everything
        # else; get it wrong and Grafana silently reflows the page, pushing unrelated panels around
        # and sometimes hiding one behind another. It renders, so it passes review.
        for path, document in self.dashboards:
            occupied = {}
            collisions = set()
            for panel in document["panels"]:
                pos = panel["gridPos"]
                for y in range(pos["y"], pos["y"] + pos["h"]):
                    for x in range(pos["x"], pos["x"] + pos["w"]):
                        if (x, y) in occupied:
                            collisions.add((occupied[(x, y)], panel.get("title")))
                        occupied[(x, y)] = panel.get("title")
            self.assertEqual(set(), collisions, "%s overlaps: %s" % (path, sorted(collisions)))

    def test_panels_stay_inside_the_24_column_grid(self):
        for path, document in self.dashboards:
            for panel in document["panels"]:
                pos = panel["gridPos"]
                self.assertLessEqual(
                    pos["x"] + pos["w"], 24,
                    "%s: %r runs past the right edge" % (path, panel.get("title")))

    def test_every_query_panel_names_a_provisioned_datasource(self):
        for path, document in self.dashboards:
            for panel in query_panels(document):
                sources = [panel.get("datasource")] + [
                    t.get("datasource") for t in panel.get("targets", [])
                ]
                for source in sources:
                    self.assertIsNotNone(
                        source, "%s: %r has a target with no datasource" % (path, panel.get("title")))
                    self.assertIn(
                        source.get("uid"), PROVISIONED_DATASOURCE_UIDS,
                        "%s: %r reads unprovisioned datasource %r"
                        % (path, panel.get("title"), source.get("uid")))

    def test_every_query_panel_asks_something(self):
        for path, document in self.dashboards:
            for panel in query_panels(document):
                targets = panel.get("targets") or []
                self.assertTrue(targets, "%s: %r has no targets" % (path, panel.get("title")))
                for target in targets:
                    query = target.get("expr") or target.get("query")
                    self.assertTrue(
                        query and query.strip(),
                        "%s: %r has an empty query" % (path, panel.get("title")))

    def test_every_panel_says_what_it_is_for(self):
        # The house style on these dashboards: each panel carries a description explaining what the
        # line means and what shape is unhealthy. It is the only documentation an operator has at
        # 03:00, and a panel added without one is a panel nobody trusts.
        for path, document in self.dashboards:
            for panel in query_panels(document):
                self.assertTrue(
                    (panel.get("description") or "").strip(),
                    "%s: %r has no description" % (path, panel.get("title")))

    def test_the_datasource_allowlist_matches_what_grafana_provisions(self):
        # Keeps the constant above honest: if a datasource is added or renamed in roles/grafana.nix
        # and not here, the checks above start rejecting valid panels.
        with open(GRAFANA_ROLE, encoding="utf-8") as handle:
            declared = set(re.findall(r'^\s*uid = "([a-z0-9-]+)";', handle.read(), re.MULTILINE))
        self.assertEqual(
            PROVISIONED_DATASOURCE_UIDS, declared,
            "roles/grafana.nix provisions %s; this file allows %s"
            % (sorted(declared), sorted(PROVISIONED_DATASOURCE_UIDS)))


class DashboardsAgainstTheFleet(unittest.TestCase):
    """The literals a panel filters on, checked against the fleet that has to produce them.

    A PromQL selector that names a host, a job or a mountpoint which does not exist is not an
    error anywhere: Prometheus answers an empty result, Grafana draws "No data", and the panel is
    indistinguishable from one whose subject is genuinely quiet. These are the three literals that
    are typed by hand into a query and defined somewhere else in this tree, so they are the three
    that can silently drift.
    """

    def setUp(self):
        self.dashboards = dashboards()

    def queries(self):
        for path, document in self.dashboards:
            for panel in query_panels(document):
                for target in panel.get("targets", []):
                    query = target.get("expr") or target.get("query") or ""
                    yield path, panel.get("title"), query

    @staticmethod
    def literals(query, label):
        """Every value `label` is pinned to, across `=` and `=~` alternations. Negations skipped."""
        found = set()
        for match in re.finditer(r'\b%s=~?"([^"]*)"' % label, query):
            found.update(match.group(1).split("|"))
        return {v for v in found if v}

    def test_every_host_a_panel_filters_on_is_a_scrape_target(self):
        # The `host` label exists only because roles/prometheus.nix attaches it to the `node` and
        # `mongodb` jobs from this list. A machine renamed here and not in a dashboard leaves the
        # dashboard drawing nothing about a machine that is running perfectly.
        with open(MONITORING_HOST, encoding="utf-8") as handle:
            known = set(re.findall(r'host = "([^"]+)";', handle.read()))
        self.assertTrue(known, "no nodeTargets found in %s" % MONITORING_HOST)
        for path, title, query in self.queries():
            for host in self.literals(query, "host"):
                self.assertIn(host, known,
                              "%s: %r filters on host=%r, which no scrape target declares"
                              % (path, title, host))

    def test_every_mountpoint_a_panel_filters_on_is_mounted_by_some_host(self):
        # A data volume is the one filesystem worth a panel of its own, and its path is written in
        # exactly one other place -- the host's `fileSystems` declaration. Typo either side and the
        # panel is empty on the day the volume fills.
        declared = {"/"}
        for name in sorted(os.listdir(HOSTS_DIR)):
            host_file = os.path.join(HOSTS_DIR, name, "default.nix")
            if not os.path.isfile(host_file):
                continue
            with open(host_file, encoding="utf-8") as handle:
                declared.update(re.findall(r'fileSystems\."([^"]+)"', handle.read()))
        for path, title, query in self.queries():
            for mount in self.literals(query, "mountpoint"):
                self.assertIn(mount, declared,
                              "%s: %r filters on mountpoint=%r, which no host mounts"
                              % (path, title, mount))

    def test_every_job_a_panel_filters_on_is_scraped(self):
        # Job names are spread over the static prometheus.yaml, the app scrape file and the three
        # generators in roles/prometheus.nix. Renaming one is a two-file change, and the half that
        # gets forgotten is always the dashboard.
        known = set()
        for source in JOB_NAME_SOURCES:
            with open(source, encoding="utf-8") as handle:
                text = handle.read()
            known.update(re.findall(r'job_name:\s*"?([\w-]+)"?', text))
            known.update(re.findall(r'job_name = "([\w-]+)"', text))
        self.assertTrue(known, "no job names found; the source list is wrong")
        for path, title, query in self.queries():
            for job in self.literals(query, "job"):
                self.assertIn(job, known,
                              "%s: %r filters on job=%r, which nothing scrapes"
                              % (path, title, job))


class ApplicationDashboardCoverage(unittest.TestCase):
    """What the application-health dashboard has to answer, beyond being well-formed.

    Structure tests keep the page from breaking; this one keeps it from being incomplete. Both
    tiers run on k3s but neither owns its own state: every read and write goes to the single mongod
    on mongo-1, and every line on the page is drawn from series monitoring-1 collected. A dashboard
    that shows the two tiers and neither of those machines cannot distinguish "the application is
    slow" from "the database is saturated" or from "the scrape stopped".
    """

    def setUp(self):
        self.document = dict(dashboards())["apps/application-health.json"]
        self.queries = [
            t.get("expr") or ""
            for p in query_panels(self.document)
            for t in p.get("targets", [])
        ]

    def assertQueried(self, needle):
        self.assertTrue(any(needle in q for q in self.queries),
                        "no panel on apps/application-health.json queries %r" % needle)

    def test_it_shows_the_database_host_resources(self):
        for metric in ("node_cpu_seconds_total", "node_memory_MemAvailable_bytes",
                       "node_filesystem_avail_bytes"):
            self.assertQueried('%s{host="mongo-1"' % metric)

    def test_it_shows_what_mongod_itself_reports(self):
        # Host metrics say the machine is fine; these say the database is. The difference is the
        # whole reason mongodb_exporter exists -- see roles/mongodb-exporter.nix.
        for metric in ("mongodb_ss_wt_cache_bytes_currently_in_the_cache",
                       "mongodb_ss_opcounters",
                       "mongodb_ss_opLatencies_latency",
                       "mongodb_ss_wt_concurrentTransactions_out"):
            self.assertQueried(metric)

    def test_it_shows_the_monitoring_box_that_produces_it(self):
        for metric in ("node_cpu_seconds_total", "node_filesystem_avail_bytes"):
            self.assertQueried('%s{host="monitoring-1"' % metric)
        self.assertQueried("prometheus_tsdb_storage_blocks_bytes")
        self.assertQueried('process_resident_memory_bytes{job=~"prometheus|grafana|alertmanager"}')

    def test_it_can_tell_a_dead_application_from_a_dead_scrape(self):
        # Every other panel goes flat for both reasons. Without a target-count panel the page
        # cannot say which happened, which is the one question an incident opens with.
        self.assertQueried("sum by (job) (up{")
        self.assertQueried("count by (job) (up{")


if __name__ == "__main__":
    unittest.main(verbosity=2)
