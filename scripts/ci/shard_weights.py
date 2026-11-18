#!/usr/bin/env python3
"""THE SHARD WEIGHTS, REFRESHED (scripts/ci/shard-weights-identity.tsv, which scripts/ci/shard_tests.py balances the
landing check's identity shards on): every suite's median seconds over the recent landing checks' identity shard jobs,
from the CI telemetry (victoria-logs-ci: `source:ci kind:suite`, the job telemetry's slowest suites of each job).

A weights file left behind its specs splits badly: FamilyIndexCompletenessSpec (~400 s) had no row, weighed the default
3 s, and the identity shards ran ~900 s against ~330 s (2026-11-17). The median, not one run: a slice-cached walk
(PopulationMonotonicity) or a loaded runner moves a single run's seconds by 3x. A suite measured in fewer than
--min-runs runs keeps its row from the current file (TitleRelationPart*: rarely selected), so a refresh never drops a
weight for want of samples; a row whose class no longer exists in the tree is dropped.

Usage:
  shard_weights.py refresh [--days 7] [--min-runs 3] [--url URL] [--current FILE] > FILE   the new file on stdout
"""

import argparse
import json
import os
import statistics
import subprocess
import sys
import urllib.parse
import urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
DEFAULT_FILE = os.path.join(HERE, "shard-weights-identity.tsv")
DEFAULT_URL = "http://victoria-logs-ci.service.consul:9429"
QUERY = 'source:ci kind:suite workflow_file:="precommit-check.yml" job:~"^check \\\\(identity " | fields run_id, suite, time_s'


def read_rows(path):
    """{class: seconds} of a weights file (comments and the header skipped)."""
    rows = {}
    if os.path.exists(path):
        with open(path, encoding="utf-8") as fh:
            for raw in fh:
                raw = raw.split("#", 1)[0].strip()
                if raw:
                    name, seconds = raw.split()
                    rows[name] = float(seconds)
    return rows


def medians(samples, min_runs):
    """{suite: median seconds} of (run id, suite, seconds) samples, a suite once a run (its largest: the telemetry
    reports some twice), only suites seen in min_runs runs or more."""
    per_run = {}
    for run_id, suite, seconds in samples:
        key = (suite, run_id)
        per_run[key] = max(per_run.get(key, 0.0), seconds)
    by_suite = {}
    for (suite, _run), seconds in per_run.items():
        by_suite.setdefault(suite, []).append(seconds)
    return {s: statistics.median(v) for s, v in by_suite.items() if len(v) >= min_runs}


def merge(measured, current, exists):
    """The new rows: every measured suite, plus a current row not measured enough, less classes that no longer exist;
    heaviest first."""
    rows = {s: v for s, v in current.items() if exists(s)}
    rows.update({s: v for s, v in measured.items() if exists(s)})
    return sorted(rows.items(), key=lambda kv: (-kv[1], kv[0]))


def render(rows, days, min_runs):
    head = [
        "# The identity specs' seconds in the landing check's identity shards: each suite's MEDIAN over the precommit-check",
        f"# identity shard jobs of the last {days} days (ci-metrics `suite` lines, victoria-logs-ci), a suite seen in fewer than",
        f"# {min_runs} runs kept from the file before. Only shard_tests.py's balance reads them: a missing or stale row moves a",
        "# class to another shard, never out of the check (an unknown class weighs 3).",
        "# Refresh: python3 scripts/ci/shard_weights.py refresh > scripts/ci/shard-weights-identity.tsv",
        "# class\tseconds",
    ]
    return "\n".join(head + [f"{name}\t{round(seconds)}" for name, seconds in rows]) + "\n"


def class_exists(root):
    """A test class name -> whether a Scala source under root declares it (`class <Simple>`)."""
    out = subprocess.run(["git", "-C", root, "grep", "-h", "-o", "-E", "class [A-Za-z0-9_]+", "--", "*.scala"],
                         capture_output=True, text=True).stdout
    simple = {line.split()[1] for line in out.splitlines() if len(line.split()) == 2}
    return lambda name: name.rsplit(".", 1)[-1] in simple


def fetch(url, days):
    form = urllib.parse.urlencode({"query": QUERY, "start": f"now-{days}d", "limit": "100000"}).encode()
    with urllib.request.urlopen(urllib.request.Request(f"{url}/select/logsql/query", data=form), timeout=60) as r:
        for line in r.read().decode().splitlines():
            if line.strip():
                d = json.loads(line)
                yield d["run_id"], d["suite"], float(d["time_s"])


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    sub = ap.add_subparsers(dest="cmd", required=True)
    r = sub.add_parser("refresh")
    r.add_argument("--days", type=int, default=7)
    r.add_argument("--min-runs", type=int, default=3)
    r.add_argument("--url", default=DEFAULT_URL)
    r.add_argument("--current", default=DEFAULT_FILE)
    r.add_argument("--samples", help="a JSON-lines file of {run_id, suite, time_s} in place of the query (tests)")
    a = ap.parse_args(argv)
    if a.samples:
        with open(a.samples, encoding="utf-8") as fh:
            samples = [(d["run_id"], d["suite"], float(d["time_s"])) for d in map(json.loads, filter(str.strip, fh))]
    else:
        samples = list(fetch(a.url, a.days))
    measured = medians(samples, a.min_runs)
    if not measured:
        print("shard_weights: no suite measured in the window: the file is left as it is", file=sys.stderr)
        return 1
    sys.stdout.write(render(merge(measured, read_rows(a.current), class_exists(os.path.dirname(os.path.dirname(HERE)))),
                            a.days, a.min_runs))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
