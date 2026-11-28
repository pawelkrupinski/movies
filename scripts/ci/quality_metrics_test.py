#!/usr/bin/env python3
"""quality_metrics.py: a fixture repository's committed baselines come back as exactly their rows -- per family,
language and measure, a 0 for every measure held and rowless, the design effects split by kind, the migration's done /
left, agentctl's ceilings -- stamped at the commit; a backfill keeps each window's last commit; and this checkout's
own baselines at HEAD read back row for row."""
from __future__ import annotations

import contextlib
import io
import os
import subprocess
import sys
import tempfile
import unittest
from typing import Dict, List

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
SCRATCH_GIT = os.path.join(ROOT, "scripts/scratch-git.sh")
sys.path.insert(0, HERE)
import quality_metrics as qm  # noqa: E402

PY_BASELINE = """# header
limit\tcognitive\t10
limit\tpyright\t0
cognitive\tscripts/a.py:f\t14
cognitive\tscripts/a.py:g\t12
pyright\tscripts/a.py\t7
pyright\tscripts/b.py\t3
"""
DESIGN = """limit\teffect\t0
effect\tscripts/a.py\t2
effect\tscripts/a.py:files\t5
effect\tscripts/b.py:network\t1
effect\tweb/src/main/assets/js/x.js:storage\t3
suffix\tworker/src/main/scala/A.scala:AHandler\t1
state\tinfra/version-dashboard/src/s.ts\t2
"""
COUPLING = """limit\tfanout\t9
module-edge\tweb->common\t40
ce\tservices.x\t6
distance-cap\tservices.x\t22
fanout\tservices.X\t12
fanout\tservices.Y\t10
"""
AREAS = "# areas\nweb-js\tglobals\tweb/src/main/assets/js\nvd\timports\tinfra/version-dashboard/src\n"
TOOLS = "# rank\tpath\tpath now\n1\tscripts/done-tool\t\n2\tscripts/left-tool\t\n3\tscripts/gone-tool\t\n"
CTX = 'CEILINGS = {\n    "alpha": 4,\n    "beta": 2,\n}\n'
LOCATORS = 'FROZEN = {"clock.wire_clock": "why"}\nREADER_CEILINGS = {"clock.wire_clock": (8, 12)}\n'


class Scratch:
    """A throwaway repository (scripts/scratch-git.sh: no GIT_* variable or user config reaches it)."""

    def __init__(self, root: str) -> None:
        self.root = root
        self.run("scratch_repo", "-q")

    def run(self, helper: str, *args: str, stamp: str = "") -> str:
        script = '. "$1"; [ -z "$STAMP" ] || export GIT_COMMITTER_DATE="$STAMP"; "$2" "$3" "${@:4}"'
        return subprocess.run(["bash", "-c", script, "_", SCRATCH_GIT, helper, self.root, *args], check=True,
                              capture_output=True, text=True, env=dict(os.environ, STAMP=stamp)).stdout.strip()

    def commit(self, files: Dict[str, str], stamp: str) -> None:
        for path, text in files.items():
            full = os.path.join(self.root, path)
            os.makedirs(os.path.dirname(full), exist_ok=True)
            with open(full, "w") as handle:
                handle.write(text)
        self.run("scratch_git", "add", "-A")
        self.run("scratch_git", "commit", "-qm", "at " + stamp, stamp=stamp)


def printed(*argv: str) -> List[str]:
    out = io.StringIO()
    with contextlib.redirect_stdout(out):
        code = qm.main(list(argv))
    assert code == 0, code
    return out.getvalue().splitlines()


def value(lines: List[str], name: str, **labels: str) -> str:
    wanted = ",".join('%s="%s"' % pair for pair in sorted(labels.items()))
    found = [line.split(" ", 1)[1] for line in lines if line.startswith("%s{%s} " % (name, wanted))]
    assert len(found) == 1, (name, wanted, found)
    return found[0]


class QualityMetricsTest(unittest.TestCase):
    def setUp(self) -> None:
        self.saved = {k: os.environ.pop(k) for k in list(os.environ) if k.startswith("GIT_")}
        self.dir = tempfile.TemporaryDirectory()
        self.movies = Scratch(os.path.join(self.dir.name, "movies"))
        self.movies.commit({
            "test/resources/py-quality-baseline.tsv": PY_BASELINE,
            "test/resources/design-baseline.tsv": DESIGN,
            "test/resources/coupling-baseline.tsv": COUPLING,
            "scripts/js-quality/ratchet-areas.tsv": AREAS,
            "scripts/shell-py-progress/migrate-tools.tsv": TOOLS,
            "scripts/done-tool": "#!/usr/bin/env python3\nprint(1)\n",
            "scripts/left-tool": "#!/bin/bash\necho 1\n",
        }, "2026-11-20T10:15:00Z")

    def tearDown(self) -> None:
        self.dir.cleanup()
        os.environ.update(self.saved)

    def test_the_committed_rows_come_back_per_family_language_and_measure(self) -> None:
        lines = printed("--repo-dir", self.movies.root)
        rows = lambda **labels: value(lines, qm.ROWS, repo="movies", **labels)  # noqa: E731
        total = lambda **labels: value(lines, qm.SUM, repo="movies", **labels)  # noqa: E731
        stamp = " 1795169700"  # 2026-11-20T10:15:00Z: the commit's, not the run's
        self.assertEqual("2" + stamp, rows(family="quality", language="py", measure="cognitive"))
        self.assertEqual("26" + stamp, total(family="quality", language="py", measure="cognitive"))
        self.assertEqual("10" + stamp, total(family="quality", language="py", measure="pyright"))
        self.assertEqual("22" + stamp, total(family="quality", language="scala", measure="fanout"))
        # held and rowless: a 0, so a cleared kind is a line at 0; a limit or an information row is no measure
        self.assertEqual("0" + stamp, rows(family="quality", language="scala", measure="cognitive"))
        self.assertEqual("0" + stamp, rows(family="quality", language="js", measure="cycle"))
        self.assertFalse([line for line in lines if 'measure="limit"' in line or 'measure="ce"' in line
                          or 'measure="module-edge"' in line or 'measure="distance-cap"' in line])
        # the design rows: each key's language by its path, an effect by its kind
        self.assertEqual("1" + stamp, rows(family="design", language="py", measure="effect-clock-process-env"))
        self.assertEqual("5" + stamp, total(family="design", language="py", measure="effect-files"))
        self.assertEqual("1" + stamp, rows(family="design", language="py", measure="effect-network"))
        self.assertEqual("3" + stamp, total(family="design", language="js", measure="effect-storage"))
        self.assertEqual("2" + stamp, total(family="design", language="js", measure="state"))
        self.assertEqual("1" + stamp, rows(family="design", language="scala", measure="suffix"))
        # the migration: a python shebang is done, a removed tool counts with it, a shell one is left
        self.assertEqual("2" + stamp, rows(family="migration", language="shell", measure="migrate-done"))
        self.assertEqual("1" + stamp, rows(family="migration", language="shell", measure="migrate-left"))

    def test_agentctl_reads_at_its_last_commit_by_the_movies_stamp(self) -> None:
        agentctl = Scratch(os.path.join(self.dir.name, "agentctl"))
        agentctl.commit({"quality/types-baseline.tsv": "limit\tmypy-errors\t0\nmypy-errors\talpha\t20\n",
                         "tests/test_no_new_ctx_parameters.py": CTX,
                         "tests/test_no_service_locators.py": LOCATORS}, "2026-11-19T00:00:00Z")
        agentctl.commit({"quality/types-baseline.tsv": "mypy-errors\talpha\t99\n"}, "2026-11-21T00:00:00Z")
        lines = printed("--repo-dir", self.movies.root, "--agentctl", agentctl.root)
        rows = lambda **labels: value(lines, qm.ROWS, repo="agentctl", **labels)  # noqa: E731
        total = lambda **labels: value(lines, qm.SUM, repo="agentctl", **labels)  # noqa: E731
        stamp = " 1795169700"
        self.assertEqual("20" + stamp, total(family="quality", language="py", measure="mypy-errors"))
        self.assertEqual("2" + stamp, rows(family="ceilings", language="py", measure="ctx-parameters"))
        self.assertEqual("6" + stamp, total(family="ceilings", language="py", measure="ctx-parameters"))
        self.assertEqual("1" + stamp, total(family="ceilings", language="py", measure="locators"))
        self.assertEqual("8" + stamp, total(family="ceilings", language="py", measure="locator-reader-modules"))
        self.assertEqual("12" + stamp, total(family="ceilings", language="py", measure="locator-reader-functions"))
        self.assertEqual("0" + stamp, rows(family="design", language="py", measure="state"))

    def test_a_backfill_reads_each_windows_last_commit(self) -> None:
        self.movies.commit({"test/resources/py-quality-baseline.tsv": "cognitive\tscripts/a.py:f\t14\n"},
                           "2026-11-20T10:45:00Z")
        self.movies.commit({"test/resources/py-quality-baseline.tsv": ""}, "2026-11-20T12:05:00Z")
        history = qm.History(self.movies.root)
        stamps = [stamp for _, stamp in history.every("HEAD", 3650, 1)]
        self.assertEqual([1795171500, 1795176300], stamps)  # 10:45 (the 10:00 hour's last), 12:05
        lines = printed("--repo-dir", self.movies.root, "--backfill-days", "3650")
        cognitive = [line.split(" ", 1)[1] for line in lines
                     if line.startswith(qm.ROWS) and 'language="py",measure="cognitive"' in line]
        self.assertEqual(["1 1795171500", "0 1795176300"], cognitive)

    def test_the_payload_is_two_gauges_stamped_in_nanoseconds(self) -> None:
        found = qm.samples(qm.MOVIES, {("quality", "py", "pyright"): (2, 10.0)}, 1795169700)
        document = qm.payload(found)
        resource = document["resourceMetrics"][0]
        self.assertEqual(qm.SERVICE, resource["resource"]["attributes"][0]["value"]["stringValue"])
        metrics = {m["name"]: m["gauge"]["dataPoints"] for m in resource["scopeMetrics"][0]["metrics"]}
        self.assertEqual({qm.ROWS: 2.0, qm.SUM: 10.0}, {name: points[0]["asDouble"] for name, points in metrics.items()})
        self.assertEqual("1795169700000000000", metrics[qm.ROWS][0]["timeUnixNano"])
        self.assertEqual({"family": "quality", "language": "py", "measure": "pyright", "repo": "movies"},
                         {a["key"]: a["value"]["stringValue"] for a in metrics[qm.ROWS][0]["attributes"]})

    def test_this_checkouts_baselines_read_back_row_for_row(self) -> None:
        totals = qm.snapshot(qm.MOVIES, qm.shell_py_progress.Tree(ROOT, "HEAD"))
        oracle: Dict[tuple, int] = {}
        for baseline in (b for b in qm.MOVIES.baselines if b.language):
            text = qm.shell_py_progress.Tree(ROOT, "HEAD").texts([baseline.path]).get(baseline.path, "")
            for line in text.splitlines():
                kind = line.split("\t")[0]
                if not line.startswith("#") and line.count("\t") == 2 and kind not in qm.SKIPPED_KINDS:
                    key = (baseline.family, baseline.language, kind)
                    oracle[key] = oracle.get(key, 0) + 1
        self.assertGreater(sum(oracle.values()), 100)
        for key, count in oracle.items():
            self.assertEqual(count, totals[key][0], key)

    def test_the_swift_and_kotlin_baselines_are_sent(self) -> None:
        totals = qm.snapshot(qm.MOVIES, qm.shell_py_progress.Tree(ROOT, "HEAD"))
        for key in (("quality", "swift", "cognitive"), ("quality", "kotlin", "copies")):
            self.assertIn(key, totals)

if __name__ == "__main__":
    unittest.main()
