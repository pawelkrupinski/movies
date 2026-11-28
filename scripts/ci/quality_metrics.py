#!/usr/bin/env python3
"""THE CODE-QUALITY SERIES (the "Code quality" Grafana board, infra/nix/files/monitoring/grafana/dashboards/apps/
code-quality.json; the owner, 2026-11-27): every ratchet baseline a revision COMMITS, read back -- nothing is
re-measured -- as two OTLP gauges per repo, language, family and measure, pushed on the CI timings' metrics path
(grafana_otlp.py: Grafana's token gate in front of Prometheus's OTLP receiver):

  kinowo_quality_rows{repo, language, family, measure}   the frozen rows: each a method, file, clone set, package or
                                                          module past its limit (the migration: the tools done / left)
  kinowo_quality_sum{...}                                 those rows' values added up (the errors, the lines, the fan-out)

    # TYPE kinowo_quality_rows gauge
    # TYPE kinowo_quality_sum gauge

  family quality   Scala's complexity / length / duplication / coupling baselines, py-quality's (pyright: the strict
                   errors), js-quality's, the Swift and Kotlin ratchets' (swift / kotlin-quality-baseline.tsv);
                   agentctl's quality/quality-baseline.tsv and its mypy-errors
  family design    the general rule core's design-baseline.tsv, each key's language by its path; an `effect` row is
                   effect-clock-process-env (ONE row a module for those three: the baseline does not split them) or
                   effect-files / effect-storage / effect-network
  family migration the shell-to-Python MIGRATE list (scripts/shell_py_progress.py): migrate-done (retired counted
                   in), migrate-left; from 2026-11-22, when the list was committed; and the shell migration gate's
                   frozen rows (shell-baseline.tsv, language shell: the queue a shell tool past a limit joins)
  family ceilings  agentctl's frozen `ctx` parameters and service locators (tests/test_no_new_ctx_parameters.py
                   CEILINGS, tests/test_no_service_locators.py FROZEN and READER_CEILINGS)

Every measure a language is held to is sent, 0 when it has no row, so a cleared kind draws a line at 0 and not a gap.
Every sample is stamped at the movies commit's committer time (agentctl: its last commit by then), so a re-push is an
identical duplicate and a backfill is the same reading at past commits (Prometheus keeps 90 days out of order).

  quality_metrics.py [--repo-dir DIR] [--rev REV] [--agentctl DIR] [--agentctl-rev REV]
                     [--backfill-days N] [--every-hours H] [--push]

  --rev             the movies revision read (default HEAD); with --backfill-days, the history's tip
  --agentctl        an agentctl clone (blobs may be lazy: a partial clone fetches the few it reads)
  --backfill-days   every H hours' last first-parent commit of the last N days instead of --rev alone
  --push            POST to <GRAFANA_URL>/ci/otlp/v1/metrics with GRAFANA_ANNOTATION_TOKEN; else print the samples

Prints `name{labels} value stamp` per sample (and with --push what was pushed). Exit 0; 1 when a push failed."""
from __future__ import annotations

import argparse
import ast
import os
import sys
from dataclasses import dataclass
from typing import Callable, Dict, Iterable, List, Optional, Sequence, Tuple, cast

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))
sys.path.insert(0, os.path.join(os.path.dirname(HERE), "lib"))
import ci_machine  # noqa: E402
import grafana_otlp  # noqa: E402
import shell_py_progress  # noqa: E402  (scripts/: the migration's Tree and its done / left reading)
import tool_git  # noqa: E402

SERVICE = "kinowo-quality"
ROWS, SUM = "kinowo_quality_rows", "kinowo_quality_sum"
SKIPPED_KINDS = frozenset({"limit", "distance-cap", "module-edge", "ce", "tolerance"})
EFFECT_USES = ("files", "storage", "network")
MIGRATE_TOOLS = "scripts/shell-py-progress/migrate-tools.tsv"
JS_AREAS = "scripts/js-quality/ratchet-areas.tsv"
BATCH = 40  # revisions a POST

FUNCTION_KINDS = ("cognitive", "cyclomatic", "inner-defs", "method-lines", "file-lines", "copies")
MOBILE_KINDS = ("cognitive", "cyclomatic", "method-lines", "file-lines", "copies")  # lizard reads no inner defs
SHELL_KINDS = ("shell-lines", "shell-decisions", "shell-function-lines", "shell-nesting")  # the migration queue
NAMING = ("suffix", "abbreviation")
EFFECTS = ("effect-clock-process-env", "effect-files", "effect-storage", "effect-network", "state", "import-io")
MODULE_DESIGN = EFFECTS + ("fanout", "distance", "layer", "unplaced")

Key = Tuple[str, str, str]  # family, language, measure
Totals = Dict[Key, Tuple[int, float]]


@dataclass(frozen=True)
class Baseline:
    path: str
    family: str
    language: str  # "" -- each key's, by its path


@dataclass(frozen=True)
class Repo:
    name: str
    baselines: Tuple[Baseline, ...]
    expected: Dict[Tuple[str, str], Tuple[str, ...]]  # (family, language) -> the measures it is held to


MOVIES = Repo("movies", (
    Baseline("test/resources/complexity-baseline.tsv", "quality", "scala"),
    Baseline("test/resources/length-baseline.tsv", "quality", "scala"),
    Baseline("test/resources/duplication-baseline.tsv", "quality", "scala"),
    Baseline("test/resources/coupling-baseline.tsv", "quality", "scala"),
    Baseline("test/resources/py-quality-baseline.tsv", "quality", "py"),
    Baseline("test/resources/js-quality-baseline.tsv", "quality", "js"),
    Baseline("test/resources/swift-quality-baseline.tsv", "quality", "swift"),
    Baseline("test/resources/kotlin-quality-baseline.tsv", "quality", "kotlin"),
    Baseline("test/resources/design-baseline.tsv", "design", ""),
    Baseline("test/resources/shell-baseline.tsv", "migration", "shell"),
), {
    ("quality", "scala"): FUNCTION_KINDS + ("fanout", "distance", "cycle-edge", "datafanout", "databag"),
    ("quality", "py"): FUNCTION_KINDS + ("cycle", "pyright"),
    ("quality", "js"): FUNCTION_KINDS + ("cycle",),
    ("quality", "swift"): MOBILE_KINDS,
    ("quality", "kotlin"): MOBILE_KINDS,
    ("migration", "shell"): SHELL_KINDS,
    ("design", "scala"): NAMING,
    ("design", "py"): NAMING + MODULE_DESIGN + ("dircycle", "databag", "datafanout"),
    ("design", "js"): NAMING + MODULE_DESIGN,
})
AGENTCTL = Repo("agentctl", (
    Baseline("quality/quality-baseline.tsv", "quality", "py"),
    Baseline("quality/types-baseline.tsv", "quality", "py"),
    Baseline("quality/design-baseline.tsv", "design", "py"),
), {
    ("quality", "py"): FUNCTION_KINDS + ("cycle", "fan-out", "mypy-errors"),
    ("design", "py"): NAMING + MODULE_DESIGN,
})


def count(value: object) -> float:
    """A ceiling's count, or one for a FROZEN entry (its value is the reason it is kept)."""
    return float(value) if isinstance(value, (int, float)) else 1.0


def part(index: int) -> Callable[[object], float]:
    """A READER_CEILINGS value's `index`-th count: (modules, functions)."""
    return lambda value: count(cast(Tuple[object, ...], value)[index]) if isinstance(value, tuple) else 0.0


# agentctl's ceilings: (test file, its dict, each measure and how a value counts)
CEILINGS: Tuple[Tuple[str, str, Tuple[Tuple[str, Callable[[object], float]], ...]], ...] = (
    ("tests/test_no_new_ctx_parameters.py", "CEILINGS", (("ctx-parameters", count),)),
    ("tests/test_no_service_locators.py", "FROZEN", (("locators", count),)),
    ("tests/test_no_service_locators.py", "READER_CEILINGS",
     (("locator-reader-modules", part(0)), ("locator-reader-functions", part(1)))))

def table_rows(text: str) -> List[Tuple[str, str, float]]:
    """A baseline's `kind TAB key TAB value` rows past their limits (the limits and information rows left out)."""
    out: List[Tuple[str, str, float]] = []
    for line in text.splitlines():
        cells = line.split("\t")
        if line.startswith("#") or len(cells) != 3 or cells[0] in SKIPPED_KINDS:
            continue
        try:
            out.append((cells[0], cells[1], float(cells[2])))
        except ValueError:
            continue
    return out


def measure_of(kind: str, key: str) -> str:
    if kind != "effect":
        return kind
    use = key.rsplit(":", 1)[-1] if ":" in key else ""
    return "effect-" + use if use in EFFECT_USES else "effect-clock-process-env"


def language_of(key: str, js_roots: Tuple[str, ...]) -> str:
    """A design key's language: its file's (or a `<from>-><to>` edge's first), as the rule core files it."""
    path = key.split("->", 1)[0].split(":", 1)[0]
    if path.endswith(".scala"):
        return "scala"
    return "js" if js_roots and path.startswith(js_roots) else "py"


def js_roots(text: str) -> Tuple[str, ...]:
    """The production JS/TS roots, ratchet-areas.tsv's third column."""
    return tuple(cells[2] for cells in (line.split("\t") for line in text.splitlines() if not line.startswith("#"))
                 if len(cells) >= 3)


def add(totals: Totals, key: Key, rows: int, total: float) -> None:
    before = totals.get(key, (0, 0.0))
    totals[key] = (before[0] + rows, before[1] + total)


def baseline_totals(totals: Totals, baseline: Baseline, text: str, roots: Tuple[str, ...]) -> None:
    for kind, key, value in table_rows(text):
        language = baseline.language or language_of(key, roots)
        add(totals, (baseline.family, language, measure_of(kind, key)), 1, value)


def migration_totals(totals: Totals, tree: shell_py_progress.Tree, listing: str) -> None:
    tools = [shell_py_progress.Tool(int(c[0]), c[1], c[2]) for c in (
        (line.split("\t") + ["", ""]) for line in listing.splitlines() if line.strip() and not line.startswith("#"))]
    left = sum(1 for tool in tools if shell_py_progress.state(tree, tool) == "left")
    add(totals, ("migration", "shell", "migrate-done"), len(tools) - left, len(tools) - left)
    add(totals, ("migration", "shell", "migrate-left"), left, left)


def literal(text: str, name: str) -> Optional[List[object]]:
    """The values of a module's top-level `name = {<literal>}`, or None."""
    for node in ast.parse(text).body:
        if isinstance(node, ast.Assign) and [getattr(t, "id", None) for t in node.targets] == [name]:
            found: object = ast.literal_eval(node.value)
            return list(cast(Dict[object, object], found).values()) if isinstance(found, dict) else None
    return None


def ceiling_totals(totals: Totals, texts: Dict[str, str]) -> None:
    for path, name, measures in CEILINGS:
        values = literal(texts[path], name) if path in texts else None
        for measure, reading in measures if values is not None else ():
            add(totals, ("ceilings", "py", measure), len(values or ()), sum(reading(v) for v in values or ()))


def snapshot(repo: Repo, tree: shell_py_progress.Tree) -> Totals:
    """Every measure of `repo` at the tree's revision."""
    extra = [MIGRATE_TOOLS, JS_AREAS] if repo is MOVIES else [path for path, _, _ in CEILINGS]
    texts = tree.texts([b.path for b in repo.baselines] + extra)
    totals: Totals = {}
    roots = js_roots(texts.get(JS_AREAS, ""))
    for baseline in repo.baselines:
        if baseline.path in texts:
            baseline_totals(totals, baseline, texts[baseline.path], roots)
    for (family, language), measures in repo.expected.items():
        for measure in measures:
            add(totals, (family, language, measure), 0, 0.0)
    if MIGRATE_TOOLS in texts:
        migration_totals(totals, tree, texts[MIGRATE_TOOLS])
    if repo is AGENTCTL:
        ceiling_totals(totals, texts)
    return totals


@dataclass(frozen=True)
class Sample:
    name: str
    labels: Tuple[Tuple[str, str], ...]
    value: float
    stamp: int  # Unix seconds

    def text(self) -> str:
        labels = ",".join('%s="%s"' % pair for pair in self.labels)
        return "%s{%s} %s %d" % (self.name, labels, ("%g" % self.value), self.stamp)


def samples(repo: Repo, totals: Totals, stamp: int) -> List[Sample]:
    out: List[Sample] = []
    for (family, language, measure), (rows, total) in sorted(totals.items()):
        labels = (("family", family), ("language", language), ("measure", measure), ("repo", repo.name))
        out += [Sample(ROWS, labels, rows, stamp), Sample(SUM, labels, total, stamp)]
    return out


def payload(found: Sequence[Sample]) -> grafana_otlp.Json:
    by_name: Dict[str, List[grafana_otlp.Json]] = {ROWS: [], SUM: []}
    for sample in found:
        by_name[sample.name].append(grafana_otlp.point(str(sample.stamp * 1000000000), sample.value, dict(sample.labels)))
    return grafana_otlp.document(SERVICE, [grafana_otlp.gauge(name, points) for name, points in by_name.items()])


class History:
    """One repository's commits, read through git (tool_git: the one way the tools run it)."""

    def __init__(self, root: str) -> None:
        self.root = root
        self.git = tool_git.Git.here(root)

    def stamp(self, rev: str) -> int:
        return int(self.git.text("log", "-1", "--format=%ct", rev).strip())

    def at(self, tip: str, stamp: int) -> Optional[str]:
        """The tip's last first-parent commit at or before `stamp`, or None."""
        return self.git.line("log", "-1", "--first-parent", "--before=%d" % stamp, "--format=%H", tip)

    def every(self, tip: str, days: int, hours: int) -> List[Tuple[str, int]]:
        """The last first-parent commit of each `hours` window of the last `days` days, oldest first."""
        listed = self.git.lines("log", "--first-parent", "--since=%d.days" % days, "--format=%H %ct", tip)
        kept: Dict[int, Tuple[str, int]] = {}
        for line in reversed(listed):
            sha, stamp = line.split()
            kept[int(stamp) // (hours * 3600)] = (sha, int(stamp))
        return [kept[window] for window in sorted(kept)]


def readings(movies: History, revs: Sequence[Tuple[str, int]], agentctl: Optional[History],
             agentctl_rev: str) -> Iterable[List[Sample]]:
    """Each revision's samples: movies at it, agentctl at its last commit by then, both at the movies stamp."""
    for rev, stamp in revs:
        found = samples(MOVIES, snapshot(MOVIES, shell_py_progress.Tree(movies.root, rev)), stamp)
        other = agentctl.at(agentctl_rev, stamp) if agentctl else None
        if agentctl and other:
            found += samples(AGENTCTL, snapshot(AGENTCTL, shell_py_progress.Tree(agentctl.root, other)), stamp)
        yield found


def options(argv: Optional[Sequence[str]]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="The committed ratchet baselines as Grafana's code-quality series.")
    parser.add_argument("--repo-dir", default=os.path.dirname(os.path.dirname(HERE)))
    parser.add_argument("--rev", default="HEAD")
    parser.add_argument("--agentctl", default="")
    parser.add_argument("--agentctl-rev", default="HEAD")
    parser.add_argument("--backfill-days", type=int, default=0)
    parser.add_argument("--every-hours", type=int, default=1)
    parser.add_argument("--push", action="store_true")
    return parser.parse_args(argv)


def send(batch: List[Sample]) -> Optional[str]:
    environment = ci_machine.environment()
    url, token = environment.get("GRAFANA_URL"), environment.get("GRAFANA_ANNOTATION_TOKEN")
    if not url or not token:
        return "GRAFANA_URL / GRAFANA_ANNOTATION_TOKEN unset"
    return grafana_otlp.post(payload(batch), url, token)


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = options(argv)
    movies = History(args.repo_dir)
    revs = (movies.every(args.rev, args.backfill_days, args.every_hours) if args.backfill_days
            else [(args.rev, movies.stamp(args.rev))])
    batch: List[Sample] = []
    failed = 0
    for index, found in enumerate(readings(movies, revs, History(args.agentctl) if args.agentctl else None,
                                           args.agentctl_rev), 1):
        batch += found
        if not args.push:
            print("\n".join(sample.text() for sample in found))
        elif index % BATCH == 0 or index == len(revs):
            why = send(batch)
            print("quality metrics: %d samples of %d revisions %s" % (len(batch), index, why or "pushed"))
            failed += bool(why)
            batch = []
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
