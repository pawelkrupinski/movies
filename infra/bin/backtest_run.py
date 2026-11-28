"""One alert backtest, start to finish (infra/bin/backtest-alerts): the window, the alerts asked for, each alert's
statistics and verdict, the catalogued incidents, the table that is printed and the baseline that is written."""

from __future__ import annotations

import argparse
import concurrent.futures
import importlib.machinery
import importlib.util
import json
import os
from dataclasses import dataclass
from typing import Any, Dict, List, Mapping, NamedTuple, Optional, Protocol, Sequence, Set, Tuple

from backtest_clock import SYSTEM, Clock
from backtest_checks import (backtest, check_incidents, conditional_metrics_to_check, excused_windows, metric_rollouts,
                             observed_fraction, rollout_queries, stale_conditional_metrics)
from backtest_prometheus import Prometheus
from backtest_rules import (ALLOWLIST, BASELINE, HERE, INCIDENTS, Rule, expand_recordings, load_recordings, load_rules,
                            load_yaml, select_alerts, stale_recordings)
from backtest_series import Stats, iso, noisy_episodes, parse_time, snap_up, verdict

from promql_selectors import metric_names  # noqa: E402  (infra/test, put on the path by backtest_checks)

Report = Dict[str, Dict[str, Any]]


class Source(Protocol):
    """What the window is read from: production's Prometheus, or a pulled snapshot of its blocks."""

    def loaded_alerts(self) -> Set[str]: ...
    def loaded_recordings(self) -> Dict[str, str]: ...
    def floor(self) -> Optional[float]: ...
    def latest(self) -> float: ...


class Window(NamedTuple):
    start: float
    end: float
    step: int
    floor: Optional[float]


class Result(NamedTuple):
    clipped: float
    stats: Stats
    observed: Optional[float]


def _read_text(path: str) -> str:
    with open(path, encoding="utf-8", errors="replace") as handle:
        return handle.read()


def _window(args: argparse.Namespace, source: Source) -> Window:
    step = args.step
    end = parse_time(args.end) if args.end else source.latest()
    end = end - (end % step)
    start = end - args.days * 86400
    floor = source.floor()
    if floor and start < floor:
        print("note: retention starts %s, so the window is clipped to it" % iso(floor))
        start = snap_up(floor, step)
    return Window(start, end, step, floor)


def _normaliser(prom: Prometheus) -> Any:
    formatted: Dict[str, str] = {}

    def normalise(name: str, expr: str) -> str:
        if expr not in formatted:
            try:
                formatted[expr] = prom.format_query(expr)
            except RuntimeError:
                formatted[expr] = expr
        return formatted[expr]
    return normalise


def _selection(args: argparse.Namespace, alerts: Mapping[str, Rule]) -> Tuple[List[str], Optional[str]]:
    """(the alerts to backtest, why the run is refused)."""
    unknown = set(args.alert or []) - set(alerts)
    if unknown:
        return [], "unknown alert(s): %s" % ", ".join(sorted(unknown))
    unknown_groups = set(args.group) - {rule["file"] for rule in alerts.values()}
    if unknown_groups:
        return [], "unknown rule file(s): %s" % ", ".join(sorted(unknown_groups))
    log_text = _read_text(args.from_log) if args.from_log else None
    return select_alerts(alerts, args.alert or [], args.group, log_text) or sorted(alerts), None


def _inline_recordings(alerts: Dict[str, Rule], records: Mapping[str, Rule], stale: Set[str],
                       selected: Sequence[str]) -> Dict[str, str]:
    """Only the selected alerts are inlined, and one that cannot be is that alert's error, not the run's: a recording
    read with matchers has no expression to stand in for it (expand_recordings), and a crash there ended every run."""
    unexpandable: Dict[str, str] = {}
    for name in selected:
        try:
            alerts[name]["expr"] = expand_recordings(alerts[name]["expr"], records, stale)
        except ValueError as err:
            unexpandable[name] = str(err)
    return unexpandable


def _allowlist_problems(allowed: Mapping[str, Any], conditional: Mapping[str, Any], alerts: Mapping[str, Rule]) -> List[str]:
    problems = ["%s: allowlisted in %s but no rule defines it" % (name, os.path.basename(ALLOWLIST))
                for name in sorted(set(allowed) - set(alerts))]
    entries = sorted(list(allowed.items()) + list(conditional.items()))
    return problems + ["%s: allowlist entry has no reason" % name for name, entry in entries
                       if not (entry or {}).get("reason")]


def _budget(defaults: Mapping[str, Any], days: float) -> Dict[str, float]:
    return {
        "days": days,
        "max_firing_fraction": float(defaults.get("max_firing_fraction", 0.05)),
        "max_episodes_per_week": float(defaults.get("max_episodes_per_week", 7)),
        "min_judged_days": float(defaults.get("min_judged_days", 7)),
    }


class AlertRuns(object):
    """Backtests of the selected alerts, each over the part of the window its metrics existed in."""

    def __init__(self, prom: Prometheus, alerts: Mapping[str, Rule], window: Window, rollouts: Mapping[str, float],
                 watched: Mapping[str, Set[str]], excused: Mapping[str, Any], unexpandable: Mapping[str, str]) -> None:
        self.prom, self.alerts, self.window = prom, alerts, window
        self.rollouts, self.watched, self.excused, self.unexpandable = rollouts, watched, excused, unexpandable

    def run(self, name: str) -> Tuple[str, Result]:
        start, end, step, floor = self.window
        if name in self.unexpandable:
            return name, Result(start, {"error": self.unexpandable[name], "returned_data": False}, None)
        clipped = snap_up(max([start] + [self.rollouts[m] for m in self.watched[name] if m in self.rollouts]), step)
        if clipped >= end - step:
            return name, Result(clipped, _nothing_fired(), None)
        rule = self.alerts[name]
        stats = backtest(self.prom, rule, clipped, end, step, floor, self.excused.get(name, ()), lead_in=clipped == start)
        observed = None if rule.get("grafana") else observed_fraction(self.prom, name, clipped, end, step)
        return name, Result(clipped, stats, observed)

    def run_all(self, selected: Sequence[str], jobs: int) -> Dict[str, Result]:
        with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as pool:
            return dict(pool.map(self.run, selected))


def _nothing_fired() -> Stats:
    return {"returned_data": True, "firing_fraction": 0.0, "episodes": 0, "budget_firing_fraction": 0.0,
            "budget_episodes": 0, "series_episodes": 0, "peak_series": 0, "runs": []}


# ── THE REPORT ───────────────────────────────────────────────────────────────────────────────────


def _outside_incident_fields(stats: Stats) -> Dict[str, Any]:
    if stats.get("budget_firing_fraction", 0) == stats.get("firing_fraction", 0):
        return {}
    return {"firing_fraction_outside_incidents": round(stats["budget_firing_fraction"], 4),
            "episodes_outside_incidents": stats["budget_episodes"]}


def _context_fields(result: Result, start: float, missing: Sequence[str], allow: Mapping[str, Any]) -> Dict[str, Any]:
    fields: Dict[str, Any] = {}
    if result.observed is not None:
        fields["observed_firing_fraction"] = round(result.observed, 4)
    if result.clipped != start:
        fields["metrics_rolled_out_at"] = iso(result.clipped)
    if missing:
        fields["missing_metrics"] = list(missing)
    if allow:
        fields["allowlisted"] = allow.get("reason", "")
    return fields


def _episode_fields(stats: Stats, status: str) -> Dict[str, Any]:
    fields: Dict[str, Any] = {}
    if stats.get("runs"):
        longest = max(stats["runs"], key=lambda r: r[1] - r[0])
        fields["longest_episode"] = [iso(longest[0]), iso(longest[1])]
    if status == "noisy":
        fields["noisy_episodes_total"], fields["noisy_episodes"] = noisy_episodes(stats)
    if stats.get("error"):
        fields["error"] = stats["error"]
    return fields


def report_entry(rule: Rule, status: str, result: Result, start: float, missing: Sequence[str],
                 allow: Mapping[str, Any]) -> Dict[str, Any]:
    stats = result.stats
    entry: Dict[str, Any] = {
        "file": rule["file"],
        "for": rule.get("for", "0s"),
        "status": status,
        "firing_fraction": round(stats.get("firing_fraction", 0.0), 4),
        "episodes": stats.get("episodes", 0),
        "series_episodes": stats.get("series_episodes", 0),
        "peak_series": stats.get("peak_series", 0),
    }
    for fields in (_outside_incident_fields(stats), _context_fields(result, start, missing, allow),
                   _episode_fields(stats, status)):
        entry.update(fields)
    return entry


def _print_table(selected: Sequence[str], report: Report) -> None:
    width = max(len(n) for n in selected)
    print("  %-*s  %-12s  %8s  %8s  %8s  %4s" % (width, "alert", "status", "firing", "(ALERTS)", "episodes", "peak"))
    for name in selected:
        e = report[name]
        obs = e.get("observed_firing_fraction")
        print("  %-*s  %-12s  %7.2f%%  %8s  %8d  %4d%s%s"
              % (width, name, e["status"], 100 * e["firing_fraction"],
                 "-" if obs is None else "%.2f%%" % (100 * obs), e["episodes"], e["peak_series"],
                 "  [since %s]" % e["metrics_rolled_out_at"] if "metrics_rolled_out_at" in e else "",
                 "  [allowlisted]" if "allowlisted" in e else ""))


def _print_notes(selected: Sequence[str], report: Report, budget: Mapping[str, float]) -> None:
    young = [n for n in selected if report[n]["status"] == "young"]
    if young:
        print("\nnote: noisy, but watched for under %g days (their metrics rolled out since): judged once they have"
              " lived that long, never failed before:" % budget["min_judged_days"])
        for name in young:
            print("  %s: since %s" % (name, report[name].get("metrics_rolled_out_at", "?")))
    pending = [n for n in selected if report[n]["status"] == "not-deployed"]
    if pending:
        print("\nnote: not loaded by Prometheus yet, and reading metrics with no series -- these fail"
              " as DEAD once deployed if the metrics still do not exist:")
        for name in pending:
            print("  %s: %s" % (name, ", ".join(report[name]["missing_metrics"])))


def refresh_metric_names(names: Set[str], days: float, end: float, path: Optional[str] = None) -> None:
    """metric-names.json from the window's metric names, as snapshot-label-values --names-only writes it (its own
    writer): the snapshot is a copy of the fleet Prometheus's history, so a conditional_metrics entry whose metric has
    arrived can be struck without asking production (Alert backtest #5/#6: five such entries, the file 9 days old)."""
    loader = importlib.machinery.SourceFileLoader("snapshot_label_values", os.path.join(HERE, "snapshot-label-values"))
    spec = importlib.util.spec_from_loader("snapshot_label_values", loader)
    assert spec is not None  # a SourceFileLoader always yields a spec
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    if path:
        setattr(module, "NAMES_OUTPUT", path)
    module.write_metric_names(names, days, end)


def _write_baseline(document: Mapping[str, Any]) -> None:
    with open(BASELINE, "w", encoding="utf-8") as handle:
        json.dump(document, handle, indent=2, sort_keys=True)
        handle.write("\n")


def _save(window: Window, budget: Mapping[str, float], report: Report, incident_report: List[Dict[str, Any]],
          existing: Set[str], days: float) -> None:
    _write_baseline({
        "window": {"start": iso(window.start), "end": iso(window.end), "step_seconds": window.step},
        "budget": {k: v for k, v in budget.items() if k != "days"},
        "alerts": report,
        "incidents": incident_report,
    })
    print("wrote %s" % os.path.relpath(BASELINE))
    refresh_metric_names(existing, days, window.end)
    print("wrote infra/test/metric-names.json from the window's metric names")


def _failure(problems: Sequence[str]) -> int:
    print("\nFAILED -- %d problem(s):" % len(problems))
    for p in problems:
        print("  - " + p)
    print("\nFix the rule (with a promtool case), catalogue the incident it fired for in %s, "
          "or allowlist it with a reason in %s."
          % (os.path.relpath(INCIDENTS), os.path.relpath(ALLOWLIST)))
    return 1


@dataclass(frozen=True)
class Inputs:
    """What the tree defines: its alerts and recordings, the allowlist, the incident catalogue and the budget."""
    alerts: Dict[str, Rule]
    records: Dict[str, Rule]
    allowed: Mapping[str, Any]
    conditional: Mapping[str, Any]
    catalogue: Sequence[Mapping[str, Any]]
    budget: Mapping[str, float]


@dataclass(frozen=True)
class Prepared:
    """A run ready to query: the window, what Prometheus holds and has loaded, and the alerts asked for."""
    inputs: Inputs
    window: Window
    existing: Set[str]
    loaded: Set[str]
    selected: List[str]
    scoped: bool
    unexpandable: Dict[str, str]


class Refused(Exception):
    """The run is asked for something that does not exist (an unknown alert or rule file)."""


def _load_inputs(days: float) -> Inputs:
    allowlist = load_yaml(ALLOWLIST, {})
    return Inputs(alerts=load_rules(),
                  # A recording changed in this tree is evaluated as written, not read from what Prometheus
                  # recorded from the deployed rule (an alert over `ci:*:reference_threshold` would otherwise
                  # be backtested against the old one, or against nothing).
                  records=load_recordings(),
                  allowed=allowlist.get("allow") or {}, conditional=allowlist.get("conditional_metrics") or {},
                  catalogue=load_yaml(INCIDENTS, {}).get("incidents") or [],
                  budget=_budget(allowlist.get("defaults") or {}, days))


def _prepare(args: argparse.Namespace, prom: Prometheus, source: Source) -> Prepared:
    inputs = _load_inputs(args.days)
    window = _window(args, source)
    existing = prom.metric_names(window.start, window.end)
    loaded = source.loaded_alerts()
    stale = stale_recordings(inputs.records, source.loaded_recordings(), _normaliser(prom), recorded=existing)
    if stale:
        print("recordings changed in this tree or not recorded in the window, evaluated as written: %s"
              % ", ".join(sorted(stale)))
    selected, refusal = _selection(args, inputs.alerts)
    if refusal:
        raise Refused(refusal)
    return Prepared(inputs, window, existing, loaded, selected, bool(args.alert or args.group or args.from_log),
                    _inline_recordings(inputs.alerts, inputs.records, stale, selected))


def _rollouts(prom: Prometheus, ready: Prepared, jobs: int) -> Tuple[Dict[str, Set[str]], Dict[str, float], List[str]]:
    """(what each selected alert reads that may have been rolled out, when each was, the stale conditional entries)."""
    alerts, conditional, window = ready.inputs.alerts, ready.inputs.conditional, ready.window
    watched = {n: rollout_queries(alerts[n]["expr"], ready.existing, conditional) for n in ready.selected}
    rollouts = metric_rollouts(prom, set().union(*watched.values()), window.start, window.end, jobs)
    checked = conditional_metrics_to_check(conditional, ready.existing, alerts, ready.selected if ready.scoped else None)
    stale = stale_conditional_metrics(metric_rollouts(prom, checked, window.start, window.end, jobs), checked)
    return watched, rollouts, stale


def _judged(ready: Prepared, results: Mapping[str, Result]) -> Tuple[Report, List[str]]:
    """(each selected alert's report entry, the problems their verdicts name)."""
    inputs = ready.inputs
    report: Report = {}
    problems: List[str] = []
    for name in ready.selected:
        rule, result = inputs.alerts[name], results[name]
        allow = inputs.allowed.get(name) or {}
        reads = metric_names(rule["expr"]) - set(inputs.conditional)
        missing = sorted(m for m in reads if m not in ready.existing)
        status, problem = verdict(result.stats, inputs.budget, allow, missing,
                                  name in ready.loaded or rule.get("grafana", False))
        report[name] = report_entry(rule, status, result, ready.window.start, missing, allow)
        if problem:
            problems.append("%s (%s): %s" % (name, rule["file"], problem))
    return report, problems


def _conclude(args: argparse.Namespace, prom: Prometheus, ready: Prepared, report: Report) -> List[str]:
    """Print the table and the notes, replay the incidents, write the baseline; the incidents' problems."""
    inputs, window = ready.inputs, ready.window
    _print_table(ready.selected, report)
    _print_notes(ready.selected, report, inputs.budget)
    incident_report: List[Dict[str, Any]] = []
    problems: List[str] = []
    if not args.no_incidents:
        incident_report, problems = check_incidents(prom, inputs.alerts, inputs.catalogue, window.step, window.floor,
                                                    set(ready.selected) if ready.scoped else None, window.end)
    if not args.no_write and not ready.scoped:
        _save(window, inputs.budget, report, incident_report, ready.existing, args.days)
    return problems


def run_backtest(args: argparse.Namespace, prom: Prometheus, source: Source, clock: Clock = SYSTEM) -> int:
    """The backtest proper, against `prom`, with what was deployed and the window from `source`."""
    started = clock.now()
    try:
        ready = _prepare(args, prom, source)
    except Refused as refusal:
        print(refusal)
        return 2
    inputs, window = ready.inputs, ready.window
    watched, rollouts, problems = _rollouts(prom, ready, args.jobs)
    problems = _allowlist_problems(inputs.allowed, inputs.conditional, inputs.alerts) + problems
    print("backtesting %d alert(s) over %s .. %s at %ds"
          % (len(ready.selected), iso(window.start), iso(window.end), window.step))
    runs = AlertRuns(prom, inputs.alerts, window, rollouts, watched, excused_windows(inputs.catalogue, window.step),
                     ready.unexpandable)
    report, verdict_problems = _judged(ready, runs.run_all(ready.selected, args.jobs))
    problems += verdict_problems + _conclude(args, prom, ready, report)
    print("backtest took %.0f s with %d parallel queries" % (clock.now() - started, args.jobs))
    if problems:
        return _failure(problems)
    print("\nok")
    return 0
