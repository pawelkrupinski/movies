"""What the alert backtest asks of Prometheus, alert by alert (infra/bin/backtest-alerts; unit-tested in
infra/test/test_backtest_alerts.py): one alert's firing statistics over a window, what a rule reads that was rolled out
inside it, the conditional-metric entries gone stale, and the catalogued incidents each replayed against its alerts."""

from __future__ import annotations

import concurrent.futures
import json
import os
import sys
from typing import Any, Dict, FrozenSet, Iterable, List, Mapping, Optional, Sequence, Set, Tuple

from backtest_prometheus import Prometheus
from backtest_rules import ALLOWLIST, Rule
from backtest_series import Stats, grid_of, iso, parse_time, rollout_start, seconds, summarise

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "test"))
from promql_selectors import metric_names, selectors  # noqa: E402  (shared with the dashboards' name check)

Problems = List[str]


def backtest(prom: Prometheus, rule: Rule, start: float, end: float, step: float, floor: Optional[float],
             excused: Sequence[Tuple[float, float]] = (), lead_in: bool = True) -> Stats:
    """Statistics for one alert over [start, end], at `step` or its group's interval if longer.

    Prometheus asks a rule its question once per group interval; a finer grid only asks the same
    one again, and a rule in a slow group is slow because each evaluation is costly
    (CityDarkInBothTiers, every five minutes, read six days of a per-city census raw: a day at a
    one-minute step ran past the query timeout, at five minutes it took 24 s)."""
    step = max(step, int(seconds(rule.get("interval"))))
    hold = seconds(rule.get("for"))
    keep = seconds(rule.get("keep_firing_for"))
    lead = start
    if lead_in:
        # Start early by the hold so an alert already pending at `start` keeps its age, on the
        # same step grid so every chunk lines up.
        lead = max(start - hold - step, floor or 0)
        lead = start - ((start - lead) // step) * step
    try:
        present = prom.presence(rule["expr"], lead, end, step)
    except RuntimeError as err:
        return {"error": str(err), "returned_data": False}
    stats = summarise(present, grid_of(lead, end, step), hold, keep, start, excused)
    stats["days"] = (end - start) / 86400.0
    return stats


def observed_fraction(prom: Prometheus, name: str, start: float, end: float, step: float) -> Optional[float]:
    """How much of the window Prometheus ITSELF had this alert firing (ALERTS), for comparison.

    It differs from the simulation wherever the deployed rule differs from this tree's, or the
    alert did not exist yet -- which is the point of printing both.
    """
    try:
        present = prom.presence('max(ALERTS{alertname="%s",alertstate="firing"})' % name,
                                start, end, step)
    except RuntimeError:
        return None
    steps: Set[float] = set().union(*present.values())
    return len(steps) / float(len(grid_of(start, end, step)))


def _matcher_text(matcher: Any) -> str:
    return '%s%s%s' % (matcher.label, matcher.op, json.dumps(matcher.value))


def rollout_queries(expr: str, existing: Set[str], conditional: Iterable[str]) -> Set[str]:
    """What `expr` reads that may have been rolled out inside the window: each metric name it reads that has
    series in the window, and each selector over such a name with its label matchers, as written. A name can
    exist from the start while the series a rule selects appear only later -- `coredns_dns_responses_total` under
    another job, `mongodb_collstats_storageStats_storageSize` for other collections (Alert backtest #1:
    CoreDnsMetricsAbsent and IdentityRetainedSizeAbsent fired in the replay until their selector appeared, and
    never in production)."""
    names: Set[str] = (metric_names(expr) - set(conditional)) & existing
    found = set(names)
    for selector in selectors(expr):
        if selector.name in names and selector.label_matchers:
            found.add("%s{%s}" % (selector.name, ",".join(_matcher_text(m) for m in selector.label_matchers)))
    return found


def conditional_metrics_to_check(conditional: Iterable[str], existing: Set[str], alerts: Mapping[str, Rule],
                                 selected: Optional[Iterable[str]] = None) -> Set[str]:
    """The conditional_metrics entries to judge for staleness: those with series in the window, and in a scoped run
    (`selected`) only those its alerts read. An entry no selected alert reads is not that run's question: run on the
    laptop for one alert (infra/bin/backtest-alerts-local), five unrelated stale entries made every scoped run FAIL,
    so a fix replay could never read head=PASS."""
    present = set(conditional) & existing
    if selected is None:
        return present
    read: Set[str] = set().union(*[metric_names(alerts[name]["expr"]) for name in selected])
    return present & read


def stale_conditional_metrics(rollouts: Mapping[str, float], conditional: Iterable[str]) -> Problems:
    """A conditional metric (published only in an exceptional state) comes and goes; one ROLLED OUT inside the
    window -- absent, then present for the rest of it -- is a metric that was waiting for a deploy, and its entry
    outlived it: rollout clipping skips a conditional metric, so the absent()-style alerts reading it are blamed
    for the days before (Alert backtest #3: CoreDnsMetricsAbsent, IdentityRetainedSizeAbsent)."""
    return ["%s: listed in conditional_metrics but rolled out at %s and present since -- refresh metric-names.json"
            " (infra/bin/snapshot-label-values --names-only, which test_metric_names.py reads in its place) and"
            " strike its entry in %s" % (m, iso(rollouts[m]), os.path.basename(ALLOWLIST))
            for m in sorted(conditional) if m in rollouts]


def _rollout_of(prom: Prometheus, metric: str, start: float, end: float, step: float) -> Tuple[str, Optional[float]]:
    query = "count(%s)" % metric if "{" in metric else 'count({__name__="%s"})' % metric
    try:
        present = prom.presence(query, start, end, step, chunk=end - start + step)
    except RuntimeError:
        return metric, None
    steps: Set[float] = set().union(*present.values())
    return metric, rollout_start(steps, grid_of(start, end, step))


def metric_rollouts(prom: Prometheus, metrics: Iterable[str], start: float, end: float, jobs: int) -> Dict[str, float]:
    """{metric or selector: rollout time} for each one in `metrics` (rollout_queries) deployed inside the window."""
    step = 900
    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as pool:
        found = pool.map(lambda metric: _rollout_of(prom, metric, start, end, step), sorted(metrics))
        return {m: t for m, t in found if t is not None}


def excused_windows(catalogue: Iterable[Mapping[str, Any]], step: float) -> Dict[str, List[Tuple[float, float]]]:
    """{alert: [(start, end)]} -- the incident windows each alert is EXPECTED to fire in."""
    windows: Dict[str, List[Tuple[float, float]]] = {}
    for incident in catalogue:
        start, end = parse_time(incident["start"]), parse_time(incident["end"])
        end += seconds(incident.get("grace", "1h"))
        for name in incident.get("expect_firing") or []:
            windows.setdefault(name, []).append((start - start % step, end))
    return windows


# ── THE INCIDENT CATALOGUE ───────────────────────────────────────────────────────────────────────


def _contradiction(ident: str, name: str, should_fire: bool, first: Optional[str], start: float, end: float) -> Problems:
    if should_fire and not first:
        return ["incident %s: %s should have fired between %s and %s and did not" % (ident, name, iso(start), iso(end))]
    if first and not should_fire:
        return ["incident %s: %s should have stayed quiet and fired at %s" % (ident, name, first)]
    return []


def _window_end(incident: Mapping[str, Any], should_fire: bool, step: float, now: float) -> float:
    end = parse_time(incident["end"])
    grace = seconds(incident.get("grace", "1h"))
    window_end = min(end + grace if should_fire else end, now)
    return window_end - window_end % step


def _check_expectation(prom: Prometheus, alerts: Mapping[str, Rule], incident: Mapping[str, Any], name: str,
                       should_fire: bool, start: float, step: float, floor: Optional[float],
                       now: float) -> Tuple[Optional[str], Problems]:
    """(when the alert first fired, or "never" -- None when it could not be asked, the contradictions)."""
    ident = incident["id"]
    if name not in alerts:
        return None, ["incident %s names %s, which no rule defines" % (ident, name)]
    window_end = _window_end(incident, should_fire, step, now)
    stats = backtest(prom, alerts[name], start, window_end, step, floor)
    if stats.get("error"):
        return None, ["incident %s: %s query failed: %s" % (ident, name, stats["error"])]
    first = iso(stats["runs"][0][0]) if stats["runs"] else None
    return first or "never", _contradiction(ident, name, should_fire, first, start, window_end)


def _expectations(incident: Mapping[str, Any]) -> List[Tuple[str, bool]]:
    return [(n, True) for n in incident.get("expect_firing") or []] + \
           [(n, False) for n in incident.get("expect_quiet") or []]


def _check_incident(prom: Prometheus, alerts: Mapping[str, Rule], incident: Mapping[str, Any], step: float,
                    floor: Optional[float], only: Optional[FrozenSet[str]], now: float) -> Tuple[Dict[str, Any], Problems]:
    ident = incident["id"]
    start = parse_time(incident["start"])
    if floor and start < floor:
        # Outside the history the replay can read: reported, never a failure (the owner, 2026-11-18,
        # Alert backtest #1) -- every catalogued incident ages past retention, and the catalogue is kept.
        why = "starts before retention (%s)" % iso(floor)
        print("  incident %-44s skipped: %s" % (ident, why))
        return {"id": ident, "skipped": why}, []
    start -= start % step
    firing: Dict[str, str] = {}
    quiet: Dict[str, str] = {}
    problems: Problems = []
    for name, should_fire in _expectations(incident):
        if only and name not in only:
            continue
        first, found = _check_expectation(prom, alerts, incident, name, should_fire, start, step, floor, now)
        _record(firing if should_fire else quiet, name, first)
        problems += found
    print("  incident %-44s %s" % (ident, "FAILED" if problems else "ok"))
    return {"id": ident, "firing": firing, "quiet": quiet}, problems


def _record(seen: Dict[str, str], name: str, first: Optional[str]) -> None:
    if first is not None:
        seen[name] = first


def check_incidents(prom: Prometheus, alerts: Mapping[str, Rule], catalogue: Iterable[Mapping[str, Any]], step: float,
                    floor: Optional[float], only: Optional[Iterable[str]], end: float) -> Tuple[List[Dict[str, Any]], Problems]:
    """Replay each catalogued incident window up to `end`, where the history does; ([result], [problem])."""
    results: List[Dict[str, Any]] = []
    problems: Problems = []
    chosen = frozenset(only) if only else None
    for incident in catalogue:
        result, found = _check_incident(prom, alerts, incident, step, floor, chosen, end - step)
        results.append(result)
        problems += found
    return results, problems
