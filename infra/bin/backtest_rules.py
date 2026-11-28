"""The alert backtest's RULES: the alerts and recordings the tree defines (the Prometheus rule files and the
Grafana-managed rules), the allowlist and incident catalogue beside them, which alerts a run is asked for, and the
inlining of a recording that is not what Prometheus deployed (infra/bin/backtest-alerts; unit-tested in
infra/test/test_backtest_alerts.py). The files are read by `read_yaml` alone; everything else decides."""

from __future__ import annotations

import glob
import os
import re
from typing import Any, Callable, Dict, Iterable, List, Mapping, Optional, Sequence, Set, Tuple

import yaml

HERE = os.path.dirname(os.path.abspath(__file__))
INFRA = os.path.dirname(HERE)
RULES_GLOB = os.path.join(INFRA, "nix", "files", "monitoring", "rules", "*.rules")
GRAFANA_RULES = os.path.join(INFRA, "nix", "files", "monitoring", "grafana", "alerting", "alert-rules.yaml")
BASELINE = os.path.join(INFRA, "test", "alert-backtest.json")
ALLOWLIST = os.path.join(INFRA, "test", "alert-backtest-allowlist.yml")
INCIDENTS = os.path.join(INFRA, "test", "alert-incidents.yml")

Rule = Dict[str, Any]
Document = Mapping[str, Any]


def read_yaml(path: str) -> Any:
    """The YAML at `path`; None when there is no such file."""
    try:
        with open(path, encoding="utf-8") as handle:
            return yaml.safe_load(handle)
    except FileNotFoundError:
        return None


def load_yaml(path: str, default: Any) -> Any:
    return read_yaml(path) or default


def rule_files(pattern: str) -> List[str]:
    return sorted(glob.glob(pattern))


def _documents(paths: Iterable[str]) -> List[Tuple[str, Document]]:
    return [(os.path.basename(path), read_yaml(path) or {}) for path in paths]


def _rules_of(document: Document) -> Iterable[Rule]:
    for group in document.get("groups") or []:
        for rule in group.get("rules") or []:
            yield {**rule, "_interval": group.get("interval")}


def prometheus_alerts(document: Document, file: str) -> Dict[str, Rule]:
    """{alert: the rule + its file and its group's interval} for the alerting rules of one rule file."""
    alerts: Dict[str, Rule] = {}
    for rule in _rules_of(document):
        if "alert" in rule:
            entry = {key: value for key, value in rule.items() if key != "_interval"}
            entry["file"], entry["interval"] = file, rule["_interval"]
            alerts[rule["alert"]] = entry
    return alerts


def grafana_alerts(document: Document, file: str) -> Dict[str, Rule]:
    return {rule["uid"]: grafana_as_prometheus(rule, file) for rule in _rules_of(document)}


def alerts_of(prometheus: Sequence[Tuple[str, Document]], grafana: Sequence[Tuple[str, Document]]) -> Dict[str, Rule]:
    alerts: Dict[str, Rule] = {}
    for file, document in prometheus:
        alerts.update(prometheus_alerts(document, file))
    for file, document in grafana:
        alerts.update(grafana_alerts(document, file))
    return alerts


def load_rules(pattern: str = RULES_GLOB, grafana: Optional[str] = GRAFANA_RULES) -> Dict[str, Rule]:
    """{alert: rule dict + file} across every Prometheus rule file and the Grafana-managed rules.

    A Grafana rule is translated into the Prometheus alert it is equivalent to, keyed by its uid:
    every one in this tree is a threshold node over the `last` of one instant query, so
    `(query) <op> <threshold>`, plus `or absent(query)` when its no-data state is Alerting. Any
    other shape is refused rather than guessed at.
    """
    return alerts_of(_documents(rule_files(pattern)), _documents([grafana] if grafana else []))


def recordings_of(documents: Sequence[Tuple[str, Document]]) -> Dict[str, Rule]:
    """{record name: {expr, interval}} for every recording rule in `documents`, with its group's interval.

    A name recorded by more than one rule is left out: which of them an expression means is not
    something to guess, so such a name is always read as Prometheus recorded it."""
    records: Dict[str, Rule] = {}
    seen: Set[str] = set()
    for _, document in documents:
        for rule in _rules_of(document):
            if "record" in rule:
                _record(records, seen, rule)
    return records


def _record(records: Dict[str, Rule], seen: Set[str], rule: Rule) -> None:
    name = rule["record"]
    if name in seen:
        records.pop(name, None)
        return
    seen.add(name)
    records[name] = {"expr": str(rule["expr"]).strip(), "interval": rule["_interval"] or "1m"}


def load_recordings(pattern: str = RULES_GLOB) -> Dict[str, Rule]:
    return recordings_of(_documents(rule_files(pattern)))


def grafana_as_prometheus(rule: Rule, file: str) -> Rule:
    nodes = {node["refId"]: node["model"] for node in rule["data"]}
    condition = nodes[rule["condition"]]
    if condition.get("type") != "threshold" or len(condition.get("conditions") or []) != 1:
        raise ValueError("%s: condition is not a single threshold" % rule["uid"])
    evaluator = condition["conditions"][0]["evaluator"]
    operator = {"gt": ">", "lt": "<"}.get(evaluator["type"])
    query = nodes[condition["expression"]]
    if operator is None or not query.get("expr") or query.get("range"):
        raise ValueError("%s: not a threshold over one instant query" % rule["uid"])
    expr = "(%s) %s %r" % (query["expr"].strip(), operator, float(evaluator["params"][0]))
    if rule.get("noDataState") == "Alerting":
        expr = "(%s) or absent(%s)" % (expr, query["expr"].strip())
    return {"alert": rule["uid"], "expr": expr, "for": rule.get("for", "0s"), "file": file,
            "grafana": True}


# ── RECORDINGS CHANGED IN THIS TREE ─────────────────────────────────────────────────────────────


def _record_reference(name: str) -> re.Pattern[str]:
    # The name as a whole identifier (recording names carry colons), outside a quoted string, with
    # an optional matcher block and `offset`.
    return re.compile(r'(?<![\w:"])' + re.escape(name) + r'(?![\w:])(\s*\{[^}]*\})?(\[[0-9a-z]+\])?'
                      r'(\s+offset\s+([0-9a-z]+))?')


def _outside_strings(expr: str, start: int) -> bool:
    return expr[:start].count('"') % 2 == 0


def _differs(name: str, rule: Rule, loaded: Mapping[str, str], normalise: Callable[[str, str], str],
             recorded: Optional[Set[str]]) -> bool:
    if name not in loaded or normalise(name, rule["expr"]) != normalise(name, loaded[name]):
        return True
    return recorded is not None and name not in recorded


def _reading_stale(records: Mapping[str, Rule], stale: Set[str]) -> Set[str]:
    return {name for name, rule in records.items()
            if name not in stale and any(_record_reference(dep).search(rule["expr"]) for dep in stale)}


def stale_recordings(records: Mapping[str, Rule], loaded: Mapping[str, str], normalise: Callable[[str, str], str],
                     recorded: Optional[Set[str]] = None) -> Set[str]:
    """The recordings whose recorded series do NOT say what the tree's rule says: changed since
    Prometheus loaded it (compared through `normalise(name, expr)`, Prometheus's own formatter), not
    loaded at all, loaded but with no series in the window (`recorded`, the metric names that have
    some: deployed after the window, as a rule loaded today over a copy that ends at midnight is --
    inlining one that is merely empty changes nothing), or reading -- however deeply -- one that is."""
    stale = {name for name, rule in records.items() if _differs(name, rule, loaded, normalise, recorded)}
    grew = _reading_stale(records, stale)
    while grew:
        stale |= grew
        grew = _reading_stale(records, stale)
    return stale


def _substitution(name: str, rule: Rule) -> Callable[[re.Match[str]], str]:
    def substitute(match: re.Match[str]) -> str:
        if not _outside_strings(match.string, match.start()):
            return match.group(0)
        if match.group(1):
            raise ValueError("cannot inline %s: it is read with matchers %s" % (name, match.group(1).strip()))
        if match.group(2):
            return "(%s)[%s:%s]%s" % (rule["expr"], match.group(2)[1:-1], rule["interval"], match.group(3) or "")
        if match.group(3):
            return "last_over_time((%s)[%s:%s] offset %s)" % (
                rule["expr"], rule["interval"], rule["interval"], match.group(4))
        return "(%s)" % rule["expr"]
    return substitute


def expand_recordings(expr: str, records: Mapping[str, Rule], stale: Iterable[str]) -> str:
    """`expr` with every stale recording replaced by its tree expression, so a backtest evaluates
    the rules AS WRITTEN rather than what Prometheus recorded from the deployed ones.

    `name` becomes `(expr)`; `name offset D` -- the recording as it was D ago -- becomes
    `last_over_time((expr)[I:I] offset D)` at the recording's own interval I, the one point it
    would have been recorded at; `name[R]` (with or without an offset) becomes the subquery
    `(expr)[R:I]`, the samples it would have recorded over R. A matcher block on a stale recording cannot be applied to an
    expression and is refused."""
    names = list(stale)
    for _ in range(10):
        before = expr
        for name in names:
            expr = _record_reference(name).sub(_substitution(name, records[name]), expr)
        if expr == before:
            return expr
    raise ValueError("recording rules nest more than ten deep, or cyclically")


# ── WHICH ALERTS A RUN IS ASKED FOR ─────────────────────────────────────────────────────────────

PROBLEM_LINE = re.compile(r"^\s*- (?:incident \S+: )?([A-Za-z_][\w]*)(?: \(| should | query failed| names )")


def alerts_named_in_log(text: str, known: Iterable[str]) -> Set[str]:
    """The alerts a failing run's problem lines name ("  - Name (file.rules): ...", "  - incident id: Name ...")."""
    known = set(known)
    found = (PROBLEM_LINE.match(line) for line in text.splitlines())
    return {match.group(1) for match in found if match and match.group(1) in known}


def select_alerts(alerts: Mapping[str, Rule], names: Iterable[str] = (), groups: Iterable[str] = (),
                  log_text: Optional[str] = None) -> Optional[List[str]]:
    """The union of the named alerts, the alerts of the named rule files and the alerts a log names.

    None when nothing narrows the run (no flag at all) or the log names no alert (then every alert is wanted)."""
    names, groups = list(names), list(groups)
    chosen = set(names) | {n for n, rule in alerts.items() if rule["file"] in groups}
    if log_text is None:
        return sorted(chosen) if (names or groups) else None
    named = alerts_named_in_log(log_text, alerts)
    return sorted(chosen | named) if (names or groups or named) else sorted(alerts)
