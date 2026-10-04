"""The label matchers every alert and panel filters on, read out of the PromQL that carries them.

Shared by the label-shape check (test_label_shapes.py) and the script that snapshots the fleet's
real label values for it (bin/snapshot-label-values), so the two agree on WHICH (metric, label)
pairs exist: a pair the extractor sees and the snapshot never fetched is a hole in the check.

This is a selector scanner, not a PromQL parser. Every brace in the expressions this tree writes
opens a vector selector (Grafana's `{{label}}` lives in legendFormat, never in expr), so reading
`name{...}` / `{__name__=~"...",...}` spans with a tokenising regex is exact for them.
"""

import glob
import json
import os
import re

HERE = os.path.dirname(os.path.abspath(__file__))
INFRA = os.path.dirname(HERE)
MONITORING = os.path.join(INFRA, "nix", "files", "monitoring")
RULE_FILES = sorted(glob.glob(os.path.join(MONITORING, "rules", "*.rules")))
GRAFANA_ALERT_RULES = os.path.join(MONITORING, "grafana", "alerting", "alert-rules.yaml")
DASHBOARD_FILES = sorted(glob.glob(os.path.join(MONITORING, "grafana", "dashboards", "*", "*.json")))

# A quoted PromQL string: double-quoted with backslash escapes. Single-quoted and backtick strings
# are legal PromQL but this tree never writes a matcher with them.
_STRING = r'"((?:[^"\\]|\\.)*)"'
_MATCHER = re.compile(r'([a-zA-Z_][a-zA-Z0-9_]*)\s*(=~|!~|!=|=)\s*' + _STRING)
_SELECTOR = re.compile(r'([a-zA-Z_:][a-zA-Z0-9_:]*)?\s*\{((?:[^{}"]|"(?:[^"\\]|\\.)*")*)\}')


def seconds(duration):
    """A Prometheus/Grafana duration ("1h30m", "30s", "2d") in seconds; 0 when unset or templated."""
    units = {"s": 1, "m": 60, "h": 3600, "d": 86400, "w": 604800}
    if not re.fullmatch(r"(\d+[smhdw])+", duration or ""):
        return 0
    return sum(int(n) * units[u] for n, u in re.findall(r"(\d+)([smhdw])", duration))


def unquote(value):
    """A PromQL double-quoted string's content, unescaped the way Prometheus does it."""
    return re.sub(r'\\(.)', lambda m: {"n": "\n", "t": "\t"}.get(m.group(1), m.group(1)), value)


class Matcher(object):
    __slots__ = ("label", "op", "value")

    def __init__(self, label, op, value):
        self.label, self.op, self.value = label, op, value

    @property
    def is_regex(self):
        return self.op in ("=~", "!~")

    @property
    def is_negative(self):
        return self.op in ("!=", "!~")

    @property
    def is_templated(self):
        """A Grafana variable is filled in at render time -- there is no literal to check."""
        return "$" in self.value

    def matches(self, value):
        """What Prometheus answers for one label value: regexes are fully anchored (RE2)."""
        hit = re.fullmatch(self.value, value) is not None if self.is_regex else self.value == value
        return not hit if self.is_negative else hit

    def selects(self, value):
        """Whether the value is one the matcher's PATTERN names, ignoring its polarity."""
        return re.fullmatch(self.value, value) is not None if self.is_regex else self.value == value

    def __repr__(self):
        return '%s%s"%s"' % (self.label, self.op, self.value)


class Selector(object):
    __slots__ = ("name", "matchers")

    def __init__(self, name, matchers):
        self.name, self.matchers = name, matchers

    @property
    def name_matcher(self):
        """The metric-name constraint: the bare name, or a `__name__` matcher inside the braces."""
        if self.name:
            return Matcher("__name__", "=", self.name)
        return next((m for m in self.matchers if m.label == "__name__"), None)

    @property
    def label_matchers(self):
        return [m for m in self.matchers if m.label != "__name__"]

    def metrics(self, known):
        """The metric names out of `known` this selector reads."""
        name = self.name_matcher
        return sorted(n for n in known if name is not None and name.matches(n))

    def __repr__(self):
        return "%s{%s}" % (self.name or "", ",".join(repr(m) for m in self.matchers))


def selectors(expr):
    """Every vector selector in `expr` that carries at least one label matcher."""
    found = []
    for match in _SELECTOR.finditer(expr):
        matchers = [Matcher(label, op, unquote(value))
                    for label, op, value in _MATCHER.findall(match.group(2))]
        if matchers:
            found.append(Selector(match.group(1), matchers))
    return found


def rule_expressions():
    """(where, expr) for every alerting and recording rule Prometheus loads."""
    import yaml  # only the rule readers need it; test_dashboards.py stays stdlib
    for path in RULE_FILES:
        with open(path, encoding="utf-8") as handle:
            document = yaml.safe_load(handle)
        for group in document["groups"]:
            for rule in group["rules"]:
                name = rule.get("alert") or rule.get("record")
                yield "%s %s" % (os.path.basename(path), name), rule["expr"]


def grafana_rule_expressions():
    """(where, expr) for every query of every Grafana-managed alert rule."""
    import yaml
    with open(GRAFANA_ALERT_RULES, encoding="utf-8") as handle:
        document = yaml.safe_load(handle)
    for group in document["groups"]:
        for rule in group["rules"]:
            for node in rule["data"]:
                expr = node.get("model", {}).get("expr")
                if expr:
                    yield "alert-rules.yaml %s/%s" % (rule["uid"], node["refId"]), expr


def dashboard_expressions():
    """(where, expr) for every Prometheus target of every panel, rows descended into."""
    def walk(panels):
        for panel in panels:
            yield panel
            for child in panel.get("panels", []):
                yield child
    for path in DASHBOARD_FILES:
        with open(path, encoding="utf-8") as handle:
            document = json.load(handle)
        for panel in walk(document.get("panels", [])):
            for target in panel.get("targets", []):
                if (target.get("datasource") or panel.get("datasource") or {}).get("uid") == "victorialogs":
                    continue
                expr = target.get("expr")
                if expr:
                    yield "%s panel %s %r" % (
                        os.path.basename(path), panel.get("id"), panel.get("title")), expr


def all_expressions():
    yield from rule_expressions()
    yield from grafana_rule_expressions()
    yield from dashboard_expressions()



# ---- metric names ------------------------------------------------------------------------------
# Shared by bin/backtest-alerts (which alerts read a metric Prometheus never had), test_metric_names.py
# (which panels and rules name a metric the fleet snapshot never saw) and bin/snapshot-label-values.

# Words that look like identifiers in PromQL and are not metric names.
_KEYWORDS = {
    "by", "without", "on", "ignoring", "group_left", "group_right", "and", "or", "unless", "bool",
    "offset", "atan2", "inf", "nan", "start", "end",
}
_QUOTED = re.compile(r'"(?:[^"\\]|\\.)*"|\'(?:[^\'\\]|\\.)*\'|`[^`]*`')
_BRACES = re.compile(r"\{[^{}]*\}")
_RANGE = re.compile(r"\[[^\]]*\]")
_LABEL_LIST = re.compile(r"\b(by|without|on|ignoring|group_left|group_right)\s*\([^()]*\)")
_IDENT = re.compile(r"(?<![A-Za-z0-9_:.$])([A-Za-z_:][A-Za-z0-9_:]*)(?![A-Za-z0-9_:])")
_NAME_MATCHER = re.compile(r'__name__\s*=\s*"([^"]+)"')
# A string (kept) or a `#` comment to the end of its line (dropped): matched together so a `#`
# inside a string is never taken for one.
_STRING_OR_COMMENT = re.compile(_QUOTED.pattern + r"|#[^\n]*")


def metric_names(expr):
    """Every metric name `expr` reads: bare selectors plus `{__name__="..."}`.

    A scanner, not a parser -- but PromQL makes it exact enough: a metric name is an identifier
    that is not a function call, not a keyword, and not inside a string, a comment, a label list,
    a matcher block or a range. `{__name__=~...}` regexes are not names and are ignored.
    """
    expr = _STRING_OR_COMMENT.sub(lambda m: "" if m.group(0).startswith("#") else m.group(0), expr)
    names = set(_NAME_MATCHER.findall(expr))
    text = _QUOTED.sub('""', expr)
    text = _BRACES.sub(" ", text)
    text = _RANGE.sub(" ", text)
    text = _LABEL_LIST.sub(" ", text)
    for match in _IDENT.finditer(text):
        word = match.group(1)
        rest = text[match.end():].lstrip()
        if rest.startswith("("):
            continue  # a function call (or an aggregation)
        if word.lower() in _KEYWORDS:
            continue
        if re.fullmatch(r"\d+[smhdwy]", word):
            continue
        names.add(word)
    return names


def name_patterns(expr):
    """Every `{__name__=~"..."}` regex matcher in `expr` -- the names `metric_names` cannot list."""
    return [s.name_matcher for s in selectors(expr)
            if not s.name and s.name_matcher is not None and s.name_matcher.op == "=~"]


def template_queries():
    """(where, query) for every Prometheus template variable of every dashboard -- a variable
    built on a dead metric empties its dropdown and blanks every panel scoped by it."""
    for path in DASHBOARD_FILES:
        with open(path, encoding="utf-8") as handle:
            document = json.load(handle)
        for variable in document.get("templating", {}).get("list", []):
            query = variable.get("query")
            query = query.get("query") if isinstance(query, dict) else query
            if variable.get("type") == "query" and query:
                inner = re.match(r'^\s*(?:label_values|query_result)\((.*)\)\s*$', query, re.S)
                yield "%s variable %s" % (os.path.basename(path), variable.get("name")), \
                    re.sub(r',\s*[A-Za-z_][A-Za-z0-9_]*\s*$', '', inner.group(1)) if inner else query


def recorded_names():
    """Every series name a Prometheus recording rule defines."""
    import yaml
    names = set()
    for path in RULE_FILES:
        with open(path, encoding="utf-8") as handle:
            for group in yaml.safe_load(handle)["groups"]:
                names.update(rule["record"] for rule in group["rules"] if "record" in rule)
    return names
