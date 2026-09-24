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
