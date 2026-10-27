#!/usr/bin/env python3
"""The absent-series alerts for every application metric an alert reads, GENERATED from the alerts themselves.

WHY. An alert over a series that is not there evaluates to an empty vector: it cannot fire, and nothing says so.
Every worker and web metric family now exists from boot (deploy.WorkerMetricsAtBootSpec, deploy.WebMetricsAtBootSpec),
so while the application answers its scrape, a family an alert reads is missing only by a fault -- unwired, renamed,
its writer never constructed -- and that alert is blind until someone looks. `AppMetricAbsent` pages on it, one rule
per family, labelled `metric`.

THE LIST IS THE ALERTS. Every `kinowo_worker_*` / `kinowo_web_*` / `kinowo_uptime_*` / `kinowo_fallback_*` family an
alerting rule in nix/files/monitoring/rules reads gets a rule here, unless
  - a hand-written alert already pages on its absence: one whose expression takes `absent(...)` of it, or whose
    name ends in `Absent` and which reads it (CinemaScrapeCensusAbsent, ShareCardCoverageAbsent, ...) -- a second
    page for the same hole adds nothing but noise; or
  - it is in NOT_GUARDED, with the reason its absence is a healthy reading.
So a new alert over a new family is guarded the moment this is re-run, and test_alert_rule_coverage.py fails until it
is: it requires the checked-in files to equal what this writes, and every app family an alert reads to be guarded.

THE SHAPE. Per country: the tier's scrape target is up, and has published no series of the family for 15 minutes.
A tier that is DOWN is WorkerDown's / TargetDown's to report, so `up == 1` gates it.

Run: python3 infra/test/app_metrics_absent.py --write   (rewrites both files below; without --write, prints a diff
verdict and exits 1 when they are stale)
"""

import os
import re
import sys

import yaml

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import promql_selectors  # noqa: E402

RULES_OUT = os.path.join(promql_selectors.MONITORING, "rules", "app-metrics-absent.rules")
TESTS_OUT = os.path.join(HERE, "alert-rules", "app-metrics-absent.yml")
ALERT = "AppMetricAbsent"
HOLD = "15m"

# Which scrape job publishes a family, by name prefix.
TIERS = (("kinowo_worker_", "kinowo-worker", "worker"),
         ("kinowo_web_", "kinowo-web", "web"),
         ("kinowo_uptime_", "kinowo-web", "web"),
         ("kinowo_fallback_", "kinowo-web", "web"))

# App families an alert reads whose absence is a healthy reading, each with why.
NOT_GUARDED = {
    "kinowo_worker_http_circuit_open":
        "one series per (leg, host) whose breaker is open; no open breaker is no series",
    "kinowo_worker_identity_billing_reversals":
        "one series per billing that reversed, removed once forgotten; no reversal is no series",
    "kinowo_fallback_active_venues":
        "rendered per shared scraper client; a country whose roster has none has no series",
    "kinowo_fallback_total_venues":
        "rendered per shared scraper client; a country whose roster has none has no series",
}

_SUFFIX = re.compile(r"_(total|bucket|count|sum|created)$")
_ABSENT_OF = re.compile(r"\babsent(?:_over_time)?\s*\(\s*([a-zA-Z_][a-zA-Z0-9_]*)")


def tier_of(name):
    for prefix, job, tier in TIERS:
        if name.startswith(prefix):
            return job, tier
    return None


def family_of(name):
    return _SUFFIX.sub("", name)


def _rule_files():
    return [p for p in promql_selectors.RULE_FILES if os.path.abspath(p) != os.path.abspath(RULES_OUT)]


def alerts():
    """[(rule file basename, alert name, expr)] for every alerting rule but the generated ones."""
    found = []
    for path in _rule_files():
        with open(path, encoding="utf-8") as handle:
            for group in yaml.safe_load(handle)["groups"]:
                for rule in group["rules"]:
                    if "alert" in rule:
                        found.append((os.path.basename(path), rule["alert"], rule["expr"]))
    return found


def read_families():
    """{family: (sample name to test for, sorted readers "file Alert")} for every app family an alert reads."""
    names, readers = {}, {}
    for rules, alert, expr in alerts():
        for name in promql_selectors.metric_names(expr):
            if tier_of(name) is None:
                continue
            family = family_of(name)
            # A histogram is tested by its _count, a counter by its _total, a gauge by its own name.
            sample = family + "_count" if name.endswith(("_bucket", "_sum")) else name
            names[family] = min(names.get(family, sample), sample, key=lambda s: (s.endswith("_bucket"), s))
            readers.setdefault(family, set()).add("%s %s" % (rules, alert))
    return {f: (names[f], sorted(readers[f])) for f in names}


def hand_guarded():
    """{family: alert} for every app family a hand-written alert already pages on the absence of."""
    guarded = {}
    for _, alert, expr in alerts():
        for name in _ABSENT_OF.findall(expr):
            guarded.setdefault(family_of(name), alert)
        if alert.endswith("Absent"):
            for name in promql_selectors.metric_names(expr):
                if tier_of(name) is not None:
                    guarded.setdefault(family_of(name), alert)
    return guarded


def generated_families():
    """{family: (sample, readers)} this file generates a rule for."""
    guarded = hand_guarded()
    return {f: v for f, v in sorted(read_families().items()) if f not in guarded and f not in NOT_GUARDED}


def _summary(family, tier):
    return "{{ $labels.country }}'s %s is up but publishes no %s" % (tier, family)


def _description(family, tier, readers):
    return ("The %s answers its scrape (`up`) but has published no %s series for this country for %s. "
            "Every %s metric family exists from boot, so this is a fault, not a quiet day: the family was unwired, "
            "renamed, or the class that writes it is no longer built. Until it is back these alerts read nothing and "
            "cannot fire: %s." % (tier, family, HOLD, tier, ", ".join(readers)))


def rules_document():
    rules = []
    for family, (sample, readers) in generated_families().items():
        job, tier = tier_of(family)
        rules.append({
            "alert": ALERT,
            "expr": "max by (country) (up{job=\"%s\"} == 1)\nunless on (country)\ncount by (country) (%s)" % (job, sample),
            "for": HOLD,
            "labels": {"severity": "warning", "metric": family},
            "annotations": {"summary": _summary(family, tier), "description": _description(family, tier, readers)},
        })
    return {"groups": [{"name": "app-metrics-absent", "rules": rules}]}


def tests_document():
    families = generated_families()
    jobs = sorted({tier_of(f)[0] for f in families})

    def up(country, value):
        return [{"series": 'up{job="%s",country="%s"}' % (job, country), "values": "%sx30" % value} for job in jobs]

    def present(country):
        return [{"series": '%s{job="%s",country="%s"}' % (sample, tier_of(f)[0], country), "values": "0x30"}
                for f, (sample, _) in families.items()]

    firing = [{"exp_labels": {"country": "de", "metric": f, "severity": "warning"},
               "exp_annotations": {"summary": _summary(f, tier_of(f)[1]).replace("{{ $labels.country }}", "de"),
                                   "description": _description(f, tier_of(f)[1], readers)}}
              for f, (_, readers) in families.items()]
    return {
        "rule_files": ["../../nix/files/monitoring/rules/app-metrics-absent.rules"],
        "evaluation_interval": "1m",
        "tests": [
            # FIRES: both countries' tiers up; PL publishes every family, DE none of them. Each family fires for DE
            # alone, after its hold -- and never for PL, which is every rule's quiet case beside its firing one.
            {"interval": "1m",
             "input_series": up("pl", 1) + up("de", 1) + present("pl"),
             "alert_rule_test": [{"eval_time": "10m", "alertname": ALERT, "exp_alerts": []},
                                 {"eval_time": "20m", "alertname": ALERT, "exp_alerts": firing}]},
            # QUIET: every family published.
            {"interval": "1m",
             "input_series": up("pl", 1) + present("pl"),
             "alert_rule_test": [{"eval_time": "20m", "alertname": ALERT, "exp_alerts": []}]},
            # QUIET: a tier that is down publishes nothing, and that is WorkerDown's / TargetDown's to say.
            {"interval": "1m",
             "input_series": up("de", 0),
             "alert_rule_test": [{"eval_time": "20m", "alertname": ALERT, "exp_alerts": []}]},
        ],
    }


HEADER = ("# GENERATED by infra/test/app_metrics_absent.py from the alerting rules beside it -- do not edit by hand.\n"
          "# Re-run `python3 infra/test/app_metrics_absent.py --write` after adding or changing an alert over an\n"
          "# application metric; test_alert_rule_coverage.py fails until this file matches.\n")


class _Dumper(yaml.SafeDumper):
    pass


def _str(dumper, value):
    return dumper.represent_scalar("tag:yaml.org,2002:str", value, style="|" if "\n" in value else None)


_Dumper.add_representer(str, _str)


def render(document):
    return HEADER + yaml.dump(document, Dumper=_Dumper, sort_keys=False, width=120, allow_unicode=True)


def outputs():
    return {RULES_OUT: render(rules_document()), TESTS_OUT: render(tests_document())}


def stale():
    """The generated files whose checked-in content differs from what the alerts generate."""
    found = []
    for path, content in outputs().items():
        current = open(path, encoding="utf-8").read() if os.path.exists(path) else None
        if current != content:
            found.append(path)
    return found


if __name__ == "__main__":
    if "--write" in sys.argv[1:]:
        for path, content in outputs().items():
            with open(path, "w", encoding="utf-8") as handle:
                handle.write(content)
            print("wrote", os.path.relpath(path))
        sys.exit(0)
    out_of_date = stale()
    for path in out_of_date:
        print("stale:", os.path.relpath(path))
    sys.exit(1 if out_of_date else 0)
