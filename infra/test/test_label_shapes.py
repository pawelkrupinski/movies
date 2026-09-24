#!/usr/bin/env python3
"""Every label matcher in every alert rule and dashboard panel, checked against the label values
the fleet ACTUALLY produces.

WHY. A matcher that misses is not an error anywhere: Prometheus answers a smaller result, the
alert watches less than its name says, and the panel draws a plausible line over part of the
fleet. promtool cannot see it either, because a test's input series are typed by the same person
with the same wrong idea of what the labels look like. Two of these shipped in one day:

- `UserStateWritesFailing` matched `route=~"/api/me.*"`. Play's `route` label carries the mount
  prefix, so the four mounted countries report `/uk/api/me/state` and the alert (and its panel)
  only ever saw Poland.
- `CinemaScrapeFailureShareHigh` excluded the per-venue enrichment rows (`Kino Muza|enrichment`)
  but not the chain-wide ones (`Cineworld Enrichment`), and one failing chain detail endpoint
  outweighed every venue scrape in the UK: its three-day "scrape failure" spike was enrichment.

Both are the same shape of mistake: the pattern treats two values that MEAN the same thing
differently. So beyond "does this pattern name any real value", the check below groups real values
into shapes that must be treated alike -- a route with and without its country mount, an
enrichment row whichever separator names it -- and fails a matcher that splits a group.

The values come from infra/test/label-values.json, a snapshot of the fleet Prometheus written by
infra/bin/snapshot-label-values. A matcher on a (metric, label) the snapshot has not seen fails
with the instruction to re-run it.

Run: python3 infra/test/test_label_shapes.py   (also run by test_alert_rules.sh)
"""

import json
import os
import re
import sys
import unittest

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import promql_selectors  # noqa: E402

SNAPSHOT = os.path.join(HERE, "label-values.json")
RESNAPSHOT = "re-run infra/bin/snapshot-label-values (fleet SOCKS proxy up) and commit label-values.json"

# Values a HEALTHY fleet does not produce, because they only exist while the thing an alert is
# about is happening. The snapshot is two weeks of a mostly healthy fleet, so their absence from
# it says nothing about whether the matcher is right. Each entry is (metric, label, value).
EVENT_ONLY_VALUES = {
    # Play reports 5xx only when a request fails; the fortnight in the snapshot had none.
    ("kinowo_web_http_requests_total", "status", "5xx"),
    # kube-state-metrics publishes a reason series only while a container is in that state.
    ("kube_pod_container_status_waiting_reason", "reason", "CrashLoopBackOff"),
    ("kube_pod_container_status_last_terminated_reason", "reason", "OOMKilled"),
    # NOT PUBLISHED YET, rather than event-only: `flux:resource_info` is recorded from the
    # `gotk_<kind>_info` series kube-state-metrics publishes once movies-gitops gives it the
    # custom-resource-state config for Flux's objects (see the `flux-objects` group in flux.rules).
    # `ready` and `suspended` are the Ready condition's status and `spec.suspend` verbatim.
    # Re-snapshot once the series exists; `ready="False"` then stays here as event-only.
    ("flux:resource_info", "ready", "False"),
    ("flux:resource_info", "suspended", "true"),
}


def mount_stripped(route):
    """`/uk/api/me/state` and `/api/me/state` are one handler served under two mounts."""
    return re.sub(r"^/[a-z]{2}(?=/)", "", route)


def enrichment_row(service):
    """Chain-wide `Cineworld Enrichment` and per-venue `Kino Muza|enrichment` are both a detail
    enrichment check, not a venue scrape -- whatever separator the row happens to be named with."""
    return "enrichment" if re.search(r"(?i)[ |]enrichment$", service) else None


# label -> (the function mapping a value to its shape or None, which matchers must honour it).
#
# ALL: any matcher that names one value of a shape must name all of them. A route is one handler
# whichever country mount serves it, so `route=~"/api/me.*"` (Poland only) is always a bug.
# EXCLUSIONS: only a NEGATIVE matcher must. An exclusion list that drops "not a scrape" rows has
# to drop every spelling of them -- missing one is the CinemaScrapeFailureShareHigh bug -- while a
# positive matcher may deliberately pick one kind: ChainEnrichmentFailing reads only the chain-wide
# `.* Enrichment` rows, and that is its scope, not an omission.
ALL, EXCLUSIONS = "all", "exclusions"
SHAPES = {
    "route": (mount_stripped, ALL),
    "service": (enrichment_row, EXCLUSIONS),
}


def load_snapshot():
    with open(SNAPSHOT, encoding="utf-8") as handle:
        metrics = json.load(handle)["metrics"]
    for labels in metrics.values():
        for label, values in list(labels.items()):
            if isinstance(values, dict):
                labels[label] = metrics[values["same_as"]][label]
    return metrics


def branches(pattern):
    """The top-level `|` alternatives of a regex, or the whole pattern if it has none."""
    parts, depth, current, escaped = [], 0, "", False
    for char in pattern:
        if escaped:
            current, escaped = current + char, False
            continue
        if char == "\\":
            current, escaped = current + char, True
            continue
        if char in "([":
            depth += 1
        elif char in ")]":
            depth -= 1
        if char == "|" and depth == 0:
            parts.append(current)
            current = ""
        else:
            current += char
    return parts + [current]


def uses(snapshot):
    """(where, selector, metric, matcher) for every literal label matcher, per metric it reads."""
    for where, expr in promql_selectors.all_expressions():
        for selector in promql_selectors.selectors(expr):
            name = selector.name_matcher
            metrics = [name.value] if name is not None and name.op == "=" else selector.metrics(snapshot)
            for metric in metrics:
                for matcher in selector.label_matchers:
                    if not matcher.is_templated:
                        yield where, selector, metric, matcher


class LabelMatchersAgainstTheFleet(unittest.TestCase):

    def setUp(self):
        self.snapshot = load_snapshot()

    def test_every_matched_label_is_in_the_snapshot(self):
        missing = sorted({"%s{%s}: %s" % (metric, matcher.label, where)
                          for where, _, metric, matcher in uses(self.snapshot)
                          if matcher.label not in self.snapshot.get(metric, {})
                          and (metric, matcher.label) not in {(m, l) for m, l, _ in EVENT_ONLY_VALUES}})
        self.assertEqual([], missing, "\nno real values for these (metric, label) pairs -- either the "
                         "label does not exist on the metric, or %s:\n%s" % (RESNAPSHOT, "\n".join(missing)))

    def test_every_pattern_names_a_value_the_fleet_produces(self):
        # Each `|` alternative on its own: a typo in one branch of a long exclusion list is exactly
        # as silent as a typo in the whole thing.
        problems = set()
        for where, selector, metric, matcher in uses(self.snapshot):
            values = self.snapshot.get(metric, {}).get(matcher.label)
            if values is None:
                continue
            alternatives = branches(matcher.value) if matcher.is_regex else [matcher.value]
            for alternative in alternatives:
                if (metric, matcher.label, alternative) in EVENT_ONLY_VALUES:
                    continue
                probe = promql_selectors.Matcher(matcher.label, "=~" if matcher.is_regex else "=", alternative)
                if not any(probe.selects(v) for v in values):
                    problems.add("%s: %s{%s} -- %r matches no real %s value"
                                 % (where, metric, matcher, alternative, matcher.label))
        self.assertEqual([], sorted(problems), "\n" + "\n".join(sorted(problems)))

    def test_no_matcher_splits_values_of_one_shape(self):
        problems = set()
        for where, selector, metric, matcher in uses(self.snapshot):
            shape_of, applies_to = SHAPES.get(matcher.label, (None, None))
            values = self.snapshot.get(metric, {}).get(matcher.label)
            if shape_of is None or values is None or (applies_to == EXCLUSIONS and not matcher.is_negative):
                continue
            groups = {}
            for value in values:
                shape = shape_of(value)
                if shape is not None:
                    groups.setdefault(shape, []).append(value)
            for shape, members in sorted(groups.items()):
                taken = [v for v in members if matcher.selects(v)]
                if taken and len(taken) != len(members):
                    left = [v for v in members if not matcher.selects(v)]
                    problems.add("%s: %s{%s} names %s but not %s, which is the same %s"
                                 % (where, metric, matcher, taken[:3], left[:3], matcher.label))
        self.assertEqual([], sorted(problems), "\n" + "\n".join(sorted(problems)))


class ShapeFunctions(unittest.TestCase):
    """The shape functions themselves, on the values that motivated them."""

    def test_a_route_and_its_mounted_copies_are_one_shape(self):
        self.assertEqual({"/api/me/state"},
                         {mount_stripped(r) for r in ("/api/me/state", "/uk/api/me/state", "/us/api/me/state")})
        self.assertEqual("/", mount_stripped("/de/"))
        self.assertEqual("/:city/movies", mount_stripped("/:city/movies"))

    def test_both_enrichment_spellings_are_one_shape(self):
        self.assertEqual("enrichment", enrichment_row("Cineworld Enrichment"))
        self.assertEqual("enrichment", enrichment_row("Kino Muza|enrichment"))
        self.assertIsNone(enrichment_row("Kino Muza"))
        self.assertIsNone(enrichment_row("TMDB"))

    def test_the_snapshot_holds_both_shapes_it_is_checked_for(self):
        # If a re-snapshot ever loses the mounted routes or the chain-wide enrichment rows, the
        # shape check above silently has nothing to compare -- fail here instead.
        snapshot = load_snapshot()
        routes = snapshot["kinowo_web_http_requests_total"]["route"]
        self.assertIn("/uk/api/me/state", routes)
        self.assertIn("/api/me/state", routes)
        services = snapshot["kinowo_uptime_recent_failures"]["service"]
        self.assertTrue(any(s.endswith(" Enrichment") for s in services))
        self.assertTrue(any(s.endswith("|enrichment") for s in services))


if __name__ == "__main__":
    unittest.main(verbosity=2)
