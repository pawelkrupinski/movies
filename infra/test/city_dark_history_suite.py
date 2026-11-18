#!/usr/bin/env python3
"""Writes test/alert-rules/city-dark-history.yml: CityDarkInBothTiers with and without the hourly history.

The alert reads the six days from eight to two days ago in four pieces (rules/runtime-invariants.rules):
the five-minute census samples recorded before `country_city:worker_movies_served:min2h` existed
(the label-less ones), the hourly minima, and an hour and five minutes of the census at each end.
This suite asks, in each state that history can be in, whether the alert says what the raw
expression said over the census as one series:

  * NO HISTORY -- only label-less census samples, no hourly minima (a city the moment the
    recording was deployed);
  * FULL HISTORY -- only labelled census samples and the hourly minima, from the start (a city
    eight days after the deploy, when the label-less piece reads nothing);
  * THE SEAM -- label-less samples up to a per-city instant, labelled ones after it, the hourly
    minima from three hours before it (as deployed: the minima were recording before the label
    was added), with the seam placed in each piece of the window, at each of its edges, and
    inside the dark spell itself.

Each city has films (4) every sample but at most ONE zero, placed so that the sweep moves it into
or out of the window from either end, and goes dark in both tiers at 204h. At every 15 minutes
from 196h to the end and 5 and 10 minutes either side of every crossing (a zero or a seam
entering or leaving each piece), the suite expects:

  1. the production expression to return EXACTLY the cities this file's model of the raw
     expression says -- one series each, labelled {country, city}: nothing missing (no gap) and
     nothing twice (no second series for a city across the seam);
  2. the raw expression itself, evaluated by promtool over the reference series, and the
     production one to differ by nothing either way.

Run: python3 infra/test/city_dark_history_suite.py (needs PyYAML: scripts/python-venv).
"""

import os
import re

import yaml

HERE = os.path.dirname(os.path.abspath(__file__))
RULES = os.path.join(HERE, "..", "nix", "files", "monitoring", "rules", "runtime-invariants.rules")
OUT = os.path.join(HERE, "alert-rules", "city-dark-history.yml")

STEP = 5                      # minutes between census samples
DARK = 204 * 60               # both tiers dark from here (minutes)
END = 240 * 60                # last sample is END - STEP
N = END // STEP
WINDOW_FAR, WINDOW_NEAR = 192 * 60, 48 * 60
FILMS = "country_city:worker_movies_served:films"
REFERENCE = "test_reference:worker_movies_served:films"
WEB = 'kinowo_web_movies_served{scope="all",country="us",city="%s",instance="web1"}'

# Zeros off the whole hour, so that for a while only a raw end piece sees them (an hourly sample
# covers a zero from the next whole hour), and one AT a seam: the first labelled sample of
# seam31h-z31h, which the eight-days-ago term must read as the latest.
BLIPS = {"none": None, "z13h20m": 13 * 60 + 20, "z30h40m": 30 * 60 + 40, "z31h": 31 * 60,
         "z100h": 100 * 60, "z170h35m": 170 * 60 + 35, "z190h50m": 190 * 60 + 50}
SEAMS = [31, 100, 156, 169, 171, 189, 200, 215]   # hours
YOUNG_FROM = 120 * 60         # a city on the roster for five days only


def raw_expression():
    """The alert as it was before the recordings (298b8f74c's parent), over the reference series."""
    return ('%(r)s == 0 unless on (country, city) (max by (country, city) (last_over_time('
            'kinowo_web_movies_served{scope="all"}[10m])) > 0) unless on (country, city) (max by '
            '(country, city) (last_over_time(kinowo_worker_city_off_season[10m])) == 1) and on '
            '(country, city) (min_over_time(%(r)s[6d] offset 2d) > 0) and on (country, city) '
            '(last_over_time(%(r)s[1h] offset 8d) > 0)' % {"r": REFERENCE})


def production_expression():
    with open(RULES, encoding="utf-8") as handle:
        rules = yaml.safe_load(handle)
    expr = next(r["expr"] for g in rules["groups"] for r in g["rules"]
                if r.get("alert") == "CityDarkInBothTiers")
    return re.sub(r"\(\s+", "(", re.sub(r"\s+\)", ")", re.sub(r"\s+", " ", expr))).strip()


def census(blip, start=0):
    """{minute: films} for one city."""
    values = {}
    for i in range(N):
        t = i * STEP
        if t < start:
            continue
        values[t] = 0 if t >= DARK or t == blip else 4
    return values


def run_length(points):
    """promtool's `values` notation for {minute: value} on the 5-minute grid ('_' where absent)."""
    tokens, i = [], 0
    while i < N:
        value = points.get(i * STEP)
        j = i
        while j + 1 < N and points.get((j + 1) * STEP) == value:
            j += 1
        count = j - i + 1
        if value is None:
            tokens.append("_x%d" % count if count > 1 else "_")
        else:
            tokens.append("%dx%d" % (value, count - 1) if count > 1 else "%d" % value)
        i = j + 1
    return " ".join(tokens)


def hourly_minima(points, since):
    """min2h as recorded: at every whole hour u >= since, the minimum over (u-2h, u]."""
    minima = {}
    for u in range(max(since, 60), END, 60):
        inside = [v for t, v in points.items() if u - 120 < t <= u]
        if inside:
            minima[u] = min(inside)
    return minima


def fires(points, t):
    """The raw expression's answer for one city at t: dark now, films in every sample of
    (t-192h, t-48h], and a sample with films in (t-193h, t-192h]."""
    now = [v for s, v in points.items() if t - 5 < s <= t]
    if not now or now[-1] != 0:
        return False
    window = [v for s, v in points.items() if t - WINDOW_FAR < s <= t - WINDOW_NEAR]
    before = [v for s, v in sorted(points.items()) if t - WINDOW_FAR - 60 < s <= t - WINDOW_FAR]
    return bool(window) and min(window) > 0 and bool(before) and before[-1] > 0


def cities():
    """[(city, reference census, label-less census, labelled census, hourly minima)]."""
    found = []
    for blip_name, blip in sorted(BLIPS.items()):
        full = census(blip)
        found.append(("nohistory-" + blip_name, full, full, {}, {}))
        found.append(("fullhistory-" + blip_name, full, {}, full, hourly_minima(full, 0)))
        for seam in SEAMS:
            cut = seam * 60
            old = {t: v for t, v in full.items() if t < cut}
            new = {t: v for t, v in full.items() if t >= cut}
            found.append(("seam%dh-%s" % (seam, blip_name), full, old, new,
                          hourly_minima(full, cut - 180)))
    young = census(None, YOUNG_FROM)
    found.append(("nohistory-young", young, young, {}, {}))
    found.append(("fullhistory-young", young, {}, young, hourly_minima(young, 0)))
    return found


def eval_times():
    times = set(range(196 * 60, END, 15))
    crossings = []
    for blip in BLIPS.values():
        if blip is not None:
            crossings += [blip + WINDOW_FAR, blip + WINDOW_NEAR]
    for seam in SEAMS:
        cut = seam * 60
        crossings += [cut, cut + WINDOW_NEAR, cut + WINDOW_NEAR + 65, cut + WINDOW_FAR - 65,
                      cut + WINDOW_FAR, cut + WINDOW_FAR + 60]
    for c in crossings:
        for d in (-10, -5, 0, 5, 10):
            t = c + d
            if 196 * 60 <= t < END:
                times.add(t - t % STEP)
    return sorted(times)


def quoted(s):
    return "'" + s.replace("'", "''") + "'"


def main():
    production, raw = production_expression(), raw_expression()
    difference = "(%s) unless on (country, city) (%s) or (%s) unless on (country, city) (%s)" % (
        raw, production, production, raw)
    found = cities()
    lines = ["# CITYDARKINBOTHTIERS WITH AND WITHOUT ITS HOURLY HISTORY: THE SAME ANSWER IN EVERY STATE.",
             "#",
             "# Written by infra/test/city_dark_history_suite.py -- its docstring says what is asked and why.",
             "# Change the rule, then run it again. No rule file: every series the alert reads is an input,",
             "# so the hourly minima can be absent (no history) or start part-way (the seam), which a",
             "# recording rule evaluated over the whole test cannot be.",
             "rule_files: []",
             "",
             "evaluation_interval: 5m",
             "",
             "tests:",
             "  - interval: 5m",
             "    input_series:"]
    for city, reference, old, new, minima in found:
        lines.append("      - series: %s" % quoted('%s{country="us",city="%s"}' % (REFERENCE, city)))
        lines.append("        values: %s" % quoted(run_length(reference)))
        if old:
            lines.append("      - series: %s" % quoted('%s{country="us",city="%s"}' % (FILMS, city)))
            lines.append("        values: %s" % quoted(run_length(old)))
        if new:
            lines.append("      - series: %s" % quoted('%s{country="us",city="%s",summarised_by="min2h"}'
                                                       % (FILMS, city)))
            lines.append("        values: %s" % quoted(run_length(new)))
        if minima:
            lines.append("      - series: %s" % quoted(
                'country_city:worker_movies_served:min2h{country="us",city="%s"}' % city))
            lines.append("        values: %s" % quoted(run_length(minima)))
        lines.append("      - series: %s" % quoted(WEB % city))
        lines.append("        values: %s" % quoted(run_length(reference)))
    lines.append("    promql_expr_test:")
    fired = 0
    for n, t in enumerate(eval_times()):
        expected = sorted(city for city, reference, _, _, _ in found if fires(reference, t))
        fired += len(expected)
        # YAML anchors: each expression is written once and named after.
        lines.append("      - expr: %s" % ("&production " + quoted(production) if n == 0 else "*production"))
        lines.append("        eval_time: %dm" % t)
        if expected:
            lines.append("        exp_samples:")
            for city in expected:
                lines.append("          - labels: %s" % quoted('{city="%s",country="us"}' % city))
                lines.append("            value: 0")
        else:
            lines.append("        exp_samples: []")
        lines.append("      - expr: %s" % ("&difference " + quoted(difference) if n == 0 else "*difference"))
        lines.append("        eval_time: %dm" % t)
        lines.append("        exp_samples: []")
    with open(OUT, "w", encoding="utf-8") as handle:
        handle.write("\n".join(lines) + "\n")
    print("wrote %s: %d cities, %d instants, %d expected firing samples"
          % (OUT, len(found), len(eval_times()), fired))


if __name__ == "__main__":
    main()
