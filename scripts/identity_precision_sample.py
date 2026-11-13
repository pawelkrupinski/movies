#!/usr/bin/env python3
"""THE IDENTITY PRECISION SAMPLE: an unbiased estimate of how many SERVED showtimes sit on a wrong film.

The judgements file (test/resources/fixtures/identity-judgements.tsv) is biased toward suspects -- a take is judged
because something flagged it -- so the share judged wrong says nothing of the corpus. This draws a random sample of
the takes prod SERVES now, with probability proportional to their upcoming showtimes, so that the share of showtimes
on wrong films is estimated without bias once every sampled take is judged.

    scripts/identity-precision-sample sample   [--countries pl,uk] [--n 150] [--seed S] [--out DIR] [--now ISO]
    scripts/identity-precision-sample estimate [--out DIR] [--judgements TSV] [--countries ...]

`sample` exports each country's served takes from the LOCAL prod mirror (mongodb://127.0.0.1:28017, read-only, after
scripts/local-mirror/freshness) with scripts/identity-precision-export.js -- one take is (venue, raw title, TMDB film),
its weight the upcoming showtimes of the screenings it serves -- and writes, under --out (default
~/.cache/movies-precision/<today>):
  population-<cc>.jsonl  every served take, as exported
  sample-<cc>.tsv        the sampled takes: draw order, showtimes, inclusion probability, certainty, the verdict the
                         judgements give now, and the billing beside the TMDB record (the evidence)
  sample.json            the seed, n, the moment and each country's population size and total showtimes
`estimate` reads the samples back with the judgements as they stand NOW (label_of: a venue's row over "*"'s, a film is
wrong where another of its database is judged right) and prints, per country and overall, the share of upcoming
showtimes on wrong films (95% CI) and the per-take wrong rate, with how many sampled takes are still unjudged.

THE DESIGN. Randomised systematic PPS without replacement: a take whose showtimes reach the sampling interval
(total / n) is taken with certainty (its contribution is exact) and removed, until none does; the rest are shuffled
by the seed and n' of them drawn systematically from one random start, so each is drawn with probability exactly
pi = n' * showtimes / remaining total. The Horvitz-Thompson estimate of the wrong showtimes is then
sum(certain wrong showtimes) + (remaining total / n') * (wrong draws); its variance is approximated as with-
replacement sampling's (conservative for a shuffled systematic draw), the interval a Wilson score interval on the
drawn share. The per-take rate weights each draw by 1 / pi (a ratio estimate). Re-run monthly; a seed reproduces a draw
of the same population.
"""

import argparse
import datetime
import json
import math
import os
import random
import re
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "ci"))
from identity_judgements import COUNTRIES, label_of, labels_by_country, read  # noqa: E402

EXPORT = os.path.join(HERE, "identity-precision-export.js")
JUDGEMENTS = os.path.join(HERE, "..", "test", "resources", "fixtures", "identity-judgements.tsv")
MIRROR = "mongodb://127.0.0.1:28017"
DATABASES = {"pl": "kinowo_prod_mirror"}
Z95 = 1.959964
LINE_BREAKS = re.compile("[\t\n\r\x0b\x0c\x1c-\x1e\x85\u2028\u2029]")
SAMPLE_COLUMNS = ("draw", "country", "venue", "rawTitle", "film", "showtimes", "inclusion", "certain", "verdict",
                  "recordTitle", "recordOriginalTitle", "recordYear", "recordRuntime", "recordDirectors",
                  "billedYear", "billedRuntime", "billedOriginalTitle", "billedDirectors", "billedIds", "basis",
                  "synopsis", "url")


# ── the draw ────────────────────────────────────────────────────────────────────────────────────

def draw(takes, n, rng):
    """[(take, inclusion probability, certain)] of a randomised systematic PPS draw of n of `takes` (each a dict with
    "showtimes" > 0), without replacement: certainty takes first (pi = 1), then the systematic draws."""
    rest = [t for t in takes if t["showtimes"] > 0]
    certain = []
    while True:
        k = n - len(certain)
        total = sum(t["showtimes"] for t in rest)
        if k <= 0 or not rest:
            return [(t, 1.0, True) for t in certain]
        big = [t for t in rest if t["showtimes"] * k >= total]
        if not big:
            break
        certain.extend(big)
        rest = [t for t in rest if t["showtimes"] * k < total]
    k = min(k, len(rest))
    rng.shuffle(rest)
    step = total / k
    start = rng.uniform(0, step)
    picks, cumulative, i = [], 0.0, 0
    for t in rest:
        lo, cumulative = cumulative, cumulative + t["showtimes"]
        while i < k and lo <= start + i * step < cumulative:
            picks.append(t)
            i += 1
    return [(t, 1.0, True) for t in certain] + [(t, k * t["showtimes"] / total, False) for t in picks]


# ── the estimate ────────────────────────────────────────────────────────────────────────────────

def wilson(wrong, n):
    """The 95% Wilson score interval of a share wrong / n."""
    if n == 0:
        return (0.0, 1.0)
    p = wrong / n
    centre = (p + Z95 ** 2 / (2 * n)) / (1 + Z95 ** 2 / n)
    half = Z95 * math.sqrt(p * (1 - p) / n + Z95 ** 2 / (4 * n * n)) / (1 + Z95 ** 2 / n)
    return (max(0.0, centre - half), min(1.0, centre + half))


def estimate(rows, population_showtimes):
    """One country's (or the pool's) estimate from its sample rows (dicts with showtimes, inclusion, certain, verdict):
    {share, low, high, takeRate, judged, wrong, unjudged, n}. Unjudged draws are left out of the drawn share (a missing-
    at-random assumption, reported beside it); a certain take's showtimes count exactly."""
    certain = [r for r in rows if r["certain"]]
    drawn = [r for r in rows if not r["certain"]]
    certain_total = sum(r["showtimes"] for r in certain)
    certain_wrong = sum(r["showtimes"] for r in certain if r["verdict"] == "wrong")
    judged = [r for r in drawn if r["verdict"] in ("right", "wrong")]
    wrong = sum(1 for r in judged if r["verdict"] == "wrong")
    rest = population_showtimes - certain_total
    p = wrong / len(judged) if judged else 0.0
    low, high = wilson(wrong, len(judged))
    share = (certain_wrong + rest * p) / population_showtimes
    by_take = [r for r in rows if r["verdict"] in ("right", "wrong")]
    weights = sum(1 / r["inclusion"] for r in by_take)
    take_rate = sum(1 / r["inclusion"] for r in by_take if r["verdict"] == "wrong") / weights if weights else 0.0
    return {"share": share, "low": (certain_wrong + rest * low) / population_showtimes,
            "high": (certain_wrong + rest * high) / population_showtimes, "takeRate": take_rate,
            "n": len(rows), "judged": len(judged) + sum(1 for r in certain if r["verdict"] in ("right", "wrong")),
            "wrong": wrong + sum(1 for r in certain if r["verdict"] == "wrong"),
            "unjudged": sum(1 for r in rows if r["verdict"] not in ("right", "wrong")),
            "_rest": rest, "_judged": len(judged), "_certainWrong": certain_wrong}


def pooled(per_country, totals):
    """The pool of countries: each country's share weighted by its showtimes, its interval a Wilson score interval at the
    pool's effective sample size (Kish: 1 / sum((drawn showtimes share)^2 / judged draws)), which stays a real
    interval when no draw is wrong -- a normal interval would collapse to [0, 0] there."""
    total = sum(totals.values())
    share = sum(e["share"] * totals[cc] for cc, e in per_country.items()) / total
    certain = sum(e["_certainWrong"] for e in per_country.values()) / total
    drawn = sum(e["_rest"] for e in per_country.values()) / total
    terms = [(e["_rest"] / total) ** 2 / e["_judged"] for e in per_country.values() if e["_judged"]]
    if not terms or drawn == 0:
        return {"share": share, "low": share, "high": share}
    effective = drawn ** 2 / sum(terms)
    p = (share - certain) / drawn
    low, high = wilson(p * effective, effective)
    return {"share": share, "low": certain + drawn * low, "high": certain + drawn * high}


# ── I/O ─────────────────────────────────────────────────────────────────────────────────────────

def export(cc, now, mirror=MIRROR):
    database = DATABASES.get(cc, f"kinowo_{cc}_prod_mirror")
    out = subprocess.run(["mongosh", "--quiet", f"{mirror}/{database}", EXPORT], check=True, capture_output=True,
                         text=True, env=dict(os.environ, NOW=now)).stdout
    return [json.loads(line) for line in out.splitlines() if line.strip()]


def cell(value):
    if isinstance(value, list):
        value = "; ".join(map(str, value))
    elif isinstance(value, dict):
        value = " ".join(f"{k}:{v}" for k, v in sorted(value.items()))
    return "" if value is None else LINE_BREAKS.sub(" ", str(value))


def sample_row(i, cc, take, inclusion, certain, verdict):
    billing, record = take.get("billing") or {}, take.get("record") or {}
    return {"draw": i, "country": cc, "venue": take["venue"], "rawTitle": take["rawTitle"], "film": take["film"],
            "showtimes": take["showtimes"], "inclusion": round(inclusion, 6), "certain": certain, "verdict": verdict or "",
            "recordTitle": record.get("title"), "recordOriginalTitle": record.get("originalTitle"),
            "recordYear": record.get("year"), "recordRuntime": record.get("runtime"),
            "recordDirectors": record.get("directors"), "billedYear": billing.get("year"),
            "billedRuntime": billing.get("runtime"), "billedOriginalTitle": billing.get("originalTitle"),
            "billedDirectors": billing.get("directors"), "billedIds": billing.get("ids"), "basis": take.get("basis"),
            "synopsis": billing.get("synopsis"), "url": billing.get("url")}


def write_sample(path, rows):
    with open(path, "w", encoding="utf-8") as f:
        f.write("\t".join(SAMPLE_COLUMNS) + "\n")
        for r in rows:
            f.write("\t".join(cell(r[c]) for c in SAMPLE_COLUMNS) + "\n")


def read_sample(path):
    with open(path, encoding="utf-8", newline="") as f:
        header = f.readline().rstrip("\n").split("\t")
        rows = [dict(zip(header, line.split("\t"))) for line in f.read().split("\n") if line.strip()]
    for r in rows:
        r["showtimes"], r["inclusion"], r["certain"] = int(r["showtimes"]), float(r["inclusion"]), r["certain"] == "True"
    return rows


def judge(rows, labels):
    for r in rows:
        r["verdict"] = label_of(labels.get(r["country"], {}), r["venue"], r["rawTitle"], r["film"]) or ""
    return rows


def default_out():
    return os.path.expanduser(f"~/.cache/movies-precision/{datetime.date.today().isoformat()}")


def countries_of(text):
    ccs = [c for c in (text or ",".join(COUNTRIES)).split(",") if c]
    unknown = [c for c in ccs if c not in COUNTRIES]
    if unknown:
        raise SystemExit(f"unknown countries: {','.join(unknown)}")
    return ccs


def cmd_sample(a):
    if subprocess.run([os.path.join(HERE, "local-mirror", "freshness"), *a.countries, "--quiet"]).returncode != 0:
        raise SystemExit("the local prod mirror is not fresh (scripts/local-mirror/freshness): not sampling a stale corpus")
    os.makedirs(a.out, exist_ok=True)
    labels = labels_by_country(read(a.judgements))
    meta = {"seed": a.seed, "n": a.n, "now": a.now, "countries": {}}
    for cc in a.countries:
        takes = export(cc, a.now)
        takes.sort(key=lambda t: (t["venue"], t["rawTitle"], t["film"]))  # the seed alone decides the draw
        with open(os.path.join(a.out, f"population-{cc}.jsonl"), "w", encoding="utf-8") as f:
            f.writelines(json.dumps(t, ensure_ascii=False) + "\n" for t in takes)
        rng = random.Random(f"{a.seed}:{cc}")
        rows = [sample_row(i, cc, t, pi, certain, None) for i, (t, pi, certain) in enumerate(draw(takes, a.n, rng), 1)]
        write_sample(os.path.join(a.out, f"sample-{cc}.tsv"), judge(rows, labels))
        meta["countries"][cc] = {"takes": len(takes), "showtimes": sum(t["showtimes"] for t in takes), "drawn": len(rows),
                                 "certain": sum(1 for r in rows if r["certain"])}
        print(f"{cc}: {len(takes)} served takes, {meta['countries'][cc]['showtimes']} upcoming showtimes; drew {len(rows)}"
              f" ({meta['countries'][cc]['certain']} certain), {sum(1 for r in rows if r['verdict'])} already judged")
    with open(os.path.join(a.out, "sample.json"), "w", encoding="utf-8") as f:
        json.dump(meta, f, indent=1)
    print(f"wrote {a.out}")


def report(out, judgements, countries):
    with open(os.path.join(out, "sample.json"), encoding="utf-8") as f:
        meta = json.load(f)
    labels = labels_by_country(read(judgements))
    per, totals, lines = {}, {}, []
    lines.append(f"seed {meta['seed']}, n {meta['n']}, served at {meta['now']}")
    lines.append("country  showtimes-wrong  95% CI            per-take  judged  wrong  unjudged")
    for cc in countries:
        if cc not in meta["countries"]:
            continue
        rows = judge(read_sample(os.path.join(out, f"sample-{cc}.tsv")), labels)
        totals[cc] = meta["countries"][cc]["showtimes"]
        e = per[cc] = estimate(rows, totals[cc])
        lines.append(f"{cc:<8} {e['share']:>14.2%}  [{e['low']:.2%}, {e['high']:.2%}]  {e['takeRate']:>8.2%}  "
                     f"{e['judged']:>6}  {e['wrong']:>5}  {e['unjudged']:>8}")
    if len(per) > 1:
        p = pooled(per, totals)
        lines.append(f"{'all':<8} {p['share']:>14.2%}  [{p['low']:.2%}, {p['high']:.2%}]")
    return "\n".join(lines)


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    sub = p.add_subparsers(dest="cmd", required=True)
    s = sub.add_parser("sample")
    s.add_argument("--n", type=int, default=150)
    s.add_argument("--seed", default=datetime.date.today().isoformat())
    s.add_argument("--now", default=datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"))
    e = sub.add_parser("estimate")
    for q in (s, e):
        q.add_argument("--countries", default=None)
        q.add_argument("--out", default=None)
        q.add_argument("--judgements", default=JUDGEMENTS)
    a = p.parse_args(argv)
    a.countries, a.out = countries_of(a.countries), a.out or default_out()
    if a.cmd == "sample":
        cmd_sample(a)
    else:
        print(report(a.out, a.judgements, a.countries))


if __name__ == "__main__":
    main()
