#!/usr/bin/env python3
"""COUNTRIES DISAGREE: a title billed in two or more countries in the same week that one country takes as a film NO
other country takes -- a triage list for the Identity findings report, never a rule (docs/identity-experiments.md,
"agreement ACROSS countries": as a rule it cured almost nothing; as a list it surfaced DE «Fallen Angels by Noël
Coward» on the Wong Kar-wai film while the UK and the US took the Coward relay).

Per title key (cross_country.title_key of the raw or the billing's own original title) whose countries take different
films (cross_country.disagreeing), every take is one (country, film) with its listings, basis and judgement in
identity-judgements.tsv (label_of, each listing in its own country: WRONG when any listing of it is judged wrong,
RIGHT when one is judged right and none wrong). A LONE take is one whose film no other country takes for the title
and whose billing does not back it -- no year or director of its own agreeing with the record (cross_country's
`backed`): "Street Fighter (1994)" beside everyone's 2026 film states its year, so it is no one's to triage. A title
is listed when it has a lone take: `check` when a take is judged wrong, `judge` when one is unjudged. Left out: a
title whose takes are all judged right (legitimately different releases or records), and one whose takes are all
relays, broadcast seasons or screening events (cross_country.EVENT_RULES: two houses' "Swan Lake"). Most listings
first.

Input: the LOCAL prod mirror's light export (cross_country_export.js, LIGHT=1: ~20 s for all five countries), written
by `--mirror` into --data, or a --data directory already holding it.

usage: countries_disagree.py [--mirror URI] --data DIR [--judgements F] [--country cc] [--limit N]
Prints the section (markdown). `--mirror` reads only the LOCAL mirror (127.0.0.1:28017) -- check
scripts/local-mirror/freshness first.
"""

import argparse
import collections
import os
import subprocess
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import cross_country as X  # noqa: E402
from identity_judgements import COUNTRIES, label_of, labels_by_country, read  # noqa: E402

HEADING = "Countries disagree"
HOW = ("The same title billed in two or more countries in the same week, one country taking it as a film no other "
       "country takes and its billing stating no year or director that backs it (first), the others' takes after "
       "`| vs`. `judge`: judge the "
       "unjudged take(s) into `test/resources/fixtures/identity-judgements.tsv`; `check`: a take is judged wrong -- "
       "fix the country taking it, or the judgement. From the local prod mirror (`scripts/ci/countries_disagree.py`).")
DATABASES = {"pl": "kinowo_prod_mirror"}
EXPORT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "ml-identity", "cross_country_export.js")


def takes_of(linked, labels, films):
    """[take] of one title's linked listings: one per (country, film taken), with its listings, venues, basis, verdict,
    whether any billing of it backs it, and whether it is an event/relay take."""
    grouped = collections.defaultdict(list)
    for l in linked:
        if l["f"]:
            grouped[(l["cc"], l["f"])].append(l)
    out = []
    for (cc, film), ls in sorted(grouped.items()):
        verdicts = {label_of(labels.get(cc, {}), l["v"], l["r"], f"tmdb:{film}") for l in ls}
        record = films.get(film) or {}
        out.append(dict(cc=cc, film=film, title=record.get("t"), year=record.get("y"), listings=len(ls),
                        rawTitle=collections.Counter(l["r"] for l in ls).most_common(1)[0][0],
                        basis=collections.Counter(l["b"] for l in ls).most_common(1)[0][0],
                        verdict="wrong" if "wrong" in verdicts else "right" if "right" in verdicts else None,
                        backed=any(l["backed"] for l in ls),
                        event=all(X.EVENT_RULES.intersection(l["rules"]) for l in ls)))
    return out


def lone(take, takes):
    """Whether no other country takes the take's film for the title and no billing of it backs it."""
    return not take["backed"] and not any(t["film"] == take["film"] and t["cc"] != take["cc"] for t in takes)


def disagreements(listings, labels, films):
    """[{key, listings, action, lone, others}] to triage, most listings first."""
    out = []
    for key, linked in X.disagreeing(X.index_by_title(listings)):
        takes = takes_of(linked, labels, films)
        alone = [t for t in takes if lone(t, takes)]
        if not alone or all(t["verdict"] == "right" for t in takes) or all(t["event"] for t in takes):
            continue
        action = "check" if any(t["verdict"] == "wrong" for t in takes) else "judge"
        out.append(dict(key=key, listings=sum(t["listings"] for t in takes), action=action, lone=alone,
                        others=[t for t in takes if t not in alone]))
    return sorted(out, key=lambda d: (-d["listings"], d["key"]))


def film_of(t):
    year = f" ({t['year']})" if t["year"] else ""
    return f"tmdb:{t['film']} {t['title'] or '?'}{year}"


def line_of(d):
    """One title: each lone take in full, then the other takes by film, their countries and listings together."""
    alone = [f"{t['cc']} «{t['rawTitle']}» → {film_of(t)} {t['basis']} ×{t['listings']} {t['verdict'] or 'unjudged'}"
             for t in d["lone"]]
    by_film = collections.defaultdict(list)
    for t in d["others"]:
        by_film[t["film"]].append(t)
    others = [f"{'/'.join(t['cc'] for t in ts)} → {film_of(ts[0])} ×{sum(t['listings'] for t in ts)} "
              f"{'/'.join(t['verdict'] or 'unjudged' for t in ts)}"
              for ts in sorted(by_film.values(), key=lambda ts: -sum(t["listings"] for t in ts))]
    return (f"- **{d['action']}** «{d['key']}» ({d['listings']} listings) -- " + " · ".join(alone)
            + (" | vs " + " · ".join(others) if others else ""))


def section(found, country=None, limit=30):
    """The report section's lines: the titles `country` bills (every title without one), at most `limit`."""
    rows = [d for d in found if country is None or any(t["cc"] == country for t in d["lone"] + d["others"])]
    if not rows:
        return []
    lines = ["", f"## {HEADING} ({len(rows)})", "", HOW] + [line_of(d) for d in rows[:limit]]
    if len(rows) > limit:
        lines.append(f"- … and {len(rows) - limit} more: `python3 scripts/ci/countries_disagree.py --data <export>`")
    return lines


def export(mirror, data, countries):
    """The light export of each country's mirror database into `data`."""
    os.makedirs(data, exist_ok=True)
    for cc in countries:
        database = DATABASES.get(cc, f"kinowo_{cc}_prod_mirror")
        for mode in ("traces", "listings", "films"):
            with open(os.path.join(data, f"{mode}-{cc}.jsonl"), "w", encoding="utf-8") as out:
                subprocess.run(["mongosh", "--quiet", f"{mirror.rstrip('/')}/{database}", os.path.abspath(EXPORT)],
                               stdout=out, check=True, env=dict(os.environ, CC=cc, MODE=mode, LIGHT="1"))


def read_found(data, judgements, countries=COUNTRIES):
    films = X.read_films(data, countries)
    return disagreements(X.read_listings(data, countries, films), labels_by_country(read(judgements)), films)


def main(argv=None):
    p = argparse.ArgumentParser()
    p.add_argument("--mirror", help="export the LOCAL prod mirror (mongodb://127.0.0.1:28017) into --data first")
    p.add_argument("--data", required=True)
    p.add_argument("--judgements", default="test/resources/fixtures/identity-judgements.tsv")
    p.add_argument("--country")
    p.add_argument("--limit", type=int, default=30)
    a = p.parse_args(argv)
    if a.mirror:
        if "127.0.0.1:28017" not in a.mirror and "localhost:28017" not in a.mirror:
            print("--mirror reads the LOCAL prod mirror only (127.0.0.1:28017)", file=sys.stderr)
            return 64
        export(a.mirror, a.data, COUNTRIES)
    print("\n".join(section(read_found(a.data, a.judgements), a.country, a.limit)).lstrip("\n"))
    return 0


if __name__ == "__main__":
    sys.exit(main())
