#!/usr/bin/env python3
"""Production match regressions, read from the identity take-change log and published as metrics.

WHY THIS EXISTS. Two regressions went unnoticed for one to two days in 2026-10: DE "Der kleine Prinz"
(16 listings lost, builds 0836455/627fa94, 2026-10-20) and PL Cinema City "Swiateczna zabawa z Peppa"
(123 listings lost, build 96fb23c, 2026-10-21 11:30Z). Both were below identity-matching.rules'
country-wide shares (0.15% of a country's listings in an hour) and both were plain to see in
VictoriaLogs, one `identity take change` line per listing. This turns those lines into three
per-film / per-build / per-listing-pair gauges that identity-regressions.rules alerts on.

WHAT IT READS. Every worker logs `identity take change: <kind> ...` with fields country, kind
(lost / changed / gained), build, venue, listing, tmdb_before/tmdb_after, title_before/title_after
and cause (see .claude/skills/identity-investigation). This fetches the last 24h of them every few
minutes (1-3k lines on an ordinary day) and computes:

  kinowo_identity_film_net_lost{country,tmdb,title,build}
      listings that held the film at the start of the last 6h and hold NO film now, minus listings
      that held no film then and hold this one now. "No matching gain elsewhere": a listing that
      moved to ANOTHER film (kind changed, net) is not a loss, and a film losing listings under one
      key while gaining them under another nets out. Only values >= PUBLISH_FLOOR are written.
  kinowo_identity_build_net_lost{country,build,cause,tmdb,title}
      per build and cause, over the last 24h: listings it unmatched (lost) minus listings it matched
      (gained). `tmdb`/`title` name the film that lost most under it. `cause` is the worker's
      (services.identity.TakeCauses): `code` when nothing the take stood on moved and the decision did,
      `evidence` when what it stood on moved (a listing it rested on left or was re-published otherwise,
      an answer was filed: PL "Kupilem motocykl, ktory jest wampirem", 11 listings, 2026-10-24, lost
      when Kino Pionier 1907's one screening passed -- right, and no build's doing), `regrouped` when
      the store moved the listing. Only `code` alerts. A line from a build before the field counts as
      `code`, as every loss did before it.
  kinowo_identity_listing_flips{country,tmdb_a,tmdb_b,title,build,venue}
      the most times one listing moved between the same two films (kind changed, either direction)
      in the last 24h, per film pair; `venue` is one listing doing it, `build` the latest flip's. Pairs
      under FLIP_FLOOR are not written; when none reaches it, one unlabelled 0 is, so the gauge is never absent.
  kinowo_identity_regressions_last_success_timestamp_seconds / _lines
      when a run last read the log, and how many lines it read.

THE BACKTEST. `backtest` replays the same functions over a past stretch (default 14 days, every 5
minutes) with the alert thresholds of identity-regressions.rules, and prints every episode that would
have fired. The thresholds are duplicated in THRESHOLDS below for that purpose only; the rules file is
what alerts, and test_identity_regressions.py fails if the two disagree.

Usage:
  identity-regressions.py publish --url http://10.20.0.11:9428 --out /etc/node-exporter/textfile.d/identity-regressions.prom
  identity-regressions.py backtest --url http://10.20.0.11:9428 [--days 14] [--step 300]
  identity-regressions.py backtest --lines dump.jsonl     (the LogsQL query's JSON lines, saved)
"""

import argparse
import collections
import datetime
import json
import os
import re
import tempfile
import urllib.parse
import urllib.request

FILM_WINDOW = 6 * 3600
BUILD_WINDOW = 24 * 3600
FLIP_WINDOW = 24 * 3600
FETCH_WINDOW = "24h"
PUBLISH_FLOOR = 5  # film_net_lost below this is not written: it alerts at 10, and the file stays small
FLIP_FLOOR = 2

FIELDS = ("_time", "country", "kind", "build", "venue", "listing",
          "tmdb_before", "tmdb_after", "title_before", "title_after", "cause")
# The cause a line from a build that logged none counts as: every loss was the build's then.
UNSTATED_CAUSE = "code"
ALERTED_CAUSE = "code"

# Mirrors identity-regressions.rules (pinned by test_identity_regressions.py). Build thresholds are per
# country; any country not named uses "*".
THRESHOLDS = {
    "film_net_lost": 10,
    "film_for_seconds": 900,
    "build_net_lost": {"de": 65, "uk": 25, "*": 10},
    "build_for_seconds": 900,
    "listing_flips": 3,
}


def parse_time(s):
    # 2026-10-20T21:13:41.080249024Z -> epoch seconds; Python's fromisoformat takes 6 fraction digits.
    head, _, frac = s.rstrip("Z").partition(".")
    t = datetime.datetime.fromisoformat(head).replace(tzinfo=datetime.timezone.utc).timestamp()
    return t + (float("0." + frac) if frac else 0.0)


# Keyed exactly as scripts/ci/identity_judgements.py keys a billing: runs of Unicode whitespace one space, none at
# the ends, case kept (its rows spell "ICONIC MOVIE" and "Iconic Movie" apart).
_WHITESPACE = "\t\n\x0b\x0c\r \x85\xa0\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a\u2028\u2029\u202f\u205f\u3000"
_RUNS = re.compile(f"[{_WHITESPACE}]+")


def normalise(text):
    return _RUNS.sub(" ", text or "").strip(" ")


def load_judgements(path):
    """{(country, venue or "*", normalised raw title, "tmdb:<id>"): verdict} from identity-judgements.tsv."""
    out = {}
    if not path:
        return out
    with open(path, encoding="utf-8") as f:
        for n, raw in enumerate(f):
            if raw.startswith("#") or n == 0 and raw.startswith("country\t"):
                continue
            cols = raw.rstrip("\n").split("\t")
            if len(cols) < 5 or cols[0] == "country":
                continue
            out[(cols[0], normalise(cols[1]) if cols[1] != "*" else "*", normalise(cols[2]), cols[3])] = cols[4]
    return out


def verdict(judgements, country, venue, listing, tmdb):
    """A venue's row overrides "*"'s, as in the judgements file."""
    if not tmdb:
        return None
    title, film = normalise(listing), "tmdb:" + tmdb
    return judgements.get((country, normalise(venue), title, film)) or judgements.get((country, "*", title, film))


def parse(lines, judgements=None):
    """Take-change lines, oldest first. A lost or changed take whose film was judged WRONG for that billing is
    marked `cure`: losing it is the fix, never a regression (DE "Iconic Movie", 31 listings, 2026-10-22)."""
    judgements = judgements or {}
    out = []
    for line in lines:
        line = line.strip()
        if not line:
            continue
        r = json.loads(line)
        if r.get("kind") not in ("lost", "changed", "gained"):
            continue
        r["t"] = parse_time(r["_time"])
        r["cause"] = r.get("cause") or UNSTATED_CAUSE
        r["cure"] = r["kind"] != "gained" and verdict(
            judgements, r.get("country", ""), r.get("venue", ""), r.get("listing", ""), r.get("tmdb_before")) == "wrong"
        out.append(r)
    out.sort(key=lambda r: r["t"])
    return out


def _key(r):
    return (r.get("country", ""), r.get("venue", ""), r.get("listing", ""))


def film_net_lost(records, now, window=FILM_WINDOW):
    """{(country, tmdb): {"count", "title", "build"}} for films with a positive net loss."""
    first, last = {}, {}
    for r in records:
        if now - window < r["t"] <= now:
            k = _key(r)
            first.setdefault(k, r)
            last[k] = r
    net = collections.Counter()
    builds = collections.defaultdict(collections.Counter)
    titles = {}
    for k, f in first.items():
        before, after = f.get("tmdb_before"), last[k].get("tmdb_after")
        if before and not after:
            if f.get("cure"):
                continue
            net[(k[0], before)] += 1
            builds[(k[0], before)][last[k].get("build", "")] += 1
            titles.setdefault((k[0], before), f.get("title_before") or k[2])
        elif after and not before:
            net[(k[0], after)] -= 1
    return {fk: {"count": n, "title": titles[fk], "build": builds[fk].most_common(1)[0][0]}
            for fk, n in net.items() if n > 0}


def build_net_lost(records, now, window=BUILD_WINDOW):
    """{(country, build, cause): {"count", "tmdb", "title"}} for builds that unmatched more than they matched, per
    cause: a gain offsets only a loss of its own cause."""
    lost, gained = collections.defaultdict(set), collections.defaultdict(set)
    films = collections.defaultdict(collections.Counter)
    titles = {}
    for r in records:
        if not (now - window < r["t"] <= now):
            continue
        bk = (r.get("country", ""), r.get("build", ""), r.get("cause") or UNSTATED_CAUSE)
        if r["kind"] == "lost":
            if r.get("cure"):
                continue
            lost[bk].add(_key(r))
            films[bk][r.get("tmdb_before", "")] += 1
            titles.setdefault((bk, r.get("tmdb_before", "")), r.get("title_before") or r.get("listing", ""))
        elif r["kind"] == "gained":
            gained[bk].add(_key(r))
    out = {}
    for bk in lost:
        n = len(lost[bk]) - len(gained.get(bk, ()))
        if n > 0:
            tmdb = films[bk].most_common(1)[0][0]
            out[bk] = {"count": n, "tmdb": tmdb, "title": titles[(bk, tmdb)]}
    return out


def cures(records, now, window=BUILD_WINDOW):
    """{(country, tmdb): {"count", "title", "build"}}: listings that lost a take judged wrong in the window."""
    keys = collections.defaultdict(set)
    builds = collections.defaultdict(collections.Counter)
    titles = {}
    for r in records:
        if r.get("cure") and now - window < r["t"] <= now:
            fk = (r.get("country", ""), r.get("tmdb_before", ""))
            keys[fk].add(_key(r))
            builds[fk][r.get("build", "")] += 1
            titles.setdefault(fk, r.get("title_before") or r.get("listing", ""))
    return {fk: {"count": len(v), "title": titles[fk], "build": builds[fk].most_common(1)[0][0]} for fk, v in keys.items()}


def listing_flips(records, now, window=FLIP_WINDOW):
    """{(country, tmdb_a, tmdb_b): {"count", "title", "build", "venue"}}: the most changes one listing
    made between the two films (either direction) inside the window."""
    per = collections.Counter()
    latest = {}
    for r in records:
        if r["kind"] != "changed" or not (now - window < r["t"] <= now):
            continue
        a, b = sorted((r.get("tmdb_before", ""), r.get("tmdb_after", "")))
        k = _key(r) + (a, b)
        per[k] += 1
        latest[k] = r
    out = {}
    for k, n in per.items():
        pk = (k[0], k[3], k[4])
        if pk not in out or n > out[pk]["count"]:
            r = latest[k]
            out[pk] = {"count": n, "title": r.get("title_after") or r.get("title_before") or k[2],
                       "build": r.get("build", ""), "venue": k[1]}
    return out


def _escape(v):
    return str(v).replace("\\", "\\\\").replace("\n", "\\n").replace('"', '\\"')


def _labels(**kv):
    return "{" + ",".join(f'{k}="{_escape(v)}"' for k, v in kv.items()) + "}"


def render(records, now):
    """The textfile, `# TYPE` headers and all."""
    out = []
    out.append("# HELP kinowo_identity_film_net_lost Listings that held the film 6h ago and hold no film now, net of listings newly matched to it.")
    out.append("# TYPE kinowo_identity_film_net_lost gauge")
    for (cc, tmdb), v in sorted(film_net_lost(records, now).items()):
        if v["count"] >= PUBLISH_FLOOR:
            out.append(f"kinowo_identity_film_net_lost{_labels(country=cc, tmdb=tmdb, title=v['title'], build=v['build'])} {v['count']}")
    out.append("# HELP kinowo_identity_build_net_lost Listings a build unmatched minus listings it matched, over its last 24h of take changes, by cause (code / evidence / regrouped).")
    out.append("# TYPE kinowo_identity_build_net_lost gauge")
    for (cc, build, cause), v in sorted(build_net_lost(records, now).items()):
        out.append(f"kinowo_identity_build_net_lost{_labels(country=cc, build=build, cause=cause, tmdb=v['tmdb'], title=v['title'])} {v['count']}")
    out.append("# HELP kinowo_identity_listing_flips Most moves one listing made between the same two films in 24h.")
    out.append("# TYPE kinowo_identity_listing_flips gauge")
    flips = [(k, v) for k, v in sorted(listing_flips(records, now).items()) if v["count"] >= FLIP_FLOOR]
    for (cc, a, b), v in flips:
        out.append(f"kinowo_identity_listing_flips{_labels(country=cc, tmdb_a=a, tmdb_b=b, title=v['title'], build=v['build'], venue=v['venue'])} {v['count']}")
    if not flips:
        # ALWAYS A SAMPLE: with no pair at the floor the gauge went absent (2026-10-24..25 and from 10-26), and an
        # absent gauge is a blind panel and an alert that cannot tell "nothing flips" from "nothing is counted".
        out.append("kinowo_identity_listing_flips 0")
    out.append("# HELP kinowo_identity_cures Listings that lost, in 24h, a take judged wrong in identity-judgements.tsv (a fix; never alerted).")
    out.append("# TYPE kinowo_identity_cures gauge")
    for (cc, tmdb), v in sorted(cures(records, now).items()):
        out.append(f"kinowo_identity_cures{_labels(country=cc, tmdb=tmdb, title=v['title'], build=v['build'])} {v['count']}")
    out.append("# HELP kinowo_identity_regressions_lines Take-change lines the last successful run read (24h).")
    out.append("# TYPE kinowo_identity_regressions_lines gauge")
    out.append(f"kinowo_identity_regressions_lines {len(records)}")
    out.append("# HELP kinowo_identity_regressions_last_success_timestamp_seconds When the take-change log was last read and published.")
    out.append("# TYPE kinowo_identity_regressions_last_success_timestamp_seconds gauge")
    out.append(f"kinowo_identity_regressions_last_success_timestamp_seconds {int(now)}")
    return "\n".join(out) + "\n"


def fetch(url, window):
    query = f"_time:{window} kind:* | fields " + ", ".join(FIELDS)
    data = urllib.parse.urlencode({"query": query}).encode()
    with urllib.request.urlopen(url.rstrip("/") + "/select/logsql/query", data=data, timeout=120) as resp:
        return resp.read().decode("utf-8").splitlines()


def publish(args):
    records = parse(fetch(args.url, FETCH_WINDOW), load_judgements(args.judgements))
    text = render(records, datetime.datetime.now(datetime.timezone.utc).timestamp())
    # ATOMIC: node_exporter must never read half a file (a torn line fails the whole file's parse).
    fd, tmp = tempfile.mkstemp(dir=os.path.dirname(args.out), prefix=".identity-regressions.")
    with os.fdopen(fd, "w") as f:
        f.write(text)
    os.chmod(tmp, 0o644)
    os.replace(tmp, args.out)


# ── backtest ──────────────────────────────────────────────────────────────────────────────────────

def build_threshold(country):
    t = THRESHOLDS["build_net_lost"]
    return t.get(country, t["*"])


def episodes(series_by_step, hold):
    """series_by_step: [(t, {key: info})] where every key present is over its threshold at t.
    Returns [(key, start, end, peak_info)] for runs held at least `hold` seconds (the rule's `for:`)."""
    open_, done = {}, []
    for t, firing in series_by_step:
        for k, info in firing.items():
            if k not in open_:
                open_[k] = [t, t, info]
            else:
                open_[k][1] = t
                if info["count"] > open_[k][2]["count"]:
                    open_[k][2] = info
        for k in [k for k in open_ if k not in firing]:
            done.append((k,) + tuple(open_.pop(k)))
    done.extend((k,) + tuple(v) for k, v in open_.items())
    return sorted([e for e in done if e[2] - e[1] >= hold], key=lambda e: e[1])


def backtest(records, start, end, step):
    a, b, c = [], [], []
    t = start
    while t <= end:
        a.append((t, {k: v for k, v in film_net_lost(records, t).items() if v["count"] >= THRESHOLDS["film_net_lost"]}))
        b.append((t, {k: v for k, v in build_net_lost(records, t).items()
                      if k[2] == ALERTED_CAUSE and v["count"] > build_threshold(k[0])}))
        c.append((t, {k: v for k, v in listing_flips(records, t).items() if v["count"] >= THRESHOLDS["listing_flips"]}))
        t += step
    return {
        "IdentityFilmListingsLost": episodes(a, THRESHOLDS["film_for_seconds"]),
        "IdentityBuildNetLosses": episodes(b, THRESHOLDS["build_for_seconds"]),
        "IdentityListingFlipFlop": episodes(c, 0),
    }


def _fmt(t):
    return datetime.datetime.fromtimestamp(t, datetime.timezone.utc).strftime("%Y-%m-%dT%H:%MZ")


def run_backtest(args):
    if args.lines:
        with open(args.lines) as f:
            records = parse(f, load_judgements(args.judgements))
    else:
        records = parse(fetch(args.url, f"{args.days}d"), load_judgements(args.judgements))
    if not records:
        print("no take-change lines")
        return
    end = records[-1]["t"] + args.step
    start = max(records[0]["t"], end - args.days * 86400)
    print(f"{len(records)} lines, {_fmt(records[0]['t'])} .. {_fmt(records[-1]['t'])}; replayed every {args.step}s")
    for alert, eps in backtest(records, start, end, args.step).items():
        print(f"\n{alert}: {len(eps)} episode(s)")
        for key, s, e, info in eps:
            print(f"  {_fmt(s)} {((e - s) / 3600):5.1f}h  {' '.join(key)}  n={info['count']}  "
                  f"{info.get('title', '')!r}  build={info.get('build', key[1] if alert == 'IdentityBuildNetLosses' else '')[:9]}")


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    sub = p.add_subparsers(dest="cmd", required=True)
    pp = sub.add_parser("publish")
    pp.add_argument("--url", required=True)
    pp.add_argument("--out", required=True)
    pp.add_argument("--judgements", help="identity-judgements.tsv: losses of takes judged wrong are cures, not regressions")
    pb = sub.add_parser("backtest")
    pb.add_argument("--url")
    pb.add_argument("--lines")
    pb.add_argument("--judgements")
    pb.add_argument("--days", type=int, default=14)
    pb.add_argument("--step", type=int, default=300)
    args = p.parse_args(argv)
    if args.cmd == "publish":
        publish(args)
    else:
        if not (args.url or args.lines):
            p.error("backtest needs --url or --lines")
        run_backtest(args)


if __name__ == "__main__":
    main()
