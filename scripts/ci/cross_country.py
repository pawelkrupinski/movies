"""ONE READING OF THE COUNTRIES' TAKES SIDE BY SIDE: each country's identity traces joined to the billings they decided,
keyed by the title billed, so a title billed in two countries in the same week can be compared across the border.

Read by the cross-country experiment (scripts/ml-identity/cross_country_experiment.py) and the Identity findings
report's "countries disagree" section (countries_disagree.py). Its input is a directory of per-country exports of the
LOCAL prod mirror (read-only, after scripts/local-mirror/freshness), written by scripts/ml-identity/cross_country_export.js
(`LIGHT=1`: only the fields the report reads):

  traces-<cc>.jsonl    db.identity_traces   -> {v, r, f (taken TMDB id), b (basis), ru (rules), e, c (candidates)}
  listings-<cc>.jsonl  db.identity_listings -> {v, t, o (originalTitle), y, rt, d (director), s0, s1 (showtime window), n}
  films-<cc>.jsonl     db.tmdb_films        -> {id, t, o, y, rt, d}
"""
import collections
import json
import os
import re
import unicodedata

DAY = 86400000
SLACK = 3 * DAY  # "the same week": screening windows overlapping within three days
CAND = re.compile(r"^(\d+) ([\d.]+)% rank (\S+) (.*?)'(.*)'(?: \((\d{4})\))?\s*$")
YEAR_IN_TITLE = re.compile(r"\((1[89]\d\d|20\d\d)\)")

# The trace rules that mark a take as a stage relay, a broadcast season or a screening event rather than a film: the
# title rules that peel a relay/event banner and the acceptance rules of the season/stage/house records. Two countries
# billing "Swan Lake" from two houses take two records, rightly; the report skips a title only when EVERY take of it
# carries one of these.
EVENT_RULES = frozenset({
    "accept:season-production", "accept:season-record", "accept:stage-production", "accept:house-production",
    "title:xtra-canonical-broadcast-season", "title:xtra-canonical-met-opera", "title:xtra-canonical-met-opera-suffix",
    "title:xtra-canonical-royal-ballet-opera", "title:xtra-canonical-nt-live", "title:xtra-exhibition-on-screen",
    "title:search-plus-event-suffix", "title:search-with-event-suffix",
})


def fold(s):
    s = unicodedata.normalize("NFD", s or "").replace("ł", "l").replace("Ł", "L")
    return "".join(ch for ch in s if not unicodedata.combining(ch)).lower()


def title_key(s):
    """Normalised original title: brackets, format tokens, punctuation and a leading article dropped."""
    s = fold(s)
    s = re.sub(r"\([^)]*\)|\[[^\]]*\]", " ", s)
    s = re.sub(r"\b(omu|omeu|ov|ome|2d|3d|imax|4dx|dolby|atmos|vose|vo|napisy|dubbing)\b", " ", s)
    s = s.replace("&", " and ")
    s = re.sub(r"[^\w]+", " ", s).strip()
    s = re.sub(r"^(the|a|an|der|die|das|el|la|los|las|le|les) ", "", s)
    return s


def person(s):
    return re.sub(r"[^a-z]", "", fold(s))


def jsonl(path):
    with open(path, encoding="utf-8") as f:
        for line in f:
            if line.strip():
                yield json.loads(line)


def read_films(data, countries):
    """{tmdb id: {t, o, y, d}} over every country's tmdb_films export."""
    films = {}
    for cc in countries:
        for f in jsonl(os.path.join(data, f"films-{cc}.jsonl")):
            try:
                fid = int(f["id"])
            except (TypeError, ValueError):
                continue
            y = f.get("y")
            films[fid] = dict(t=f.get("t"), o=f.get("o"), y=int(y) if y and str(y).isdigit() else None,
                              d={person(x) for x in (f.get("d") or [])} - {""})
    return films


def candidates_of(trace):
    out = {}
    for c in trace.get("c") or []:
        m = CAND.match(c)
        if m:
            out[int(m.group(1))] = dict(p=float(m.group(2)) / 100, denied="DENIED" in m.group(4))
    return out


def read_listings(data, countries, films):
    """One dict per trace, with the facts its billing states: cc, v (venue), r (raw title), f (taken film), b (basis),
    rules, year, dirs, rt, o, s0, s1, shows (showtimes), backed (a fact of its own agreeing with the taken record:
    director / year / None), bare, strict_bare, keys (its title keys), cands."""
    listings = []
    for cc in countries:
        facts = collections.defaultdict(list)
        for row in jsonl(os.path.join(data, f"listings-{cc}.jsonl")):
            facts[(row["v"], row["t"])].append(row)
        for t in jsonl(os.path.join(data, f"traces-{cc}.jsonl")):
            fs = facts.get((t["v"], t["r"]), [])
            own_year = next((f["y"] for f in fs if f.get("y")), None)
            m = YEAR_IN_TITLE.search(t["r"] or "")
            if own_year is None and m:
                own_year = int(m.group(1))
            directors = {person(d) for f in fs for d in f.get("d") or []} - {""}
            runtime = next((f["rt"] for f in fs if f.get("rt")), None)
            original = next((f["o"] for f in fs if f.get("o")), None)
            film = t.get("f")
            rec = films.get(film) if film else None
            backed = None
            if rec:
                if directors and rec["d"] and directors & rec["d"]:
                    backed = "director"
                elif own_year and rec["y"] and abs(own_year - rec["y"]) <= 1:
                    backed = "year"
            listings.append(dict(cc=cc, v=t["v"], r=t["r"], f=film, b=t.get("b"), rules=t.get("ru") or [],
                                 year=own_year, dirs=directors, rt=runtime, o=original,
                                 s0=min((f["s0"] for f in fs if f.get("s0")), default=None),
                                 s1=max((f["s1"] for f in fs if f.get("s1")), default=None),
                                 shows=sum(f.get("n") or 0 for f in fs), backed=backed,
                                 bare=own_year is None and not directors,
                                 strict_bare=own_year is None and not directors and not runtime,
                                 keys={k for k in {title_key(original), title_key(t["r"])} if k},
                                 cands=candidates_of(t)))
    return listings


def index_by_title(listings):
    """{title key: [listing]}: a listing under its raw title's key and its own original title's."""
    index = collections.defaultdict(list)
    for l in listings:
        for k in l["keys"]:
            index[k].append(l)
    return index


def overlap(a, b):
    """Whether two listings screen in the same week (windows overlapping within SLACK)."""
    if a["s0"] is None or b["s0"] is None:
        return False
    return a["s0"] <= b["s1"] + SLACK and b["s0"] <= a["s1"] + SLACK


def same_week_across_border(ls):
    """The listings of one title key that screen in the same week as a listing of ANOTHER country."""
    return [l for l in ls if any(m["cc"] != l["cc"] and overlap(l, m) for m in ls)]


def disagreeing(index):
    """[(title key, linked listings)] of the keys billed in >=2 countries in the same week whose countries take
    different films (at least two countries taking one)."""
    out = []
    for k, ls in index.items():
        if len({l["cc"] for l in ls}) < 2:
            continue
        linked = same_week_across_border(ls)
        taken = collections.defaultdict(set)
        for l in linked:
            if l["f"]:
                taken[l["cc"]].add(l["f"])
        if len(taken) >= 2 and len(set().union(*taken.values())) >= 2:
            out.append((k, linked))
    return out
