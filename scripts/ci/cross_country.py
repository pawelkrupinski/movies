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
from __future__ import annotations

import collections
import json
import os
import re
import unicodedata
from typing import Any, Dict, Iterable, Iterator, List, Optional, Set, Tuple, TypedDict

Json = Dict[str, Any]   # one line of an export, as json.loads reads it


class Film(TypedDict):
    t: Optional[str]
    o: Optional[str]
    y: Optional[int]
    d: Set[str]


class Candidate(TypedDict):
    p: float
    denied: bool


class Listing(TypedDict):
    cc: str
    v: str
    r: str
    f: Optional[int]
    b: Optional[str]
    rules: List[str]
    year: Optional[int]
    dirs: Set[str]
    rt: Optional[int]
    o: Optional[str]
    s0: Optional[int]
    s1: Optional[int]
    shows: int
    backed: Optional[str]
    bare: bool
    strict_bare: bool
    keys: Set[str]
    cands: Dict[int, Candidate]

DAY = 86400000
SLACK = 3 * DAY  # "the same week": screening windows overlapping within three days
CAND = re.compile(r"^(\d+) ([\d.]+)% rank (\S+) (.*?)'(.*)'(?: \((\d{4})\))?\s*$")
YEAR_IN_TITLE = re.compile(r"\((1[89]\d\d|20\d\d)\)")

# The trace rules that mark a take as a stage relay, a broadcast season or a screening event rather than a film: the
# title rules that peel a relay/event banner and the acceptance rules of the season/stage/house records. Two countries
# billing "Swan Lake" from two houses take two records, rightly; the report skips a title only when EVERY take of it
# carries one of these.
EVENT_RULES = frozenset({
    "accept:season-production", "accept:season-record", "accept:stage-production", "accept:relay-record", "accept:relay-director",
    "accept:house-production",
    "title:xtra-canonical-broadcast-season", "title:xtra-canonical-met-opera", "title:xtra-canonical-met-opera-suffix",
    "title:xtra-canonical-royal-ballet-opera", "title:xtra-canonical-nt-live", "title:xtra-exhibition-on-screen",
    "title:search-plus-event-suffix", "title:search-with-event-suffix",
})


def fold(s: Optional[str]) -> str:
    s = unicodedata.normalize("NFD", s or "").replace("ł", "l").replace("Ł", "L")
    return "".join(ch for ch in s if not unicodedata.combining(ch)).lower()


def title_key(s: Optional[str]) -> str:
    """Normalised original title: brackets, format tokens, punctuation and a leading article dropped."""
    s = fold(s)
    s = re.sub(r"\([^)]*\)|\[[^\]]*\]", " ", s)
    s = re.sub(r"\b(omu|omeu|ov|ome|2d|3d|imax|4dx|dolby|atmos|vose|vo|napisy|dubbing)\b", " ", s)
    s = s.replace("&", " and ")
    s = re.sub(r"[^\w]+", " ", s).strip()
    s = re.sub(r"^(the|a|an|der|die|das|el|la|los|las|le|les) ", "", s)
    return s


def person(s: Optional[str]) -> str:
    return re.sub(r"[^a-z]", "", fold(s))


def jsonl(path: str) -> Iterator[Json]:
    with open(path, encoding="utf-8") as f:
        for line in f:
            if line.strip():
                yield json.loads(line)


def _film_record(f: Json) -> Optional[Tuple[int, Film]]:
    """{t, o, y, d} of one tmdb_films row, or None where its id is no number."""
    try:
        fid = int(f["id"])
    except (TypeError, ValueError):
        return None
    y = f.get("y")
    return fid, Film(t=f.get("t"), o=f.get("o"), y=int(y) if y and str(y).isdigit() else None,
                     d={person(x) for x in (f.get("d") or [])} - {""})


def read_films(data: str, countries: Iterable[str]) -> Dict[int, Film]:
    """{tmdb id: {t, o, y, d}} over every country's tmdb_films export."""
    records = (_film_record(f) for cc in countries for f in jsonl(os.path.join(data, f"films-{cc}.jsonl")))
    return dict(r for r in records if r)


def candidates_of(trace: Json) -> Dict[int, Candidate]:
    out: Dict[int, Candidate] = {}
    for c in trace.get("c") or []:
        m = CAND.match(c)
        if m:
            out[int(m.group(1))] = Candidate(p=float(m.group(2)) / 100, denied="DENIED" in m.group(4))
    return out


def _first(rows: List[Json], key: str) -> Any:
    """The first truthy `key` of `rows`, else None."""
    return next((f[key] for f in rows if f.get(key)), None)


def _own_year(rows: List[Json], raw: Optional[str]) -> Optional[int]:
    """The year the billing's own facts state, else the one in its raw title's brackets."""
    year = _first(rows, "y")
    m = YEAR_IN_TITLE.search(raw or "")
    return int(m.group(1)) if year is None and m else year


def _backed(rec: Optional[Film], directors: Set[str], own_year: Optional[int]) -> Optional[str]:
    """`director` / `year` where a fact of the billing agrees with the taken record, else None."""
    if not rec:
        return None
    if directors and rec["d"] and directors & rec["d"]:
        return "director"
    if own_year and rec["y"] and abs(own_year - rec["y"]) <= 1:
        return "year"
    return None


def _window(rows: List[Json], key: str, extreme: Any) -> Optional[int]:
    """The earliest (`min`) or latest (`max`) showtime `key` of the rows, else None."""
    return extreme((f[key] for f in rows if f.get(key)), default=None)


def _showtimes(rows: List[Json]) -> int:
    return sum(f.get("n") or 0 for f in rows)


def _title_keys(original: Optional[str], raw: str) -> Set[str]:
    """The non-empty title keys of the billing's original title and raw title."""
    return {k for k in {title_key(original), title_key(raw)} if k}


def _listing(cc: str, t: Json, rows: List[Json], films: Dict[int, Film]) -> Listing:
    """The facts of one trace `t` and the listing rows (`rows`) of its billing."""
    own_year = _own_year(rows, t["r"])
    directors = {person(d) for f in rows for d in f.get("d") or []} - {""}
    runtime = _first(rows, "rt")
    original = _first(rows, "o")
    film = t.get("f")
    bare = own_year is None and not directors
    return Listing(cc=cc, v=t["v"], r=t["r"], f=film, b=t.get("b"), rules=t.get("ru") or [],
                   year=own_year, dirs=directors, rt=runtime, o=original, s0=_window(rows, "s0", min),
                   s1=_window(rows, "s1", max), shows=_showtimes(rows),
                   backed=_backed(films.get(film) if film else None, directors, own_year),
                   bare=bare, strict_bare=bare and not runtime,
                   keys=_title_keys(original, t["r"]),
                   cands=candidates_of(t))


def read_listings(data: str, countries: Iterable[str], films: Dict[int, Film]) -> List[Listing]:
    """One dict per trace, with the facts its billing states: cc, v (venue), r (raw title), f (taken film), b (basis),
    rules, year, dirs, rt, o, s0, s1, shows (showtimes), backed (a fact of its own agreeing with the taken record:
    director / year / None), bare, strict_bare, keys (its title keys), cands."""
    listings: List[Listing] = []
    for cc in countries:
        facts: Dict[Tuple[str, str], List[Json]] = collections.defaultdict(list)
        for row in jsonl(os.path.join(data, f"listings-{cc}.jsonl")):
            facts[(row["v"], row["t"])].append(row)
        listings += [_listing(cc, t, facts.get((t["v"], t["r"]), []), films)
                     for t in jsonl(os.path.join(data, f"traces-{cc}.jsonl"))]
    return listings


def index_by_title(listings: Iterable[Listing]) -> Dict[str, List[Listing]]:
    """{title key: [listing]}: a listing under its raw title's key and its own original title's."""
    index: Dict[str, List[Listing]] = collections.defaultdict(list)
    for l in listings:
        for k in l["keys"]:
            index[k].append(l)
    return index


def overlap(a: Listing, b: Listing) -> bool:
    """Whether two listings screen in the same week (windows overlapping within SLACK)."""
    a0, a1, b0, b1 = a["s0"], a["s1"], b["s0"], b["s1"]
    if a0 is None or a1 is None or b0 is None or b1 is None:
        return False
    return a0 <= b1 + SLACK and b0 <= a1 + SLACK


def same_week_across_border(ls: List[Listing]) -> List[Listing]:
    """The listings of one title key that screen in the same week as a listing of ANOTHER country."""
    return [l for l in ls if any(m["cc"] != l["cc"] and overlap(l, m) for m in ls)]


def _taken_by_country(linked: Iterable[Listing]) -> Dict[str, Set[int]]:
    """{cc: the films it takes} of the linked listings."""
    taken: Dict[str, Set[int]] = collections.defaultdict(set)
    for l in linked:
        if l["f"]:
            taken[l["cc"]].add(l["f"])
    return taken


def disagreeing(index: Dict[str, List[Listing]]) -> List[Tuple[str, List[Listing]]]:
    """[(title key, linked listings)] of the keys billed in >=2 countries in the same week whose countries take
    different films (at least two countries taking one)."""
    out: List[Tuple[str, List[Listing]]] = []
    for k, ls in index.items():
        if len({l["cc"] for l in ls}) < 2:
            continue
        linked = same_week_across_border(ls)
        taken = _taken_by_country(linked)
        if len(taken) >= 2 and len(set().union(*taken.values())) >= 2:
            out.append((k, linked))
    return out
