"""WHAT THE SCRAPE DATA DID BETWEEN TWO RECORDINGS (scripts/recording_diff.py, which prints it): each venue's listings paired
(re-spelled, re-titled), the fields that moved, the venues whose detail vanished, the title markers, and the origins whose
refusal share moved. Imported by recording_diff.py, which has put scripts/ and scripts/ci on sys.path."""
from __future__ import annotations

import gzip
import json
import os
from collections import defaultdict
from typing import Any, Callable, Dict, FrozenSet, List, Optional, Set, Tuple, cast

import recording_health  # noqa: E402  (scripts/ci/recording_health.py)
from recording_health import Manifest, OriginCounts  # noqa: E402
from title_markers import marker_spellings  # noqa: E402
from title_respelling import respelling, strip_accents  # noqa: E402

Entry = Dict[str, Any]      # one scrape entry of a venue's films
Films = Dict[str, Entry]    # a venue's raw title -> its first entry
Corpus = List[Dict[str, Any]]
DETAIL_BEFORE, DETAIL_AFTER, DETAIL_MIN = 0.5, 0.1, 3
ORIGIN_SHARE_MOVE = 0.2      # a refusal share that moved this much between the runs is reported


def load_corpus(corpus_dir: str, cc: str) -> Corpus:
    with gzip.open(os.path.join(corpus_dir, f"cinema-scrapes-{cc}.json.gz"), "rt", encoding="utf-8") as handle:
        return json.load(handle)


def listing_title(entry: Entry) -> str:
    """The listing as the identity tools name it (`venue|raw title`): the scraper's raw title when it kept one."""
    movie = entry["movie"]
    return movie.get("rawTitle") or movie["title"]


def venues_of(corpus: Corpus) -> Dict[str, Films]:
    """venue -> {raw title -> the first scrape entry billed so}."""
    out: Dict[str, Films] = {}
    for venue in corpus:
        films = out.setdefault(venue["_id"], {})
        for entry in venue.get("films", []):
            films.setdefault(listing_title(entry), entry)
    return out


def _synopsis(entry: Entry) -> Any:
    return entry.get("synopsis") or entry.get("synopsisExcerpt")


FIELDS: Dict[str, Callable[[Entry], Any]] = {
    "synopsis": _synopsis,
    "runtime": lambda e: e["movie"].get("runtimeMinutes"),
    "year": lambda e: e["movie"].get("releaseYear"),
    "director": lambda e: tuple(e.get("director") or ()) or None,
    "cast": lambda e: tuple(e.get("cast") or ()) or None,
    "originalTitle": lambda e: e["movie"].get("originalTitle"),
}
DETAIL_FIELDS = ("synopsis", "runtime", "director", "cast")


def field_values(entry: Entry) -> Dict[str, Any]:
    values = {name: read(entry) for name, read in FIELDS.items()}
    for feed, value in (entry.get("externalIds") or {}).items():
        values[f"feed:{feed}"] = value
    return values


def same_field(field: str, a: Any, b: Any) -> bool:
    if field == "synopsis":   # a re-worded synopsis is no shape change; one cut to an excerpt (or grown back) is
        return 0.5 <= len(a) / max(len(b), 1) <= 2
    return bool(a == b)


def _field_move(field: str, a: Any, b: Any) -> Optional[str]:
    """How a field moved between a listing's two entries -- lost, gained or changed -- or None when it did not."""
    if a and not b:
        return "lost"
    if b and not a:
        return "gained"
    return "changed" if a and b and not same_field(field, a, b) else None


def _field_changes(pairs: List[Tuple[Entry, Entry]]) -> Dict[str, Dict[str, int]]:
    """{field: {lost, gained, changed}} over (before, after) entries, the fields that moved only."""
    out: Dict[str, Dict[str, int]] = defaultdict(lambda: {"lost": 0, "gained": 0, "changed": 0})
    for before, after in pairs:
        va, vb = field_values(before), field_values(after)
        for field in set(va) | set(vb):
            move = _field_move(field, va.get(field), vb.get(field))
            if move:
                out[field][move] += 1
    return dict(out)


def _detail_share(films: Films) -> float:
    if not films:
        return 0.0
    return sum(1 for e in films.values() if any(FIELDS[f](e) for f in DETAIL_FIELDS)) / len(films)


def _showtimes(entry: Entry) -> FrozenSet[str]:
    return frozenset(s["dateTime"] for s in entry.get("showtimes") or ())


def _pair_first(lost: List[str], added: List[str], pairs: Callable[[str, str], Any]) -> List[Tuple[str, str, Any]]:
    """Each lost title paired with the first added one `pairs` accepts (its truthy answer kept), both taken out of the lists."""
    found = []
    for old in list(lost):
        for new in added:
            answer = pairs(old, new)
            if answer:
                found.append((old, new, answer))
                lost.remove(old)
                added.remove(new)
                break
    return found


def pair_listings(fa: Films, fb: Films, lost: Set[str], added: Set[str]) -> Tuple[List[Tuple[str, str, Any]], List[Tuple[str, str]], List[str], List[str]]:
    """A venue's lost and added titles paired into the same listings: RE-SPELLED (`respelling`), then RE-TITLED -- the
    very same showtimes under another title (a feed's film name replacing the venue's, a renamed event). Returns
    (respelled [(old, new, kinds)], retitled [(old, new)], lost, added), the last two what stayed unpaired, sorted."""
    gone, new_ones = sorted(lost), sorted(added)
    respelled = _pair_first(gone, new_ones, respelling)
    retitled = [(old, new) for old, new, _ in
                _pair_first(gone, new_ones, lambda old, new: bool(_showtimes(fa[old])) and _showtimes(fa[old]) == _showtimes(fb[new]))]
    return respelled, retitled, gone, new_ones


Share = Tuple[str, float, float, int]


def _detail_move(fa: Films, fb: Films) -> Tuple[Tuple[float, float], Optional[str]]:
    """A venue's detail share before and after, and whether the detail `vanished` or `appeared` (enough listings, a swing)."""
    before, after = _detail_share(fa), _detail_share(fb)
    if len(fb) < DETAIL_MIN:
        return (before, after), None
    if before >= DETAIL_BEFORE and after <= DETAIL_AFTER:
        return (before, after), "vanished"
    return (before, after), "appeared" if before <= DETAIL_AFTER and after >= DETAIL_BEFORE else None


def _venue_comparison(fa: Films, fb: Films) -> Dict[str, Any]:
    respelled, retitled, lost, added = pair_listings(fa, fb, set(fa) - set(fb), set(fb) - set(fa))
    pairs = ([(fa[t], fb[t]) for t in set(fa) & set(fb)] + [(fa[o], fb[n]) for o, n, _ in respelled]
             + [(fa[o], fb[n]) for o, n in retitled])
    return {"added": added, "lost": lost, "respelled": respelled, "retitled": retitled,
            "fields": _field_changes(pairs), "listings": (len(fa), len(fb))}


def _marker_moves(va: Dict[str, Films], vb: Dict[str, Films], keep: Callable[[str], bool]) -> Dict[str, List[Tuple[str, str, List[Tuple[str, str]]]]]:
    """The title markers' spellings that appeared (`new`) or vanished (`gone`) between the corpora, three examples each."""
    sa = marker_spellings(cast(Dict[str, Dict[str, object]], {v: f for v, f in va.items() if keep(v)}))
    sb = marker_spellings(cast(Dict[str, Dict[str, object]], {v: f for v, f in vb.items() if keep(v)}))
    return {"new": [(m, s, sb[(m, s)][:3]) for m, s in sorted(set(sb) - set(sa))],
            "gone": [(m, s, sa[(m, s)][:3]) for m, s in sorted(set(sa) - set(sb))]}


def _kept(names: Set[str], keep: Callable[[str], bool]) -> List[str]:
    return sorted(v for v in names if keep(v))


def compare_corpus(a: Corpus, b: Corpus, venue_filter: Optional[str] = None) -> Dict[str, Any]:
    """What the scrape data did between corpus `a` and corpus `b` (lists of venue documents)."""
    va, vb = venues_of(a), venues_of(b)
    keep: Callable[[str], bool] = (lambda v: fold(venue_filter) in fold(v)) if venue_filter else (lambda v: True)
    venues: Dict[str, Dict[str, Any]] = {}
    detail: Dict[str, List[Share]] = {"vanished": [], "appeared": []}
    for venue in _kept(set(va) & set(vb), keep):
        fa, fb = va[venue], vb[venue]
        venues[venue] = _venue_comparison(fa, fb)
        venues[venue]["detail"], moved = _detail_move(fa, fb)
        if moved:
            before, after = venues[venue]["detail"]
            detail[moved].append((venue, round(before, 2), round(after, 2), len(fb)))
    return {"venues": venues, "venues_added": _kept(set(vb) - set(va), keep), "venues_lost": _kept(set(va) - set(vb), keep),
            "detail_vanished": detail["vanished"], "detail_appeared": detail["appeared"], "markers": _marker_moves(va, vb, keep)}


def fold(s: str) -> str:
    return strip_accents(s).casefold()


# ---------------------------------------------------------------------------------------------------------------------
# The trees

def _share(counts: OriginCounts) -> float:
    asked = recording_health.asked(counts)
    return counts["refused"] / asked if asked else 0


def _origin_verdict(origin: str, ca: OriginCounts, cb: OriginCounts, flags: Dict[str, str]) -> Optional[str]:
    """Why an origin is reported -- the check's own flag, or its refusal share moved enough on enough requests -- or None."""
    if origin in flags:
        return flags[origin]
    enough = max(recording_health.asked(ca), recording_health.asked(cb)) >= recording_health.MIN_REQUESTS
    if enough and abs(_share(cb) - _share(ca)) >= ORIGIN_SHARE_MOVE:
        return "refusing more" if _share(cb) > _share(ca) else "recovered"
    return None


def compare_origins(a: Manifest, b: Manifest) -> List[Dict[str, Any]]:
    """Origins whose refusal share moved between manifests `a` and `b` (recording_health.manifest), or that b flags."""
    flags = {f.origin: f.reason for f in recording_health.judge(b, a)}
    out = []
    for origin in sorted(set(a["origins"]) | set(b["origins"])):
        ca = a["origins"].get(origin, recording_health.origin_counts())
        cb = b["origins"].get(origin, recording_health.origin_counts())
        verdict = _origin_verdict(origin, ca, cb, flags)
        if verdict is None:
            continue
        out.append({"origin": origin, "verdict": verdict, "before": recording_health.share(ca),
                    "after": recording_health.share(cb), "tree": (ca["tree"], cb["tree"]),
                    "refusals": ", ".join(f"{k}×{n}" for k, n in sorted(cb["refusals"].items(), key=lambda kv: -kv[1]))})
    return sorted(out, key=lambda m: (m["verdict"] not in ("refusing", "gone"), m["origin"]))


def health(tree: str, since: Any, cache: Optional[str] = None) -> Manifest:
    """The tree's per-origin manifest, kept in `cache` (a JSON path) once computed: a tree is never re-recorded."""
    if cache and os.path.isfile(cache):
        with open(cache) as handle:
            return json.load(handle)
    manifest = recording_health.manifest(tree, recording_health.since(str(since)))
    if cache:
        os.makedirs(os.path.dirname(cache), exist_ok=True)
        with open(cache + ".part", "w") as handle:
            json.dump(manifest, handle)
        os.replace(cache + ".part", cache)
    return manifest
