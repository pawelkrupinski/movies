#!/usr/bin/env python3
"""TWO RECORDINGS COMPARED: did the input change, or the code? (scripts/recording-diff)

Reads two "Record scrape fixtures" recordings laid out as the repository's own tree (`corpus/cinema-scrapes-<cc>.json.gz`
and, when given, `enrichment-<cc>/`) and prints, per country and venue, what the DATA did between them:

  * listings added and lost, and RE-SPELLED ones — a lost and an added listing at one venue whose titles differ only by
    whitespace, case, accents, punctuation or truncation, each named (a resolver keyed on the spelling sees a new title),
    and RE-TITLED ones — the very same showtimes under another title (a feed's film name replacing the venue's own);
  * field shape changes on the listings present in both — synopsis, runtime, year, director, cast, original title and
    each feed id (`externalIds`) lost, gained or changed;
  * venues whose detail pages vanished (or appeared): most listings had a synopsis, runtime, director or cast, now
    almost none do;
  * origins whose refusal share moved, from the enrichment trees (scripts/ci/recording_health.py's manifest);
  * new (and gone) spellings of known title markers — sneak, preview, pokaz, double bill, certificates;
  * with --title, each matching listing: billed alike in both, re-spelled, or in one run only, and the fields that moved
    value by value — "same spelling, same fields" means the input did not move, so a moved take is the code's.

    recording_diff.py <cc> <dirA> <dirB> [--since-a S --since-b S] [--venue SUBSTRING] [--title SUBSTRING] [--limit N]
                      [--health-cache DIR] [--label-a NAME --label-b NAME]

<dirX> holds `corpus/` and optionally `enrichment-<cc>/`; --since-X is that run's start (epoch seconds or ISO-8601), which
the tree's per-origin manifest needs. Tests: recording_diff_test.py beside it.
"""
import argparse
import gzip
import json
import os
import re
import sys
import unicodedata
from collections import Counter, defaultdict

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "ci"))
import recording_health  # noqa: E402  (scripts/ci/recording_health.py)

TRUNCATION_MIN = 20          # a shorter title below this many letters is another title, not a truncation of the longer
DETAIL_BEFORE, DETAIL_AFTER, DETAIL_MIN = 0.5, 0.1, 3
ORIGIN_SHARE_MOVE = 0.2      # a refusal share that moved this much between the runs is reported
FIELD_MOVE_MIN = 2           # a venue's field change needs this many listings to be printed

# Known title markers: each is a family of spellings the title rules have to read. `follow`: the marker word stands
# alone and its qualifier is the next word ("Sneak Over", "Pokaz przyjazny"), so the spelling takes the next word too.
MARKERS = [
    ("sneak", re.compile(r"sneak", re.I), True),
    ("preview", re.compile(r"preview|vorpremiere|przedpremier|avant-?premi", re.I), True),
    ("pokaz", re.compile(r"pokaz", re.I), True),
    ("double bill", re.compile(r"double[\s_-]*(?:bill|feature)|doppel(?:vorstellung|programm)|podw[oó]jn\w*", re.I), False),
    ("certificate", re.compile(r"[(\[](?:U|PG|12A?|15|18|R18|PG-13|NC-17|R|FSK ?\d{1,2})[)\]]", re.I), False),
]


# ---------------------------------------------------------------------------------------------------------------------
# Re-spellings

def _ws(s):
    return re.sub(r"\s+", " ", s).strip()


def _case(s):
    return s.casefold()


def _accents(s):
    return "".join(c for c in unicodedata.normalize("NFKD", s) if not unicodedata.combining(c))


def _punct(s):
    # a mark at either end leaves no space there: only one inside the title is a word break
    return re.sub(r"[^\w\s]|_", " ", s).strip()


# applied in this order, so the whitespace a removed punctuation mark leaves is collapsed
_TRANSFORMS = [("case", _case), ("accents", _accents), ("punctuation", _punct), ("whitespace", _ws)]
_ORDER = ["whitespace", "case", "accents", "punctuation"]


def _canonical(s, skip=None):
    for name, transform in _TRANSFORMS:
        if name != skip:
            s = transform(s)
    return s


def _related(a, b, truncation):
    if a == b:
        return True
    if not truncation:
        return False
    short, long_ = sorted((a, b), key=len)
    return len(short.replace(" ", "")) >= TRUNCATION_MIN and long_.startswith(short)


def respelling(a, b):
    """How title `b` re-spells title `a`: the kinds of difference (whitespace, case, accents, punctuation, truncation),
    each one needed — without it the two are not the same title; None when they are two titles."""
    if a == b:
        return None
    ca, cb = _canonical(a), _canonical(b)
    truncation = ca != cb
    if not _related(ca, cb, truncation):
        return None
    kinds = [kind for kind in _ORDER if not _related(_canonical(a, kind), _canonical(b, kind), truncation)]
    return kinds + (["truncation"] if truncation else [])


# ---------------------------------------------------------------------------------------------------------------------
# The corpus

def load_corpus(corpus_dir, cc):
    with gzip.open(os.path.join(corpus_dir, f"cinema-scrapes-{cc}.json.gz"), "rt", encoding="utf-8") as handle:
        return json.load(handle)


def listing_title(entry):
    """The listing as the identity tools name it (`venue|raw title`): the scraper's raw title when it kept one."""
    movie = entry["movie"]
    return movie.get("rawTitle") or movie["title"]


def _venues(corpus):
    """venue → {raw title → the first scrape entry billed so}."""
    out = {}
    for venue in corpus:
        films = out.setdefault(venue["_id"], {})
        for entry in venue.get("films", []):
            films.setdefault(listing_title(entry), entry)
    return out


def _synopsis(entry):
    return entry.get("synopsis") or entry.get("synopsisExcerpt")


FIELDS = {
    "synopsis": _synopsis,
    "runtime": lambda e: e["movie"].get("runtimeMinutes"),
    "year": lambda e: e["movie"].get("releaseYear"),
    "director": lambda e: tuple(e.get("director") or ()) or None,
    "cast": lambda e: tuple(e.get("cast") or ()) or None,
    "originalTitle": lambda e: e["movie"].get("originalTitle"),
}
DETAIL_FIELDS = ("synopsis", "runtime", "director", "cast")


def _field_values(entry):
    values = {name: read(entry) for name, read in FIELDS.items()}
    for feed, value in (entry.get("externalIds") or {}).items():
        values[f"feed:{feed}"] = value
    return values


def _same(field, a, b):
    if field == "synopsis":   # a re-worded synopsis is no shape change; one cut to an excerpt (or grown back) is
        return 0.5 <= len(a) / max(len(b), 1) <= 2
    return a == b


def _field_changes(pairs):
    """{field: {lost, gained, changed}} over (before, after) entries, the fields that moved only."""
    out = defaultdict(lambda: {"lost": 0, "gained": 0, "changed": 0})
    for before, after in pairs:
        va, vb = _field_values(before), _field_values(after)
        for field in set(va) | set(vb):
            a, b = va.get(field), vb.get(field)
            if a and not b:
                out[field]["lost"] += 1
            elif b and not a:
                out[field]["gained"] += 1
            elif a and b and not _same(field, a, b):
                out[field]["changed"] += 1
    return dict(out)


def _detail_share(films):
    if not films:
        return 0.0
    return sum(1 for e in films.values() if any(FIELDS[f](e) for f in DETAIL_FIELDS)) / len(films)


def _spellings(title, marker, follow):
    """The marker's spellings in a title: the word holding the match, and its next word when the marker stands alone."""
    words = title.split()
    found = []
    for i, word in enumerate(words):
        match = marker.search(word)
        if not match:
            continue
        spelling = word
        if follow and match.group(0) == word and i + 1 < len(words):
            spelling = f"{word} {words[i + 1]}"
        found.append(spelling.casefold())
    if not found:   # a marker spanning words ("Double Bill"): the words it spans
        for match in marker.finditer(title):
            start = title.rfind(" ", 0, match.start()) + 1
            end = title.find(" ", match.end())
            found.append(title[start:end if end >= 0 else len(title)].casefold())
    return found


def _marker_spellings(venues):
    """(marker, spelling) → [(venue, title)...]"""
    out = defaultdict(list)
    for venue, films in venues.items():
        for title in films:
            for name, marker, follow in MARKERS:
                for spelling in _spellings(title, marker, follow):
                    out[(name, spelling)].append((venue, title))
    return out


def _showtimes(entry):
    return frozenset(s["dateTime"] for s in entry.get("showtimes") or ())


def pair_listings(fa, fb, lost, added):
    """A venue's lost and added titles paired into the same listings: RE-SPELLED (`respelling`), then RE-TITLED — the
    very same showtimes under another title (a feed's film name replacing the venue's, a renamed event). Returns
    (respelled [(old, new, kinds)], retitled [(old, new)], lost, added), the last two what stayed unpaired, sorted."""
    lost, added = sorted(lost), sorted(added)
    respelled, retitled = [], []
    for old in list(lost):
        for new in added:
            kinds = respelling(old, new)
            if kinds:
                respelled.append((old, new, kinds))
                lost.remove(old)
                added.remove(new)
                break
    for old in list(lost):
        for new in added:
            if _showtimes(fa[old]) and _showtimes(fa[old]) == _showtimes(fb[new]):
                retitled.append((old, new))
                lost.remove(old)
                added.remove(new)
                break
    return respelled, retitled, lost, added


def compare_corpus(a, b, venue_filter=None):
    """What the scrape data did between corpus `a` and corpus `b` (lists of venue documents)."""
    va, vb = _venues(a), _venues(b)
    keep = (lambda v: _fold(venue_filter) in _fold(v)) if venue_filter else (lambda v: True)
    venues = {}
    detail_vanished, detail_appeared = [], []
    for venue in sorted((set(va) & set(vb))):
        if not keep(venue):
            continue
        fa, fb = va[venue], vb[venue]
        respelled, retitled, lost, added = pair_listings(fa, fb, set(fa) - set(fb), set(fb) - set(fa))
        pairs = ([(fa[t], fb[t]) for t in set(fa) & set(fb)] + [(fa[o], fb[n]) for o, n, _ in respelled]
                 + [(fa[o], fb[n]) for o, n in retitled])
        venues[venue] = {"added": added, "lost": lost, "respelled": respelled, "retitled": retitled,
                         "fields": _field_changes(pairs), "listings": (len(fa), len(fb))}
        before, after = _detail_share(fa), _detail_share(fb)
        venues[venue]["detail"] = (before, after)
        if len(fb) >= DETAIL_MIN and before >= DETAIL_BEFORE and after <= DETAIL_AFTER:
            detail_vanished.append((venue, round(before, 2), round(after, 2), len(fb)))
        if len(fb) >= DETAIL_MIN and before <= DETAIL_AFTER and after >= DETAIL_BEFORE:
            detail_appeared.append((venue, round(before, 2), round(after, 2), len(fb)))
    sa = _marker_spellings({v: f for v, f in va.items() if keep(v)})
    sb = _marker_spellings({v: f for v, f in vb.items() if keep(v)})
    return {
        "venues": venues,
        "venues_added": sorted(v for v in set(vb) - set(va) if keep(v)),
        "venues_lost": sorted(v for v in set(va) - set(vb) if keep(v)),
        "detail_vanished": detail_vanished,
        "detail_appeared": detail_appeared,
        "markers": {
            "new": [(m, s, sb[(m, s)][:3]) for m, s in sorted(set(sb) - set(sa))],
            "gone": [(m, s, sa[(m, s)][:3]) for m, s in sorted(set(sa) - set(sb))],
        },
    }


def _fold(s):
    return _accents(s).casefold()


# ---------------------------------------------------------------------------------------------------------------------
# The trees

def compare_origins(a, b):
    """Origins whose refusal share moved between manifests `a` and `b` (recording_health.manifest), or that b flags."""
    flags = {f.origin: f.reason for f in recording_health.judge(b, a)}
    out = []
    for origin in sorted(set(a["origins"]) | set(b["origins"])):
        ca = a["origins"].get(origin, recording_health._origin_counts())
        cb = b["origins"].get(origin, recording_health._origin_counts())
        share_a = ca["refused"] / recording_health.asked(ca) if recording_health.asked(ca) else 0
        share_b = cb["refused"] / recording_health.asked(cb) if recording_health.asked(cb) else 0
        enough = max(recording_health.asked(ca), recording_health.asked(cb)) >= recording_health.MIN_REQUESTS
        if origin in flags:
            verdict = flags[origin]
        elif enough and abs(share_b - share_a) >= ORIGIN_SHARE_MOVE:
            verdict = "refusing more" if share_b > share_a else "recovered"
        else:
            continue
        out.append({"origin": origin, "verdict": verdict, "before": recording_health._share(ca),
                    "after": recording_health._share(cb), "tree": (ca["tree"], cb["tree"]),
                    "refusals": ", ".join(f"{k}×{n}" for k, n in sorted(cb["refusals"].items(), key=lambda kv: -kv[1]))})
    return sorted(out, key=lambda m: (m["verdict"] not in ("refusing", "gone"), m["origin"]))


def health(tree, since, cache=None):
    """The tree's per-origin manifest, kept in `cache` (a JSON path) once computed: a tree is never re-recorded."""
    if cache and os.path.isfile(cache):
        with open(cache) as handle:
            return json.load(handle)
    manifest = recording_health.manifest(tree, recording_health._since(str(since)))
    if cache:
        os.makedirs(os.path.dirname(cache), exist_ok=True)
        with open(cache + ".part", "w") as handle:
            json.dump(manifest, handle)
        os.replace(cache + ".part", cache)
    return manifest


# ---------------------------------------------------------------------------------------------------------------------
# The report

def _quote(s):
    return f"«{s}»"


def _value(field, v):
    if v is None:
        return "—"
    if field == "synopsis":
        return f"{len(v)} chars"
    return ", ".join(v) if isinstance(v, tuple) else str(v)


def listing_view(a, b, venue_filter, title_filter, labels):
    """Every listing whose title holds `title_filter` (case and accents aside), at the venues matching `venue_filter`:
    billed alike in both, re-spelled, or in one run only, and each field that moved, value by value."""
    va, vb = _venues(a), _venues(b)
    wanted = _fold(title_filter)
    lines = []
    for venue in sorted(set(va) | set(vb)):
        if venue_filter and _fold(venue_filter) not in _fold(venue):
            continue
        fa, fb = va.get(venue, {}), vb.get(venue, {})
        ta = sorted(t for t in fa if wanted in _fold(t))
        tb = sorted(t for t in fb if wanted in _fold(t))
        if not ta and not tb:
            continue
        lines.append(f"-- {venue}" + ("" if venue in va else f" (not in {labels[0]})") + ("" if venue in vb else f" (not in {labels[1]})"))
        # paired against every listing of the venue: a title the filter matches in one run only can be re-titled
        respelled, retitled, only_a, only_b = pair_listings(fa, fb, set(fa) - set(fb), set(fb) - set(fa))
        mine = set(ta) | set(tb)
        pairs = ([(t, t, None) for t in ta if t in fb] + [p for p in respelled if p[0] in mine or p[1] in mine]
                 + [(o, n, "retitled") for o, n in retitled if o in mine or n in mine])
        only_a = [t for t in only_a if t in mine]
        only_b = [t for t in only_b if t in mine]
        for old, new, kinds in pairs:
            va_, vb_ = _field_values(fa[old]), _field_values(fb[new])
            moved = [f"{f} {_value(f, va_.get(f))} → {_value(f, vb_.get(f))}" for f in sorted(set(va_) | set(vb_))
                     if (va_.get(f) or vb_.get(f)) and not (va_.get(f) and vb_.get(f) and _same(f, va_[f], vb_[f]))]
            head = (f"  {_quote(old)}: in both, spelled alike" if kinds is None
                    else f"  {_quote(old)} → {_quote(new)}: RE-TITLED (same showtimes)" if kinds == "retitled"
                    else f"  {_quote(old)} → {_quote(new)}: RE-SPELLED ({', '.join(kinds)})")
            lines.append(head + (", fields alike" if not moved else "; " + "; ".join(moved)))
        lines.extend(f"  {_quote(t)}: only in {labels[0]}" for t in only_a)
        lines.extend(f"  {_quote(t)}: only in {labels[1]}" for t in only_b)
    return lines or [f"no listing billed with {_quote(title_filter)}" + (f" at {_quote(venue_filter)}" if venue_filter else "")]


def report(cc, a, b, origins_a, origins_b, venue_filter=None, limit=15, coverage=None, labels=("A", "B"),
           title_filter=None):
    """The text report for one country; `a`/`b` corpora, `origins_*` manifests (None: not compared). With
    `title_filter`, the matching listings first, one by one."""
    result = compare_corpus(a, b, venue_filter)
    venues = result["venues"]
    lines = [f"== {cc.upper()}  {labels[0]} → {labels[1]}" + (f"  (venues matching {_quote(venue_filter)})" if venue_filter else "")]
    if title_filter:
        lines.append(f"LISTINGS billed with {_quote(title_filter)}:")
        lines.extend(listing_view(a, b, venue_filter, title_filter, labels))
        lines.append("")

    n_added = sum(len(v["added"]) for v in venues.values())
    n_lost = sum(len(v["lost"]) for v in venues.values())
    respelled = [(venue, *r) for venue, v in venues.items() for r in v["respelled"]]
    retitled = [(venue, *r) for venue, v in venues.items() for r in v["retitled"]]
    kinds = Counter(k for *_, ks in respelled for k in ks)
    lines.append(f"listings: {n_added} added, {n_lost} lost, {len(respelled)} RE-SPELLED"
                 + (f" ({', '.join(f'{k} {n}' for k, n in kinds.most_common())})" if kinds else "")
                 + f", {len(retitled)} RE-TITLED (same showtimes) at {len(venues)} venues in both; venues added {len(result['venues_added'])}, lost {len(result['venues_lost'])}")
    if coverage:
        lines.append("prod coverage: " + ", ".join(f"{k} {coverage[0].get(k)}→{coverage[1].get(k)}"
                                                   for k in sorted(set(coverage[0]) | set(coverage[1]))
                                                   if k != "recordedAt" and coverage[0].get(k) != coverage[1].get(k)))

    verdicts = []
    if respelled:
        verdicts.append(f"{len(respelled)} re-spelled listings")
    if retitled:
        verdicts.append(f"{len(retitled)} re-titled listings")
    if result["detail_vanished"]:
        verdicts.append(f"detail pages vanished at {len(result['detail_vanished'])} venues")
    origins = compare_origins(origins_a, origins_b) if origins_a is not None and origins_b is not None else None
    if origins:
        verdicts.append(f"{len(origins)} origins' refusals moved")
    if result["markers"]["new"]:
        verdicts.append(f"{len(result['markers']['new'])} new marker spellings")
    lines.append("NEW DATA: " + "; ".join(verdicts) if verdicts else
                 "no re-spelling, vanished detail, origin or marker change: a moved take is more likely the CODE's")

    if respelled:
        lines.append("")
        lines.append("RE-SPELLED (same listing, another spelling):")
        groups = defaultdict(list)
        for venue, old, new, ks in respelled:
            groups[(old, new, tuple(ks))].append(venue)
        for (old, new, ks), at in sorted(groups.items(), key=lambda kv: -len(kv[1]))[:limit]:
            lines.append(f"  RE-SPELLED ({', '.join(ks)}) at {len(at)} venue(s) [{_names(at, 3)}]")
            lines.append(f"      {_quote(old)}")
            lines.append(f"    → {_quote(new)}")
        if len(groups) > limit:
            lines.append(f"  … {len(groups) - limit} more")

    if retitled:
        lines.append("")
        lines.append("RE-TITLED (the same showtimes under another title):")
        for venue, old, new in retitled[:limit]:
            lines.append(f"  {venue}: {_quote(old)} → {_quote(new)}")
        if len(retitled) > limit:
            lines.append(f"  … {len(retitled) - limit} more")

    if result["detail_vanished"] or result["detail_appeared"]:
        lines.append("")
        lines.append("DETAIL PAGES (share of listings with a synopsis, runtime, director or cast):")
        for venue, before, after, n in result["detail_vanished"][:limit]:
            lines.append(f"  VANISHED  {venue}: {before:.0%} → {after:.0%} of {n}")
        for venue, before, after, n in result["detail_appeared"][:limit]:
            lines.append(f"  appeared  {venue}: {before:.0%} → {after:.0%} of {n}")

    field_totals = defaultdict(lambda: {"lost": 0, "gained": 0, "changed": 0, "venues": []})
    for venue, v in venues.items():
        for field, counts in v["fields"].items():
            total = field_totals[field]
            for k in ("lost", "gained", "changed"):
                total[k] += counts[k]
            moved = counts["lost"] + counts["gained"] + counts["changed"]
            if moved >= FIELD_MOVE_MIN:
                total["venues"].append((moved, venue, counts))
    if field_totals:
        lines.append("")
        lines.append("FIELDS on listings in both (lost / gained / changed), the venues moving most:")
        for field, total in sorted(field_totals.items(), key=lambda kv: -(kv[1]["lost"] + kv[1]["gained"] + kv[1]["changed"])):
            top = sorted(total["venues"], key=lambda t: (-t[0], t[1]))[:3]
            where = "; ".join(f"{venue} {c['lost']}/{c['gained']}/{c['changed']}" for _, venue, c in top)
            lines.append(f"  {field:<16} {total['lost']:>5} / {total['gained']:>5} / {total['changed']:>5}" + (f"   {where}" if where else ""))

    if origins is None:
        lines.append("")
        lines.append("ORIGINS: not compared (no enrichment tree for one of the runs)")
    elif origins:
        lines.append("")
        lines.append("ORIGINS whose refusals moved (refused / asked, before → after; tree files):")
        for m in origins[:limit]:
            lines.append(f"  {m['verdict'].upper():<14} {m['origin']}: {m['before']} → {m['after']}; tree {m['tree'][0]} → {m['tree'][1]}"
                         + (f"; {m['refusals']}" if m["refusals"] else ""))
    else:
        lines.append("")
        lines.append("ORIGINS: no refusal share moved")

    for kind in ("new", "gone"):
        found = result["markers"][kind]
        if not found:
            continue
        lines.append("")
        lines.append(f"MARKER SPELLINGS {kind.upper()} in {labels[1] if kind == 'new' else labels[0]}:")
        for marker, spelling, examples in found[:limit * 2]:
            lines.append(f"  {marker:<12} {_quote(spelling)}  e.g. " + "; ".join(f"{v}: {_quote(t)}" for v, t in examples))
        if len(found) > limit * 2:
            lines.append(f"  … {len(found) - limit * 2} more")

    if result["venues_added"] or result["venues_lost"]:
        lines.append("")
        lines.append(f"VENUES added: {_names(result['venues_added'], limit)}")
        lines.append(f"VENUES lost:  {_names(result['venues_lost'], limit)}")

    if venue_filter:
        for venue, v in sorted(venues.items()):
            lines.append("")
            lines.append(f"-- {venue} ({v['listings'][0]} → {v['listings'][1]} listings; with detail "
                         f"{v['detail'][0]:.0%} → {v['detail'][1]:.0%})")
            lines.extend(f"  + {t}" for t in v["added"])
            lines.extend(f"  - {t}" for t in v["lost"])
            lines.extend(f"  ~ {_quote(o)} → {_quote(n)} ({', '.join(ks)})" for o, n, ks in v["respelled"])
            lines.extend(f"  ~ {_quote(o)} → {_quote(n)} (re-titled, same showtimes)" for o, n in v["retitled"])
            for field, c in sorted(v["fields"].items()):
                lines.append(f"  {field}: lost {c['lost']}, gained {c['gained']}, changed {c['changed']}")
    elif venues:
        busiest = sorted(venues.items(), key=lambda kv: -(len(kv[1]["added"]) + len(kv[1]["lost"]) + len(kv[1]["respelled"]) + len(kv[1]["retitled"])))
        busiest = [(v, d) for v, d in busiest if d["added"] or d["lost"] or d["respelled"] or d["retitled"]][:limit]
        if busiest:
            lines.append("")
            lines.append("VENUES moving most (added / lost / re-spelled / re-titled; --venue X lists them):")
            for venue, d in busiest:
                lines.append(f"  {venue}: {len(d['added'])} / {len(d['lost'])} / {len(d['respelled'])} / {len(d['retitled'])}")
    return "\n".join(lines) + "\n"


def _names(items, limit):
    items = list(items)
    return ", ".join(items[:limit]) + (f", … {len(items) - limit} more" if len(items) > limit else "") if items else "—"


def _coverage(directory, cc):
    path = os.path.join(directory, "corpus", f"prod-coverage-{cc}.json")
    if not os.path.isfile(path):
        return {}
    with open(path) as handle:
        return json.load(handle)


def main(argv):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("cc")
    parser.add_argument("dir_a")
    parser.add_argument("dir_b")
    parser.add_argument("--since-a")
    parser.add_argument("--since-b")
    parser.add_argument("--venue")
    parser.add_argument("--title")
    parser.add_argument("--limit", type=int, default=15)
    parser.add_argument("--health-cache", help="a directory keeping each tree's manifest (<label>-<cc>.json)")
    parser.add_argument("--label-a", default="A")
    parser.add_argument("--label-b", default="B")
    args = parser.parse_args(argv)

    corpora = [load_corpus(os.path.join(d, "corpus"), args.cc) for d in (args.dir_a, args.dir_b)]
    manifests = []
    for directory, since, label in ((args.dir_a, args.since_a, args.label_a), (args.dir_b, args.since_b, args.label_b)):
        tree = os.path.join(directory, f"enrichment-{args.cc}")
        if since and os.path.isdir(tree):
            cache = os.path.join(args.health_cache, f"{label}-{args.cc}.json") if args.health_cache else None
            manifests.append(health(tree, since, cache))
        else:
            manifests.append(None)
    coverage = (_coverage(args.dir_a, args.cc), _coverage(args.dir_b, args.cc))
    sys.stdout.write(report(args.cc, corpora[0], corpora[1], manifests[0], manifests[1], args.venue, args.limit,
                            coverage if any(coverage) else None, (args.label_a, args.label_b), args.title))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
