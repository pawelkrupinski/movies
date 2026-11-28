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
import json
import os
import sys
from collections import Counter, defaultdict
from typing import Any, Dict, List, Optional, Set, Tuple, Union

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "ci"))
sys.path.insert(0, HERE)
from recording_diff_compare import (  # noqa: E402
    compare_corpus as compare_corpus, compare_origins as compare_origins, field_values, fold, health, load_corpus as load_corpus,
    pair_listings, same_field, venues_of)
from title_respelling import respelling as respelling  # noqa: E402,F401  (its tests')

FIELD_MOVE_MIN = 2           # a venue's field change needs this many listings to be printed



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


def _venue_heading(venue: str, in_a: bool, in_b: bool, labels: "Tuple[str, str]") -> str:
    return f"-- {venue}" + ("" if in_a else f" (not in {labels[0]})") + ("" if in_b else f" (not in {labels[1]})")


def _pair_head(old: str, new: str, kinds: "Optional[Union[str, List[str]]]") -> str:
    if kinds is None:
        return f"  {_quote(old)}: in both, spelled alike"
    if kinds == "retitled":
        return f"  {_quote(old)} → {_quote(new)}: RE-TITLED (same showtimes)"
    return f"  {_quote(old)} → {_quote(new)}: RE-SPELLED ({', '.join(kinds)})"


def _pair_line(fa: "Dict[str, Any]", fb: "Dict[str, Any]", old: str, new: str, kinds: "Optional[Union[str, List[str]]]") -> str:
    """One listing in both runs: how it is billed and each field that moved, value by value."""
    va, vb = field_values(fa[old]), field_values(fb[new])
    moved = [f"{f} {_value(f, va.get(f))} → {_value(f, vb.get(f))}" for f in sorted(set(va) | set(vb))
             if (va.get(f) or vb.get(f)) and not (va.get(f) and vb.get(f) and same_field(f, va[f], vb[f]))]
    return _pair_head(old, new, kinds) + (", fields alike" if not moved else "; " + "; ".join(moved))


def _own_pairs(fb: "Dict[str, Any]", mine: "Set[str]", ta: "List[str]", respelled: "List[Any]",
               retitled: "List[Any]") -> "List[Tuple[str, str, Optional[Union[str, List[str]]]]]":
    """The listings of one venue in both runs that the filter names: alike, re-spelled, or re-titled."""
    return ([(t, t, None) for t in ta if t in fb] + [p for p in respelled if p[0] in mine or p[1] in mine]
            + [(o, n, "retitled") for o, n in retitled if o in mine or n in mine])


def _venue_listing_lines(fa: "Dict[str, Any]", fb: "Dict[str, Any]", wanted: str, labels: "Tuple[str, str]") -> "List[str]":
    """The listings of one venue whose title holds `wanted`: billed alike, re-spelled, re-titled or in one run only."""
    ta = sorted(t for t in fa if wanted in fold(t))
    mine = set(ta) | {t for t in fb if wanted in fold(t)}
    # paired against every listing of the venue: a title the filter matches in one run only can be re-titled
    respelled, retitled, only_a, only_b = pair_listings(fa, fb, set(fa) - set(fb), set(fb) - set(fa))
    lines = [_pair_line(fa, fb, old, new, kinds) for old, new, kinds in _own_pairs(fb, mine, ta, respelled, retitled)]
    lines += [f"  {_quote(t)}: only in {labels[0]}" for t in only_a if t in mine]
    return lines + [f"  {_quote(t)}: only in {labels[1]}" for t in only_b if t in mine]


def listing_view(a: "List[Dict[str, Any]]", b: "List[Dict[str, Any]]", venue_filter: "Optional[str]", title_filter: str,
                 labels: "Tuple[str, str]") -> "List[str]":
    """Every listing whose title holds `title_filter` (case and accents aside), at the venues matching `venue_filter`:
    billed alike in both, re-spelled, or in one run only, and each field that moved, value by value."""
    va, vb = venues_of(a), venues_of(b)
    lines: "List[str]" = []
    for venue in sorted(set(va) | set(vb)):
        if venue_filter and fold(venue_filter) not in fold(venue):
            continue
        found = _venue_listing_lines(va.get(venue, {}), vb.get(venue, {}), fold(title_filter), labels)
        if found:
            lines += [_venue_heading(venue, venue in va, venue in vb, labels)] + found
    return lines or [f"no listing billed with {_quote(title_filter)}" + (f" at {_quote(venue_filter)}" if venue_filter else "")]


def _summary_lines(result: "Dict[str, Any]", respelled: "List[Any]", retitled: "List[Any]") -> "List[str]":
    venues = result["venues"]
    kinds = Counter(k for *_, ks in respelled for k in ks)
    return [f"listings: {sum(len(v['added']) for v in venues.values())} added, {sum(len(v['lost']) for v in venues.values())} lost, "
            f"{len(respelled)} RE-SPELLED" + (f" ({', '.join(f'{k} {n}' for k, n in kinds.most_common())})" if kinds else "")
            + f", {len(retitled)} RE-TITLED (same showtimes) at {len(venues)} venues in both; venues added "
            f"{len(result['venues_added'])}, lost {len(result['venues_lost'])}"]


def _coverage_lines(coverage: "Optional[Tuple[Dict[str, Any], Dict[str, Any]]]") -> "List[str]":
    if not coverage:
        return []
    return ["prod coverage: " + ", ".join(f"{k} {coverage[0].get(k)}→{coverage[1].get(k)}"
                                          for k in sorted(set(coverage[0]) | set(coverage[1]))
                                          if k != "recordedAt" and coverage[0].get(k) != coverage[1].get(k))]


def _verdict_lines(result: "Dict[str, Any]", respelled: "List[Any]", retitled: "List[Any]", origins: "Optional[List[Any]]") -> "List[str]":
    moved = [(respelled, "{} re-spelled listings"), (retitled, "{} re-titled listings"),
             (result["detail_vanished"], "detail pages vanished at {} venues"), (origins, "{} origins' refusals moved"),
             (result["markers"]["new"], "{} new marker spellings")]
    verdicts = [text.format(len(found)) for found, text in moved if found]
    if verdicts:
        return ["NEW DATA: " + "; ".join(verdicts)]
    return ["no re-spelling, vanished detail, origin or marker change: a moved take is more likely the CODE's"]


def _respelled_lines(respelled: "List[Any]", limit: int) -> "List[str]":
    if not respelled:
        return []
    lines = ["", "RE-SPELLED (same listing, another spelling):"]
    groups: "Dict[Any, List[str]]" = defaultdict(list)
    for venue, old, new, ks in respelled:
        groups[(old, new, tuple(ks))].append(venue)
    for (old, new, ks), at in sorted(groups.items(), key=lambda kv: -len(kv[1]))[:limit]:
        lines += [f"  RE-SPELLED ({', '.join(ks)}) at {len(at)} venue(s) [{_names(at, 3)}]", f"      {_quote(old)}", f"    → {_quote(new)}"]
    return lines + ([f"  … {len(groups) - limit} more"] if len(groups) > limit else [])


def _retitled_lines(retitled: "List[Any]", limit: int) -> "List[str]":
    if not retitled:
        return []
    lines = ["", "RE-TITLED (the same showtimes under another title):"]
    lines += [f"  {venue}: {_quote(old)} → {_quote(new)}" for venue, old, new in retitled[:limit]]
    return lines + ([f"  … {len(retitled) - limit} more"] if len(retitled) > limit else [])


def _detail_lines(result: "Dict[str, Any]", limit: int) -> "List[str]":
    if not (result["detail_vanished"] or result["detail_appeared"]):
        return []
    lines = ["", "DETAIL PAGES (share of listings with a synopsis, runtime, director or cast):"]
    for key, word in (("detail_vanished", "VANISHED "), ("detail_appeared", "appeared ")):
        lines += [f"  {word} {venue}: {before:.0%} → {after:.0%} of {n}" for venue, before, after, n in result[key][:limit]]
    return lines


def _field_totals(venues: "Dict[str, Any]") -> "Dict[str, Dict[str, Any]]":
    totals: "Dict[str, Dict[str, Any]]" = defaultdict(lambda: {"lost": 0, "gained": 0, "changed": 0, "venues": []})
    for venue, v in venues.items():
        for field, counts in v["fields"].items():
            for k in ("lost", "gained", "changed"):
                totals[field][k] += counts[k]
            moved = counts["lost"] + counts["gained"] + counts["changed"]
            if moved >= FIELD_MOVE_MIN:
                totals[field]["venues"].append((moved, venue, counts))
    return totals


def _field_lines(venues: "Dict[str, Any]") -> "List[str]":
    totals = _field_totals(venues)
    if not totals:
        return []
    lines = ["", "FIELDS on listings in both (lost / gained / changed), the venues moving most:"]
    for field, total in sorted(totals.items(), key=lambda kv: -(kv[1]["lost"] + kv[1]["gained"] + kv[1]["changed"])):
        top = sorted(total["venues"], key=lambda t: (-t[0], t[1]))[:3]
        where = "; ".join(f"{venue} {c['lost']}/{c['gained']}/{c['changed']}" for _, venue, c in top)
        lines.append(f"  {field:<16} {total['lost']:>5} / {total['gained']:>5} / {total['changed']:>5}" + (f"   {where}" if where else ""))
    return lines


def _origin_lines(origins: "Optional[List[Dict[str, Any]]]", limit: int) -> "List[str]":
    if origins is None:
        return ["", "ORIGINS: not compared (no enrichment tree for one of the runs)"]
    if not origins:
        return ["", "ORIGINS: no refusal share moved"]
    lines = ["", "ORIGINS whose refusals moved (refused / asked, before → after; tree files):"]
    return lines + [f"  {m['verdict'].upper():<14} {m['origin']}: {m['before']} → {m['after']}; tree {m['tree'][0]} → {m['tree'][1]}"
                    + (f"; {m['refusals']}" if m["refusals"] else "") for m in origins[:limit]]


def _marker_lines(result: "Dict[str, Any]", labels: "Tuple[str, str]", limit: int) -> "List[str]":
    lines: "List[str]" = []
    for kind, label in (("new", labels[1]), ("gone", labels[0])):
        found = result["markers"][kind]
        if found:
            lines += ["", f"MARKER SPELLINGS {kind.upper()} in {label}:"]
            lines += [f"  {marker:<12} {_quote(spelling)}  e.g. " + "; ".join(f"{v}: {_quote(t)}" for v, t in examples)
                      for marker, spelling, examples in found[:limit * 2]]
            lines += [f"  … {len(found) - limit * 2} more"] if len(found) > limit * 2 else []
    return lines


def _venue_name_lines(result: "Dict[str, Any]", limit: int) -> "List[str]":
    if not (result["venues_added"] or result["venues_lost"]):
        return []
    return ["", f"VENUES added: {_names(result['venues_added'], limit)}", f"VENUES lost:  {_names(result['venues_lost'], limit)}"]


def _venue_detail_lines(venues: "Dict[str, Any]") -> "List[str]":
    """With a venue filter: each venue's listings added, lost, re-spelled, re-titled and its field counts."""
    lines: "List[str]" = []
    for venue, v in sorted(venues.items()):
        lines += ["", f"-- {venue} ({v['listings'][0]} → {v['listings'][1]} listings; with detail {v['detail'][0]:.0%} → {v['detail'][1]:.0%})"]
        lines += [f"  + {t}" for t in v["added"]] + [f"  - {t}" for t in v["lost"]]
        lines += [f"  ~ {_quote(o)} → {_quote(n)} ({', '.join(ks)})" for o, n, ks in v["respelled"]]
        lines += [f"  ~ {_quote(o)} → {_quote(n)} (re-titled, same showtimes)" for o, n in v["retitled"]]
        lines += [f"  {field}: lost {c['lost']}, gained {c['gained']}, changed {c['changed']}" for field, c in sorted(v["fields"].items())]
    return lines


def _busiest_lines(venues: "Dict[str, Any]", limit: int) -> "List[str]":
    moved = lambda d: len(d["added"]) + len(d["lost"]) + len(d["respelled"]) + len(d["retitled"])  # noqa: E731
    busiest = sorted(((v, d) for v, d in venues.items() if moved(d)), key=lambda kv: -moved(kv[1]))[:limit]
    if not busiest:
        return []
    lines = ["", "VENUES moving most (added / lost / re-spelled / re-titled; --venue X lists them):"]
    return lines + [f"  {venue}: {len(d['added'])} / {len(d['lost'])} / {len(d['respelled'])} / {len(d['retitled'])}" for venue, d in busiest]


def _title_view_lines(a: "List[Dict[str, Any]]", b: "List[Dict[str, Any]]", venue_filter: "Optional[str]",
                      title_filter: "Optional[str]", labels: "Tuple[str, str]") -> "List[str]":
    if not title_filter:
        return []
    return [f"LISTINGS billed with {_quote(title_filter)}:", *listing_view(a, b, venue_filter, title_filter, labels), ""]


def report(cc: str, a: "List[Dict[str, Any]]", b: "List[Dict[str, Any]]", origins_a: "Optional[Dict[str, Any]]",
           origins_b: "Optional[Dict[str, Any]]", venue_filter: "Optional[str]" = None, limit: int = 15,
           coverage: "Optional[Tuple[Dict[str, Any], Dict[str, Any]]]" = None, labels: "Tuple[str, str]" = ("A", "B"),
           title_filter: "Optional[str]" = None) -> str:
    """The text report for one country; `a`/`b` corpora, `origins_*` manifests (None: not compared). With
    `title_filter`, the matching listings first, one by one."""
    result = compare_corpus(a, b, venue_filter)
    venues = result["venues"]
    lines = [f"== {cc.upper()}  {labels[0]} → {labels[1]}" + (f"  (venues matching {_quote(venue_filter)})" if venue_filter else "")]
    lines += _title_view_lines(a, b, venue_filter, title_filter, labels)
    respelled = [(venue, *r) for venue, v in venues.items() for r in v["respelled"]]
    retitled = [(venue, *r) for venue, v in venues.items() for r in v["retitled"]]
    origins = None if origins_a is None or origins_b is None else compare_origins(origins_a, origins_b)
    lines += _summary_lines(result, respelled, retitled) + _coverage_lines(coverage)
    lines += _verdict_lines(result, respelled, retitled, origins)
    lines += _respelled_lines(respelled, limit) + _retitled_lines(retitled, limit) + _detail_lines(result, limit)
    lines += _field_lines(venues) + _origin_lines(origins, limit) + _marker_lines(result, labels, limit)
    lines += _venue_name_lines(result, limit)
    lines += _venue_detail_lines(venues) if venue_filter else _busiest_lines(venues, limit)
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


DOCSTRING = __doc__ or ""  # None under python -OO


def main(argv):
    parser = argparse.ArgumentParser(description=DOCSTRING.split("\n")[0])
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
