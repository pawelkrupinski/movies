"""LABELS AND REPORTS of poster_eval: the judged and weak labels of each cluster, the vote's tallies per country and the
variant's differences against the baseline, and the coverage buckets (why a listing has no usable poster comparison)."""
from __future__ import annotations

import urllib.parse
from collections import Counter, defaultdict
from typing import Any, Callable, Dict, List, NamedTuple, Optional, Sequence, Set, Tuple

from poster_eval_common import Baseline, Countries, Meta, Variant
from identity_judgements import normalise  # noqa: E402  (after poster_eval_common, which puts scripts/ci on sys.path)
from poster_eval_vote import Result

Out = Callable[[str], None]
Judgement = Any  # an identity_judgements.Row: country, venue, rawTitle, film, verdict (that module is untyped)
Title = Tuple[str, str, str]  # (country, venue or "*", raw title), each normalised


class Label(NamedTuple):
    right: Set[int]  # films judged right
    wrong: Set[int]  # films judged wrong
    other: bool  # judged right as no TMDB film
    judged: bool  # judged at all


class Judged:
    def __init__(self) -> None:
        self.right: Set[int] = set()
        self.wrong: Set[int] = set()
        self.other = False


def _judgement_table(rows: Sequence[Judgement]) -> Dict[Title, Judged]:
    table: Dict[Title, Judged] = defaultdict(Judged)
    for r in rows:
        k = (r.country, normalise(r.venue), normalise(r.rawTitle))
        if r.verdict == "right" and not r.film.startswith("tmdb:"):
            table[k].other = True
        elif r.film.startswith("tmdb:") and r.verdict in ("right", "wrong"):
            (table[k].right if r.verdict == "right" else table[k].wrong).add(int(r.film[5:]))
    return table


def _cluster_label(c: Dict[str, Any], table: Dict[Title, Judged]) -> Label:
    right: Set[int] = set()
    wrong: Set[int] = set()
    other = False
    for venue, raw in c["keys"]:
        j = table.get((c["cc"], normalise(venue), normalise(raw))) or table.get((c["cc"], "*", normalise(raw)))
        if j:
            right |= j.right
            wrong |= j.wrong
            other = other or j.other
    wrong -= right
    return Label(right, wrong, other, bool(right or wrong or other))


def labels(meta: Meta, rows: Sequence[Judgement]) -> List[Label]:
    """Per cluster: (right films, wrong films, judged right as no TMDB film, judged at all)."""
    table = _judgement_table(rows)
    return [_cluster_label(c, table) for c in meta["clusters"]]


def judge(film: int, label: Label) -> Optional[str]:
    right, wrong, other, _ = label
    if film < 0:
        return None
    return "TP" if film in right else ("FP" if (film in wrong or right or other) else None)


def _tally_judged(t: "Counter[str]", taken: Optional[int], f: int, v: bool, lab: Label) -> None:
    j = judge(f, lab)
    t["tp"] += j == "TP"
    t["fp"] += j == "FP"
    if taken is None:
        return
    if taken in lab.right:
        t["right_takes"] += 1
        t["right_vetoed"] += v
    elif taken in lab.wrong or lab.right or lab.other:
        t["wrong_takes"] += 1
        t["wrong_vetoed"] += v


def _tally_weak(t: "Counter[str]", taken: int, f: int, v: bool) -> None:
    if f >= 0:
        t["agree"] += f == taken
        t["disagree"] += f != taken
    t["weak_takes"] += 1
    t["weak_vetoed"] += v


def tally(meta: Meta, labs: Sequence[Label], vote: Sequence[Any], take_vetoed: Sequence[Any], ccs: Set[str]) -> "Counter[str]":
    t: Counter[str] = Counter()
    for c, lab, f, v in zip(meta["clusters"], labs, vote, take_vetoed):
        if c["cc"] not in ccs:
            continue
        if lab.judged:
            _tally_judged(t, c["film"], int(f), bool(v), lab)
        elif c["film"] is not None:
            _tally_weak(t, c["film"], int(f), bool(v))
    return t


def describe(variant: Variant) -> str:
    return f"posters {variant['posters']} vote<={variant['vote']} veto-match<={variant['veto_match']} veto>{variant['veto']}"


def fmt(film: int) -> str:
    return "none" if film < 0 else str(film)


Header = (f"{'':4} {'':8} {'clusters':>8} {'judged':>6} | {'vote TP':>7} {'FP':>4} {'prec':>6} | {'weak agree':>10} {'dis':>4} "
          f"{'prec':>6} | {'veto: right-takes vetoed':>24} {'wrong-takes vetoed':>18} {'weak vetoed':>11}")


def _country_rows(out: Out, meta: Meta, labs: Sequence[Label], results: Dict[str, Result], cc: str, ccs: Set[str]) -> None:
    n = sum(c["cc"] in ccs for c in meta["clusters"])
    judged = sum(lab.judged for c, lab in zip(meta["clusters"], labs) if c["cc"] in ccs)
    for name in ["baseline"] + (["variant"] if "variant" in results else []):
        t = tally(meta, labs, *results[name][:2], ccs)
        out(f"{cc:4} {name:8} {n:8} {judged:6} | {t['tp']:7} {t['fp']:4} {t['tp'] / max(1, t['tp'] + t['fp']):6.3f} | "
            f"{t['agree']:10} {t['disagree']:4} {t['agree'] / max(1, t['agree'] + t['disagree']):6.3f} | "
            f"{t['right_vetoed']:>12}/{t['right_takes']:<11} {t['wrong_vetoed']:>9}/{t['wrong_takes']:<8} {t['weak_vetoed']:>5}/{t['weak_takes']:<5}")


def _judged_differences(where: str, c: Dict[str, Any], lab: Label, i: int, base: Result, var: Result) -> List[Tuple[str, str]]:
    (v0, _, b0), (v1, k1, b1) = base, var
    j0, j1 = judge(int(v0[i]), lab), judge(int(v1[i]), lab)
    films = f"right {sorted(lab.right) or '-'} wrong {sorted(lab.wrong) or '-'} prod {c['film']}"
    out: List[Tuple[str, str]] = []
    if j1 == "FP" and j0 != "FP":
        out.append(("NEW FP (judged)", f"{where}: votes {v1[i]} at {b1[i]} bits (baseline {fmt(v0[i])}); {films}"))
    if j0 == "TP" and j1 != "TP":
        out.append(("LOST TP (judged)", f"{where}: baseline voted {v0[i]} at {b0[i]} bits, now {fmt(v1[i])}; {films}"))
    if k1[i] and not base[1][i] and c["film"] in lab.right:
        out.append(("NEW WRONG VETO (judged right take)", f"{where}: vetoes prod's {c['film']}"))
    return out


def _weak_differences(where: str, c: Dict[str, Any], i: int, base: Result, var: Result) -> List[Tuple[str, str]]:
    (v0, _, _), (v1, _, b1) = base, var
    out: List[Tuple[str, str]] = []
    if v1[i] >= 0 and v1[i] != c["film"] and v1[i] != v0[i]:
        out.append(("NEW DISAGREEMENT (weak)", f"{where}: votes {v1[i]} at {b1[i]} bits (baseline {fmt(v0[i])}); prod {c['film']}"))
    if v0[i] == c["film"] and v1[i] != c["film"]:
        out.append(("LOST AGREEMENT (weak)", f"{where}: baseline voted prod's {c['film']}, now {fmt(v1[i])}"))
    return out


SectionTitles = ("NEW FP (judged)", "LOST TP (judged)", "NEW WRONG VETO (judged right take)", "NEW DISAGREEMENT (weak)", "LOST AGREEMENT (weak)")


def _cluster_differences(where: str, c: Dict[str, Any], lab: Label, i: int, results: Dict[str, Result]) -> List[Tuple[str, str]]:
    if lab.judged:
        return _judged_differences(where, c, lab, i, results["baseline"], results["variant"])
    if c["film"] is not None:
        return _weak_differences(where, c, i, results["baseline"], results["variant"])
    return []


def _differences(out: Out, meta: Meta, labs: Sequence[Label], results: Dict[str, Result]) -> None:
    sections: Dict[str, List[str]] = defaultdict(list)
    for i, (c, lab) in enumerate(zip(meta["clusters"], labs)):
        venue, title = c["keys"][0]
        for section, line in _cluster_differences(f"{c['cc']} {venue} | {title}", c, lab, i, results):
            sections[section].append(line)
    for title in SectionTitles:
        lines = sections.get(title, [])
        out(f"{title}: {len(lines)}")
        for line in lines[:25]:
            out("  " + line)
        if len(lines) > 25:
            out(f"  ... {len(lines) - 25} more")


def report(meta: Meta, labs: Sequence[Label], results: Dict[str, Result], variant: Variant, out: Out = print) -> None:
    out(f"baseline: {describe(Baseline)}" + (f" | variant: {describe(variant)}" if "variant" in results else ""))
    present = [cc for cc in Countries if any(c["cc"] == cc for c in meta["clusters"])]
    out(Header)
    for cc in present + ["all"]:
        _country_rows(out, meta, labs, results, cc, set(present) if cc == "all" else {cc})
    if "variant" in results:
        _differences(out, meta, labs, results)


# ── coverage ─────────────────────────────────────────────────────────────────────────────────────────────────────────

Buckets = ("1 no poster url", "2 feed poster (no evidence by design)", "3 url never asked", "4 asked, unread",
           "5 venue hashed, no candidates", "6 venue hashed, no candidate film hashed", "7 usable")


def bucket(row: Sequence[Any], read: Set[str], unread: Set[str], film_hashed: Set[str]) -> int:
    cc, _, _, url, feed, cands, take, _ = row
    if not url:
        return 0
    if feed:
        return 1
    if url in read:
        films: Set[int] = set(cands)
        if take is not None:
            films.add(take)
        if not films:
            return 4
        return 6 if any(f"{cc}|{f}" in film_hashed for f in films) else 5
    return 3 if url in unread else 2


class CoverageCounts:
    """Per country and bucket: the listings, the judged and right-take ones, and the top venues and hosts."""

    def __init__(self) -> None:
        self.by: Dict[str, List["Counter[str]"]] = defaultdict(self._buckets)
        self.venues: Dict[str, List["Counter[str]"]] = defaultdict(self._buckets)
        self.hosts: Dict[str, List["Counter[str]"]] = defaultdict(self._buckets)

    @staticmethod
    def _buckets() -> List["Counter[str]"]:
        return [Counter() for _ in Buckets]

    def add(self, cc: str, b: int, venue: str, url: str, judged: Optional[Set[int]], take: Optional[int]) -> None:
        self.by[cc][b]["all"] += 1
        self.by[cc][b]["judged"] += bool(judged)
        self.by[cc][b]["right take"] += bool(judged) and take in (judged or set())
        self.venues[cc][b][venue] += 1
        if url:
            self.hosts[cc][b][urllib.parse.urlsplit(url).netloc] += 1

    def print_country(self, out: Out, cc: str) -> None:
        total = sum(c["all"] for c in self.by[cc])
        out(f"== {cc}: {total} listings")
        out(f"  {'':42} {'all':>7} {'%':>5} {'judged':>6} {'right take':>10}  top venues / hosts")
        for b, name in enumerate(Buckets):
            c = self.by[cc][b]
            if c["all"]:
                top_v = ", ".join(f"{v} {n}" for v, n in self.venues[cc][b].most_common(3))
                top_h = ", ".join(f"{h} {n}" for h, n in self.hosts[cc][b].most_common(3))
                out(f"  {name:42} {c['all']:7} {100 * c['all'] / total:5.1f} {c['judged']:6} {c['right take']:10}  {top_v}"
                    + (f" | {top_h}" if top_h else ""))


def _right_films(rows: Sequence[Judgement]) -> Dict[Title, Set[int]]:
    right: Dict[Title, Set[int]] = defaultdict(set)
    for r in rows:
        if r.verdict == "right" and r.film.startswith("tmdb:"):
            right[(r.country, normalise(r.venue), normalise(r.rawTitle))].add(int(r.film[5:]))
    return right


def coverage(meta: Meta, rows: Sequence[Judgement], out: Out = print) -> None:
    read, unread, film_hashed = set(meta["venue_filed_any"]), set(meta["venue_unread"]), set(meta["film_filed_any"])
    right = _right_films(rows)
    counts = CoverageCounts()
    for row in meta["listings"]:
        cc, venue, raw, url, _, _, take, _ = row
        judged = right.get((cc, normalise(venue), normalise(raw))) or right.get((cc, "*", normalise(raw)))
        counts.add(cc, bucket(row, read, unread, film_hashed), venue, url, judged, take)
    for cc in [c for c in Countries if c in counts.by]:
        counts.print_country(out, cc)
