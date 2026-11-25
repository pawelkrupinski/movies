"""The printed forms of flip_timeline.py (scripts/flip-timeline): one listing's timeline with its reversals, and the --all
table of a country's reversals (and its TSV)."""
from __future__ import annotations

from collections import defaultdict
from typing import Optional, TextIO

from flip_timeline_model import (ASK_NOW_WITHIN, Json, Take, clock, film, hms, span, asks_of, attach_projections, histogram_line,
                                 key_label, merge_logged, parse_projections, population_moves, trigger_label)
from flip_timeline_reversals import CLASSES, analyse_all, classify, late_asks, reversals

TSV_COLUMNS = ("class", "cc", "venue", "listing", "from", "via", "dropped_at", "restored_after_s", "dropped_by",
               "restored_by", "dropped_trigger", "restored_trigger", "build", "why")

Alike = tuple[Optional[int], Optional[int], float, float, str, str]
Row = tuple[float, int, str]   # (time, order, text)


def _projection_rows(raw: Json, touching: dict[str, Json]) -> list[Row]:
    rows: list[Row] = []
    for p in touching.values():
        rows.append((p["start"], 0, f"PROJ  {trigger_label(p['trigger']):<18} {hms(p['start'])} -> {hms(p['end'])} "
                     f"({p['end'] - p['start']:.1f} s; agreement {hms(p['agreement'][0])}-{hms(p['agreement'][1])})"
                     + (f" {p['mode']}" if p["mode"] else "") + (f" pod {p['pod']}" if p["pod"] else "")
                     + (f" build {p['build']}" if p["build"] else "")))
        hist = histogram_line(raw.get("histogram", {}).get(p["id"], ""))
        if hist:
            rows.append((p["end"], 3, f"HIST  inline asks (country-wide, around {hms(p['start'])}): {hist}"))
    return rows


def _members_moved(t: Take) -> str:
    if not (t.joined or t.left):
        return ""
    joined = f"; joined {', '.join(key_label(k) for k in t.joined[:3])}{' …' if len(t.joined) > 3 else ''}" if t.joined else ""
    left = f"; left {', '.join(key_label(k) for k in t.left[:3])}{' …' if len(t.left) > 3 else ''}" if t.left else ""
    return joined + left


def _take_extra(t: Take) -> str:
    return "".join([f" · rules {t.rules}" if t.rules else "", f" · awaits {t.awaits}" if t.awaits else "",
                    f" · unanswered {t.unanswered}" if t.unanswered else ""])


def _logged_only(t: Take) -> str:
    return " (VictoriaLogs only)" if t.logged_only else ""


def _take_row(group: list[Take]) -> Row:
    t = group[0]
    who = t.label if len(group) == 1 else f"{len(group)} listings ({', '.join(g.label for g in group[:2])}{', …' if len(group) > 2 else ''})"
    extra = _take_extra(t)
    where = f"proj {hms(t.projection['start'])}" if t.projection else f"trigger {trigger_label(t.trigger)}"
    return (t.at, 2, f"TAKE  {t.kind:<7} {film(t.before)} -> {film(t.after)}  {who}\n"
            f"{'':20}stage {t.stage} · {t.basis} · {t.decided_by or '-'}"
            + (f" (was {t.previous_decided_by})" if t.previous_decided_by else "")
            + f"{extra} · {t.member_count} member(s){_members_moved(t)} · cause {t.cause or '-'} · build {t.build or '?'} [{where}]"
            + _logged_only(t))


def _take_rows(takes: list[Take]) -> list[Row]:
    groups: dict[tuple[object, ...], list[Take]] = defaultdict(list)
    for take in takes:
        groups[take.shape].append(take)
    return [_take_row(group) for group in groups.values()]


def _late_projections(takes: list[Take], asks: list[Json], projections: list[Json]) -> dict[int, Json]:
    """The projection each late ask (by id) was filed after."""
    lates: dict[int, Json] = {}
    for take in takes:
        if take.projection:
            nxt = [p for p in projections if p["start"] > take.projection["start"]]
            for ask in late_asks(asks, take.projection, nxt[0]["start"] if nxt else take.at + 600):
                lates[id(ask)] = take.projection
    return lates


def _ask_rows(takes: list[Take], asks: list[Json], projections: list[Json]) -> list[Row]:
    lates = _late_projections(takes, asks, projections)
    rows: list[Row] = []
    for ask in asks:
        mode = (f"inline in {hms(ask['inline']['start'])} (sent ~{hms(ask['inline']['agreement'][0])}, "
                f"+{ask['at'] - ask['inline']['agreement'][0]:.1f} s)" if ask["inline"] else "queued")
        outcome = ask["outcome"] + (f" after {ask['failures']} failure(s)" if ask["failures"] else "")
        late = lates.get(id(ask))
        rows.append((ask["at"], 1, f"ASK   {ask['family']:<10} {ask['what']} «{ask['subject']}» {outcome} · {mode}"
                     + (f"\n{'':20}LATE: after projection {hms(late['start'])} gave up waiting "
                        f"({hms(late['agreement'][0])} + {ASK_NOW_WITHIN:.0f} s); it ended {hms(late['end'])}" if late else "")))
    return rows


def _listing_rows(history: Json, touching: dict[str, Json], keys: set[str]) -> tuple[list[Row], int]:
    """The LIST rows within a minute of a projection of the listing, and how many publications fell elsewhere."""
    rows: list[Row] = []
    elsewhere = 0
    for listing in history.get("listings", []):
        at = listing["at"] / 1000
        if not any(p["start"] - 60 <= at <= p["end"] for p in touching.values()):
            elsewhere += 1
            continue
        rows.append((at, 1, f"LIST  {key_label(listing['id'])} published" + (" (this listing)" if listing["id"] in keys else "")))
    return rows, elsewhere


def _reversal_lines(pairs: list[tuple[Take, Take]], asks: list[Json], listings: list[Json]) -> list[str]:
    out = ["", f"REVERSALS ({len(pairs)})" if pairs else "REVERSALS: none (no take went back to a film it had left)"]
    alike: dict[Alike, list[tuple[str, float, float]]] = defaultdict(list)
    for first, second in pairs:
        verdict, why = classify(first, second, asks, listings)
        alike[(first.before, first.after, round(first.at, 1), round(second.at, 1), verdict, why)].append((first.label, first.at, second.at))
    for (was, now, _, _, verdict, why), seen in alike.items():
        labels = [label for label, _, _ in seen]
        t1, t2 = seen[0][1], seen[0][2]
        who = labels[0] if len(labels) == 1 else f"{len(labels)} listings ({labels[0]}, …)"
        out.append(f"  {verdict:<18} {film(was)} -> {film(now)} ({clock(t1)}) -> {film(was)} (+{span(t2 - t1)})  {who}")
        out.append(f"  {'':18} {why}")
    return out


def _header_lines(raw: Json, title: str) -> list[str]:
    out = [title]
    out.extend(f"   {line}" for line in raw.get("freshness", "").splitlines()[:2])
    out.extend(f"   NOTE {note}" for note in raw.get("notes", []))
    return out


def render(raw: Json) -> str:
    history = raw["history"]
    projections = parse_projections(raw.get("projections", []))
    takes = [Take(t) for t in history["takes"]]
    takes += merge_logged(takes, raw.get("changes", []), raw)
    attach_projections(takes, projections)
    population_moves(takes, history.get("siblings", []))
    asks = asks_of(history, projections)
    keys = {t.key for t in takes}

    out = _header_lines(raw, f"== {raw['cc'].upper()} «{raw['venue']}» | «{raw['title']}», last {raw['since']}"
                        + (f" — {len(keys)} listing(s)" if takes else ""))
    if not takes:
        out.append(f"no take change of it kept in identity_history in the last {raw['since']}")
        return "\n".join(out) + "\n"

    touching = {t.projection["id"]: t.projection for t in takes if t.projection}
    listing_rows, elsewhere = _listing_rows(history, touching, keys)
    rows = _projection_rows(raw, touching) + _take_rows(takes) + _ask_rows(takes, asks, projections)
    rows += [(a["at"] / 1000, 1, f"READ  {a['kind'].split(':', 1)[-1]} {a['id']} filed") for a in history.get("answers", [])]
    rows += listing_rows
    out.extend(f"{clock(at)}  {text}" for at, _, text in sorted(rows, key=lambda r: (r[0], r[1])))
    if elsewhere:
        out.append(f"({elsewhere} other publication(s) of its members in the window, none within a minute of a projection of it)")
    out.extend(_reversal_lines(reversals(takes), asks, history.get("listings", [])))
    return "\n".join(out) + "\n"


def _write_tsv(tsv: TextIO, rows: list[Json]) -> None:
    tsv.write("\t".join(TSV_COLUMNS) + "\n")
    for r in rows:
        tsv.write("\t".join(str(r[c] if r[c] is not None else "").replace("\t", " ").replace("\n", " ") for c in TSV_COLUMNS) + "\n")


def _count_table(rows: list[Json]) -> list[str]:
    counts = {c: sum(1 for r in rows if r["class"] == c) for c in CLASSES}
    out = [f"{'class':<20}{'count':>6}{'share':>8}"]
    out.extend(f"{name:<20}{counts[name]:>6}{(counts[name] / len(rows) if rows else 0):>8.0%}" for name in CLASSES)
    out.append(f"{'total':<20}{len(rows):>6}")
    return out


def _reversal_row_lines(rows: list[Json]) -> list[str]:
    out = [""]
    for r in sorted(rows, key=lambda r: (CLASSES.index(r["class"]), r["dropped_at"])):
        out.append(f"{r['class']:<18} {r['dropped_at'][5:19]}  {r['venue']} | {r['listing']}  {film(r['from'])} -> "
                   f"{film(r['via'])} -> back (+{span(r['restored_after_s'])})")
        out.append(f"{'':19}{r['why']}")
    return out


def render_all(raw: Json, summary: bool = False, tsv: TextIO | None = None) -> str:
    """The per-class count table, a line per reversal (unless `summary`), and `tsv` (a file object) a row per reversal."""
    rows = analyse_all(raw)
    out = _header_lines(raw, f"== {raw['cc'].upper()} all reversals, last {raw['since']} — {len(rows)} reversal(s) among "
                        f"{len(raw['history']['takes'])} take change(s)")
    out.extend(_count_table(rows))
    if not summary:
        out.extend(_reversal_row_lines(rows))
    if tsv is not None:
        _write_tsv(tsv, rows)
    return "\n".join(out) + "\n"
