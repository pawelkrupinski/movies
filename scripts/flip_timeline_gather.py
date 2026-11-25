"""The outside reads of flip_timeline.py (scripts/flip-timeline), every call through `run` as listing_history's: the
local mirror's identity_history, and VictoriaLogs and Prometheus over `ssh monitoring-1`."""
from __future__ import annotations

import argparse
import json
import os
from typing import Any

import flip_timeline_outside as outside
from flip_timeline_model import (Json, Take, ask_needle, chunks, iso, logsql_changes, parse_projections, projection_of,
                                 title_pattern)
from flip_timeline_queries import (ALL_ASKS_QUERY, ALL_LISTINGS_QUERY, ASKS_QUERY, LISTINGS_QUERY, SIBLINGS_QUERY, TAKES_QUERY)
from flip_timeline_reversals import REVERSAL_WITHIN, analyse_all, reversals

Run = outside.Run

REPO = outside.REPO
WINDOW_BEFORE, WINDOW_AFTER = 90, 120   # the projection lines read around each take / ask
MAX_WINDOWS = 40
PROMETHEUS_INSTANT = outside.PROMETHEUS.replace("query_range", "query")
HISTOGRAM = "kinowo_worker_identity_agreement_inline_ask_seconds_bucket"
PAGE = 400                  # take changes read per mirror call: a country's week is far more, so the reads page
CHUNK = 100                 # takes (their windows, their read ids) per asks / answers call; member ids per siblings call
MEMBER_CHUNK = 200


def projection_windows(times: list[float]) -> list[list[float]]:
    spans = sorted((t - WINDOW_BEFORE, t + WINDOW_AFTER) for t in times)
    merged: list[list[float]] = []
    for lo, hi in spans:
        if merged and lo <= merged[-1][1]:
            merged[-1][1] = max(merged[-1][1], hi)
        else:
            merged.append([lo, hi])
    return merged


def projections_logsql(cc: str, windows: list[list[float]]) -> str:
    ranges = " or ".join(f"_time:[{iso(lo)}, {iso(hi)}]" for lo, hi in windows)
    return f'country:{cc} "identity projection:" ({ranges}) | sort by (_time)'


def _header(args: argparse.Namespace, since: float, **extra: Any) -> str:
    consts: Json = {"VENUE": outside.re2_escape(args.venue), "TITLE": title_pattern(args.title), "SINCE": int(since * 1000)}
    consts.update(extra)
    return "const " + ", ".join(f"{k} = {json.dumps(v)}" for k, v in consts.items()) + ";\n"


def _mongo(run: Run, raw: Json, args: argparse.Namespace, header: str, query: str) -> Any:
    """One mirror script's JSON answer, or None (said in raw's notes) when it did not answer."""
    _, text = run(["mongosh", "--quiet", outside.mirror_url(args.cc), "--eval", header + query])
    try:
        return json.loads(text.strip().splitlines()[-1])
    except (ValueError, IndexError):
        raw["notes"].append(f"mirror query failed: {text.strip()[:300]}")
        return None


def _mongo_list(run: Run, raw: Json, args: argparse.Namespace, header: str, query: str) -> list[Json]:
    """A script's JSON list, empty when the mirror did not answer."""
    found: list[Json] = _mongo(run, raw, args, header, query) or []
    return found


def _mongo_dict(run: Run, raw: Json, args: argparse.Namespace, header: str, query: str) -> Json:
    """A script's JSON object, empty when the mirror did not answer."""
    found: Json = _mongo(run, raw, args, header, query) or {}
    return found


def _json_lines(text: str) -> list[Json]:
    out: list[Json] = []
    for line in text.splitlines():
        try:
            out.append(json.loads(line))
        except ValueError:
            continue
    return out


def read_takes(args: argparse.Namespace, run: Run, raw: Json, since: float) -> list[Json]:
    """ALL take changes of the window, one page of PAGE after another (the cursor is the last one's time and id)."""
    takes: list[Json] = []
    after: Json = {"AFTER_AT": 0, "AFTER_ID": ""}
    while True:
        page = _mongo(run, raw, args, _header(args, since, PAGE=PAGE, **after), TAKES_QUERY)
        if page is None:
            break
        takes.extend(page)
        if len(page) < PAGE:
            break
        after = {"AFTER_AT": page[-1]["at"], "AFTER_ID": page[-1]["oid"]}
    return takes


def read_siblings(args: argparse.Namespace, run: Run, raw: Json, since: float, members: set[str]) -> list[Json]:
    """The other members' take changes (whose decision a listing joined or left), a chunk of ids at a time; a member's
    consecutive changes with the same members are kept once, so what is held is the decision changes only."""
    kept: list[Json] = []
    last: dict[str, Any] = {}
    for ids in chunks(sorted(members), MEMBER_CHUNK):
        for entry in _mongo_list(run, raw, args, _header(args, since, IDS=ids), SIBLINGS_QUERY):
            if last.get(entry["id"]) != entry["members"]:
                last[entry["id"]] = entry["members"]
                kept.append(entry)
    return kept


def read_projections(cc: str, times: list[float], run: Run) -> list[Json]:
    """The projection summary lines around `times`, a batch of windows per VictoriaLogs call."""
    lines: list[Json] = []
    for batch in chunks(projection_windows(times), MAX_WINDOWS):
        code, text = run(outside.remote_curl(outside.LOGS, [("query", projections_logsql(cc, batch)), ("limit", 20000)]))
        if code == 0:
            lines.extend(line for line in _json_lines(text) if "identity projection:" in line.get("_msg", ""))
    return lines


def _strip_cursors(takes: list[Json]) -> None:
    for take in takes:
        take.pop("readIds", None)
        take.pop("oid", None)


def _ask_constants(chunk: list[Json], members: set[str]) -> Json:
    """The windows around a chunk of takes, the answers they read, and the films they moved between."""
    return {"WINDOWS": [[t["at"] - 30000, t["at"] + 2000] for t in chunk],
            "READ_IDS": sorted({r for t in chunk for r in t["readIds"]}), "MEMBERS": sorted(members),
            "FILMS": sorted({f for t in chunk for f in (t["after"].get("tmdb_before"), t["after"].get("tmdb_after")) if f})}


def _read_asks(args: argparse.Namespace, run: Run, raw: Json, since: float, takes: list[Json], members: set[str]) -> None:
    """The asks and the answers read around each chunk of takes, into raw's history."""
    history = raw["history"]
    for chunk in chunks(takes, CHUNK):
        found = _mongo_dict(run, raw, args, _header(args, since, **_ask_constants(chunk, members)), ASKS_QUERY)
        history["asks"].extend(a for a in found.get("asks", []) if a not in history["asks"])
        history["answers"].extend(a for a in found.get("answers", []) if a not in history["answers"])


def _read_history(args: argparse.Namespace, run: Run, raw: Json, since: float) -> None:
    history = raw["history"]
    takes = read_takes(args, run, raw, since)
    members: set[str] = set()
    for take in takes:
        members.update(take["after"].get("members") or [])
    _read_asks(args, run, raw, since, takes, members)
    _strip_cursors(takes)
    history["takes"] = takes
    history["siblings"] = read_siblings(args, run, raw, since, members)
    for ids in chunks(sorted(members), MEMBER_CHUNK):
        history["listings"].extend(_mongo_list(run, raw, args, _header(args, since, IDS=ids), LISTINGS_QUERY))
    asks: list[Json] = history["asks"]
    asks.sort(key=lambda a: a["at"])


def _histograms(args: argparse.Namespace, run: Run, raw: Json, projections: list[Json]) -> None:
    """The inline-ask histogram around each projection that dropped a take."""
    for take in raw["history"]["takes"]:
        projection = projection_of(take["at"] / 1000, projections)
        if take["after"].get("kind") == "lost" and projection and projection["id"] not in raw["histogram"]:
            span = max(60, int(projection["end"] - projection["start"]) + 30)
            query = (f'sum by (family, part, le) (increase({HISTOGRAM}{{country="{args.cc}"}}[{span}s]))')
            code, text = run(outside.remote_curl(PROMETHEUS_INSTANT, [("query", query), ("time", round(projection["end"] + 30, 3))]))
            raw["histogram"][projection["id"]] = text if code == 0 else ""


def _mirror_state(args: argparse.Namespace, run: Run, raw: Json) -> bool:
    """Notes what the mirror's freshness says into raw; whether the mirror is readable."""
    code, text = run([os.path.join(REPO, "scripts/local-mirror/freshness"), args.cc, "--need", "identity_history", "--quiet"])
    raw["freshness"] = text.strip()
    if code == 2:
        raw["notes"].append("the mirror is not readable")
        return False
    if code != 0:
        raw["notes"].append("the mirror is not fresh: the newest history may be missing")
    return True


def gather(args: argparse.Namespace, run: Run, now: float) -> Json:
    since = now - outside.seconds(args.since)
    raw: Json = {"cc": args.cc, "venue": args.venue, "title": args.title, "since": args.since, "now": now, "notes": [],
                 "freshness": "", "history": {"takes": [], "siblings": [], "asks": [], "answers": [], "listings": []}}
    if _mirror_state(args, run, raw):
        _read_history(args, run, raw, since)
    history = raw["history"]
    code, text = run(outside.remote_curl(outside.LOGS, [("query", logsql_changes(args.cc, args.venue, args.title, args.since)),
                                                       ("limit", outside.LOG_LIMIT)]))
    raw["changes"] = _json_lines(text) if code == 0 else []
    if code != 0:
        raw["notes"].append(f"VictoriaLogs did not answer: {text.strip()[:200]}")
    times = [t["at"] / 1000 for t in history["takes"]] + [a["at"] / 1000 for a in history["asks"] if "|title|" in a["id"]]
    raw["projections"] = read_projections(args.cc, times, run) if times and code == 0 else []
    raw["histogram"] = {}
    _histograms(args, run, raw, parse_projections(raw["projections"]))
    return raw


def _reversal_windows(pairs: list[tuple[Take, Take]]) -> list[Json]:
    return [{"lo": int((((a.projection or {}).get("start") or a.at - 30) - 5) * 1000), "hi": int((b.at + 2) * 1000),
             "needle": ask_needle(a.listing), "key": a.key, "members": sorted(set(a.members) | set(b.members) or {a.key}),
             "films": sorted({f for f in (a.before, a.after) if f})} for a, b in pairs]


def _unexplained_windows(rows: list[Json]) -> list[Json]:
    """The windows of the reversals nothing else explained (RULE, UNCLASSIFIED): where member listings were re-published."""
    return [_listing_window(r["first"], r["second"]) for r in rows if r["class"] in ("RULE", "UNCLASSIFIED")]


def _listing_window(first: Take, second: Take) -> Json:
    return {"lo": int((((first.projection or {}).get("start") or first.at - 30) - 60) * 1000),
            "hi": int((((second.projection or {}).get("start") or second.at)) * 1000),
            "members": sorted((set(first.members) | set(second.members)) or {first.key})}


def gather_all(args: argparse.Namespace, run: Run, now: float) -> Json:
    """Page every take change of the country, then read around the reversals only: their projections, the asks naming
    their titles, and (for those nothing else explained) the member listings re-published inside their window."""
    since = now - outside.seconds(args.since)
    args.venue = args.title = ""
    raw: Json = {"cc": args.cc, "all": True, "since": args.since, "now": now, "notes": [], "venue": "", "title": "",
                 "history": {"takes": [], "siblings": [], "asks": [], "answers": [], "listings": []}, "projections": []}
    if not _mirror_state(args, run, raw):
        return raw
    history = raw["history"]
    history["takes"] = read_takes(args, run, raw, since)
    _strip_cursors(history["takes"])
    takes = [Take(t) for t in history["takes"]]
    pairs = reversals(takes, by="key", within=REVERSAL_WITHIN)
    times = [t.at for pair in pairs for t in pair]
    if times:
        raw["projections"] = read_projections(args.cc, times, run)
    projections = parse_projections(raw["projections"])
    for take in takes:
        take.projection = projection_of(take.at, projections)
    for batch in chunks(_reversal_windows(pairs), 50):
        history["asks"].extend(_mongo_list(run, raw, args, "", ALL_ASKS_QUERY.replace("WINDOWS", json.dumps(batch))))
    for batch in chunks(_unexplained_windows(analyse_all(raw)), 20):
        history["listings"].extend(_mongo_list(run, raw, args, "", ALL_LISTINGS_QUERY.replace("WINDOWS", json.dumps(batch))))
    return raw
