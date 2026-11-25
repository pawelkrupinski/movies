"""The data model of flip_timeline.py (scripts/flip-timeline): title patterns, the clock formats, a projection's summary
line, one take change (`Take`), the asks, and the histogram line. No outside call is made here."""
from __future__ import annotations

import datetime
import json
import re
from collections import defaultdict
from typing import Any

import flip_timeline_outside as outside

Json = dict[str, Any]

ASK_NOW_WITHIN = 5.0          # AgreementStage.AskNowWithin: how long a projection waits for its inline asks
PROJECTION_SLACK = 1.5        # a take is written just after its projection's summary line


def title_pattern(title: str | None) -> str:
    """A regex (RE2 and PCRE alike) for a title read the way the resolver reads one: its words, in order, whatever
    punctuation or brackets lie between them — «(IMAX) Sinners» finds «IMAX Sinners» and «(IMAX) Sinners» alike."""
    words = re.findall(r"\w+", title or "")
    return "[^0-9A-Za-z]+".join(outside.re2_escape(w) for w in words) if words else outside.re2_escape(title or "")


def clean_title(title: str | None) -> str:
    """The title the family asks name: bracketed qualifiers ((IMAX), [3D]) dropped."""
    cleaned = re.sub(r"\([^)]*\)|\[[^\]]*\]", " ", title or "").strip()
    return cleaned or (title or "")


def ask_needle(title: str | None) -> str:
    """The regex of a family ask id naming `title`: its last `|`-part is the title, or its slug, with an optional year."""
    return r"\|" + title_pattern(clean_title(title)) + r"([^0-9A-Za-z]+[0-9]{4})?$"


def logsql_changes(cc: str, venue: str, title: str, since: str) -> str:
    """listing_history.logsql with the title read as title_pattern does."""
    pattern = f"(?i)identity take change: (lost|changed|gained) [^|]*{outside.re2_escape(venue)}[^|]* \\| .*{title_pattern(title)}"
    return (f'_time:{since} "identity take change" country:{cc} _msg:~{json.dumps(pattern, ensure_ascii=False)} | sort by (_time)')


def clock(ts: float, frac: bool = True) -> str:
    text = datetime.datetime.fromtimestamp(ts, datetime.timezone.utc).strftime("%m-%d %H:%M:%S.%f")
    return text[:-4] if frac else text[:-7]


def hms(ts: float) -> str:
    return clock(ts)[6:]


def span(seconds: float) -> str:
    if seconds < 120:
        return f"{seconds:.1f} s"
    return f"{seconds / 60:.0f} min" if seconds < 7200 else f"{seconds / 3600:.1f} h"


def iso(ts: float) -> str:
    return datetime.datetime.fromtimestamp(ts, datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.%fZ")


def film(tmdb: int | None) -> str:
    return f"tmdb {tmdb}" if tmdb else "none"


def key_label(key: str) -> str:
    """`P\\0venue\\0title\\0…` / `N\\0venue\\0url\\0title` → `venue | title`."""
    parts = key.split("\x00")
    if len(parts) >= 4 and parts[0] == "N":
        return f"{parts[1]} | {parts[3]}"
    return f"{parts[1]} | {parts[2]}" if len(parts) >= 3 else key


def chunks(items: list[Any], size: int) -> list[list[Any]]:
    return [items[i:i + size] for i in range(0, len(items), size)]


# ---------------------------------------------------------------------------------------------------------------------
# Projections (VictoriaLogs `identity projection:` lines)

def _phase(message: str, name: str) -> float:
    match = re.search(rf"\b{name} ([0-9.]+)s", message)
    return float(match.group(1)) if match else 0.0


def parse_projection(line: Json) -> Json | None:
    """A projection's summary line → its id, trigger, start, end and agreement phase (the phases before it summed)."""
    if not line.get("projection_id") or "identity projection:" not in line.get("_msg", ""):
        return None
    message = line["_msg"]
    start, end = outside.timestamp(line["projection_id"]), outside.timestamp(line["_time"])
    agreement_start = start + sum(_phase(message, p) for p in ("listings", "snapshot", "resolve"))
    return {"id": line["projection_id"], "trigger": line.get("projection_trigger") or "?", "mode": line.get("projection_mode") or "",
            "start": start, "end": end, "agreement": (agreement_start, agreement_start + _phase(message, "agreement")),
            "pod": line.get("pod") or "", "build": (line.get("build") or "")[:7]}


def parse_projections(lines: list[Json]) -> list[Json]:
    """Every summary line that parses, in start order."""
    return sorted((p for p in map(parse_projection, lines) if p), key=lambda p: p["start"])


def trigger_label(trigger: str) -> str:
    return "+".join("boot" if t == "deploy" else t for t in trigger.split("+"))


def projection_of(at: float, projections: list[Json]) -> Json | None:
    """The projection that wrote a take kept at `at`: the latest begun before it and ended at most a moment before."""
    running = [p for p in projections if p["start"] <= at <= p["end"] + PROJECTION_SLACK]
    return max(running, key=lambda p: p["start"]) if running else None


# ---------------------------------------------------------------------------------------------------------------------
# One take change

class Take:
    def __init__(self, entry: Json) -> None:
        after: Json = entry["after"]
        self.at: float = entry["at"] / 1000
        self.key: str = entry["id"]
        self.venue: str = after.get("venue") or ""
        self.listing: str = after.get("listing") or ""
        self.label = f"{self.venue} | {self.listing}"
        self.logged_only: bool = entry.get("logged_only", False)
        self.kind: str = after.get("kind", "")
        self.before: int | None = after.get("tmdb_before")
        self.after: int | None = after.get("tmdb_after")
        self.stage: str = after.get("stage", "")
        self.basis: str = after.get("basis", "")
        self.decided_by: str = after.get("decided_by", "")
        self.previous_decided_by: str = after.get("previous_decided_by", "")
        self.rules: str = after.get("rules", "")
        self.trigger: str = after.get("trigger", "")
        self.cause: str = after.get("cause", "")
        self.members: list[str] = list(after.get("members") or []) if isinstance(after.get("members"), list) else []
        self.member_count = len(self.members) or int(after.get("members") or 0)
        self.awaits: Any = after.get("awaits")
        self.unanswered: Any = after.get("unanswered")
        self.build: str = (entry.get("build") or "")[:7]
        self.projection: Json | None = None
        self.projection_id: str | None = after.get("projection_id")
        self.joined: list[str] = []
        self.left: list[str] = []

    @classmethod
    def logged(cls, line: Json) -> "Take":
        """A take change only VictoriaLogs holds (older than identity_history's week): its line's fields."""
        after = dict(line)
        after["trigger"] = line.get("projection_trigger", "")
        for side in ("before", "after"):
            after[f"tmdb_{side}"] = int(line[f"tmdb_{side}"]) if line.get(f"tmdb_{side}") else None
        key = f"P\x00{line.get('venue')}\x00{line.get('listing')}\x00"
        return cls({"at": outside.timestamp(line["_time"]) * 1000, "id": key, "build": line.get("build", ""), "after": after,
                    "logged_only": True})

    @property
    def shape(self) -> tuple[str, int | None, int | None, str, str, float]:
        return (self.kind, self.before, self.after, self.stage, self.decided_by, round(self.at, 1))


def _listing_matches(raw: Json, line: Json) -> bool:
    """Whether a log line names the raw read's venue and a title it reads alike."""
    return outside.fold(raw["venue"]) in outside.fold(line["venue"]) and \
        bool(re.search(title_pattern(outside.fold(raw["title"])), outside.fold(line.get("listing", "")), re.I))


def _is_listing_line(raw: Json, line: Json) -> bool:
    return bool(line.get("kind") and line.get("venue") and line.get("_time")) and _listing_matches(raw, line)


def _twin(takes: list[Take], line: Json) -> Take | None:
    """The history's take that a log line logged: same listing, within 2 s."""
    at = outside.timestamp(line["_time"])
    label = f"{line['venue']} | {line.get('listing', '')}"
    return next((t for t in takes if t.label == label and abs(t.at - at) < 2.0), None)


def merge_logged(takes: list[Take], lines: list[Json], raw: Json) -> list[Take]:
    """The log's take changes of the listing: each one's projection id onto the history's take it logged (same listing,
    within 2 s), and those the history no longer keeps as takes of their own."""
    out: list[Take] = []
    for line in lines:
        if _is_listing_line(raw, line):
            twin = _twin(takes, line)
            if twin:
                twin.projection_id = twin.projection_id or line.get("projection_id")
            else:
                out.append(Take.logged(line))
    return out


def _decisions(takes: list[Take], siblings: list[Json]) -> dict[str, list[tuple[float, set[str]]]]:
    """Each listing's decisions' members, in time order: the siblings' kept changes and the takes themselves."""
    decisions: dict[str, list[tuple[float, set[str]]]] = defaultdict(list)
    for entry in siblings:
        decisions[entry["id"]].append((entry["at"] / 1000, set(entry["members"])))
    for take in takes:
        decisions[take.key].append((take.at, set(take.members)))
    for history in decisions.values():
        history.sort(key=lambda d: d[0])
    return decisions


def population_moves(takes: list[Take], siblings: list[Json]) -> None:
    """Each take's `joined` / `left`: the members its decision gained or lost since the listing's previous decision — or,
    with none kept, the members whose own previous decision did not hold the listing (they joined it)."""
    decisions = _decisions(takes, siblings)

    def previous(key: str, at: float) -> set[str] | None:
        earlier = [members for when, members in decisions.get(key, []) if when < at - 0.05]
        return earlier[-1] if earlier else None

    for take in takes:
        now = set(take.members)
        before = previous(take.key, take.at) if now else None
        if before is not None:
            take.joined, take.left = sorted(now - before), sorted(before - now)
        elif now:
            take.joined = sorted(m for m in now - {take.key}
                                 if previous(m, take.at) is not None and take.key not in (previous(m, take.at) or ()))


def attach_projections(takes: list[Take], projections: list[Json]) -> None:
    """Each take's projection: the one its log line names, else the one running when it was written."""
    by_id = {p["id"]: p for p in projections}
    for take in takes:
        take.projection = by_id.get(take.projection_id or "") or projection_of(take.at, projections)


def asks_of(history: Json, projections: list[Json]) -> list[Json]:
    """The asks, each with its family, what it asked, outcome, and whether it was filed inline (while a projection of the
    listing ran, from its agreement phase on) or queued."""
    out: list[Json] = []
    for entry in history["asks"]:
        parts = entry["id"].split("|", 3)
        family, what, subject = (parts + ["", "", ""])[1:4] if len(parts) >= 2 else ("?", "", entry["id"])
        at = entry["at"] / 1000
        inline = next((p for p in projections if p["agreement"][0] <= at <= p["end"]), None)
        out.append({"at": at, "family": family, "what": what, "subject": subject,
                    "outcome": entry.get("outcome") or "filed", "failures": entry.get("failures") or 0, "inline": inline})
    return out


def histogram_line(text: str) -> str | None:
    """`family part: n asks, k over 5 s` from an increase-by-le query; None when it holds no samples."""
    try:
        result = json.loads(text)["data"]["result"]
    except (ValueError, KeyError, TypeError):
        return None
    buckets: dict[tuple[str, str], dict[str, float]] = defaultdict(dict)
    for series in result:
        metric = series["metric"]
        buckets[(metric.get("family", "?"), metric.get("part", "?"))][metric.get("le", "")] = float(series["value"][1])
    parts: list[str] = []
    for (family, part), by_le in sorted(buckets.items()):
        total = by_le.get("+Inf", 0.0)
        if total < 0.5:
            continue
        within = by_le.get("5.0", by_le.get("5", 0.0))
        parts.append(f"{family} {part} {total:.0f} ({total - within:.0f} over 5 s)")
    return "; ".join(parts) if parts else None
