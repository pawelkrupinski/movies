"""Cron arithmetic: parsing a schedule, its systemd OnCalendar form, the slots around a moment, and the start-hour spread.

A part of nix/files/scheduled_dispatch.py, which carries the argument for all of it (the header there).
"""
from __future__ import annotations
import datetime
from typing import Dict, Iterable, List, Mapping, Optional, Set, Tuple, TypedDict


DAYS = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]

# The heavy measure-class jobs (the owner, 2026-11-08): each in a clock hour no other daily or weekly job starts in, on
# any day of the week.
HEAVY = ("record-scrape-fixtures", "identity-drift-explorer", "nightly-stress", "order-independence",
         "convergence-daily", "identity-family-explorer", "identity-capture", "identity-refit")

# ---------------------------------------------------------------- cron

# What a cron field matches: the set of its values, or None for `*` (every value).
CronField = Optional[Set[int]]

FIELDS: List[Tuple[str, int, int]] = [("minute", 0, 59), ("hour", 0, 23), ("day of month", 1, 31), ("month", 1, 12), ("day of week", 0, 7)]


def _part(part: str, low: int, high: int) -> Tuple[int, int, int]:
    """(first, last, step) of one comma-separated piece of a cron field: `*`, `a` or `a-b`, each with an optional `/step`."""
    step = 1
    if "/" in part:
        part, step_text = part.split("/", 1)
        step = int(step_text)
    if part == "*":
        return low, high, step
    if "-" in part:
        first, last = (int(x) for x in part.split("-", 1))
        return first, last, step
    first = int(part)
    return first, (high if step > 1 else first), step


def _field(text: str, name: str, low: int, high: int) -> CronField:
    """The values one cron field matches, or None for `*` (every value)."""
    if text == "*":
        return None
    values: Set[int] = set()
    for part in text.split(","):
        start, end, step = _part(part, low, high)
        if not (low <= start <= end <= high) or step < 1:
            raise ValueError(f"cron {name} out of range: {text!r}")
        values.update(range(start, end + 1, step))
    if name == "day of week" and 7 in values:
        values = (values - {7}) | {0}
    return values


def parse_cron(cron: str) -> List[CronField]:
    """[minutes, hours, days of month, months, days of week]: each a set, or None for `*`."""
    parts = cron.split()
    if len(parts) != 5:
        raise ValueError(f"cron needs five fields: {cron!r}")
    return [_field(p, *spec) for p, spec in zip(parts, FIELDS)]


def is_daily_or_rarer(cron: str) -> bool:
    minutes, hours = parse_cron(cron)[:2]
    return minutes is not None and hours is not None and len(minutes) == 1 and len(hours) == 1


def _join(values: Optional[Iterable[int]], width: int = 2) -> str:
    return "*" if values is None else ",".join(str(v).zfill(width) for v in sorted(values))


def on_calendar(cron: str) -> List[str]:
    """systemd OnCalendar expressions (UTC) firing exactly when the cron does. Cron ORs a restricted day of month
    with a restricted day of week; OnCalendar ANDs them, so that case is two expressions."""
    minutes, hours, days, months, weekdays = parse_cron(cron)
    clock = f"{_join(hours)}:{_join(minutes)}:00 UTC"

    def expression(day_values: CronField, weekday_values: CronField) -> str:
        weekday = "" if weekday_values is None else ",".join(DAYS[d] for d in sorted(weekday_values)) + " "
        return f"{weekday}*-{_join(months)}-{_join(day_values)} {clock}"

    if days is not None and weekdays is not None:
        return [expression(days, None), expression(None, weekdays)]
    return [expression(days, weekdays)]


def matches(cron: str, at: datetime.datetime) -> bool:
    minutes, hours, days, months, weekdays = parse_cron(cron)
    weekday = (at.weekday() + 1) % 7  # Python's Monday=0 -> cron's Sunday=0

    def hit(values: CronField, value: int) -> bool:
        return values is None or value in values

    if days is not None and weekdays is not None:
        day_ok = at.day in days or weekday in weekdays
    else:
        day_ok = hit(days, at.day) and hit(weekdays, weekday)
    return hit(minutes, at.minute) and hit(hours, at.hour) and hit(months, at.month) and day_ok


def latest_slot(crons: List[str], now: datetime.datetime) -> datetime.datetime:
    """The newest minute <= now that any of the crons fires at (looked for over the last 32 days)."""
    at = now.replace(second=0, microsecond=0)
    for _ in range(32 * 24 * 60):
        if any(matches(c, at) for c in crons):
            return at
        at -= datetime.timedelta(minutes=1)
    raise ValueError(f"no slot of {crons} in the last 32 days")


def next_slot(crons: List[str], after: datetime.datetime) -> datetime.datetime:
    """The first minute > after that any of the crons fires at."""
    at = after.replace(second=0, microsecond=0) + datetime.timedelta(minutes=1)
    for _ in range(32 * 24 * 60):
        if any(matches(c, at) for c in crons):
            return at
        at += datetime.timedelta(minutes=1)
    raise ValueError(f"no slot of {crons} in the next 32 days")


def start_hours(cron: str) -> Set[Tuple[int, int]]:
    """{(day of week, hour)} the cron starts a run in. A restricted day of month can fall on any day of the week."""
    _, hours, days, _, weekdays = parse_cron(cron)
    if weekdays is None or days is not None:
        weekdays = set(range(7))
    return {(d, h) for d in weekdays for h in (range(24) if hours is None else hours)}


class ScheduledWorkflow(TypedDict):
    file: str
    crons: List[str]
    onCalendar: List[str]


def _clash(a: str, b: str, slots: Mapping[str, Set[Tuple[int, int]]]) -> Optional[str]:
    """What two covered workflows' start hours say against each other, or None when they keep to their own hours."""
    shared = slots[a] & slots[b]
    if shared:
        day, hour = min(shared)
        return f"{a} and {b} both start in hour {hour:02d} UTC ({DAYS[day]}): one daily or weekly job an hour"
    hours = {h for _, h in slots[a]} & {h for _, h in slots[b]}
    if hours and (a in HEAVY or b in HEAVY):
        return (f"{a} and {b} share hour {min(hours):02d} UTC on different days: "
                f"{a if a in HEAVY else b} is a heavy measure job, its hour its own every day")
    return None


def staggering(covered: Mapping[str, ScheduledWorkflow]) -> List[str]:
    """The owner, 2026-11-08: "stagger the daily and weekly jobs hour-wise". No two covered (daily or weekly) workflows
    start in the same clock hour on any day, and a HEAVY one's hour is its own on every day."""
    slots: Dict[str, Set[Tuple[int, int]]] = {
        name: set().union(*(start_hours(c) for c in w["crons"])) for name, w in covered.items()}
    names = sorted(slots)
    clashes = (_clash(a, b, slots) for i, a in enumerate(names) for b in names[i + 1:])
    return [clash for clash in clashes if clash]
