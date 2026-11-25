"""The reversals of flip_timeline.py (scripts/flip-timeline): each X -> Y -> X of one listing, and the class (first match
wins) and the why of each. The --all analysis of a whole country's read is `analyse_all`."""
from __future__ import annotations

import re
from collections import defaultdict

from flip_timeline_model import (ASK_NOW_WITHIN, Json, Take, clock, hms, iso, asks_of, ask_needle, attach_projections,
                                 key_label, parse_projections, population_moves)

REVERSAL_WITHIN = 86400     # --all: a take that went back within 24 h is a reversal (the backtest's window); later is a new decision

CLASSES = ("LATE-ANSWER", "SIBLING/POPULATION", "RULE", "DEPLOY/RESTART", "UNCLASSIFIED")


def late_asks(asks: list[Json], dropping: Json, restoring_start: float) -> list[Json]:
    """The asks filed after `dropping` gave up waiting (its agreement start + AskNowWithin, or its agreement's end if
    sooner) and before the restoring projection began."""
    gave_up = min(dropping["agreement"][0] + ASK_NOW_WITHIN, dropping["agreement"][1])
    return [a for a in asks if gave_up < a["at"] <= restoring_start and a["at"] >= dropping["start"]]


def reversals(takes: list[Take], by: str = "label", within: float | None = None) -> list[tuple[Take, Take]]:
    """Per listing (`by` its label, or its key), each consecutive pair of changes whose second goes back to the film the
    first left."""
    by_key: dict[str, list[Take]] = defaultdict(list)
    for take in takes:
        by_key[getattr(take, by)].append(take)
    out: list[tuple[Take, Take]] = []
    for seq in by_key.values():
        seq.sort(key=lambda t: t.at)
        for first, second in zip(seq, seq[1:]):
            if first.before != first.after and second.after == first.before and second.before == first.after \
                    and (within is None or second.at - first.at <= within):
                out.append((first, second))
    return out


Verdict = tuple[str, str]


def _trigger_of(take: Take) -> str:
    return take.projection["trigger"] if take.projection else take.trigger


def _pod_verdict(p1: Json | None, p2: Json | None) -> Verdict | None:
    if p1 and p2 and p1["pod"] and p2["pod"] and p1["pod"] != p2["pod"]:
        return "DEPLOY/RESTART", f"the pod changed between them: {p1['pod']} -> {p2['pod']}"
    return None


def _restart_verdict(first: Take, second: Take) -> Verdict | None:
    boot1, boot2 = "deploy" in _trigger_of(first).split("+"), "deploy" in _trigger_of(second).split("+")
    if boot1 or boot2:
        side = "the dropping" if boot1 else "the restoring"
        return "DEPLOY/RESTART", f"{side} projection was a boot (trigger deploy)"
    if first.build and second.build and first.build != second.build:
        return "DEPLOY/RESTART", f"the build changed between them: {first.build} -> {second.build}"
    return _pod_verdict(first.projection, second.projection)


def _late_answer_verdict(first: Take, second: Take, asks: list[Json]) -> Verdict | None:
    p1, p2 = first.projection, second.projection
    if "answer" not in _trigger_of(second).split("+"):
        return None
    restore_start = p2["start"] if p2 else second.at
    late = late_asks(asks, p1, restore_start) if p1 else [a for a in asks if first.at - 30 <= a["at"] <= restore_start]
    if late:
        last = late[-1]
        gave_up = (f", after projection {hms(p1['start'])} gave up waiting (agreement {hms(p1['agreement'][0])} "
                   f"+ {ASK_NOW_WITHIN:.0f} s)" if p1 else "")
        return "LATE-ANSWER", (f"{len(late)} ask(s) of the cluster filed late, the last {last['family']} {last['what']} "
                               f"«{last['subject']}» at {hms(last['at'])}{gave_up}; the answer-triggered projection "
                               f"{hms(restore_start)} restored it by {second.decided_by}")
    if second.stage == "poster":
        return "LATE-ANSWER", (f"inferred: restored by {second.decided_by} in an answer-triggered projection; poster "
                               "answers are not kept in identity_history, so the late filing itself is not visible")
    return None


def _population_verdict(first: Take, second: Take, listings: list[Json]) -> Verdict | None:
    p1, p2 = first.projection, second.projection
    moved = sorted(set(first.joined + first.left + second.joined + second.left))
    if moved:
        return "SIBLING/POPULATION", (f"the decision's members moved: {len(moved)} listing(s) joined or left "
                                      f"({', '.join(key_label(m) for m in moved[:3])}{', …' if len(moved) > 3 else ''})")
    lo = p1["start"] if p1 else first.at - 30
    hi = p2["start"] if p2 else second.at
    published = [item for item in listings if lo - 60 <= item["at"] / 1000 <= hi and item["id"] != first.key]
    if published:
        return "SIBLING/POPULATION", (f"{len(published)} member listing(s) re-published between them "
                                      f"({key_label(published[0]['id'])} at {hms(published[0]['at'] / 1000)})")
    return None


def _unexplained_verdict(first: Take, second: Take) -> Verdict:
    p1, p2 = first.projection, second.projection
    if not (p1 and p2):
        gap = "the dropping" if not p1 else "the restoring"
        return "UNCLASSIFIED", (f"no projection line found for {gap} take (VictoriaLogs holds none around {clock(first.at if not p1 else second.at)}): "
                                "its trigger, pod and asks cannot be judged")
    if "answer" in _trigger_of(second).split("+"):
        return "UNCLASSIFIED", (f"restored by {second.decided_by or second.basis} in an answer-triggered projection, but no ask "
                                "naming the listing's title was found: asks are keyed by the resolver's search title, which "
                                "the listing's text may not spell")
    return "RULE", (f"same build, members and answers: {first.decided_by or first.basis} then "
                    f"{second.decided_by or second.basis} on the same inputs")


def classify(first: Take, second: Take, asks: list[Json], listings: list[Json]) -> Verdict:
    """(class, why) of one reversal: `first` took the listing off a film, `second` put it back."""
    return (_restart_verdict(first, second) or _late_answer_verdict(first, second, asks)
            or _population_verdict(first, second, listings) or _unexplained_verdict(first, second))


# ---------------------------------------------------------------------------------------------------------------------
# --all: every reversal of a country, by the flip-prediction backtest's definition (a listing's take change that goes back
# to the film its previous change left: X -> Y -> X, consecutive changes of ONE listing key), each classified.

def _within(ask: Json, lo: float, hi: float) -> bool:
    return lo <= ask["at"] / 1000 <= hi


def _posters(asks: list[Json], key: str, lo: float, hi: float) -> list[Json]:
    return [a for a in asks if key in a.get("for", ()) and _within(a, lo, hi)]


def _records(asks: list[Json], start: float, hi: float) -> list[Json]:
    return [a for a in asks if "|record|" in a["id"] and _within(a, start, hi)]


def _new_to(titled: list[Json], asks: list[Json]) -> list[Json]:
    return [a for a in asks if a not in titled]


def cluster_asks(asks: list[Json], first: Take, second: Take) -> list[Json]:
    """The asks of the reversed listing's family in its window: those naming its title and, when it filed any, the record
    asks (the candidates the title answers found) filed from its first title ask on."""
    needle = re.compile(ask_needle(first.listing), re.I)
    lo = ((first.projection or {}).get("start") or first.at - 30) - 5
    hi = second.at + 2
    titled = [a for a in asks if _within(a, lo, hi) and needle.search(a["id"])]
    posters = _posters(asks, first.key, lo, hi)
    if not titled:
        return posters
    start = min(a["at"] for a in titled) / 1000 - 0.001
    return sorted(titled + _new_to(titled, _records(asks, start, hi)) + _new_to(titled, posters), key=lambda a: a["at"])


def _reversal_row(raw: Json, first: Take, second: Take, verdict: Verdict) -> Json:
    p1, p2 = first.projection, second.projection
    return {"class": verdict[0], "cc": raw["cc"], "venue": first.venue, "listing": first.listing, "from": first.before,
            "via": first.after, "dropped_at": iso(first.at), "restored_after_s": round(second.at - first.at, 1),
            "dropped_by": first.decided_by, "restored_by": second.decided_by,
            "dropped_trigger": p1["trigger"] if p1 else first.trigger,
            "restored_trigger": p2["trigger"] if p2 else second.trigger,
            "build": second.build, "why": verdict[1], "first": first, "second": second}


def analyse_all(raw: Json, within: float = REVERSAL_WITHIN) -> list[Json]:
    """Every reversal of `raw` (a gather_all result) restored within `within` seconds, with its class and why: a list of
    dicts."""
    history = raw["history"]
    projections = parse_projections(raw.get("projections", []))
    takes = [Take(t) for t in history["takes"]]
    attach_projections(takes, projections)
    population_moves(takes, [])
    rows: list[Json] = []
    for first, second in reversals(takes, by="key", within=within):
        own = asks_of({"asks": cluster_asks(history.get("asks", []), first, second)}, projections)
        members = set(first.members) | set(second.members)
        listings = [item for item in history.get("listings", []) if item["id"] in members]
        rows.append(_reversal_row(raw, first, second, classify(first, second, own, listings)))
    return rows
