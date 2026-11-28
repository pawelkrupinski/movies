#!/usr/bin/env python3
"""MAIN'S CONVERGENCE RUN IN 20 RUNNERS (the owner, 2026-11-06: "reduce convergence run to 20 runners at most and balance
the times"): GitHub Free runs 20 jobs at once for the whole account, and a run of ~43 rows queued every row at once, so
which rows started first was GitHub's pick -- a 20-minute drift row often started 10 minutes late, and the run ended
28-50 minutes after it began (runs 42112839746 .. 42225884509) for ~16 minutes of work a runner.

So the preflight hands each country's leg (country-convergence-leg.yml) its rows LONGEST FIRST (`phases`) and a
`max-parallel` (its SHARE of the 20): GitHub starts a matrix's rows in their listed order and the next one when a running
one ends, so each country runs a longest-processing-time list schedule on its own slots, and the run never holds more
than the shares' sum. The shares are chosen greedily: every country starts with one slot and the next slot goes to the
country whose rows end last (its order comparison, the `order-independence-p01|p12` jobs, after them) until the 20 are given out or another
slot would shorten no country. The rows, their checks and their names are the leg's own; only their order and how many
run at once change.

The costs are MEASURED: scripts/ci/convergence-row-minutes.tsv, each row's median job minutes (setup and post-steps
included: a row's slot is held for all of it) over main's recent complete runs. A row the file does not name weighs
UNMEASURED_MINUTES; a `drift-twice` run's drift rows weigh twice theirs. Refresh the file after a change that moves the
rows' times (`costs`), or the plan drifts from what the rows cost; a stale file only orders rows worse, it drops none.

  convergence_rows.py plan [--drift-twice]        the shares and predicted minutes of main's suite, from the costs
  convergence_rows.py costs --runs ID,ID,...      rewrites the costs file from those runs' jobs (gh api)

Used by convergence_rows.py `legs`; tested by convergence_slots_test.py.
"""
from __future__ import annotations

import datetime
import os
import statistics
from typing import Any, Callable, Dict, List, Mapping, Optional, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
COSTS_FILE = os.path.join(HERE, "convergence-row-minutes.tsv")
# GitHub Free's concurrent jobs for the whole account
RUNNERS = 20
# a leg's matrix with no cap of ours (GitHub's own ceiling on a matrix)
UNCAPPED = 256
# a row the costs file does not name: about a mid-sized row
UNMEASURED_MINUTES = 8.0
# the order comparison after a split country's rows (the `order-independence-p01|p12` jobs)
COMPARE_PHASES = ("order-independence-p01", "order-independence-p12")


# an unmeasured row that costs a multiple of a measured one: the A/A rows (harness_aa.py) run only on a push that
# changed a harness, too rarely to be measured -- each drift pass replays the head's row-1 seeds once (the two side by
# side, compared after them: AA_COMPARE_MINUTES), the takes one makes both of the takes check's sides in one row (the
# takes row's head and its takes-base row's base)
LIKE = {"takes-drift-aa-pass0": ("takes-drift-1", 1.0), "takes-drift-aa-pass1": ("takes-drift-1", 1.0),
        "takes-aa": ("takes", 2.0)}
# the A/A passes' comparison (`takes-drift-aa`, in Identity takes' `takes-drift-aa` job): two TSV files and no sbt -- about the
# order comparison's setup alone
AA_COMPARE_MINUTES = 1.5


Costs = Dict[Tuple[str, str], float]
Shares = Dict[str, Dict[str, Any]]


def cost(code: str, phase: str, costs: Costs, drift_twice: bool = False) -> float:
    like, times = LIKE.get(phase, (None, 1.0))
    minutes = costs.get((code, phase)) or (costs[(code, like)] * times if like is not None and (code, like) in costs else UNMEASURED_MINUTES)
    return minutes * 2 if drift_twice and phase.startswith("takes-drift-") else minutes


# a row another row waits on, started just before it whatever its own cost: the `takes` row waits for its `takes-base`
# row's stored base only while that row RUNS, and resolves the base itself when it is still queued
# (identity-takes-check.sh, THE BASE ROW) -- a base row capped behind it would cost the takes row 2.5-4 minutes
STARTS_BEFORE = {"takes-base": "takes"}


def longest_first(code, phases, costs, drift_twice=False):
    """The phases by cost, longest first (a STARTS_BEFORE row just before the row it serves); equal costs keep their given
    order."""
    def weight(phase):
        served = STARTS_BEFORE.get(phase)
        if served is not None and served in phases:
            return (-cost(code, served, costs, drift_twice), 0)
        return (-cost(code, phase, costs, drift_twice), 1)
    return sorted(phases, key=weight)


def makespan(minutes, slots):
    """When the last of `minutes` (in start order) ends with `slots` at once, each next row taking the first free slot."""
    free = [0.0] * max(1, min(slots, len(minutes) or 1))
    for m in minutes:
        free.sort()
        free[0] += m
    return max(free)


def plan(phases_by_code: Mapping[str, List[str]], costs: Costs, total: int = RUNNERS, tails: Optional[Mapping[str, float]] = None,
         drift_twice: bool = False) -> Shares:
    """{code: {"phases": [...longest first], "max-parallel": n, "minutes": predicted end}} -- the shares summing to at most
    `total` (one each at least), each next slot to the country that ends last."""
    return plan_suites({"": phases_by_code}, costs, total, {"": tails or {}}, drift_twice)[""]


def _share_slots(legs: List[Tuple[str, str]], rows: Mapping[Tuple[str, str], int], ends: Callable[[Tuple[str, str], int], float],
                 total: int) -> Dict[Tuple[str, str], int]:
    """Every leg one slot at least, each next slot to the leg that ends last (`rows`: how many rows each leg runs)."""
    share = {leg: 1 for leg in legs}
    while sum(share.values()) < total:
        # a leg can still end sooner with more slots (perhaps only with more than one: 4 rows of 10 end at 20 on 2 or 3)
        growable = [leg for leg in legs if share[leg] < rows[leg] and ends(leg, rows[leg]) < ends(leg, share[leg])]
        if not growable:
            break
        # a slot that cannot shorten the run still gives a leg room for a row slower than its median
        share[max(growable, key=lambda leg: (ends(leg, share[leg]), -legs.index(leg)))] += 1
    return share


def plan_suites(phases_by_suite: Mapping[str, Mapping[str, List[str]]], costs: Costs, total: int = RUNNERS,
                tails_by_suite: Optional[Mapping[str, Mapping[str, float]]] = None, drift_twice: bool = False) -> Dict[str, Shares]:
    """plan() over the legs of suites that run side by side (Identity model convergence and Identity takes, the owner
    2026-11-08: one account's 20 runners for both): {suite: {code: {...}}}, every (suite, country) leg one slot at least,
    each next slot to the leg that ends last, of all of them. A fixed split of the 20 between the suites ends later (10
    and 10: ~26 min against ~20 for one pool), as each suite's slowest country then waits on its own suite's slots."""
    tails_by_suite = tails_by_suite or {}
    legs = [(suite, c) for suite, by_code in phases_by_suite.items() for c in by_code]
    order = {(suite, c): longest_first(c, phases_by_suite[suite][c], costs, drift_twice) for suite, c in legs}
    minutes = {(suite, c): [cost(c, p, costs, drift_twice) for p in ps] for (suite, c), ps in order.items()}

    def ends(leg: Tuple[str, str], n: int) -> float:
        return round(makespan(minutes[leg], n) + (tails_by_suite.get(leg[0]) or {}).get(leg[1], 0.0), 1)

    share = _share_slots(legs, {leg: len(rows) for leg, rows in order.items()}, ends, total)
    out: Dict[str, Shares] = {suite: {} for suite in phases_by_suite}
    for suite, c in legs:
        out[suite][c] = {"phases": order[(suite, c)], "max-parallel": share[(suite, c)], "minutes": ends((suite, c), share[(suite, c)])}
    return out


def ends_at(shares: Shares) -> float:
    """When a plan's last country ends."""
    return max((s["minutes"] for s in shares.values()), default=0.0)


# ── the measured costs ────────────────────────────────────────────────────────────────────────────────────────────────


def read_costs(path: str = COSTS_FILE) -> Costs:
    out: Costs = {}
    with open(path, encoding="utf-8") as f:
        for line in f:
            if line.startswith("#") or line.startswith("code\t") or not line.strip():
                continue
            code, phase, minutes = line.rstrip("\n").split("\t")[:3]
            out[(code, phase)] = float(minutes)
    return out


def write_costs(path: str, costs: Mapping[Tuple[str, str], Tuple[float, int]], runs: List[str]) -> None:
    with open(path, "w", encoding="utf-8") as f:
        f.write("# Each convergence row's median job minutes (setup and post-steps included) over main's runs "
                f"{', '.join(runs)};\n# scripts/ci/convergence_rows.py `costs` writes it, `plan` reads it.\n")
        f.write("code\tphase\tminutes\truns\n")
        for (code, phase), (minutes, n) in sorted(costs.items()):
            f.write(f"{code}\t{phase}\t{minutes:.1f}\t{n}\n")


def _at(text: str) -> datetime.datetime:
    return datetime.datetime.fromisoformat(text.replace("Z", "+00:00"))


def costs_of(runs: List[List[Dict[str, Any]]], codes: Mapping[str, str]) -> Dict[Tuple[str, str], Tuple[float, int]]:
    """{(code, phase): (median minutes, rows measured)} over the runs' jobs (each run a list of jobs-API job dicts); a row
    named `leg (<country>) / <phase> (<country>)` on github.com, `<phase> (<country>)` elsewhere. Skipped rows and rows
    that never started weigh nothing."""
    seen = {}
    for jobs in runs:
        for job in jobs:
            name = job["name"].split(" / ")[-1]
            if " (" not in name or job.get("conclusion") in ("skipped", None) or not job.get("started_at"):
                continue
            phase, country = name[:-1].split(" (", 1)
            if country not in codes:
                continue
            minutes = (_at(job["completed_at"]) - _at(job["started_at"])).total_seconds() / 60
            seen.setdefault((codes[country], phase), []).append(minutes)
    return {k: (round(statistics.median(v), 1), len(v)) for k, v in seen.items()}


def describe(shares: Shares) -> List[str]:
    """The plan as lines: each country's share, predicted end and row order, then the total."""
    if not shares:
        return ["no cap: not main's suite (a takes-only run), every row at once"]
    lines = [f"{c}: max-parallel {s['max-parallel']}, ends ~{s['minutes']} min: {', '.join(s['phases'])}"
             for c, s in shares.items()]
    return lines + [f"{sum(s['max-parallel'] for s in shares.values())} runners; the rows end "
                    f"~{ends_at(shares)} min after they start"]
