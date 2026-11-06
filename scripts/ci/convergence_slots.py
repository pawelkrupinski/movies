#!/usr/bin/env python3
"""MAIN'S CONVERGENCE RUN IN 20 RUNNERS (the owner, 2026-11-06: "reduce convergence run to 20 runners at most and balance
the times"): GitHub Free runs 20 jobs at once for the whole account, and a run of ~43 rows queued every row at once, so
which rows started first was GitHub's pick -- a 20-minute drift row often started 10 minutes late, and the run ended
28-50 minutes after it began (runs 42112839746 .. 42225884509) for ~16 minutes of work a runner.

So the preflight hands each country's leg (country-convergence-leg.yml) its rows LONGEST FIRST (`phases`) and a
`max-parallel` (its SHARE of the 20): GitHub starts a matrix's rows in their listed order and the next one when a running
one ends, so each country runs a longest-processing-time list schedule on its own slots, and the run never holds more
than the shares' sum. The shares are chosen greedily: every country starts with one slot and the next slot goes to the
country whose rows end last (its order comparison, `order-compare`, after them) until the 20 are given out or another
slot would shorten no country. The rows, their checks and their names are the leg's own; only their order and how many
run at once change.

The costs are MEASURED: scripts/ci/convergence-row-minutes.tsv, each row's median job minutes (setup and post-steps
included: a row's slot is held for all of it) over main's recent complete runs. A row the file does not name weighs
UNMEASURED_MINUTES; a `drift-twice` run's drift rows weigh twice theirs. Refresh the file after a change that moves the
rows' times (`costs`), or the plan drifts from what the rows cost; a stale file only orders rows worse, it drops none.

  convergence_slots.py plan [--drift-twice]       the shares and predicted minutes of main's suite, from the costs
  convergence_slots.py costs --runs ID,ID,...     rewrites the costs file from those runs' jobs (gh api)

Used by convergence_reuse.py `legs`; tested by convergence_slots_test.py.
"""
from __future__ import annotations

import datetime
import json
import os
import statistics
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
COSTS_FILE = os.path.join(HERE, "convergence-row-minutes.tsv")
# GitHub Free's concurrent jobs for the whole account
RUNNERS = 20
# a leg's matrix with no cap of ours (GitHub's own ceiling on a matrix)
UNCAPPED = 256
# a row the costs file does not name: about a mid-sized row
UNMEASURED_MINUTES = 8.0
# the order comparison after a split country's rows (order-compare)
COMPARE_PHASES = ("order-independence-p01", "order-independence-p12")


# an unmeasured row that costs a multiple of a measured one: the A/A row replays the head's drift seed twice
# (harness_aa.py), and runs only on a push that changed the harness, too rarely to be measured
LIKE = {"takes-drift-aa": ("takes-drift-1", 2.0)}


def cost(code, phase, costs, drift_twice=False):
    like, times = LIKE.get(phase, (None, 1.0))
    minutes = costs.get((code, phase)) or (costs[(code, like)] * times if (code, like) in costs else UNMEASURED_MINUTES)
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
        if served in phases:
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


def plan(phases_by_code, costs, total=RUNNERS, tails=None, drift_twice=False):
    """{code: {"phases": [...longest first], "max-parallel": n, "minutes": predicted end}} -- the shares summing to at most
    `total` (one each at least), each next slot to the country that ends last."""
    tails = tails or {}
    order = {c: longest_first(c, ps, costs, drift_twice) for c, ps in phases_by_code.items()}
    minutes = {c: [cost(c, p, costs, drift_twice) for p in ps] for c, ps in order.items()}

    def ends(code, n):
        return round(makespan(minutes[code], n) + tails.get(code, 0.0), 1)

    share = {c: 1 for c in order}
    while sum(share.values()) < total:
        # a country can still end sooner with more slots (perhaps only with more than one: 4 rows of 10 end at 20 on 2 or 3)
        growable = [c for c in order if share[c] < len(order[c]) and ends(c, len(order[c])) < ends(c, share[c])]
        if not growable:
            break
        # a slot that cannot shorten the run still gives a country room for a row slower than its median
        share[max(growable, key=lambda c: (ends(c, share[c]), -list(order).index(c)))] += 1
    return {c: {"phases": order[c], "max-parallel": share[c], "minutes": ends(c, share[c])} for c in order}


# ── the measured costs ────────────────────────────────────────────────────────────────────────────────────────────────


def read_costs(path=COSTS_FILE):
    out = {}
    with open(path, encoding="utf-8") as f:
        for line in f:
            if line.startswith("#") or line.startswith("code\t") or not line.strip():
                continue
            code, phase, minutes = line.rstrip("\n").split("\t")[:3]
            out[(code, phase)] = float(minutes)
    return out


def write_costs(path, costs, runs):
    with open(path, "w", encoding="utf-8") as f:
        f.write("# Each convergence row's median job minutes (setup and post-steps included) over main's runs "
                f"{', '.join(runs)};\n# scripts/ci/convergence_slots.py `costs` writes it, `plan` reads it.\n")
        f.write("code\tphase\tminutes\truns\n")
        for (code, phase), (minutes, n) in sorted(costs.items()):
            f.write(f"{code}\t{phase}\t{minutes:.1f}\t{n}\n")


def _at(text):
    return datetime.datetime.fromisoformat(text.replace("Z", "+00:00"))


def costs_of(runs, codes):
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


def cmd_costs(run_ids):
    sys.path.insert(0, HERE)
    import convergence_reuse as reuse  # noqa: E402
    with open(os.path.join(ROOT, reuse.WORKFLOW)) as f:
        codes = dict(reuse.countries(f.read()))
    runs = []
    for run in run_ids:
        out = subprocess.run(["gh", "api", "--paginate", f"repos/{{owner}}/{{repo}}/actions/runs/{run}/jobs?per_page=100",
                              "--jq", ".jobs[]"], capture_output=True, text=True, check=True, cwd=ROOT).stdout
        runs.append([json.loads(line) for line in out.splitlines() if line.strip()])
    write_costs(COSTS_FILE, costs_of(runs, codes), run_ids)
    return 0


def cmd_plan(drift_twice):
    sys.path.insert(0, HERE)
    import convergence_reuse as reuse  # noqa: E402
    with open(os.path.join(ROOT, reuse.WORKFLOW)) as f:
        rows = reuse.matrix_rows(f.read())
    print("\n".join(describe(reuse.leg_plan(rows, reuse.leg_inputs({"INPUT_DRIFT_TWICE": "true" if drift_twice else ""}),
                                             "both"))))
    return 0


def describe(shares):
    """The plan as lines: each country's share, predicted end and row order, then the total."""
    if not shares:
        return ["no cap: not main's suite (a takes-only or drift-only run), every row at once"]
    lines = [f"{c}: max-parallel {s['max-parallel']}, ends ~{s['minutes']} min: {', '.join(s['phases'])}"
             for c, s in shares.items()]
    return lines + [f"{sum(s['max-parallel'] for s in shares.values())} of {RUNNERS} runners; the rows end "
                    f"~{max(s['minutes'] for s in shares.values())} min after they start"]


def main(argv):
    if argv[:1] == ["plan"]:
        return cmd_plan("--drift-twice" in argv[1:])
    if argv[:2] == ["costs", "--runs"] and len(argv) == 3:
        return cmd_costs([r for r in argv[2].split(",") if r])
    print(__doc__, file=sys.stderr)
    return 64


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
