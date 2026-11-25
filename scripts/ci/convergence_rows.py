#!/usr/bin/env python3
# Purpose: The rows main's convergence legs run, and the 20-runner plan that orders them (convergence_reuse.py's matrix)
"""THE ROWS MAIN'S CONVERGENCE LEGS RUN (split out of convergence_reuse.py): the `leg` matrix of each suite from the workflow's
LEG_ROWS, the `<phase> (<country>)` rows it expands to (expected_rows), and the plan that hands each country its rows longest
first and its share of the 20 runners (convergence_slots.py). Pure over its arguments: it reads no run and no commit.

  convergence_rows.py plan [--drift-twice]       the shares and predicted minutes of main's suite, from the costs
  convergence_rows.py costs --runs ID,ID,...     rewrites convergence_slots's costs file from those runs' jobs (gh api)
"""
from __future__ import annotations

import json
import os
import re
import sys
from typing import Any, Dict, List, Mapping, Optional, Tuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import convergence_slots  # noqa: E402
import drift_seeds  # noqa: E402
import gh_bounded  # noqa: E402
from convergence_forge import ROOT, SEED_KEYS, WORKFLOW, Unproven  # noqa: E402

Row = Dict[str, Any]
Matrix = Dict[str, List[Any]]
Inputs = Mapping[str, str]
Shares = Dict[str, Dict[str, Any]]

# The leg's own expansion of a country into rows (country-convergence-leg.yml's `phase:` matrix), unless main's plan hands
# the rows in their start order (`phases`, matrix_phases() longest first). expected_rows() models exactly this; ConvergenceReuseSpec fails the moment the leg's expression is anything else.
PHASES_EXPRESSION = (
    "${{ fromJson(inputs.phases || inputs.drift-only && (inputs.drift-seed-1 == '' && inputs.drift-seed-2 != '' && '[\"takes-drift-2\"]' || "
    "inputs.drift-seed-2 == '' && inputs.drift-seed-1 != '' && '[\"takes-drift-1\"]' || '[\"takes-drift-1\",\"takes-drift-2\"]') || "
    "inputs.takes-only && '[\"takes\",\"takes-base\"]' || format('[\"convergence\"{0}{1}{2}{3}]', "
    "inputs.order-command != '' && (inputs.order-split && (inputs.order-one-phase && "
    "',\"order-independence-p01-pass0\",\"order-independence-p01-pass1\"' || ',\"order-independence-p01-pass0\","
    "\"order-independence-p01-pass1\",\"order-independence-p12-pass1\",\"order-independence-p12-pass2\"') || "
    "',\"order-independence\"') || '', "
    "inputs.next-day-command != '' && ',\"next-day\"' || '', inputs.mode == 'hermetic' && inputs.takes-row && "
    "',\"takes\",\"takes-base\",\"takes-drift-1\",\"takes-drift-2\"' || '', inputs.mode == 'hermetic' && "
    "inputs.takes-row && inputs.harness-aa && ',\"takes-drift-aa-pass0\",\"takes-drift-aa-pass1\",\"takes-aa\"' || '')) }}"
)
# A split country's order rows on main: each pass in a row of its own, then the two comparisons (the callers' `order-independence-p01|p12` jobs).
ORDER_SPLIT_ROWS = ["order-independence-p01-pass0", "order-independence-p01-pass1", "order-independence-p12-pass1",
                    "order-independence-p12-pass2", "order-independence-p01", "order-independence-p12"]


def _true(value: object) -> bool:
    return str(value).lower() == "true"


def _splits_order(row: Row) -> bool:
    return bool(row.get("order", "")) and _true(row.get("order-split", "false"))


def matrix_rows(workflow_text: str) -> List[Row]:
    """Each `include:` row of the `leg` matrix as a dict (the one-line flow mappings the workflow writes)."""
    rows: List[Row] = []
    for m in re.finditer(r"^\s*-\s*\{(.*)\}\s*$", workflow_text, re.M):
        row: Row = {}
        for k, v in re.findall(r"([\w-]+):\s*('[^']*'|[^,]*)", m.group(1)):
            row[k] = v.strip().strip("'")
        if "country" in row and "cmd" in row:
            rows.append(row)
    return rows


def expected_rows(workflow_text: str, suite: str = "convergence") -> List[str]:
    """`<phase> (<country>)` of every row main's run of `suite` expands to: PHASES_EXPRESSION with takes-only false,
    hermetic and both order phases (main's order-phases.sh answer) -- for convergence its convergence, order and next-day
    rows and the order comparisons, for takes the takes check's and every drift seed's rows. Not the harnesses' A/A rows
    (`takes-drift-aa` and its two passes, `takes-aa`, harness_aa.py): only a push that changed the harness runs it, and no
    verdict reused from a landing check is one -- a reused run skips it with every other row."""
    out: List[str] = []
    for row in matrix_rows(workflow_text):
        phases = matrix_phases(row, suite=suite)
        if suite == "convergence" and _splits_order(row):
            # a split country's passes are compared by the callers' `order-independence-p01|p12` jobs, a row per pair named as the
            # two-pass row was
            phases += [p for p in ORDER_SPLIT_ROWS if "-pass" not in p]
        out += [f"{p} ({row['country']})" for p in phases]
    if not out:
        raise Unproven("no `leg` matrix rows in the workflow")
    return out


def countries(workflow_text: str) -> List[Tuple[str, str]]:
    return [(r["country"], r["code"]) for r in matrix_rows(workflow_text)]


def _scalar_kind(text: str) -> Any:
    if text in ("true", "false"):
        return text == "true"
    for kind in (int, float):
        try:
            return kind(text)
        except ValueError:
            pass
    return text


def scalar(text: str) -> Any:
    """A flow-map value as YAML types it: quotes make a string, `true`/`false` a boolean, digits a number."""
    text = text.strip()
    if len(text) >= 2 and text[0] == text[-1] and text[0] in "'\"":
        return text[1:-1]
    return _scalar_kind(text)


def parse_rows(text: str) -> List[Row]:
    """The `- { key: value, ... }` rows of a matrix written as flow maps (values hold no comma or brace), as dicts."""
    rows: List[Row] = []
    for m in re.finditer(r"-\s*\{([^}]*)}", text):
        row: Row = {}
        for field in m.group(1).split(","):
            key, sep, value = field.partition(":")
            if sep:
                row[key.strip()] = scalar(value)
        rows.append(row)
    return rows


def _kept_rows(rows: List[Row], inputs: Inputs) -> List[Row]:
    partial = inputs.get("takes-only") == "true" or inputs.get("drift-only") == "true"
    wanted = {c.strip() for c in inputs.get("takes-countries", "").split(",") if c.strip()}
    return [r for r in rows if not (partial and wanted) or r["code"] in wanted]


def leg_matrix(rows: List[Row], inputs: Inputs, suite: str = "convergence") -> Matrix:
    """The `leg` job's matrix (identity-model-convergence.yml, identity-takes.yml) as JSON-able data: a `country` vector the
    rows extend, so a row is named `leg (<country>)` by default, holding only the countries this run starts -- a takes-only
    or drift-only takes run with `takes-countries` keeps those, and a convergence run asked for takes rows alone (the
    rows are the takes run's) none. A country left out is no row at all: a matrix job skipped for it would show its
    `name:` template raw in the run graph (`${{ matrix.phase }} (...)`)."""
    partial = inputs.get("takes-only") == "true" or inputs.get("drift-only") == "true"
    if suite == "convergence" and partial:
        return {"country": [], "include": []}
    kept = _kept_rows(rows, inputs)
    return {"country": [r["country"] for r in kept], "include": kept}


def order_matrix(matrix: Matrix, suite: str = "convergence") -> Matrix:
    """The order-comparison jobs' matrix (`order-independence-p01|p12 (<country>)` in identity-model-convergence.yml and
    verify-scrape-recording.yml): the `leg` matrix's countries that split their order replay into pass rows. A country with
    none gets no row, and a suite that hands its legs no order command (Identity takes) none at all -- the jobs are then
    skipped under their plain names, never a raw `${{ ... }}` one."""
    kept = [r for r in matrix["include"] if suite == "convergence" and _splits_order(r)]
    return {"country": [r["country"] for r in kept], "include": kept}


def matrix_phases(row: Row, order_phases: str = "both", harness_aa: bool = False, suite: str = "convergence") -> List[str]:
    """The `convergence` matrix rows main's leg for `row` expands to in `suite` (PHASES_EXPRESSION, hermetic, neither
    takes-only nor drift-only; the A/A rows when the preflight decided them), in the workflow's order: convergence's
    convergence, order passes and next-day, takes' takes, takes-base and drift rows. The comparison jobs after them
    (the order comparisons, the A/A passes' `takes-drift-aa`) are not among them."""
    if suite == "takes":
        return (["takes", "takes-base"] + drift_phases(row["code"])
                + (["takes-drift-aa-pass0", "takes-drift-aa-pass1", "takes-aa"] if harness_aa else []))
    phases = ["convergence"]
    if row.get("order", ""):
        if _true(row.get("order-split", "false")):
            phases += [p for p in ORDER_SPLIT_ROWS if "-pass" in p and (order_phases != "one" or "-p01-" in p)]
        else:
            phases.append("order-independence")
    if row.get("next-day", ""):
        phases.append("next-day")
    return phases


def drift_phases(code: str, pins: str = "") -> List[str]:
    """The country's takes-drift rows (drift_seeds.rows_of): 1 and 2, and any further row its fixed seeds are spread
    over; a pinned dispatch's pinned rows alone."""
    return [f"takes-drift-{r}" for r in drift_seeds.rows_of(code, pins=pins)]


def suite_tails(rows: List[Row], suite: str, order_phases: str = "both", harness_aa: bool = False,
                costs: Optional[convergence_slots.Costs] = None) -> Dict[str, float]:
    """What follows a country's rows in `suite`'s comparison job: convergence's order comparisons (a split country),
    takes' A/A comparison (when the preflight decided the A/A rows)."""
    costs = convergence_slots.read_costs() if costs is None else costs
    if suite == "takes":
        return {r["code"]: convergence_slots.AA_COMPARE_MINUTES for r in rows} if harness_aa else {}
    compares = convergence_slots.COMPARE_PHASES[:1] if order_phases == "one" else convergence_slots.COMPARE_PHASES
    return {r["code"]: max(convergence_slots.cost(r["code"], p, costs) for p in compares)
            for r in rows if _splits_order(r)}


def main_plan(rows: List[Row], inputs: Inputs, order_phases: str = "both", harness_aa: bool = False) -> Dict[str, Shares]:
    """{suite: {code: share}}: both of main's suites in the account's 20 runners at once (convergence_slots.plan_suites).
    Convergence's preflight dispatches the takes run as it starts, so the two run side by side; each preflight plans both
    from the same rows and costs and keeps its own suite's part. The other suite is planned as main runs it (both order
    phases, no A/A row) whatever this run's own order phases or A/A decision: what it does not know it plans as main's."""
    costs = convergence_slots.read_costs()
    twice = inputs.get("drift-twice") == "true"
    phases = {"convergence": {r["code"]: matrix_phases(r, order_phases, suite="convergence") for r in rows},
              "takes": {r["code"]: matrix_phases(r, harness_aa=harness_aa, suite="takes") for r in rows}}
    tails = {"convergence": suite_tails(rows, "convergence", order_phases, costs=costs),
             "takes": suite_tails(rows, "takes", harness_aa=harness_aa, costs=costs)}
    return convergence_slots.plan_suites(phases, costs, tails_by_suite=tails, drift_twice=twice)


def leg_plan(rows: List[Row], inputs: Inputs, order_phases: str = "both", harness_aa: bool = False,
             suite: str = "convergence") -> Shares:
    """Main's `suite` in its part of the 20 runners (main_plan): each country's rows longest first and its share. A
    drift-only takes run plans its takes-drift rows alike (every row a country has, or the pinned ones) in all 20 (no
    convergence run beside it); a takes-only run gets {} (two rows a country: the leg's own expression, no cap)."""
    if suite == "takes" and inputs.get("takes-only") == "true":
        return {}
    if suite == "takes" and inputs.get("drift-only") == "true":
        pins = ",".join(inputs.get(k, "") for k in SEED_KEYS)
        return convergence_slots.plan({r["code"]: drift_phases(r["code"], pins) for r in rows},
                                      convergence_slots.read_costs(), drift_twice=inputs.get("drift-twice") == "true")
    return main_plan(rows, inputs, order_phases, harness_aa)[suite]


def _planned_row(row: Row, shares: Shares) -> Row:
    if not shares:
        return dict(row, **{"phases": "", "max-parallel": convergence_slots.UNCAPPED})
    share = shares[row["code"]]
    return dict(row, **{"phases": json.dumps(share["phases"], separators=(",", ":")), "max-parallel": share["max-parallel"]})


def planned_matrix(rows: List[Row], inputs: Inputs, order_phases: str = "both", harness_aa: bool = False,
                   suite: str = "convergence") -> Matrix:
    """leg_matrix with each row's `phases` (JSON, longest first; empty: the leg's own expression) and `max-parallel`."""
    matrix = leg_matrix(rows, inputs, suite)
    shares = leg_plan(matrix["include"], inputs, order_phases, harness_aa, suite) if matrix["include"] else {}
    matrix["include"] = [_planned_row(r, shares) for r in matrix["include"]]
    return matrix


def recorded_matrix(rows: List[Row], codes: str) -> Matrix:
    """verify-scrape-recording.yml's `leg` matrix: the full convergence leg of each recorded country (`codes`,
    comma-separated), in the rows' order, each on the leg's own phases expression and uncapped. Not a takes-only
    narrowing: the convergence suite answers that with no row (Verify scrape recording run 42524021121 started no leg)."""
    wanted = {c.strip() for c in codes.split(",") if c.strip()}
    kept = [dict(r, phases="", **{"max-parallel": convergence_slots.UNCAPPED}) for r in rows if r["code"] in wanted]
    return {"country": [r["country"] for r in kept], "include": kept}


def leg_rows_text(workflow_text: str) -> str:
    """The convergence preflight's LEG_ROWS block, as written: the takes preflight's rows. parse_rows types its values as
    YAML would (a number a number, `true` a boolean) -- matrix_rows' strings, handed to the leg's number and boolean
    inputs, made GitHub drop the call (Identity takes #1: no `leg` job at all)."""
    if "LEG_ROWS: |" not in workflow_text:
        raise ValueError("no LEG_ROWS block in the convergence workflow")
    return workflow_text.split("LEG_ROWS: |", 1)[1].split("run:", 1)[0]


# ── the slots tool's two commands (they read these rows, so they live here and convergence_slots imports nothing of them) ─


def cmd_costs(run_ids: List[str]) -> int:
    with open(os.path.join(ROOT, WORKFLOW)) as f:
        codes = dict(countries(f.read()))
    runs: List[List[Dict[str, Any]]] = []
    for run in run_ids:
        out = gh_bounded.gh(["api", "--paginate", f"repos/{{owner}}/{{repo}}/actions/runs/{run}/jobs?per_page=100",
                             "--jq", ".jobs[]"], cwd=ROOT)
        runs.append([json.loads(line) for line in out.splitlines() if line.strip()])
    convergence_slots.write_costs(convergence_slots.COSTS_FILE, convergence_slots.costs_of(runs, codes), run_ids)
    return 0


def cmd_plan(drift_twice: bool) -> int:
    with open(os.path.join(ROOT, WORKFLOW)) as f:
        rows = matrix_rows(f.read())
    plans = main_plan(rows, {"drift-twice": "true" if drift_twice else "false"})
    for suite, shares in plans.items():
        print(f"{suite}:")
        print("\n".join(f"  {line}" for line in convergence_slots.describe(shares)))
    print(f"{sum(s['max-parallel'] for shares in plans.values() for s in shares.values())} of {convergence_slots.RUNNERS} "
          "runners for both")
    return 0


def main(argv: List[str]) -> int:
    if argv[:1] == ["plan"]:
        return cmd_plan("--drift-twice" in argv[1:])
    if argv[:2] == ["costs", "--runs"] and len(argv) == 3:
        return cmd_costs([r for r in argv[2].split(",") if r])
    print(__doc__, file=sys.stderr)
    return 64


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
