#!/usr/bin/env python3
# Purpose: Main reuses a convergence verdict it can prove applies, instead of running the suite again
# MAIN REUSES A CONVERGENCE VERDICT IT CAN PROVE APPLIES: a commit an agent's landing check already ran the whole
# `Identity model convergence` suite over is not run again on main (identity-model-convergence.yml's `reuse-run`).
#
# Main's kick (.github/scripts/kick-convergence.sh) used to dispatch the full suite -- 17 rows, ~5 runner-hours -- for a
# commit whose identical tree a landing check (scripts/identity-landing-check --full, on a measure/ branch) had just run
# it over. It still dispatches, so the gate (the pre-push red gate, `agentctl ci-why`) reads a run of main, but names the
# source run; this run's preflight checks the source against the commit and, only when every check holds, skips every leg
# and goes green, its summary naming the source. Anything it cannot prove is the full suite, as before.
#
# WHAT MUST HOLD (judge(), each a reason when it does not):
#   - THE TREE: the key (key()) of the source's commit equals this commit's. It hashes every path
#     .github/convergence-paths.txt lists, as that commit's own copy lists them -- the file the kick's path gate reads, and
#     ConvergenceDispatchGateSpec holds it to every file the convergence workflows run, the workflows included.
#   - THE PAIR: each country's pinned recording and the fills published over it (convergence-setup.sh `pinned`,
#     convergence-fill.sh `fills`), as the source's preflight read them, are what this run would replay now.
#   - THE INPUTS: what the source's legs were handed (its preflight's outputs, `inputs` below) is what main's are: hermetic
#     (the workflow's own), every country and row (no takes-only, drift-only, takes-countries, recording), the takes skip gate in shadow
#     or off, no runner or JVM options, both order-independence phases (scripts/ci/order-phases.sh: off main a ref may run
#     p01 alone). takes-no-store and a JFR profile change no verdict.
#   - EVERY ROW GREEN: the source run succeeded, and every row main's matrix runs (expected_rows(): the phases the leg
#     expands each country to) and every `leg (<country>)` caller ran and succeeded in it -- read from the run's jobs, not
#     its conclusion alone (a matrix of reusable calls takes the WORST row's result, and a skipped row is no verdict). A
#     reuse run itself skips its rows, so it is never a source.
#   - THE TAKES BASE: each country's takes check judged the head against a base (its `identity takes (<code>)` status:
#     "base <sha> vs head <sha>"); main's would take the newest commit before this one whose check passed (the takes
#     check's pick_base). Same commit, or the same key.
#   - THIS RUN asks for main's suite itself: its own inputs pass the same test.
#   - THE DRIFT SEEDS ARE THE SOURCE'S (rotating seeds, scripts/ci/drift_seeds.py): the source's takes-drift rows replayed
#     ITS run's pair, not this run's -- a reuse is the verdict of the seeds that ran, never of this run's as if equal.
#     The source's facts name them (`drift-seed-1`/`-2` in its inputs; a run from before the rotation replayed 1 and 2),
#     `verify` prints them (`drift-seeds-judged=S1,S2`) and says them in the summary, and this run's facts name no seed
#     of its own (it judged none: `drift_seeds_from` names the source).
#
# TWO SUITES (the owner, 2026-11-08): Identity model convergence (convergence, the order rows, next-day) and Identity takes
# (takes, takes-base, the takes-drift rows) each judge a reuse of their own rows (--suite, else $CONVERGENCE_SUITE, else
# convergence). Main's kick names a CONVERGENCE run; the takes run's preflight judges the takes run that one dispatched
# (takes_child, by its title `Identity takes (convergence run <id>)`). Only a takes run judges takes bases, and only it
# has the NO FALSE GREEN check (`complete`). Both suites' rows are planned in ONE pool of 20 runners (main_plan).
#
# usage:
#   convergence_reuse.py key [REV]                      the key of REV (default HEAD)
#   convergence_reuse.py inputs                         the legs' inputs from the dispatch's (env INPUT_*): `name=value`
#                                                       lines for $GITHUB_OUTPUT, and the JSON to $LEG_INPUTS_FILE
#   convergence_reuse.py facts --sha SHA --inputs F [--order-phases P] [--reused-from RUN]
#                                                       this run's facts (the artifact `convergence-reuse-facts`)
#   convergence_reuse.py verify --source RUN --sha SHA --inputs F [--order-phases P] [--suite S]
#                                                       prints `reused-from=RUN` when RUN's verdict holds, nothing when not
#                                                       (each reason on stderr and in $GITHUB_STEP_SUMMARY); copies the
#                                                       source's statuses onto SHA when it is another commit
#   convergence_reuse.py find --sha SHA                 the newest source run whose facts name SHA's key, or nothing (the
#                                                       kick's hint: verify decides)
#   convergence_reuse.py expected-rows [REV] [--suite S]  the rows main's matrix runs, `<phase> (<country>)` a line
#   convergence_reuse.py takes-run --parent RUN         the Identity takes run convergence run RUN dispatched (exit 1: none)
#   convergence_reuse.py complete --run RUN --sha SHA [--reused-from RUN] [--inputs F] [--order-phases P]
#                                                       NO FALSE GREEN (below): exit 1, each reason on stderr, when a takes
#                                                       or takes-drift row of RUN was skipped, cancelled or never ran and
#                                                       no proven reuse names a source
#   convergence_reuse.py judge --run F --jobs F --facts F --mine F
#                                                       judge() over JSON files: each reason a line, none when it holds
#
# NO FALSE GREEN (convergence-failures 2026-10-29, G7): a main run whose takes or takes-drift rows did not run (run 1747:
# its takes rows skipped, the run green, an 18-hour red episode read as two) is no verdict, and must never count green for
# THE GATE. Its `verdict complete` job (identity-model-convergence.yml) runs incomplete() over the run's own jobs and
# fails the run when any of those rows has no decided conclusion -- unless the preflight's `reused-from` names a run this
# check proved (verify), whose rows ran. A takes row whose skip gate left out its diff (takes_skip_gate.py) still ran
# and concluded: the gate's own proof stands.
# env: GH_REPO, GH_TOKEN (gh's own), FIXTURE_RELEASE_TAG (the pairs). Tested by convergence_reuse_test.py and
# ConvergenceReuseSpec.
# The reads, the matrix and the judgement live in convergence_forge.py, convergence_rows.py and convergence_judge.py; this
# file is the commands over them, and re-exports their names (the tests and scripts/identity_drift_ci_runs.py read them here).


from __future__ import annotations

import datetime
import json
import os
import subprocess
import sys
from typing import Any, Callable, Dict, List, Mapping, NamedTuple, Optional, Tuple

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import drift_seeds as drift_seeds  # noqa: E402,F401
from convergence_forge import PATHS_FILE as PATHS_FILE  # noqa: E402,F401
from convergence_forge import ROOT, SEED_KEYS, SUITE_FILES as SUITE_FILES, SUITES as SUITES, WORKFLOW as WORKFLOW  # noqa: E402,F401
from convergence_forge import TAKES_WORKFLOW_FILE, WORKFLOW_FILE, Status, StatusCache, Unproven as Unproven  # noqa: E402,F401
from convergence_forge import copy_statuses, key as key, listed as listed, main_base, same_tree  # noqa: E402,F401
from convergence_forge import gh, pair_of, repo, patterns_of as patterns_of, run_facts, run_jobs, run_json, source_base  # noqa: E402,F401
from convergence_forge import takes_child, under, workflow_at as workflow_at  # noqa: E402,F401
from convergence_judge import Facts, Mine  # noqa: E402,F401
from convergence_judge import drift_rows as drift_rows, incomplete as incomplete, inputs_unlike_main as inputs_unlike_main  # noqa: E402,F401
from convergence_judge import jobs_by_name as jobs_by_name, judge as judge, seeds_of as seeds_of  # noqa: E402,F401
from convergence_rows import PHASES_EXPRESSION as PHASES_EXPRESSION, Matrix, Row  # noqa: E402,F401
from convergence_rows import countries as countries, drift_phases as drift_phases, expected_rows as expected_rows  # noqa: E402,F401
from convergence_rows import leg_matrix as leg_matrix, leg_plan as leg_plan, leg_rows_text, main_plan as main_plan  # noqa: E402,F401
from convergence_rows import matrix_phases as matrix_phases, matrix_rows as matrix_rows, order_matrix as order_matrix  # noqa: E402,F401
from convergence_rows import parse_rows as parse_rows, planned_matrix as planned_matrix, recorded_matrix  # noqa: E402,F401
from convergence_rows import suite_tails as suite_tails  # noqa: E402,F401
import ci_machine  # noqa: E402
import convergence_slots  # noqa: E402

_under = under
FIND_RUNS = 30
FIND_HOURS = 24
Opts = Dict[str, str]
Handler = Callable[[Opts, List[str]], int]

USAGE = ("usage: convergence_reuse.py key|inputs|facts|verify|find|complete|legs|drift-rows|expected-rows|takes-run|judge ... "
         "(see the header)")

# ── the inputs ────────────────────────────────────────────────────────────────────────────────────────────────────────

# name -> (environment variable the preflight sets, default when the dispatch names none)
INPUTS = {
    "profile": ("INPUT_PROFILE", "false"),
    "jvm-options": ("INPUT_JVM_OPTIONS", ""),
    "runner": ("INPUT_RUNNER", ""),
    "takes-base": ("INPUT_TAKES_BASE", ""),
    "takes-only": ("INPUT_TAKES_ONLY", "false"),
    "drift-only": ("INPUT_DRIFT_ONLY", "false"),
    "drift-seeds": ("INPUT_DRIFT_SEEDS", ""),
    "recording": ("INPUT_RECORDING", ""),
    "takes-countries": ("INPUT_TAKES_COUNTRIES", ""),
    "takes-no-store": ("INPUT_TAKES_NO_STORE", "false"),
    "takes-skip": ("INPUT_TAKES_SKIP", "shadow"),
    "drift-twice": ("INPUT_DRIFT_TWICE", "false"),
    "drift-explore": ("INPUT_DRIFT_EXPLORE", ""),
}
BOOLEANS = {"profile", "takes-only", "drift-only", "takes-no-store", "drift-twice"}


def leg_inputs(env: Mapping[str, str]) -> Dict[str, str]:
    """The legs' inputs from the dispatch's, normalised: a boolean is `true` or `false` (a dispatch's arrives through the
    environment as the STRING "true"), an empty value its default."""
    out: Dict[str, str] = {}
    for name, (var, default) in INPUTS.items():
        value = (env.get(var) or "").strip()
        if name in BOOLEANS:
            value = "true" if value.lower() == "true" else "false"
        out[name] = value or default
    return out


def run_seeds(env: Mapping[str, str], inputs: Mapping[str, str]) -> Dict[str, str]:
    """This run's two takes-drift pins (drift_seeds.resolve): the dispatch's, or none -- each row then replays its fixed set."""
    one, two = drift_seeds.resolve(int(env.get("GITHUB_RUN_NUMBER") or 1), inputs.get("drift-seeds", ""),
                                   inputs.get("drift-only") == "true")
    return {"drift-seed-1": str(one or ""), "drift-seed-2": str(two or "")}


def pairs(workflow_text: str) -> Dict[str, str]:
    """Each country's pair as a hermetic leg would resolve it now: the pin's line, the fills published over its tree."""
    return {code: pair_of(code) for _, code in countries(workflow_text)}


# ── commands ──────────────────────────────────────────────────────────────────────────────────────────────────────────


def read_json(path: str) -> Any:
    with open(path) as f:
        return json.load(f)


def summary(lines: List[str]) -> None:
    path = ci_machine.environment().get("GITHUB_STEP_SUMMARY")
    if path:
        with open(path, "a") as f:
            f.write("\n".join(lines) + "\n")


def suite_of(opts: Optional[Mapping[str, str]] = None) -> str:
    """The suite a command judges: --suite, else $CONVERGENCE_SUITE, else convergence."""
    suite = (opts or {}).get("suite") or ci_machine.environment().get("CONVERGENCE_SUITE") or "convergence"
    if suite not in SUITES:
        raise ValueError(f"suite {suite!r}: one of {', '.join(SUITES)}")
    return suite


def cmd_inputs(_: Opts) -> int:
    inputs = leg_inputs(ci_machine.environment())
    try:
        inputs.update(run_seeds(ci_machine.environment(), inputs))
    except ValueError as e:
        print(f"::error title=drift seeds::{e}", file=sys.stderr)
        return 2
    for name, value in inputs.items():
        print(f"{name}={value}")
    pinned = f", pinned {inputs['drift-seeds']!r}" if inputs["drift-seeds"] else ", each row its fixed set"
    summary([f"Takes-drift seeds this run: takes-drift-1 replays seed {inputs['drift-seed-1'] or '(none)'}, takes-drift-2 "
             f"seed {inputs['drift-seed-2'] or '(none)'} (run #{ci_machine.environment().get('GITHUB_RUN_NUMBER', '?')}"
             f"{pinned}; scripts/ci/drift_seeds.py)"])
    if ci_machine.environment().get("LEG_INPUTS_FILE"):
        with open(ci_machine.environment()["LEG_INPUTS_FILE"], "w") as f:
            json.dump(inputs, f, sort_keys=True)
    return 0


def cmd_complete(opts: Opts) -> int:
    try:
        why = incomplete(run_jobs(opts["run"]), expected_rows(workflow_at(opts["sha"]), "takes"), opts.get("reused-from", ""),
                         read_json(opts["inputs"]) if opts.get("inputs") else None, opts.get("order-phases", "both"))
    except (Unproven, subprocess.CalledProcessError, OSError, ValueError, KeyError) as e:
        why = [f"could not read the run's rows: {e}"]
    if not why:
        print(f"convergence-reuse: run {opts['run']}: every takes and takes-drift row decided")
        return 0
    for reason in why:
        print(f"::error title=no verdict::{reason} -- a run whose takes rows did not run is no green (G7)", file=sys.stderr)
    summary(["### No verdict: this run's takes rows did not all run", "",
             "A green run must have judged every takes and takes-drift row (or reused a proven run's verdict).", ""]
            + [f"- {w}" for w in why])
    return 1


def _leg_matrix_of(rows: List[Row], suite: str) -> Matrix:
    """The `leg` matrix of a normal dispatch (planned from the legs' inputs file), its plan on the step summary."""
    inputs = read_json(ci_machine.environment()["LEG_INPUTS_FILE"])  # the file `inputs` wrote
    order_phases = ci_machine.environment().get("ORDER_PHASES") or "both"
    harness_aa = ci_machine.environment().get("HARNESS_AA") == "true"
    matrix = planned_matrix(rows, inputs, order_phases, harness_aa, suite)
    if matrix["include"]:
        summary([f"Rows longest first, each country's share of the runners ({suite}; scripts/ci/convergence_slots.py):", ""]
                + [f"- {line}" for line in
                   convergence_slots.describe(leg_plan(matrix["include"], inputs, order_phases, harness_aa, suite))])
    return matrix


def _workflow_leg_rows() -> str:
    with open(os.path.join(ROOT, WORKFLOW)) as f:
        return leg_rows_text(f.read())


def cmd_legs(opts: Opts) -> int:
    """The `leg` matrix (leg_matrix) of the suite ($CONVERGENCE_SUITE), from LEG_ROWS (the rows as written in the workflow;
    unset: the convergence workflow's own, which the takes and verify preflights read) and the dispatch's inputs; for
    verify-scrape-recording.yml, LEG_CODES (the recording's complete countries, comma-separated) instead (recorded_matrix),
    and no row is an error: GitHub drops a reusable call over an empty matrix without a word."""
    suite = suite_of(opts)
    rows = parse_rows(ci_machine.environment().get("LEG_ROWS") or _workflow_leg_rows())
    codes = ci_machine.environment().get("LEG_CODES")
    if codes is not None:
        matrix = recorded_matrix(rows, codes)
        if not matrix["include"]:
            print(f"::error::no leg row for the recorded countries {codes!r} (rows: "
                  f"{','.join(r['code'] for r in rows)})", file=sys.stderr)
            return 1
    else:
        matrix = _leg_matrix_of(rows, suite)
    print(f"leg-matrix={json.dumps(matrix, separators=(',', ':'))}")
    print(f"any-leg={'true' if matrix['country'] else 'false'}")
    order = order_matrix(matrix, suite)
    print(f"order-matrix={json.dumps(order, separators=(',', ':'))}")
    print(f"any-order={'true' if order['country'] else 'false'}")
    return 0


def cmd_facts(opts: Opts) -> int:
    workflow = workflow_at(opts["sha"])
    inputs = read_json(opts["inputs"])
    facts: Dict[str, Any] = {"version": 1, "suite": suite_of(opts), "run_id": ci_machine.environment().get("GITHUB_RUN_ID", ""),
                             "sha": opts["sha"], "key": key(opts["sha"]), "inputs": inputs,
                             "order_phases": opts.get("order-phases", "both"), "reused_from": opts.get("reused-from", "")}
    if facts["reused_from"]:
        # a reusing run judged no seed of its own: the verdict it carries is the source's seeds' (seeds_of the source)
        facts["inputs"] = {k: v for k, v in inputs.items() if k not in SEED_KEYS}
        facts["drift_seeds_from"] = facts["reused_from"]
    try:
        facts["pairs"] = {} if inputs.get("recording") else pairs(workflow)
    except Unproven as e:
        print(f"convergence-reuse: {e} -- no run can reuse this one", file=sys.stderr)
        facts["pairs"] = None
    json.dump(facts, sys.stdout, sort_keys=True, indent=1)
    print()
    return 0


class Verdict(NamedTuple):
    """What `verify` read: why the source's verdict does not hold (empty: it does), the run it judged (a takes run's id for
    the takes suite), that run, its facts and what this run computed."""
    why: List[str]
    run_id: str
    run: Status
    facts: Optional[Facts]
    mine: Mine


def _source_run_id(run_id: str, suite: str) -> str:
    """The run whose rows are judged: the named one, or -- for the takes suite, whose reuse source names a convergence run
    (main's kick finds those) -- the takes run it dispatched."""
    if suite != "takes":
        return run_id
    child = run_id if str(run_json(run_id).get("path", "")).endswith(TAKES_WORKFLOW_FILE) else takes_child(run_id)
    if not child:
        raise Unproven(f"convergence run {run_id} dispatched no Identity takes run that is listed")
    return child


def _takes_bases(workflow: str, sha: str, run: Status, suite: str) -> Dict[str, Tuple[Optional[str], Optional[str], bool]]:
    cache: StatusCache = {}
    bases: Dict[str, Tuple[Optional[str], Optional[str], bool]] = {}
    for _, code in countries(workflow) if suite == "takes" else []:
        theirs = source_base(code, run.get("head_sha", ""), run, cache)
        main = main_base(code, sha, cache)
        bases[code] = (theirs, main, same_tree(theirs, main))
    return bases


def _mine_of(run: Status, sha: str, suite: str, opts: Opts) -> Mine:
    workflow = workflow_at(sha)
    bases = _takes_bases(workflow, sha, run, suite)
    return {"key": key(sha), "pairs": pairs(workflow), "inputs": read_json(opts["inputs"]),
            "order_phases": opts.get("order-phases", "both"), "expected_rows": expected_rows(workflow, suite),
            "takes_bases": bases, "suite": suite}


def _judged(opts: Opts, suite: str) -> Verdict:
    run_id = opts["source"]
    try:
        run_id = _source_run_id(run_id, suite)
        run = run_json(run_id)
        mine = _mine_of(run, opts["sha"], suite, opts)
        source_facts = run_facts(run_id)
        return Verdict(judge(run, run_jobs(run_id), source_facts, mine), run_id, run, source_facts, mine)
    except (Unproven, subprocess.CalledProcessError, OSError, ValueError, KeyError) as e:
        return Verdict([f"could not check: {e}"], run_id, {}, None, {})


def _announce_reuse(verdict: Verdict) -> None:
    run, run_id, mine = verdict.run, verdict.run_id, verdict.mine
    url = run.get("html_url") or run_id
    theirs, own = seeds_of((verdict.facts or {}).get("inputs")), seeds_of(mine["inputs"])
    summary([f"### Reused run {run_id}'s verdict ({url})", "",
             f"Run {run_id} ran every row this run would over the same tree (key {mine['key'][:12]}), pairs, inputs and "
             f"takes bases, at {str(run.get('head_sha'))[:9]}, and every row passed: no leg runs here.", "",
             f"Its takes-drift rows replayed seeds {theirs} (its run's): the drift verdict reused is those seeds', not this "
             f"run's own rotation ({own}), which no row replayed."])
    print(f"reused-from={run_id}")
    print(f"drift-seeds-judged={theirs}")


def cmd_verify(opts: Opts) -> int:
    verdict = _judged(opts, suite_of(opts))
    if verdict.why:
        for reason in verdict.why:
            print(f"convergence-reuse: not run {verdict.run_id}'s verdict: {reason}", file=sys.stderr)
        summary([f"### Run {verdict.run_id}'s verdict does not hold here: the full suite runs", ""]
                + [f"- {w}" for w in verdict.why])
        return 0
    sha = opts["sha"]
    if verdict.run.get("head_sha") != sha:
        try:
            copy_statuses(verdict.run, sha)
        except Unproven as e:
            print(f"::warning::could not copy run {verdict.run_id}'s statuses onto {sha[:9]}: {e}", file=sys.stderr)
    _announce_reuse(verdict)
    return 0


def _is_source(run: Status, mine: str) -> bool:
    """Whether `run`'s facts name this key and main's inputs, and it reused nothing itself."""
    try:
        facts = run_facts(run["id"])
    except Unproven:
        return False
    return bool(facts and facts.get("key") == mine and not facts.get("reused_from")
                and not inputs_unlike_main(facts.get("inputs") or {}, facts.get("order_phases")))


def _older_than_window(run: Status, now: datetime.datetime) -> bool:
    created = datetime.datetime.fromisoformat(str(run.get("created_at", "")).replace("Z", "+00:00"))
    return (now - created).total_seconds() > FIND_HOURS * 3600


def cmd_find(opts: Opts) -> int:
    """The newest green run, from the last day, whose facts name this commit's key and main's inputs; nothing when none
    (or anything fails: the kick then dispatches the full suite)."""
    try:
        mine = key(opts["sha"])
        answer: Dict[str, Any] = gh("api", f"repos/{repo()}/actions/runs?workflow={WORKFLOW_FILE}&status=success&per_page={FIND_RUNS}") or {}
        runs: List[Status] = answer.get("workflow_runs") or []
        now = datetime.datetime.now(datetime.timezone.utc)
        for run in runs:
            if _older_than_window(run, now):
                break
            if _is_source(run, mine):
                print(run["id"])
                return 0
    except (Unproven, subprocess.CalledProcessError, OSError, ValueError, KeyError) as e:
        print(f"convergence-reuse: no source run found: {e}", file=sys.stderr)
    return 0


def parse(argv: List[str]) -> Tuple[Opts, List[str]]:
    opts: Opts = {}
    rest: List[str] = []
    it = iter(argv)
    for a in it:
        if a.startswith("--"):
            opts[a[2:]] = next(it, "")
        else:
            rest.append(a)
    return opts, rest


def _cmd_key(_: Opts, rest: List[str]) -> int:
    print(key(rest[0] if rest else "HEAD"))
    return 0


def _cmd_expected_rows(opts: Opts, rest: List[str]) -> int:
    print("\n".join(expected_rows(workflow_at(rest[0] if rest else "HEAD"), suite_of(opts))))
    return 0


def _cmd_takes_run(opts: Opts, _: List[str]) -> int:
    """The Identity takes run a convergence run dispatched."""
    try:
        child = takes_child(opts["parent"])
    except Unproven as e:
        print(f"convergence-reuse: {e}", file=sys.stderr)
        return 2
    if child:
        print(child)
    return 0 if child else 1


def _cmd_judge(opts: Opts, _: List[str]) -> int:
    facts = read_json(opts["facts"]) if opts.get("facts") else None
    mine = read_json(opts["mine"])
    bases: Dict[str, List[Any]] = mine.get("takes_bases") or {}
    mine["takes_bases"] = {c: tuple(b) for c, b in bases.items()}
    print("\n".join(judge(read_json(opts["run"]), read_json(opts["jobs"]), facts, mine)))
    return 0


def _cmd_drift_rows(_: Opts, __: List[str]) -> int:
    """stdin: `gh run view --json jobs`."""
    answer: Dict[str, Any] = json.load(sys.stdin) or {}
    print("\n".join(drift_rows(answer.get("jobs") or [])))
    return 0


def _cmd_phases_expression(_: Opts, __: List[str]) -> int:
    print(PHASES_EXPRESSION)
    return 0


def _opts_only(command: Callable[[Opts], int]) -> Handler:
    return lambda opts, _rest: command(opts)


HANDLERS: Dict[str, Handler] = {
    "key": _cmd_key, "expected-rows": _cmd_expected_rows, "takes-run": _cmd_takes_run, "judge": _cmd_judge,
    "drift-rows": _cmd_drift_rows, "phases-expression": _cmd_phases_expression,
    "inputs": _opts_only(cmd_inputs), "facts": _opts_only(cmd_facts), "verify": _opts_only(cmd_verify),
    "find": _opts_only(cmd_find), "complete": _opts_only(cmd_complete), "legs": _opts_only(cmd_legs),
}


def main(argv: List[str]) -> int:
    if not argv:
        print(USAGE, file=sys.stderr)
        return 64
    command, (opts, rest) = argv[0], parse(argv[1:])
    if command not in HANDLERS:
        print(USAGE, file=sys.stderr)
        return 64
    return HANDLERS[command](opts, rest)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
