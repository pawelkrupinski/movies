"""THE JUDGEMENT OF A REUSED CONVERGENCE VERDICT (split out of convergence_reuse.py): why a source run's verdict does NOT hold
for a commit (`judge`), which inputs are not main's (`inputs_unlike_main`), a run's jobs by their own name (`jobs_by_name`),
and the NO FALSE GREEN check of a takes run's rows (`incomplete`). Pure over what convergence_forge.py and the facts artifact
read: it touches no commit and no forge."""
from __future__ import annotations

import re
from typing import Any, Dict, List, Mapping, Optional, Tuple

from convergence_forge import SEED_KEYS, SUITE_FILES

Job = Dict[str, Any]
Facts = Dict[str, Any]
Mine = Dict[str, Any]


def seeds_of(inputs: Optional[Mapping[str, Any]]) -> str:
    """`S1,S2`: the seeds a run's takes-drift rows replayed, as its facts' inputs name them; a run from before the rotation
    names none and replayed seeds 1 and 2."""
    if not any(k in (inputs or {}) for k in SEED_KEYS):
        return "1,2"
    return ",".join(str((inputs or {}).get(k, "")) for k in SEED_KEYS)


def inputs_unlike_main(inputs: Mapping[str, Any], order_phases: Optional[str]) -> List[str]:
    """Why legs handed `inputs` (and `order_phases`) do not run what main's run does; empty when they do."""
    why: List[str] = []
    if inputs.get("takes-only") != "false":
        why.append("takes-only: only the takes rows ran")
    # A drift-only run (scripts/identity-drift-ci) ran the takes-drift rows alone: never a source for main's full verdict.
    if inputs.get("drift-only", "false") != "false":
        why.append("drift-only: only the takes-drift rows ran")
    if inputs.get("drift-explore", ""):
        why.append(f"drift-explore {inputs['drift-explore']!r}: the explorer's random seeds, not main's suite")
    if inputs.get("drift-seeds", ""):
        why.append(f"drift-seeds {inputs['drift-seeds']!r}: not main's suite")
    for name in ("takes-countries", "recording", "jvm-options", "runner"):
        if inputs.get(name, ""):
            why.append(f"{name} {inputs[name]!r}: not main's suite")
    if inputs.get("takes-skip") not in ("shadow", "off"):
        why.append(f"takes-skip {inputs.get('takes-skip')!r}: main takes every take in full (shadow)")
    if order_phases != "both":
        why.append(f"order-independence phases {order_phases!r}: main runs both (p01 and p12)")
    return why


def _caller_job(caller: str, rows: List[Job]) -> Job:
    done = all(r.get("status") == "completed" for r in rows)
    ok = done and all(r.get("conclusion") in ("success", "skipped") for r in rows)
    return {"name": caller, "status": "completed" if done else "in_progress",
            "conclusion": ("success" if ok else "failure") if done else None}


def jobs_by_name(jobs: List[Job]) -> Dict[str, Job]:
    """A run's jobs by their own name: github.com names a reusable call's inner job `leg (poland) / takes (poland)` and
    lists no caller job. This reads the inner name (`takes (poland)`), and each caller (`leg (poland)`) as the worst of
    its rows (success only if every row succeeded), unless the run lists the caller itself. The one reader of job names:
    the verdict (`complete`, `judge`) and scripts/identity-drift-ci (`drift-rows`) both go through it."""
    out: Dict[str, Job] = {}
    callers: Dict[str, List[Job]] = {}
    for j in jobs:
        name = j.get("name") or ""
        caller, sep, inner = name.partition(" / ")
        if sep:
            out.setdefault(inner, j)
            callers.setdefault(caller, []).append(j)
        else:
            out[name] = j
    for caller, rows in callers.items():
        if caller not in out:
            out[caller] = _caller_job(caller, rows)
    return out


DRIFT_ROW = re.compile(r"^takes-drift-[0-9]+ \(")


def drift_rows(jobs: List[Job]) -> List[str]:
    """`name\tstatus\tconclusion` lines for each takes-drift row of a run's jobs, read through jobs_by_name."""
    return [f"{name}\t{j.get('status') or ''}\t{j.get('conclusion') or ''}"
            for name, j in jobs_by_name(jobs).items() if DRIFT_ROW.match(name)]


def _run_reasons(source_run: Job, file: str) -> List[str]:
    why: List[str] = []
    if not str(source_run.get("path", "")).endswith(file):
        why.append(f"run {source_run.get('id')} is no {file} run")
    if source_run.get("status") != "completed" or source_run.get("conclusion") != "success":
        why.append(f"run {source_run.get('id')} is {source_run.get('status')}/{source_run.get('conclusion')}, not a green "
                   "completed run")
    return why


def _pair_reasons(facts: Facts, mine: Mine) -> List[str]:
    if facts.get("pairs") == mine["pairs"]:
        return []
    theirs: Dict[str, str] = facts.get("pairs") or {}
    moved = sorted(c for c in set(theirs) | set(mine["pairs"]) if theirs.get(c) != mine["pairs"].get(c))
    return [f"the recorded pair or its fills moved since: {', '.join(moved) or 'unreadable'}"]


def _facts_reasons(facts: Facts, mine: Mine) -> List[str]:
    why: List[str] = []
    if facts.get("reused_from"):
        why.append(f"the source reused run {facts['reused_from']} itself and ran no row")
    if facts.get("key") != mine["key"]:
        why.append(f"the tree differs: key {str(facts.get('key'))[:12]} there, {mine['key'][:12]} here")
    why += _pair_reasons(facts, mine)
    why += [f"the source's legs: {r}" for r in inputs_unlike_main(facts.get("inputs") or {}, facts.get("order_phases"))]
    why += [f"this run: {r}" for r in inputs_unlike_main(mine["inputs"], mine["order_phases"])]
    return why


def _rows_reasons(jobs: List[Job], mine: Mine) -> List[str]:
    why: List[str] = []
    result = jobs_by_name(jobs)
    callers = sorted({re.sub(r"^[^(]*", "leg ", r) for r in mine["expected_rows"]})
    for name in list(mine["expected_rows"]) + callers:
        job = result.get(name)
        if job is None:
            why.append(f"`{name}` did not run there")
        elif job.get("conclusion") != "success":
            why.append(f"`{name}` ended {job.get('status')}/{job.get('conclusion')} there")
    return why


def _base_reason(code: str, base: Tuple[Optional[str], Optional[str], bool]) -> Optional[str]:
    theirs, main, same = base
    if not theirs:
        return f"takes ({code}): the source's base is unknown (no `identity takes ({code})` status of it)"
    if not main:
        return f"takes ({code}): no commit before this one passed its takes check -- main's base is unknown"
    if not same:
        return f"takes ({code}): judged against {theirs[:9]} there, main would judge against {main[:9]}"
    return None


def _bases_reasons(takes_bases: Mapping[str, Tuple[Optional[str], Optional[str], bool]]) -> List[str]:
    reasons = [_base_reason(code, base) for code, base in sorted(takes_bases.items())]
    return [r for r in reasons if r]


def judge(source_run: Job, jobs: List[Job], facts: Optional[Facts], mine: Mine) -> List[str]:
    """Why the source run's verdict does NOT hold for this commit; empty when it does.

    source_run: the run as the actions API answers it (id, path, status, conclusion, head_sha)
    jobs:       its jobs ({name, status, conclusion})
    facts:      its facts artifact (facts()), None when it has none
    mine:       {"key", "pairs", "inputs", "order_phases", "expected_rows", "takes_bases": {code: (source base, main
                base, same key)}, "suite"} -- what this run computed (a convergence run judges no takes base: {})"""
    why = _run_reasons(source_run, SUITE_FILES[mine.get("suite") or "convergence"])
    if facts is None:
        return why + ["the source recorded no reuse facts"]
    return why + _facts_reasons(facts, mine) + _rows_reasons(jobs, mine) + _bases_reasons(mine["takes_bases"])


# ── no false green ────────────────────────────────────────────────────────────────────────────────────────────────────

# the takes check and every fixed-seed drift row (takes-drift-1, -2, and any further row drift_seeds.rows_of names)
VERDICT_ROW = re.compile(r"^(takes|takes-drift-[0-9]+) \(")
DECIDED = ("success", "failure")
# what leaves a takes row out: a takes-only run, some countries, another recording (not the skip gate's mode: a takes
# row it skips the diff of still runs and decides, on its own proof; nor the order phases, no takes row)
TAKES_ROW_LEAVERS = ("takes-only", "drift-only", "takes-countries", "recording")


def incomplete(jobs: List[Job], rows: List[str], reused_from: str = "", inputs: Optional[Mapping[str, Any]] = None,
               order_phases: str = "both") -> List[str]:
    """Why a run with `jobs` gives no verdict for THE GATE: each takes or takes-drift row of `rows` (expected_rows())
    that did not run to a decided conclusion, and inputs that are not main's suite; empty when every one decided, or
    when `reused_from` names the proven source whose rows did."""
    if reused_from:
        return []
    why = [f"this run's legs: {reason}" for reason in inputs_unlike_main(inputs, order_phases)
           if reason.startswith(TAKES_ROW_LEAVERS)] if inputs is not None else []
    result = jobs_by_name(jobs)
    for name in rows:
        if not VERDICT_ROW.match(name):
            continue
        job = result.get(name)
        if job is None:
            why.append(f"`{name}` did not run")
        elif job.get("conclusion") not in DECIDED:
            why.append(f"`{name}` ended {job.get('status')}/{job.get('conclusion')}: no verdict")
    return why
