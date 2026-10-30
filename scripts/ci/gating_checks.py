#!/usr/bin/env python3
"""THE GATING CHECKS REGISTRY: a check gates only once it has been green (ci-failures-2026-10-30, recommendation 5).

WHY. 22 red runs and ~35 red hours in the five days to 2026-10-30 came from checks that landed red: the convergence flip
gate (33 h, E9), agentctl's CI enabled with Linux-only failures, a lint born against a test it already failed, and 24
`measure` runs wasted on a workflow's own lints. Convergence adopted "a new check lands green" on 10-29 (5e848677b, the
drift gate's commit rules); this generalises it to every check THE GATE (Main and Identity model convergence on main)
reads: a gate workflow, a job in one (a reusable workflow's included: they run under the caller's run), and a runner
label a gate job asks for.

.github/gating-checks.tsv lists them, one row each, with the run that PROVED it green:

    kind      workflow                        name                      proven   note
    workflow  main.yml                        -                         2701     the gate itself
    job       main.yml                        test (heavy1)             2690
    job       main.yml                        ci                        aggregate   fails only when a job it runs does
    label     -                               main-lane                 2690
    job       identity-model-convergence.yml  takes (germany)           unproven  born 2026-10-30, owner: ...

  - `proven` is the run NUMBER (the #n of its URL) of a green run on main in which the check ran and passed; `aggregate`
    marks a job that fails only because one it waits on did (a reusable call's wrapper, timings, a verdict job);
    `unproven` a check that has not had its green run yet.
  - AN UNPROVEN CHECK REPORTS, IT DOES NOT GATE: a gate run red only in unproven jobs (and the aggregates that follow
    them) is no red for the push gate (scripts/hooks/pre-push's red gate asks `report-only`). Its red is still listed --
    and still somebody's -- but no push waits on it.
  - `lint` holds the registry to the workflows: every job a gate workflow (or a workflow it calls) defines has a row,
    every row names a job that exists, every literal runner label a gate job asks for is a proven `label` row, and
    every gate workflow the hook reads is a proven `workflow` row. A new job, label or gate workflow therefore lands
    WITH its row, as `unproven`; RegistryCoverage runs it in the unit rows and the hook's `newchecks` check.
  - `prove --run <run id>` records a green gate run: every job (and its labels) that ran green in it and is unproven or
    missing gets that run's number. Run it after the first green run of a new check, and commit the registry.

usage: gating_checks.py lint [--root DIR]
       gating_checks.py report-only --workflow-file F (--run RUN_ID | --jobs-json FILE)
       gating_checks.py prove --workflow-file F (--run RUN_ID --number N | --jobs-json FILE --number N)
Exit: lint 0 clean / 1 problems; report-only 0 when the run's red is report-only, 1 when it gates, 2 unreadable.
Tested by gating_checks_test.py.
"""

from __future__ import annotations

import fnmatch
import json
import os
import re
import subprocess
import sys

REGISTRY = ".github/gating-checks.tsv"
HEADER = "kind\tworkflow\tname\tproven\tnote"
GATE_WORKFLOWS = ("main.yml", "identity-model-convergence.yml")
HOOK = "scripts/hooks/pre-push"
EXPR = re.compile(r"\$\{\{.*?\}\}")
CALL = re.compile(r"^\s+uses:\s*\./\.github/workflows/(\S+\.ya?ml)\s*$", re.M)
LABEL_LINE = re.compile(r"^\s+(runs-on|lane|runner):\s*(.+)$")
QUOTED = re.compile(r"'([^']+)'")


def load(path: str) -> list[dict]:
    rows = []
    with open(path) as f:
        for n, line in enumerate(f, 1):
            line = line.rstrip("\n")
            if not line.strip() or line.startswith("#") or line == HEADER:
                continue
            cols = line.split("\t")
            if len(cols) < 4:
                raise ValueError(f"{path}:{n}: {len(cols)} column(s), want kind, workflow, name, proven[, note]")
            rows.append({"kind": cols[0], "workflow": cols[1], "name": cols[2], "proven": cols[3],
                         "note": cols[4] if len(cols) > 4 else "", "line": n})
    return rows


def save(path: str, rows: list[dict]) -> None:
    with open(path) as f:
        head = [l for l in f.read().splitlines() if l.startswith("#")]
    with open(path, "w") as f:
        f.write("\n".join(head + [HEADER]) + "\n")
        for r in rows:
            f.write("\t".join([r["kind"], r["workflow"], r["name"], r["proven"], r.get("note", "")]).rstrip("\t") + "\n")


def is_proven(row: dict) -> bool:
    return row["proven"].isdigit()


# ── the workflows ───────────────────────────────────────────────────────────────────────────────────────────────────
def jobs_block(text: str) -> list[str]:
    lines = text.splitlines()
    try:
        start = next(i for i, l in enumerate(lines) if l.rstrip() == "jobs:")
    except StopIteration:
        return []
    return lines[start + 1:]


def yaml_jobs(text: str) -> list[tuple[str, str, str]]:
    """(key, display-name template, the job's own lines) of each top-level job: its `name:`, else its key."""
    out, key, body = [], None, []
    for line in jobs_block(text) + ["END:"]:
        m = re.match(r"^    ([A-Za-z][\w-]*):\s*$", line)
        if m or (line and not line.startswith(" ") and not line.startswith("#")):
            if key:
                name = next((re.sub(r"^\s+name:\s*", "", l).strip().strip("'\"") for l in body
                             if re.match(r"^        name:\s*\S", l)), key)
                out.append((key, name, "\n".join(body)))
            key, body = (m.group(1), []) if m else (None, [])
            if not m:
                break
        elif key:
            body.append(line)
    return out


def glob_of(template: str) -> str:
    return EXPR.sub("*", template)


def workflow_globs(root: str, file: str, seen: frozenset = frozenset()) -> list[str]:
    """Every job-name glob a run of `file` shows: its own jobs and, for a reusable call, the called workflow's."""
    text = open(os.path.join(root, ".github/workflows", file)).read()
    globs = []
    for _key, name, body in yaml_jobs(text):
        globs.append(glob_of(name))
        call = CALL.search(body)
        if call and call.group(1) not in seen:
            globs += workflow_globs(root, call.group(1), seen | {file})
    return globs


def workflow_labels(root: str, file: str, seen: frozenset = frozenset()) -> set[str]:
    text = open(os.path.join(root, ".github/workflows", file)).read()
    labels = set()
    for line in jobs_block(text):
        m = LABEL_LINE.match(line)
        if m:
            value = m.group(2).split(" #")[0].strip()
            if "${{" in value:
                labels |= {q for q in QUOTED.findall(value) if q != "https://github.com"}
            elif value:
                labels.add(value.strip("'\""))
    for call in CALL.findall(text):
        if call not in seen:
            labels |= workflow_labels(root, call, seen | {file})
    return labels


def hook_gate_files(root: str) -> list[str]:
    try:
        text = open(os.path.join(root, HOOK)).read()
    except OSError:
        return list(GATE_WORKFLOWS)
    m = re.search(r"^GatingWorkflowFiles=\(([^)]*)\)", text, re.M)
    return re.findall(r'"([^"]+)"', m.group(1)) if m else []


def lint(root: str) -> list[str]:
    try:
        rows = load(os.path.join(root, REGISTRY))
    except (OSError, ValueError) as e:
        return [str(e)]
    problems = []
    for r in rows:
        if r["kind"] not in ("workflow", "job", "label"):
            problems.append(f"line {r['line']}: kind {r['kind']!r} (workflow, job or label)")
        if not (is_proven(r) or r["proven"] in ("unproven", "aggregate")):
            problems.append(f"line {r['line']}: proven {r['proven']!r} (a run number, `aggregate` or `unproven`)")
        if r["proven"] == "unproven" and not r["note"].strip():
            problems.append(f"line {r['line']}: an unproven row says who owns it and why: {r['name']}")
    keys = [(r["kind"], r["workflow"], r["name"]) for r in rows]
    problems += [f"twice: {k}" for k in sorted({k for k in keys if keys.count(k) > 1})]
    for file in hook_gate_files(root):
        if not any(r["kind"] == "workflow" and r["workflow"] == file and is_proven(r) for r in rows):
            problems.append(f"{file} gates pushes (the hook's GatingWorkflowFiles) but has no proven `workflow` row")
    for file in GATE_WORKFLOWS:
        try:
            globs = workflow_globs(root, file)
        except OSError as e:
            problems.append(f"{file}: cannot read it ({e.strerror})")
            continue
        named = [r for r in rows if r["kind"] == "job" and r["workflow"] == file]
        for g in globs:
            if not any(fnmatch.fnmatchcase(r["name"], g) for r in named):
                problems.append(f"{file}: job {g!r} has no row: add it as `unproven` (with its owner) until a green run proves it")
        for r in named:
            if not any(fnmatch.fnmatchcase(r["name"], g) for g in globs):
                problems.append(f"{file}: row {r['name']!r} names no job the workflow runs: remove it")
        for label in sorted(workflow_labels(root, file)):
            if not any(r["kind"] == "label" and r["name"] == label and is_proven(r) for r in rows):
                problems.append(f"{file}: runner label {label!r} has no proven `label` row: a new tier gates only after a green run on it")
    return problems


# ── a red run: does it gate? ────────────────────────────────────────────────────────────────────────────────────────
def report_only(rows: list[dict], file: str, jobs: list[dict]) -> tuple[bool, str]:
    """(True, why) when every failed job of a red run is unproven -- or an aggregate following one -- and one is."""
    state = {r["name"]: r["proven"] for r in rows if r["kind"] == "job" and r["workflow"] == file}
    failed = [j["name"] for j in jobs if j.get("conclusion") in ("failure", "timed_out")]
    if not failed:
        return False, "no failed job"
    unproven = [n for n in failed if state.get(n, "unproven") == "unproven"]
    gating = [n for n in failed if state.get(n, "unproven") not in ("unproven", "aggregate")]
    if gating:
        return False, "gates: " + ", ".join(gating)
    if not unproven:
        return False, "only aggregates failed: " + ", ".join(failed)
    return True, "report only, unproven: " + ", ".join(unproven)


def run_jobs(run: str) -> list[dict]:
    here = os.path.dirname(os.path.abspath(__file__))
    out = subprocess.run([os.path.join(here, "..", "forge-gh"), "run", "view", run, "--json", "jobs"],
                         check=True, capture_output=True, text=True, timeout=60).stdout
    return json.loads(out).get("jobs", [])


def prove(rows: list[dict], file: str, jobs: list[dict], number: str, labels: set[str]) -> list[str]:
    """Rows proven by green run `number`: its green jobs unproven or missing, and the workflow's labels. Changed names."""
    changed = []
    for j in jobs:
        if j.get("conclusion") != "success":
            continue
        row = next((r for r in rows if r["kind"] == "job" and r["workflow"] == file and r["name"] == j["name"]), None)
        if row is None:
            rows.append({"kind": "job", "workflow": file, "name": j["name"], "proven": number, "note": ""})
            changed.append(j["name"])
        elif row["proven"] == "unproven":
            row["proven"], row["note"] = number, ""
            changed.append(j["name"])
    for label in sorted(labels):
        row = next((r for r in rows if r["kind"] == "label" and r["name"] == label), None)
        if row is None:
            rows.append({"kind": "label", "workflow": "-", "name": label, "proven": number, "note": ""})
            changed.append(f"label {label}")
        elif row["proven"] == "unproven":
            row["proven"], row["note"] = number, ""
            changed.append(f"label {label}")
    return changed


def main(argv: list[str]) -> int:
    args = {"--root": "."}
    cmd, rest = (argv[0], argv[1:]) if argv else ("", [])
    while rest:
        if len(rest) < 2:
            print(__doc__.split("usage:")[1], file=sys.stderr)
            return 2
        args[rest[0]], rest = rest[1], rest[2:]
    root = args["--root"]
    if cmd == "lint":
        problems = lint(root)
        for p in problems:
            print(f"gating-checks: {p}", file=sys.stderr)
        return 1 if problems else 0
    if cmd not in ("report-only", "prove") or "--workflow-file" not in args:
        print("usage: " + __doc__.split("usage:")[1].split("Exit:")[0].strip(), file=sys.stderr)
        return 2
    file = args["--workflow-file"]
    try:
        rows = load(os.path.join(root, REGISTRY))
        if "--jobs-json" in args:
            with open(args["--jobs-json"]) as f:
                jobs = json.load(f).get("jobs", [])
        else:
            jobs = run_jobs(args["--run"])
    except (OSError, ValueError, KeyError, subprocess.SubprocessError) as e:
        print(f"gating-checks: cannot read the run's jobs or the registry: {e}", file=sys.stderr)
        return 2
    if cmd == "report-only":
        only, why = report_only(rows, file, jobs)
        print(why)
        return 0 if only else 1
    number = args.get("--number")
    if not number or not number.isdigit():
        print("gating-checks: prove needs --number <the run's #n>", file=sys.stderr)
        return 2
    changed = prove(rows, file, jobs, number, workflow_labels(root, file))
    save(os.path.join(root, REGISTRY), rows)
    print(f"proven by #{number}: " + (", ".join(changed) or "nothing new"))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
