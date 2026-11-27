"""THE SBT-BACKED CHECKS BESIDE THE SBT CHECK (scripts/hooks/pre-push's header says what each guards): the flip backlog
(`flipbacklog`: the slices the change brings walked, judged as main's spec judges them), the identity drift check
(`identitydrift`: each claimed red's takes-drift rows, and a check's birth, on CI), and THE HEAVY ROWS (`itrest`,
`itidentity`, `e2ecorpus`: Main's own commands with Main's row filter, against the landing check's MongoDB). Run as
light checks (pre_push_checks.Processes); their lines are the shell hook's."""
from __future__ import annotations

import re
from typing import List

from pre_push_area import fetched, merge_base, need_jvm, nonempty, split_value, walk_rows
from pre_push_light import Check, PYTHON_FIX, tail

PROGRESS = r"^identity-drift-ci: run [0-9]+: [0-9]+/[0-9]+ drift row"
EXIT_MARK = "drift-ci-exit:"
# `{ identity-drift-ci ... 2>&1; echo drift-ci-exit:$?; } | tee <log> | <its progress lines on stderr>`: a run queued
# behind a saturated ci-runner polls for many minutes, and a hook silent that long is ended by `agentctl land` (#1041)
DRIFT_CI = ('log=$0; { scripts/identity-drift-ci "$@" 2>&1; echo "drift-ci-exit:$?"; } | tee "$log" '
            '| { grep --line-buffered -E "' + PROGRESS + '" >&2 || true; }')
WALKED = re.compile(r"^(# )?(drift|identity)-slices/")
HEAVY_FAILURES = re.compile(r"\*\*\* FAILED|\[error\]|Failed tests:|TEST FAILED")
HEAVY_PASSED = re.compile(r"Tests: succeeded|All tests passed|Passed: Total")


# ── THE IDENTITY DRIFT CHECK: 1 blocks; 2 (no run could be made: CI unreachable, no token) warns and passes ──
def drift_ci(c: Check, log: str, args: List[str]) -> int:
    """scripts/identity-drift-ci <args>: its output kept in `log`, each poll's progress line shown as it comes; its
    exit status."""
    c.run(["bash", "-c", DRIFT_CI, log, *args])
    marks = [line[len(EXIT_MARK):] for line in c.log_text(log) if line.startswith(EXIT_MARK)]
    return int(marks[-1]) if marks and marks[-1].isdigit() else 1


def drift_shown(c: Check, log: str) -> None:
    c.shown(tail([line for line in c.log_text(log)
                  if not (re.search(PROGRESS, line) or line.startswith(EXIT_MARK))], 40))


def birth(c: Check, born: str, head: str) -> int:
    c.say.log(f"drift: a check's birth or tightening ({born}): every drift row at {head[:9]}, against main's newest run")
    log = c.tmp("drift-birth.log")
    code = drift_ci(c, log, ["--head", head, "--against", "main"])
    drift_shown(c, log)
    if code == 1:
        c.say.warn("drift: the check finds what main's run does not -- a new or tightened check lands green: put what "
                   "it finds at birth on its allow-list, TEMPORARY with an owner and when it goes, under New-Check: "
                   f"(full log: {log})")
        return 1
    if code != 0:
        c.say.warn(f"drift: no verdict on the check's birth (exit {code}): not held against the push; main's run judges it")
    return 0


def for_red(c: Check, run: str, head: str) -> int:
    c.say.log(f"drift: the takes-drift rows run {run} failed, at {head[:9]}, drift-only on ci-runner "
              "(scripts/identity-drift-ci)")
    log = c.tmp(f"drift-{run}.log")
    code = drift_ci(c, log, ["--head", head, "--for-red", run])
    drift_shown(c, log)
    if code == 1:
        c.say.warn(f"drift: Fixes-Red: {run} does not fix its drift rows at {head[:9]} (full log: {log})")
        return 1
    if code != 0:
        c.say.warn(f"drift: no verdict for Fixes-Red: {run} (exit {code}): not held against the push; main's run judges it")
    return 0


def identitydrift(c: Check) -> int:
    reds, born, head = c.text("drift_reds"), c.text("drift_birth"), c.text("head_sha")
    if c.repo.env.get("GITHUB_ACTIONS") == "true":
        c.say.warn("drift: not run from a CI job (it pushes a measure/ branch and dispatches from the laptop): main's "
                   f"run judges {'Fixes-Red: ' + reds if reds else ''}{' the birth of a check' if born else ''}")
        return 0
    code = birth(c, born, head) if born else 0
    for run in reds.split():
        code |= for_red(c, run, head)
    return code


# ── THE FLIP BACKLOG IS COMPLETE ──
def flipbacklog(c: Check) -> int:
    if not c.need("flipbacklog", "python3", PYTHON_FIX):
        return 1
    found = merge_base(c)
    if found is None:
        c.say.warn(f"flipbacklog: no merge base of {c.text('base_sha')[:9]} and {c.text('head_sha')[:9]}")
        return 1
    code, slices = c.answer(["python3", "scripts/ci/population_walk_diff.py", "changed-slices", found])
    c.repo.files.replace_text(c.tmp("flipbacklog.slices"), slices)
    if code != 0:
        c.say.warn("flipbacklog: the slices the change brings could not be read")
        return 1
    dirs = [f"test/resources/fixtures/{s}" for s in nonempty(slices)]
    if not dirs:
        c.say.log("flipbacklog: no slice the change brings")
        return 0
    return walked(c, dirs) if need_jvm(c, "flipbacklog") else 1


def walked(c: Check, dirs: List[str]) -> int:
    kept = walk_rows(c, "flipbacklog")
    c.say.log(f"flipbacklog: {len(dirs)} slice(s) the change brings, walked (PopulationMonotonicitySpec)")
    log = c.tmp("flipbacklog.log")
    walk = "worker/Test/runMain services.identity.PopulationMonotonicitySpec " + (f"--cache {kept} " if kept else "")
    if c.logged(["scripts/sbt-warm", walk + " ".join(dirs)], log) != 0:
        c.shown(tail(c.log_text(log), 20))
        c.say.warn(f"flipbacklog: the walk did not run (full log: {log})")
        return 1
    lines = (re.sub(r"^\[info\] ", "", line, count=1) for line in c.log_text(log))
    c.repo.files.replace_text(c.tmp("flipbacklog.walk"), "".join(line + "\n" for line in lines if WALKED.search(line)))
    return c.run(["python3", "scripts/ci/population_walk_diff.py", "judge-walk", c.tmp("flipbacklog.walk")])


# ── THE HEAVY ROWS: they ride the landing check's rest job, the MongoDB named to them alone ──
def mongo_env(c: Check, e2e: bool) -> List[str]:
    """`env`'s words naming the row's MongoDB: none for an e2e row (as Main's), else the landing check's."""
    if e2e:
        return ["-u", "MONGODB_URI", "-u", "MONGODB_DB"]
    db = split_value(c, "PREPUSH_HEAVY_MONGODB_DB") or c.repo.env.get("MONGODB_DB", "")
    return [f"MONGODB_URI={heavy_uri(c)}"] + ([f"MONGODB_DB={db}"] if db else [])


def heavy_uri(c: Check) -> str:
    return split_value(c, "PREPUSH_HEAVY_MONGODB_URI") or c.repo.env.get("MONGODB_URI", "")


def heavy_row(c: Check, check: str, command: str, *row_env: str) -> int:
    """Main's own `command` (with `row_env`): the e2e rows as Main's run, with no MongoDB named; the integration rows
    against $PREPUSH_HEAVY_MONGODB_URI / _DB (or $MONGODB_URI, for a caller from before)."""
    if not need_jvm(c, check):
        return 1
    e2e = check.startswith("e2e")
    if not heavy_uri(c) and not e2e:
        c.say.warn(f"{check}: no $PREPUSH_HEAVY_MONGODB_URI -- the integration specs need a MongoDB replica set "
                   "(scripts/ci/start-mongo-replset.sh)")
        return 1
    log = c.tmp(f"{check}.log")
    if c.logged(["env", *mongo_env(c, e2e), *row_env, "sbt", "-batch", command], log) != 0:
        c.shown([line for line in c.log_text(log) if HEAVY_FAILURES.search(line)][:80])
        c.say.warn(f"full log: {log}")
        return 1
    c.printed(tail([line for line in c.log_text(log) if HEAVY_PASSED.search(line)], 3))
    return 0


def itrest(c: Check) -> int:
    """The rest row's deploy specs read the GitOps manifests: fetched here when nothing put them in place."""
    if not c.holds("-d", "infra/kubernetes") and c.holds("-x", "infra/bin/fetch-gitops"):
        if not fetched(c, c.tmp("fetch-gitops.log")):
            c.shown(tail(c.log_text(c.tmp("fetch-gitops.log")), 8))
            c.say.warn("itrest: infra/bin/fetch-gitops could not fetch the GitOps manifests the deploy specs read")
            return 1
    return heavy_row(c, "itrest", "itAll", "IT_ROW=rest")


def itidentity(c: Check) -> int:
    return heavy_row(c, "itidentity", "itAll", "IT_ROW=identity")


def e2ecorpus(c: Check) -> int:
    return heavy_row(c, "e2ecorpus", "e2eCorpus")
