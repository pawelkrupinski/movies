#!/usr/bin/env python3
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
# usage:
#   convergence_reuse.py key [REV]                      the key of REV (default HEAD)
#   convergence_reuse.py inputs                         the legs' inputs from the dispatch's (env INPUT_*): `name=value`
#                                                       lines for $GITHUB_OUTPUT, and the JSON to $LEG_INPUTS_FILE
#   convergence_reuse.py facts --sha SHA --inputs F [--order-phases P] [--reused-from RUN]
#                                                       this run's facts (the artifact `convergence-reuse-facts`)
#   convergence_reuse.py verify --source RUN --sha SHA --inputs F [--order-phases P]
#                                                       prints `reused-from=RUN` when RUN's verdict holds, nothing when not
#                                                       (each reason on stderr and in $GITHUB_STEP_SUMMARY); copies the
#                                                       source's statuses onto SHA when it is another commit
#   convergence_reuse.py find --sha SHA                 the newest source run whose facts name SHA's key, or nothing (the
#                                                       kick's hint: verify decides)
#   convergence_reuse.py expected-rows [REV]            the rows main's matrix runs, `<phase> (<country>)` a line
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
# check proved (verify), whose rows ran. A takes row whose skip gate left out its diff (takes-skip-gate.sh) still ran
# and concluded: the gate's own proof stands.
# env: GH_REPO, GH_TOKEN (gh's own), FIXTURE_RELEASE_TAG (the pairs). Tested by convergence_reuse_test.py and
# ConvergenceReuseSpec.


from __future__ import annotations

import hashlib
import json
import os
import re
import subprocess
import sys
import tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import drift_seeds  # noqa: E402

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
PATHS_FILE = ".github/convergence-paths.txt"
WORKFLOW = ".github/workflows/identity-model-convergence.yml"
WORKFLOW_FILE = "identity-model-convergence.yml"
FACTS_ARTIFACT = "convergence-reuse-facts"
FACTS_NAME = "facts.json"
# The leg's own expansion of a country into rows (country-convergence-leg.yml's `phase:` matrix). expected_rows() models
# exactly this; ConvergenceReuseSpec fails the moment the leg's expression is anything else.
PHASES_EXPRESSION = (
    "${{ fromJson(inputs.drift-only && (inputs.drift-seed-1 == '' && inputs.drift-seed-2 != '' && '[\"takes-drift-2\"]' || "
    "inputs.drift-seed-2 == '' && inputs.drift-seed-1 != '' && '[\"takes-drift-1\"]' || '[\"takes-drift-1\",\"takes-drift-2\"]') || "
    "inputs.takes-only && '[\"takes\",\"takes-base\"]' || format('[\"convergence\"{0}{1}{2}{3}]', "
    "inputs.order-command != '' && (inputs.order-split && (inputs.order-one-phase && "
    "',\"order-independence-p01-pass0\",\"order-independence-p01-pass1\"' || ',\"order-independence-p01-pass0\","
    "\"order-independence-p01-pass1\",\"order-independence-p12-pass1\",\"order-independence-p12-pass2\"') || "
    "',\"order-independence\"') || '', "
    "inputs.next-day-command != '' && ',\"next-day\"' || '', inputs.mode == 'hermetic' && inputs.takes-row && "
    "',\"takes\",\"takes-base\",\"takes-drift-1\",\"takes-drift-2\"' || '', inputs.mode == 'hermetic' && "
    "inputs.takes-row && inputs.harness-aa && ',\"takes-drift-aa\"' || '')) }}"
)
# A split country's order rows on main: each pass in a row of its own, then the two comparisons (`order-compare`).
ORDER_SPLIT_ROWS = ["order-independence-p01-pass0", "order-independence-p01-pass1", "order-independence-p12-pass1",
                    "order-independence-p12-pass2", "order-independence-p01", "order-independence-p12"]
GH_SECONDS = 60
STATUS_PAGES = 4
BASE_WALK = 100
FIND_RUNS = 30
FIND_HOURS = 24


USAGE = "usage: convergence_reuse.py key|inputs|facts|verify|find|complete|legs|drift-rows|expected-rows|judge ... (see the header)"


class Unproven(Exception):
    """A fact this run could not read: no reuse, said why."""


# ── the key ───────────────────────────────────────────────────────────────────────────────────────────────────────────


def git(*args, cwd=None):
    return subprocess.run(["git", *args], cwd=cwd or ROOT, check=True, capture_output=True, text=True).stdout


def patterns_of(text):
    """The paths file's patterns, as the changed-paths action's matches.sh reads them: `dir/**`, an exact path, either
    `!`-prefixed to take paths out; `#` comments, blanks, a YAML list's `- ` and quotes dropped."""
    out = []
    for line in text.splitlines():
        line = line.strip()
        line = re.sub(r"^-\s*", "", line).strip("'\"")
        if line and not line.startswith("#"):
            out.append(line)
    return out


def _under(path, pattern):
    return path.startswith(pattern[:-2]) if pattern.endswith("/**") else path == pattern


def listed(path, patterns):
    included = any(_under(path, p) for p in patterns if not p.startswith("!"))
    return included and not any(_under(path, p[1:]) for p in patterns if p.startswith("!"))


def key(rev="HEAD", cwd=None):
    """sha256 over `<mode> <type> <object>\\t<path>` of every path the commit's own paths file lists, in path order: two
    commits share a key exactly when every such file is byte-identical (and the file itself is listed)."""
    patterns = patterns_of(git("show", f"{rev}:{PATHS_FILE}", cwd=cwd))
    if not patterns:
        raise Unproven(f"{rev} lists no convergence paths")
    digest = hashlib.sha256()
    for line in git("ls-tree", "-r", "--full-tree", rev, cwd=cwd).splitlines():
        meta, path = line.split("\t", 1)
        if listed(path, patterns):
            digest.update(f"{meta}\t{path}\n".encode())
    return digest.hexdigest()


# ── the rows main runs ────────────────────────────────────────────────────────────────────────────────────────────────


def matrix_rows(workflow_text):
    """Each `include:` row of the `leg` matrix as a dict (the one-line flow mappings the workflow writes)."""
    rows = []
    for m in re.finditer(r"^\s*-\s*\{(.*)\}\s*$", workflow_text, re.M):
        row = {}
        for k, v in re.findall(r"([\w-]+):\s*('[^']*'|[^,]*)", m.group(1)):
            row[k] = v.strip().strip("'")
        if "country" in row and "cmd" in row:
            rows.append(row)
    return rows


def expected_rows(workflow_text):
    """`<phase> (<country>)` of every row main's run expands to: PHASES_EXPRESSION with takes-only false, hermetic, the
    takes rows on (the check's and a drift seed's each) and both order phases (main's order-phases.sh answer). Not the
    harness's A/A row (`takes-drift-aa`, harness_aa.py): only a push that changed the harness runs it, and no verdict
    reused from a landing check is one -- a reused run skips it with every other row."""
    out = []
    for row in matrix_rows(workflow_text):
        phases = ["convergence"]
        if row.get("order", ""):
            split = row.get("order-split", "false") == "true"
            # a split country's passes each in a row of their own, and the leg's `order-compare` job per pair (named
            # as the two-pass row was)
            phases += ORDER_SPLIT_ROWS if split else ["order-independence"]
        if row.get("next-day", ""):
            phases.append("next-day")
        phases += ["takes", "takes-base", "takes-drift-1", "takes-drift-2"]
        out += [f"{p} ({row['country']})" for p in phases]
    if not out:
        raise Unproven("no `leg` matrix rows in the workflow")
    return out


def countries(workflow_text):
    return [(r["country"], r["code"]) for r in matrix_rows(workflow_text)]


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


def leg_inputs(env):
    """The legs' inputs from the dispatch's, normalised: a boolean is `true` or `false` (Forgejo hands a dispatch's as the
    STRING "true"), an empty value its default."""
    out = {}
    for name, (var, default) in INPUTS.items():
        value = (env.get(var) or "").strip()
        if name in BOOLEANS:
            value = "true" if value.lower() == "true" else "false"
        out[name] = value or default
    return out


SEED_KEYS = ("drift-seed-1", "drift-seed-2")


def scalar(text):
    """A flow-map value as YAML types it: quotes make a string, `true`/`false` a boolean, digits a number."""
    text = text.strip()
    if len(text) >= 2 and text[0] == text[-1] and text[0] in "'\"":
        return text[1:-1]
    if text in ("true", "false"):
        return text == "true"
    for kind in (int, float):
        try:
            return kind(text)
        except ValueError:
            pass
    return text


def parse_rows(text):
    """The `- { key: value, ... }` rows of a matrix written as flow maps (values hold no comma or brace), as dicts."""
    rows = []
    for m in re.finditer(r"-\s*\{([^}]*)}", text):
        row = {}
        for field in m.group(1).split(","):
            key, sep, value = field.partition(":")
            if sep:
                row[key.strip()] = scalar(value)
        rows.append(row)
    return rows


def leg_matrix(rows, inputs):
    """The `leg` job's matrix as JSON-able data (identity-model-convergence.yml): a `country` vector the rows extend, so a
    row is named `leg (<country>)` by default, holding only the countries this run starts -- a takes-only or drift-only run
    with `takes-countries` keeps those. A country left out is no row at all: a matrix job skipped for it would show its
    `name:` template raw in the run graph (`${{ matrix.phase }} (...)`)."""
    wanted = {c.strip() for c in inputs.get("takes-countries", "").split(",") if c.strip()}
    narrowed = (inputs.get("takes-only") == "true" or inputs.get("drift-only") == "true") and wanted
    kept = [r for r in rows if not narrowed or r["code"] in wanted]
    return {"country": [r["country"] for r in kept], "include": kept}


def run_seeds(env, inputs):
    """This run's two takes-drift pins (drift_seeds.resolve): the dispatch's, or none -- each row then replays its fixed set."""
    one, two = drift_seeds.resolve(int(env.get("GITHUB_RUN_NUMBER") or 1), inputs.get("drift-seeds", ""),
                                   inputs.get("drift-only") == "true")
    return {"drift-seed-1": str(one or ""), "drift-seed-2": str(two or "")}


def seeds_of(inputs):
    """`S1,S2`: the seeds a run's takes-drift rows replayed, as its facts' inputs name them; a run from before the rotation
    names none and replayed seeds 1 and 2."""
    if not any(k in (inputs or {}) for k in SEED_KEYS):
        return "1,2"
    return ",".join(str((inputs or {}).get(k, "")) for k in SEED_KEYS)


def inputs_unlike_main(inputs, order_phases):
    """Why legs handed `inputs` (and `order_phases`) do not run what main's run does; empty when they do."""
    why = []
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


# ── the judgement ─────────────────────────────────────────────────────────────────────────────────────────────────────


def jobs_by_name(jobs):
    """A run's jobs by their own name: github.com names a reusable call's inner job `leg (poland) / takes (poland)` and
    lists no caller job. This reads the inner name (`takes (poland)`), and each caller (`leg (poland)`) as the worst of
    its rows (success only if every row succeeded), unless the run lists the caller itself. The one reader of job names:
    the verdict (`complete`, `judge`) and scripts/identity-drift-ci (`drift-rows`) both go through it."""
    out, callers = {}, {}
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
            done = all(r.get("status") == "completed" for r in rows)
            ok = done and all(r.get("conclusion") in ("success", "skipped") for r in rows)
            out[caller] = {"name": caller, "status": "completed" if done else "in_progress",
                           "conclusion": ("success" if ok else "failure") if done else None}
    return out


DRIFT_ROW = re.compile(r"^takes-drift-[12] \(")


def drift_rows(jobs):
    """`name\tstatus\tconclusion` lines for each takes-drift row of a run's jobs, read through jobs_by_name."""
    return [f"{name}\t{j.get('status') or ''}\t{j.get('conclusion') or ''}"
            for name, j in jobs_by_name(jobs).items() if DRIFT_ROW.match(name)]


def judge(source_run, jobs, facts, mine):
    """Why the source run's verdict does NOT hold for this commit; empty when it does.

    source_run: the run as the actions API answers it (id, path, status, conclusion, head_sha)
    jobs:       its jobs ({name, status, conclusion})
    facts:      its facts artifact (facts()), None when it has none
    mine:       {"key", "pairs", "inputs", "order_phases", "expected_rows", "takes_bases": {code: (source base, main
                base, same key)}} -- what this run computed"""
    why = []
    if not str(source_run.get("path", "")).endswith(WORKFLOW_FILE):
        why.append(f"run {source_run.get('id')} is no {WORKFLOW_FILE} run")
    if source_run.get("status") != "completed" or source_run.get("conclusion") != "success":
        why.append(f"run {source_run.get('id')} is {source_run.get('status')}/{source_run.get('conclusion')}, not a green "
                   "completed run")
    if facts is None:
        why.append("the source recorded no reuse facts")
        return why
    if facts.get("reused_from"):
        why.append(f"the source reused run {facts['reused_from']} itself and ran no row")
    if facts.get("key") != mine["key"]:
        why.append(f"the tree differs: key {str(facts.get('key'))[:12]} there, {mine['key'][:12]} here")
    if facts.get("pairs") != mine["pairs"]:
        moved = sorted(c for c in set(facts.get("pairs") or {}) | set(mine["pairs"])
                       if (facts.get("pairs") or {}).get(c) != mine["pairs"].get(c))
        why.append(f"the recorded pair or its fills moved since: {', '.join(moved) or 'unreadable'}")
    for reason in inputs_unlike_main(facts.get("inputs") or {}, facts.get("order_phases")):
        why.append(f"the source's legs: {reason}")
    for reason in inputs_unlike_main(mine["inputs"], mine["order_phases"]):
        why.append(f"this run: {reason}")
    result = jobs_by_name(jobs)
    callers = sorted({re.sub(r"^[^(]*", "leg ", r) for r in mine["expected_rows"]})
    for name in list(mine["expected_rows"]) + callers:
        job = result.get(name)
        if job is None:
            why.append(f"`{name}` did not run there")
        elif job.get("conclusion") != "success":
            why.append(f"`{name}` ended {job.get('status')}/{job.get('conclusion')} there")
    for code, (theirs, main, same) in sorted(mine["takes_bases"].items()):
        if not theirs:
            why.append(f"takes ({code}): the source's base is unknown (no `identity takes ({code})` status of it)")
        elif not main:
            why.append(f"takes ({code}): no commit before this one passed its takes check -- main's base is unknown")
        elif not same:
            why.append(f"takes ({code}): judged against {theirs[:9]} there, main would judge against {main[:9]}")
    return why


# ── no false green ────────────────────────────────────────────────────────────────────────────────────────────────────

VERDICT_PHASES = ("takes", "takes-drift-1", "takes-drift-2")
DECIDED = ("success", "failure")


def incomplete(jobs, rows, reused_from="", inputs=None, order_phases="both"):
    """Why a run with `jobs` gives no verdict for THE GATE: each takes or takes-drift row of `rows` (expected_rows())
    that did not run to a decided conclusion, and inputs that are not main's suite; empty when every one decided, or
    when `reused_from` names the proven source whose rows did."""
    if reused_from:
        return []
    # what leaves a takes row out: a takes-only run, some countries, another recording (not the skip gate's mode: a takes
    # row it skips the diff of still runs and decides, on its own proof; nor the order phases, no takes row)
    why = [f"this run's legs: {reason}" for reason in inputs_unlike_main(inputs, order_phases)
           if reason.startswith(("takes-only", "drift-only", "takes-countries", "recording"))] if inputs is not None else []
    result = jobs_by_name(jobs)
    for name in rows:
        if not name.startswith(tuple(f"{p} (" for p in VERDICT_PHASES)):
            continue
        job = result.get(name)
        if job is None:
            why.append(f"`{name}` did not run")
        elif job.get("conclusion") not in DECIDED:
            why.append(f"`{name}` ended {job.get('status')}/{job.get('conclusion')}: no verdict")
    return why


def cmd_complete(opts):
    try:
        why = incomplete(run_jobs(opts["run"]), expected_rows(workflow_at(opts["sha"])), opts.get("reused-from", ""),
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


# ── reading the forge ─────────────────────────────────────────────────────────────────────────────────────────────────


def gh(*args, parse=True):
    try:
        res = subprocess.run(["gh", *args], capture_output=True, text=True, timeout=GH_SECONDS)
    except (OSError, subprocess.TimeoutExpired) as e:
        raise Unproven(f"gh {' '.join(args[:2])}: {e}")
    if res.returncode != 0:
        raise Unproven(f"gh {' '.join(args[:3])}: {res.stderr.strip()[:200]}")
    if not parse:
        return res.stdout
    try:
        return json.loads(res.stdout)
    except ValueError:
        raise Unproven(f"gh {' '.join(args[:3])}: no JSON in its answer")


def repo():
    name = os.environ.get("GH_REPO") or os.environ.get("GITHUB_REPOSITORY")
    if not name:
        raise Unproven("no GH_REPO")
    return name


def run_json(run_id):
    return gh("api", f"repos/{repo()}/actions/runs/{run_id}")


def run_jobs(run_id):
    return (gh("api", f"repos/{repo()}/actions/runs/{run_id}/jobs?per_page=100") or {}).get("jobs") or []


def run_facts(run_id):
    """The run's facts artifact, or None when it has none (a run from before them); a failed read is Unproven."""
    with tempfile.TemporaryDirectory() as tmp:
        try:
            res = subprocess.run(["gh", "run", "download", str(run_id), "-n", FACTS_ARTIFACT, "-D", tmp],
                                 capture_output=True, text=True, timeout=GH_SECONDS)
        except (OSError, subprocess.TimeoutExpired) as e:
            raise Unproven(f"gh run download {run_id}: {e}")
        if res.returncode != 0:
            if "no artifact" in (res.stderr + res.stdout).lower():
                return None
            raise Unproven(f"gh run download {run_id}: {res.stderr.strip()[:200]}")
        path = os.path.join(tmp, FACTS_NAME)
        if not os.path.isfile(path):
            return None
        with open(path) as f:
            return json.load(f)


def statuses(sha, cache):
    """Every status on `sha`, newest id first."""
    if sha not in cache:
        out = []
        for page in range(1, STATUS_PAGES + 1):
            batch = gh("api", f"repos/{repo()}/commits/{sha}/statuses?limit=50&page={page}")
            out += batch or []
            if not batch or len(batch) < 50:
                break
        cache[sha] = sorted(out, key=lambda s: s.get("id", 0), reverse=True)
    return cache[sha]


def state(status):
    return status.get("status") or status.get("state") or ""


def takes_context(code):
    return f"identity takes ({code})"


def source_base(code, sha, run, cache):
    """The base the source's takes check judged `sha` against: its `identity takes (<code>)` status of that run."""
    marks = (f"/actions/runs/{run.get('id')}", f"/actions/runs/{run.get('run_number')}")
    for s in statuses(sha, cache):
        if s.get("context") == takes_context(code) and str(s.get("target_url", "")).endswith(marks):
            m = re.search(r"\bbase ([0-9a-f]{7,40}) vs head", s.get("description", ""))
            return m.group(1) if (m and state(s) == "success") else None
    return None


def main_base(code, sha, cache):
    """The base main's takes check would take for `sha`: the newest commit before it, first parents, whose newest
    `identity takes (<code>)` status passed (identity-takes-check.sh's pick_base)."""
    for commit in git("rev-list", "--first-parent", f"--max-count={BASE_WALK}", f"{sha}^").split():
        for s in statuses(commit, cache):
            if s.get("context") == takes_context(code):
                if state(s) == "success":
                    return commit
                break
    return None


def same_tree(a, b):
    if not a or not b:
        return False
    try:
        a = git("rev-parse", "--verify", f"{a}^{{commit}}").strip()
        b = git("rev-parse", "--verify", f"{b}^{{commit}}").strip()
    except subprocess.CalledProcessError:
        return False
    return a == b or key(a) == key(b)


def pairs(workflow_text):
    """Each country's pair as a hermetic leg would resolve it now: the pin's line, the fills published over its tree."""
    out = {}
    setup = os.path.join(ROOT, ".github/scripts/convergence-setup.sh")
    fill = os.path.join(ROOT, ".github/scripts/convergence-fill.sh")
    for _, code in countries(workflow_text):
        try:
            pin = subprocess.run(["bash", setup, "pinned", code], capture_output=True, text=True, check=True,
                                 timeout=GH_SECONDS).stdout.strip()
            corpus = pin.split()[0] if pin else ""
            fills = subprocess.run(["bash", fill, "fills", code, corpus], capture_output=True, text=True, check=True,
                                   timeout=GH_SECONDS).stdout.split() if corpus else []
        except (OSError, subprocess.SubprocessError) as e:
            raise Unproven(f"could not read {code}'s pair: {e}")
        if not pin:
            raise Unproven(f"no pair pinned for {code}")
        out[code] = " ".join([pin] + sorted(fills))
    return out


def workflow_at(rev):
    return git("show", f"{rev}:{WORKFLOW}")


# ── commands ──────────────────────────────────────────────────────────────────────────────────────────────────────────


def read_json(path):
    with open(path) as f:
        return json.load(f)


def cmd_inputs(_):
    inputs = leg_inputs(os.environ)
    try:
        inputs.update(run_seeds(os.environ, inputs))
    except ValueError as e:
        print(f"::error title=drift seeds::{e}", file=sys.stderr)
        return 2
    for name, value in inputs.items():
        print(f"{name}={value}")
    summary([f"Takes-drift seeds this run: takes-drift-1 replays seed {inputs['drift-seed-1'] or '(none)'}, takes-drift-2 "
             f"seed {inputs['drift-seed-2'] or '(none)'} (run #{os.environ.get('GITHUB_RUN_NUMBER', '?')}"
             f"{', pinned ' + repr(inputs['drift-seeds']) if inputs['drift-seeds'] else ', each row its fixed set'}; scripts/ci/drift_seeds.py)"])
    if os.environ.get("LEG_INPUTS_FILE"):
        with open(os.environ["LEG_INPUTS_FILE"], "w") as f:
            json.dump(inputs, f, sort_keys=True)
    return 0


def cmd_legs(_):
    """The `leg` matrix (leg_matrix), from LEG_ROWS (the rows as written in the workflow) and the dispatch's inputs; for
    verify-scrape-recording.yml, LEG_CODES (the recording's complete countries, comma-separated) narrows it instead."""
    rows = parse_rows(os.environ.get("LEG_ROWS", ""))
    codes = os.environ.get("LEG_CODES")
    if codes is not None:
        inputs = {"takes-only": "true", "takes-countries": codes}
    else:
        inputs = read_json(os.environ["LEG_INPUTS_FILE"])  # the file `inputs` wrote
    matrix = leg_matrix(rows, inputs)
    print(f"leg-matrix={json.dumps(matrix, separators=(',', ':'))}")
    print(f"any-leg={'true' if matrix['country'] else 'false'}")
    return 0


def cmd_facts(opts):
    workflow = workflow_at(opts["sha"])
    inputs = read_json(opts["inputs"])
    facts = {"version": 1, "run_id": os.environ.get("GITHUB_RUN_ID", ""), "sha": opts["sha"], "key": key(opts["sha"]),
             "inputs": inputs, "order_phases": opts.get("order-phases", "both"),
             "reused_from": opts.get("reused-from", "")}
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


def summary(lines):
    path = os.environ.get("GITHUB_STEP_SUMMARY")
    if path:
        with open(path, "a") as f:
            f.write("\n".join(lines) + "\n")


def copy_statuses(run, sha):
    """The source's statuses of its run, onto `sha` when it is another commit: the next takes check's pick_base and the
    next reuse read them there."""
    cache = {}
    marks = (f"/actions/runs/{run.get('id')}", f"/actions/runs/{run.get('run_number')}")
    latest = {}
    for s in statuses(run["head_sha"], cache):
        if str(s.get("target_url", "")).endswith(marks) and s.get("context") not in latest:
            latest[s["context"]] = s
    for context, s in sorted(latest.items()):
        gh("api", "-X", "POST", f"repos/{repo()}/statuses/{sha}", "-f", f"state={state(s)}", "-f", f"context={context}",
           "-f", f"description={('reused: ' + s.get('description', ''))[:140]}", "-f",
           f"target_url={s.get('target_url', '')}", parse=False)


def cmd_verify(opts):
    run_id, sha = opts["source"], opts["sha"]
    try:
        run = run_json(run_id)
        workflow = workflow_at(sha)
        cache = {}
        bases = {}
        for _, code in countries(workflow):
            theirs = source_base(code, run.get("head_sha", ""), run, cache)
            main = main_base(code, sha, cache)
            bases[code] = (theirs, main, same_tree(theirs, main))
        mine = {"key": key(sha), "pairs": pairs(workflow), "inputs": read_json(opts["inputs"]),
                "order_phases": opts.get("order-phases", "both"), "expected_rows": expected_rows(workflow),
                "takes_bases": bases}
        source_facts = run_facts(run_id)
        why = judge(run, run_jobs(run_id), source_facts, mine)
    except (Unproven, subprocess.CalledProcessError, OSError, ValueError, KeyError) as e:
        why = [f"could not check: {e}"]
    if why:
        for reason in why:
            print(f"convergence-reuse: not run {run_id}'s verdict: {reason}", file=sys.stderr)
        summary([f"### Run {run_id}'s verdict does not hold here: the full suite runs", ""] + [f"- {w}" for w in why])
        return 0
    if run.get("head_sha") != sha:
        try:
            copy_statuses(run, sha)
        except Unproven as e:
            print(f"::warning::could not copy run {run_id}'s statuses onto {sha[:9]}: {e}", file=sys.stderr)
    url = run.get("html_url") or run_id
    theirs, own = seeds_of((source_facts or {}).get("inputs")), seeds_of(mine["inputs"])
    summary([f"### Reused run {run_id}'s verdict ({url})", "",
             f"Run {run_id} ran every row this run would over the same tree (key {mine['key'][:12]}), pairs, inputs and "
             f"takes bases, at {str(run.get('head_sha'))[:9]}, and every row passed: no leg runs here.", "",
             f"Its takes-drift rows replayed seeds {theirs} (its run's): the drift verdict reused is those seeds', not this "
             f"run's own rotation ({own}), which no row replayed."])
    print(f"reused-from={run_id}")
    print(f"drift-seeds-judged={theirs}")
    return 0


def cmd_find(opts):
    """The newest green run, from the last day, whose facts name this commit's key and main's inputs; nothing when none
    (or anything fails: the kick then dispatches the full suite)."""
    import datetime
    try:
        mine = key(opts["sha"])
        runs = gh("api", f"repos/{repo()}/actions/runs?workflow={WORKFLOW_FILE}&status=success&per_page={FIND_RUNS}")
        now = datetime.datetime.now(datetime.timezone.utc)
        for run in (runs or {}).get("workflow_runs") or []:
            created = datetime.datetime.fromisoformat(str(run.get("created_at", "")).replace("Z", "+00:00"))
            if (now - created).total_seconds() > FIND_HOURS * 3600:
                break
            try:
                facts = run_facts(run["id"])
            except Unproven:
                continue
            if facts and facts.get("key") == mine and not facts.get("reused_from") \
                    and not inputs_unlike_main(facts.get("inputs") or {}, facts.get("order_phases")):
                print(run["id"])
                return 0
    except (Unproven, subprocess.CalledProcessError, OSError, ValueError, KeyError) as e:
        print(f"convergence-reuse: no source run found: {e}", file=sys.stderr)
    return 0


def parse(argv):
    opts, rest = {}, []
    it = iter(argv)
    for a in it:
        if a.startswith("--"):
            opts[a[2:]] = next(it, "")
        else:
            rest.append(a)
    return opts, rest


def main(argv):
    if not argv:
        print(USAGE, file=sys.stderr)
        return 64
    command, (opts, rest) = argv[0], parse(argv[1:])
    if command == "key":
        print(key(rest[0] if rest else "HEAD"))
        return 0
    if command == "expected-rows":
        print("\n".join(expected_rows(workflow_at(rest[0] if rest else "HEAD"))))
        return 0
    if command == "judge":
        facts = read_json(opts["facts"]) if opts.get("facts") else None
        mine = read_json(opts["mine"])
        mine["takes_bases"] = {c: tuple(b) for c, b in (mine.get("takes_bases") or {}).items()}
        print("\n".join(judge(read_json(opts["run"]), read_json(opts["jobs"]), facts, mine)))
        return 0
    if command == "drift-rows":  # stdin: `gh run view --json jobs`
        print("\n".join(drift_rows((json.load(sys.stdin) or {}).get("jobs") or [])))
        return 0
    if command == "phases-expression":
        print(PHASES_EXPRESSION)
        return 0
    handlers = {"inputs": cmd_inputs, "facts": cmd_facts, "verify": cmd_verify, "find": cmd_find, "complete": cmd_complete, "legs": cmd_legs}
    if command not in handlers:
        print(USAGE, file=sys.stderr)
        return 64
    return handlers[command](opts)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
