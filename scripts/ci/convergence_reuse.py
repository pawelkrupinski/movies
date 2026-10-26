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
#     (the workflow's own), every country and row (no takes-only, takes-countries, recording), the takes skip gate in shadow
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
#   convergence_reuse.py judge --run F --jobs F --facts F --mine F
#                                                       judge() over JSON files: each reason a line, none when it holds
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

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
PATHS_FILE = ".github/convergence-paths.txt"
WORKFLOW = ".github/workflows/identity-model-convergence.yml"
WORKFLOW_FILE = "identity-model-convergence.yml"
FACTS_ARTIFACT = "convergence-reuse-facts"
FACTS_NAME = "facts.json"
CODES = ("pl", "de", "uk", "es", "us")
# The leg's own expansion of a country into rows (country-convergence-leg.yml's `phase:` matrix). expected_rows() models
# exactly this; ConvergenceReuseSpec fails the moment the leg's expression is anything else.
PHASES_EXPRESSION = (
    "${{ fromJson(inputs.takes-only && '[\"takes\",\"takes-base\"]' || format('[\"convergence\"{0}{1}{2}]', "
    "inputs.order-command != '' && (inputs.order-split && (inputs.order-one-phase && ',\"order-independence-p01\"' || "
    "',\"order-independence-p01\",\"order-independence-p12\"') || ',\"order-independence\"') || '', "
    "inputs.next-day-command != '' && ',\"next-day\"' || '', inputs.mode == 'hermetic' && inputs.takes-row && "
    "',\"takes\"' || '')) }}"
)
GH_SECONDS = 60
STATUS_PAGES = 4
BASE_WALK = 100
FIND_RUNS = 30
FIND_HOURS = 24


USAGE = "usage: convergence_reuse.py key|inputs|facts|verify|find|expected-rows|judge ... (see the header)"


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
    takes row on and both order phases (main's order-phases.sh answer)."""
    out = []
    for row in matrix_rows(workflow_text):
        phases = ["convergence"]
        if row.get("order", ""):
            split = row.get("order-split", "false") == "true"
            phases += ["order-independence-p01", "order-independence-p12"] if split else ["order-independence"]
        if row.get("next-day", ""):
            phases.append("next-day")
        phases.append("takes")
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
    "recording": ("INPUT_RECORDING", ""),
    "takes-countries": ("INPUT_TAKES_COUNTRIES", ""),
    "takes-no-store": ("INPUT_TAKES_NO_STORE", "false"),
    "takes-skip": ("INPUT_TAKES_SKIP", "shadow"),
}
BOOLEANS = {"profile", "takes-only", "takes-no-store"}


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


def inputs_unlike_main(inputs, order_phases):
    """Why legs handed `inputs` (and `order_phases`) do not run what main's run does; empty when they do."""
    why = []
    if inputs.get("takes-only") != "false":
        why.append("takes-only: only the takes rows ran")
    for name in ("takes-countries", "recording", "jvm-options", "runner"):
        if inputs.get(name, ""):
            why.append(f"{name} {inputs[name]!r}: not main's suite")
    if inputs.get("takes-skip") not in ("shadow", "off"):
        why.append(f"takes-skip {inputs.get('takes-skip')!r}: main takes every take in full (shadow)")
    if order_phases != "both":
        why.append(f"order-independence phases {order_phases!r}: main runs both (p01 and p12)")
    return why


# ── the judgement ─────────────────────────────────────────────────────────────────────────────────────────────────────


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
    result = {j.get("name"): j for j in jobs}
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
    for name, value in inputs.items():
        print(f"{name}={value}")
    if os.environ.get("LEG_INPUTS_FILE"):
        with open(os.environ["LEG_INPUTS_FILE"], "w") as f:
            json.dump(inputs, f, sort_keys=True)
    return 0


def cmd_facts(opts):
    workflow = workflow_at(opts["sha"])
    inputs = read_json(opts["inputs"])
    facts = {"version": 1, "run_id": os.environ.get("GITHUB_RUN_ID", ""), "sha": opts["sha"], "key": key(opts["sha"]),
             "inputs": inputs, "order_phases": opts.get("order-phases", "both"),
             "reused_from": opts.get("reused-from", "")}
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
        why = judge(run, run_jobs(run_id), run_facts(run_id), mine)
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
    summary([f"### Reused run {run_id}'s verdict ({url})", "",
             f"Run {run_id} ran every row this run would over the same tree (key {mine['key'][:12]}), pairs, inputs and "
             f"takes bases, at {str(run.get('head_sha'))[:9]}, and every row passed: no leg runs here."])
    print(f"reused-from={run_id}")
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
    if command == "phases-expression":
        print(PHASES_EXPRESSION)
        return 0
    handlers = {"inputs": cmd_inputs, "facts": cmd_facts, "verify": cmd_verify, "find": cmd_find}
    if command not in handlers:
        print(USAGE, file=sys.stderr)
        return 64
    return handlers[command](opts)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
