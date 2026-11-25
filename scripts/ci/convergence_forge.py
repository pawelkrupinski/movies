"""WHAT CONVERGENCE_REUSE READS OF THE REPOSITORY AND THE FORGE (split out of convergence_reuse.py): the tree key of a
commit, the runs, jobs, facts and statuses `gh` answers, and the pair a hermetic leg would resolve now. Every read that
cannot be made is `Unproven`: no reuse, said why. The judgement (convergence_judge.py) and the matrix (convergence_rows.py)
read nothing themselves."""
from __future__ import annotations

import hashlib
import json
import os
import re
import subprocess
import tempfile
from typing import Any, Dict, List, Optional, Tuple

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
PATHS_FILE = ".github/convergence-paths.txt"
WORKFLOW = ".github/workflows/identity-model-convergence.yml"
WORKFLOW_FILE = "identity-model-convergence.yml"
# TWO SUITES, TWO WORKFLOWS (the owner, 2026-11-08): `Identity takes` runs every leg's takes rows (takes, takes-base, the
# takes-drift rows, the harnesses' A/A rows), `Identity model convergence` the rest (convergence, the order passes and their
# comparisons, next-day). Both gate. The countries' rows (LEG_ROWS) are written once, in the convergence workflow, and the
# takes preflight reads them there. Convergence's preflight dispatches the takes run of every run it makes, titled
# TAKES_TITLE: a takes run's parent is the convergence run it was dispatched for (takes_child).
TAKES_WORKFLOW = ".github/workflows/identity-takes.yml"
TAKES_WORKFLOW_FILE = "identity-takes.yml"
SEED_KEYS = ("drift-seed-1", "drift-seed-2")
SUITES = ("convergence", "takes")
SUITE_FILES = {"convergence": WORKFLOW_FILE, "takes": TAKES_WORKFLOW_FILE}
TAKES_TITLE = "Identity takes (convergence run {})"
FACTS_ARTIFACT = "convergence-reuse-facts"
FACTS_NAME = "facts.json"
GH_SECONDS = 60
STATUS_PAGES = 4
BASE_WALK = 100

Status = Dict[str, Any]
StatusCache = Dict[str, List[Status]]


class Unproven(Exception):
    """A fact this run could not read: no reuse, said why."""


# ── the key ───────────────────────────────────────────────────────────────────────────────────────────────────────────


def git(*args: str, cwd: Optional[str] = None) -> str:
    return subprocess.run(["git", *args], cwd=cwd or ROOT, check=True, capture_output=True, text=True).stdout


def patterns_of(text: str) -> List[str]:
    """The paths file's patterns, as the changed-paths action's matches.sh reads them: `dir/**`, an exact path, either
    `!`-prefixed to take paths out; `#` comments, blanks, a YAML list's `- ` and quotes dropped."""
    out: List[str] = []
    for line in text.splitlines():
        line = line.strip()
        line = re.sub(r"^-\s*", "", line).strip("'\"")
        if line and not line.startswith("#"):
            out.append(line)
    return out


def under(path: str, pattern: str) -> bool:
    return path.startswith(pattern[:-2]) if pattern.endswith("/**") else path == pattern


def listed(path: str, patterns: List[str]) -> bool:
    included = any(under(path, p) for p in patterns if not p.startswith("!"))
    return included and not any(under(path, p[1:]) for p in patterns if p.startswith("!"))


def key(rev: str = "HEAD", cwd: Optional[str] = None) -> str:
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


def same_tree(a: Optional[str], b: Optional[str]) -> bool:
    if not a or not b:
        return False
    try:
        a = git("rev-parse", "--verify", f"{a}^{{commit}}").strip()
        b = git("rev-parse", "--verify", f"{b}^{{commit}}").strip()
    except subprocess.CalledProcessError:
        return False
    return a == b or key(a) == key(b)


def workflow_at(rev: str) -> str:
    return git("show", f"{rev}:{WORKFLOW}")


# ── reading the forge ─────────────────────────────────────────────────────────────────────────────────────────────────


def gh(*args: str, parse: bool = True) -> Any:
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


def repo() -> str:
    name = os.environ.get("GH_REPO") or os.environ.get("GITHUB_REPOSITORY")
    if not name:
        raise Unproven("no GH_REPO")
    return name


def run_json(run_id: str) -> Status:
    return gh("api", f"repos/{repo()}/actions/runs/{run_id}")


def run_jobs(run_id: str) -> List[Status]:
    answer: Dict[str, Any] = gh("api", f"repos/{repo()}/actions/runs/{run_id}/jobs?per_page=100") or {}
    return answer.get("jobs") or []


def run_facts(run_id: str) -> Optional[Dict[str, Any]]:
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


def statuses(sha: str, cache: StatusCache) -> List[Status]:
    """Every status on `sha`, newest id first."""
    if sha not in cache:
        out: List[Status] = []
        for page in range(1, STATUS_PAGES + 1):
            batch = gh("api", f"repos/{repo()}/commits/{sha}/statuses?limit=50&page={page}")
            out += batch or []
            if not batch or len(batch) < 50:
                break
        cache[sha] = sorted(out, key=lambda s: s.get("id", 0), reverse=True)
    return cache[sha]


def state(status: Status) -> str:
    return status.get("status") or status.get("state") or ""


def takes_context(code: str) -> str:
    return f"identity takes ({code})"


def _run_marks(run: Status) -> Tuple[str, str]:
    return (f"/actions/runs/{run.get('id')}", f"/actions/runs/{run.get('run_number')}")


def source_base(code: str, sha: str, run: Status, cache: StatusCache) -> Optional[str]:
    """The base the source's takes check judged `sha` against: its `identity takes (<code>)` status of that run."""
    marks = _run_marks(run)
    for s in statuses(sha, cache):
        if s.get("context") == takes_context(code) and str(s.get("target_url", "")).endswith(marks):
            m = re.search(r"\bbase ([0-9a-f]{7,40}) vs head", s.get("description", ""))
            return m.group(1) if (m and state(s) == "success") else None
    return None


def main_base(code: str, sha: str, cache: StatusCache) -> Optional[str]:
    """The base main's takes check would take for `sha`: the newest commit before it, first parents, whose newest
    `identity takes (<code>)` status passed (identity-takes-check.sh's pick_base)."""
    for commit in git("rev-list", "--first-parent", f"--max-count={BASE_WALK}", f"{sha}^").split():
        for s in statuses(commit, cache):
            if s.get("context") == takes_context(code):
                if state(s) == "success":
                    return commit
                break
    return None


def pair_of(code: str) -> str:
    """One country's pair: the pin's line, then the fills published over its tree, sorted."""
    setup = os.path.join(ROOT, ".github/scripts/convergence-setup.sh")
    fill = os.path.join(ROOT, ".github/scripts/convergence-fill.sh")
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
    return " ".join([pin] + sorted(fills))


def takes_child(run_id: str) -> Optional[str]:
    """The Identity takes run convergence's run `run_id` dispatched (its title, TAKES_TITLE), or None when none is listed."""
    title = TAKES_TITLE.format(run_id)
    for page in range(1, STATUS_PAGES + 1):
        answer: Dict[str, Any] = gh("api", f"repos/{repo()}/actions/workflows/{TAKES_WORKFLOW_FILE}/runs?per_page=100&page={page}") or {}
        runs: List[Status] = answer.get("workflow_runs") or []
        for run in runs:
            if run.get("display_title") == title:
                return str(run["id"])
        if len(runs) < 100:
            break
    return None


def copy_statuses(run: Status, sha: str) -> None:
    """The source's statuses of its run, onto `sha` when it is another commit: the next takes check's pick_base and the
    next reuse read them there."""
    cache: StatusCache = {}
    marks = _run_marks(run)
    latest: Dict[str, Status] = {}
    for s in statuses(run["head_sha"], cache):
        if str(s.get("target_url", "")).endswith(marks) and s.get("context") not in latest:
            latest[s["context"]] = s
    for context, s in sorted(latest.items()):
        gh("api", "-X", "POST", f"repos/{repo()}/statuses/{sha}", "-f", f"state={state(s)}", "-f", f"context={context}",
           "-f", f"description={('reused: ' + s.get('description', ''))[:140]}", "-f",
           f"target_url={s.get('target_url', '')}", parse=False)
