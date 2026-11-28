"""THE REUSE and A REMOTE PASS (scripts/hooks/pre-push's header): a passed run's key answers a re-push of the SAME
tree, base, changed paths, hook, red-gate state, known-red baseline and PREPUSH_SKIP; a precommit-check.yml run GitHub
vouches for answers for the commit it checked. The key is `git hash-object` of the same text the shell hook hashed."""
from __future__ import annotations

import re
from dataclasses import dataclass
from fnmatch import fnmatchcase
from typing import List, Optional

from pre_push_hook.plan.pre_push_repo import Repo
from seams.tool_files import Files

WORKFLOW = ".github/workflows/precommit-check.yml"
VERDICT_JOBS = re.compile(r"""^[ \t]+VERDICT_JOBS:[ \t]*['"]?([^'"#\n]*)""", re.M)
FAILED = ("failure", "timed_out", "startup_failure", "action_required")
PASSED = ("success", "skipped", "neutral")


@dataclass(frozen=True)
class Verdict:
    """What a key is made of, bar the hook's own files."""
    head: str
    base: str
    gate: str
    baseline: str        # the known-red baseline file, or ""
    skip: str
    checks: List[str]
    changed: List[str]


def hook_digest(repo: Repo, files: Files, hook: str) -> Optional[str]:
    """The hook, the shell it sources and its Python (scripts/hooks/pre-push-files.txt), hashed as one text."""
    scripts = hook.rsplit("/", 2)[0]
    listed = files.read_text_or(f"{scripts}/hooks/pre-push-files.txt", "")
    names = [hook, f"{scripts}/hooks-path.sh", f"{scripts}/tool-conventions.sh"] + [
        f"{scripts}/{line[len('scripts/'):]}" for line in listed.split("\n") if line.startswith("scripts/")]
    try:
        text = "".join(files.read_text(name) for name in names)
    except (OSError, UnicodeDecodeError):
        return None
    return hashed(repo, text)


def hashed(repo: Repo, text: str) -> Optional[str]:
    code, out = repo.git("hash-object", "--stdin", text_in=text)
    return out.strip() if code == 0 and out.strip() else None


def key(repo: Repo, files: Files, hook: str, v: Verdict) -> Optional[str]:
    """The reuse key, None when any part of it cannot be read."""
    known = hashed(repo, files.read_text_or(v.baseline, "")) if v.baseline else "none"
    tree = repo.git_out("rev-parse", "--verify", "-q", f"{v.head}^{{tree}}").strip()
    base_tree = repo.git_out("rev-parse", "--verify", "-q", f"{v.base}^{{tree}}").strip()
    digest = hook_digest(repo, files, hook)
    if not (known and tree and base_tree and digest):
        return None
    skip = "".join(w + " " for w in sorted(set(v.skip.split())))
    text = (f"tree {tree}\nbase {base_tree}\nhook {digest}\ngate {v.gate}\nbaseline {known}\nskip {skip}\n"
            f"checks {' '.join(v.checks)}\n" + "".join(p + "\n" for p in v.changed))
    return hashed(repo, text)


def verdict_globs(repo: Repo, head: str) -> List[str]:
    code, text = repo.git("show", f"{head}:{WORKFLOW}")
    found = VERDICT_JOBS.search(text) if code == 0 else None
    return [g.strip() for g in found.group(1).split(",")] if found else []


def remote_pass(repo: Repo, run: str, head: str, seconds: float) -> bool:
    """Whether GitHub vouches for precommit-check.yml run `run` on `head`: it succeeded, or, still going, every
    verdict job (its VERDICT_JOBS globs) concluded in success or skipped and no job of it failed."""
    code, said = repo.output(["gh", "api", f"repos/{{owner}}/{{repo}}/actions/runs/{run}", "--jq",
                              '"\\(.path) \\(.conclusion) \\(.head_sha)"'], timeout=seconds, quiet=True)
    said = said.strip()
    if code != 0 or said == f"{WORKFLOW} success {head}":
        return code == 0
    globs = verdict_globs(repo, head) if said == f"{WORKFLOW} null {head}" else []
    if not globs:
        return False
    code, jobs = repo.output(["gh", "api", f"repos/{{owner}}/{{repo}}/actions/runs/{run}/jobs?per_page=100", "--jq",
                              '.jobs[] | "\\(.status)\\t\\(.conclusion)\\t\\(.name)"'], timeout=seconds, quiet=True)
    return code == 0 and jobs_pass(jobs, globs)


def job_says(line: str, globs: List[str]) -> Optional[bool]:
    """A job's word: False when it failed, or is a verdict job not (yet) passed; True for a passed verdict job; None
    for any other."""
    status, _, rest = line.partition("\t")
    conclusion, _, name = rest.partition("\t")
    if not status:
        return None
    if conclusion in FAILED:
        return False
    if not any(g and fnmatchcase(name, g) for g in globs):
        return None
    return status == "completed" and conclusion in PASSED


def jobs_pass(jobs: str, globs: List[str]) -> bool:
    said = [job_says(line, globs) for line in jobs.split("\n")]
    return False not in said and True in said
