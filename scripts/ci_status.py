#!/usr/bin/env python3
"""The logic behind scripts/ci-status (its header has the usage); tests: scripts/ci_status_test.py."""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import subprocess
import sys
import time

GH = os.environ.get("KINOWO_GH") or "gh"
REPO = os.environ.get("KINOWO_GH_REPO") or os.environ.get("KINOWO_REPO") or "pawelkrupinski/movies"
GATE_WORKFLOWS = ["main.yml", "identity-model-convergence.yml", "identity-takes.yml"]
CACHE_SECONDS = 30
GH_TIMEOUT = 60
OK = ("success", "skipped", "neutral")


def cache_dir() -> str:
    return os.environ.get("CI_STATUS_CACHE_DIR") or os.path.expanduser("~/.cache/ci-status")


def gh_api(path: str, use_cache: bool):
    """The JSON gh api answers for `path`, from the cache when it is under CACHE_SECONDS old; None when unreachable."""
    key = os.path.join(cache_dir(), hashlib.sha1(f"{REPO} {path}".encode()).hexdigest())
    try:
        if use_cache and time.time() - os.path.getmtime(key) < CACHE_SECONDS:
            with open(key) as f:
                return json.load(f)
    except (OSError, ValueError):
        pass
    try:
        done = subprocess.run([GH, "api", f"repos/{REPO}/{path}"], capture_output=True, text=True,
                              timeout=GH_TIMEOUT, stdin=subprocess.DEVNULL)
        if done.returncode != 0:
            return None
        data = json.loads(done.stdout)
    except (subprocess.TimeoutExpired, OSError, ValueError):
        return None
    try:
        os.makedirs(cache_dir(), exist_ok=True)
        with open(key, "w") as f:
            json.dump(data, f)
    except OSError:
        pass
    return data


def resolve_sha(rev: str) -> str:
    try:
        done = subprocess.run(["git", "rev-parse", "--verify", "-q", rev], capture_output=True, text=True, timeout=10)
        return done.stdout.strip() if done.returncode == 0 else rev
    except (subprocess.TimeoutExpired, OSError):
        return rev


def matches(run: dict, workflow: str) -> bool:
    path = (run.get("path") or "").split("@")[0]
    return workflow in (os.path.basename(path), run.get("name"))


def workflow_lines(workflows, branch, sha, use_cache):
    runs = gh_api(f"actions/runs?branch={branch}&per_page=60", use_cache)
    if runs is None:
        return None
    lines = []
    for workflow in workflows:
        hits = [r for r in runs.get("workflow_runs", []) if matches(r, workflow)
                and (not sha or r.get("head_sha", "").startswith(sha))]
        hit = max(hits, key=lambda r: r.get("created_at", ""), default=None)
        if hit is None:
            lines.append("\t".join([workflow, "-", sha[:10] or "-", "none", "-", "-"]))
        else:
            lines.append("\t".join([workflow, str(hit["id"]), hit["head_sha"][:10], hit.get("status") or "-",
                                    hit.get("conclusion") or "-", hit.get("created_at", "-")]))
    return lines


def job_annotations(job_id, use_cache):
    """A job that did not succeed with no failed step (cancelled before a step ran, or never given a runner) has no log to
    read: its cause is in its check run's annotations. iOS #526 (run 43903912001): ui-tests (macos-latest) sat 15 min
    with no runner and was cancelled, the cause only in "The job was not acquired by Runner of type hosted even after
    multiple attempts" -- and ci-why, reading logs, called it UNCLEAR. Each distinct annotation once, indented."""
    data = gh_api(f"check-runs/{job_id}/annotations", use_cache)
    seen = []
    for annotation in data if isinstance(data, list) else []:
        line = f"  {annotation.get('annotation_level') or '-'}: {(annotation.get('message') or '').strip()}"
        if line not in seen:
            seen.append(line)
    return seen


def run_lines(run_id, use_cache):
    data = gh_api(f"actions/runs/{run_id}/jobs?per_page=100", use_cache)
    if data is None:
        return None
    jobs = data.get("jobs", [])
    lines = [f"run {run_id}: {len(jobs)} jobs, {sum(1 for j in jobs if j.get('conclusion') in OK)} ok"]
    for job in jobs:
        if job.get("conclusion") in OK:
            continue
        steps = ", ".join(s["name"] for s in job.get("steps", []) if s.get("conclusion") == "failure")
        lines.append("\t".join([job["name"], str(job.get("id", "-")), job.get("status") or "-",
                                job.get("conclusion") or "-", steps or "-"]))
        if not steps and job.get("id") is not None:
            lines += job_annotations(job["id"], use_cache)
    lines.append(f"why (reads the failed step's log): ~/bin/agentctl ci-why {run_id}")
    return lines


def main(argv):
    parser = argparse.ArgumentParser(prog="ci-status", add_help=False)
    parser.add_argument("--sha")
    parser.add_argument("--workflow", action="append")
    parser.add_argument("--branch", default="main")
    parser.add_argument("--run")
    parser.add_argument("--no-cache", action="store_true")
    args = parser.parse_args(argv)
    use_cache = not args.no_cache
    if args.run:
        lines = run_lines(args.run, use_cache)
    else:
        sha = resolve_sha(args.sha) if args.sha else ""
        lines = workflow_lines(args.workflow or GATE_WORKFLOWS, args.branch, sha, use_cache)
    if lines is None:
        print("ci-status: gh could not be reached (or answered nothing)", file=sys.stderr)
        return 2
    print("\n".join(lines))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
