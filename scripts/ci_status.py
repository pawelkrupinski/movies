#!/usr/bin/env python3
"""The logic behind scripts/ci-status (its header has the usage); tests: scripts/ci_status_test.py."""
from __future__ import annotations

import argparse
import hashlib
import os
import sys
from typing import Any, Callable

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from processes import machine  # noqa: E402
import tool_git  # noqa: E402
from vcs import tool_github  # noqa: E402
from seams import tool_state  # noqa: E402
from seams.tool_files import Files  # noqa: E402

GATE_WORKFLOWS = ["main.yml", "identity-model-convergence.yml", "identity-takes.yml"]
CACHE_SECONDS = 30
GH_TIMEOUT = 60
OK = ("success", "skipped", "neutral")


def gh_binary() -> str:
    return machine.environment().get("KINOWO_GH") or "gh"


def repository() -> str:
    env = machine.environment()
    return env.get("KINOWO_GH_REPO") or env.get("KINOWO_REPO") or "pawelkrupinski/movies"


def cache_dir() -> str:
    return machine.environment().get("CI_STATUS_CACHE_DIR") or os.path.expanduser("~/.cache/ci-status")


class CachedApi:
    """gh api's answers for one repository, each kept CACHE_SECONDS in <cache>/<sha1 of the repository and path>, read
    and written through the injected files; `use_cache` off asks gh every time (and still refreshes the cache)."""

    def __init__(self, github: tool_github.GitHub, files: Files, wall: Callable[[], float], repository: str,
                 cache: str, use_cache: bool) -> None:
        self.github, self.files, self.wall = github, files, wall
        self.repository, self.cache, self.use_cache = repository, cache, use_cache

    def get(self, path: str) -> Any:
        """The JSON gh api answers for `path`, from the cache when it is fresh; None when unreachable."""
        kept = tool_state.JsonState(self.files, os.path.join(
            self.cache, hashlib.sha1(f"{self.repository} {path}".encode()).hexdigest()))
        try:
            if self.use_cache and self.wall() - self.files.mtime(kept.path) < CACHE_SECONDS:
                return kept.load()
        except (OSError, ValueError):
            pass
        data = self.github.json(["api", f"repos/{self.repository}/{path}"], GH_TIMEOUT, quiet=True, closed_stdin=True)
        if data is None:
            return None
        try:
            kept.save(data)
        except OSError:
            pass
        return data


def resolve_sha(rev: str) -> str:
    return tool_git.Git.here().line("rev-parse", "--verify", "-q", rev, timeout=10) or rev


def matches(run: dict, workflow: str) -> bool:
    path = (run.get("path") or "").split("@")[0]
    return workflow in (os.path.basename(path), run.get("name"))


def workflow_lines(api: CachedApi, workflows, branch, sha):
    runs = api.get(f"actions/runs?branch={branch}&per_page=60")
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


def job_annotations(api: CachedApi, job_id):
    """A job that did not succeed with no failed step (cancelled before a step ran, or never given a runner) has no log to
    read: its cause is in its check run's annotations. iOS #526 (run 43903912001): ui-tests (macos-latest) sat 15 min
    with no runner and was cancelled, the cause only in "The job was not acquired by Runner of type hosted even after
    multiple attempts" -- and ci-why, reading logs, called it UNCLEAR. Each distinct annotation once, indented."""
    data = api.get(f"check-runs/{job_id}/annotations")
    seen = []
    for annotation in data if isinstance(data, list) else []:
        line = f"  {annotation.get('annotation_level') or '-'}: {(annotation.get('message') or '').strip()}"
        if line not in seen:
            seen.append(line)
    return seen


def run_lines(api: CachedApi, run_id):
    data = api.get(f"actions/runs/{run_id}/jobs?per_page=100")
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
            lines += job_annotations(api, job["id"])
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
    github = tool_github.GitHub(tool_git.shared_runner(), machine.environment(), gh_binary())
    api = CachedApi(github, Files.here(), machine.wall, repository(), cache_dir(), use_cache=not args.no_cache)
    if args.run:
        lines = run_lines(api, args.run)
    else:
        sha = resolve_sha(args.sha) if args.sha else ""
        lines = workflow_lines(api, args.workflow or GATE_WORKFLOWS, args.branch, sha)
    if lines is None:
        print("ci-status: gh could not be reached (or answered nothing)", file=sys.stderr)
        return 2
    print("\n".join(lines))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
