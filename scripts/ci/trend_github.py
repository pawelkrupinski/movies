"""GitHub as the trend tools read it (scripts/ci/ci_trends.py): the API through urllib with GH_TOKEN/GITHUB_TOKEN when set
(the collector), else the `gh` CLI (a laptop), with a disk cache for what cannot change any more -- a completed run's
jobs and metrics artifacts -- and a run's record built from them.

    from trend_github import GitHub, record_of, latest_attempts
"""
from __future__ import annotations

import io
import json
import os
import re
import subprocess
import time
import urllib.parse
import urllib.request
import zipfile
from typing import Any, Callable, Dict, Iterable, List, Optional, Tuple

from trend_records import Record, job_record, run_record

METRICS_ARTIFACT_PREFIX = "ci-metrics-"
# Where completed runs' jobs and metrics are kept: CI_TIMINGS_CACHE, else systemd's STATE_DIRECTORY (the collector),
# else the user's cache. A cache that cannot be written is skipped, never an error.
CACHE_DIR = os.environ.get("CI_TIMINGS_CACHE") or (
    os.path.join(os.environ["STATE_DIRECTORY"].split(":")[0], "cache") if os.environ.get("STATE_DIRECTORY")
    else os.path.join(os.environ.get("XDG_CACHE_HOME") or os.path.expanduser("~/.cache"), "ci-timings"))
GH_TIMEOUT_SECONDS = 120
_NEXT_PAGE = re.compile(r'<([^>]+)>;\s*rel="next"')
Page = Tuple[Any, Optional[str]]


class DropAuthOnRedirect(urllib.request.HTTPRedirectHandler):
    """An artifact download redirects to a signed blob-storage URL, which refuses a request that also carries the
    API's bearer token: the token stays on api.github.com."""

    def redirect_request(self, req: urllib.request.Request, fp: Any, code: int, msg: str, headers: Any,
                         newurl: str) -> Optional[urllib.request.Request]:
        new = super().redirect_request(req, fp, code, msg, headers, newurl)
        if new is not None and urllib.parse.urlsplit(newurl).netloc != urllib.parse.urlsplit(req.full_url).netloc:
            for name in list(new.headers):
                if name.lower() == "authorization":
                    del new.headers[name]
            new.unredirected_hdrs.pop("Authorization", None)
        return new


_OPENER = urllib.request.build_opener(DropAuthOnRedirect)


def parse_metrics(text: str) -> List[Record]:
    lines: List[Record] = []
    for raw in text.splitlines():
        raw = raw.strip()
        if raw.startswith("{"):
            try:
                lines.append(json.loads(raw))
            except ValueError:
                continue
    return lines


def _gh_cli(url: str) -> Tuple[bytes, str]:
    """One GET through `gh api -i`: the body and the `Link` header."""
    args = ["gh", "api", "-i", url.replace("https://api.github.com/", "")]
    done = subprocess.run(args, check=True, capture_output=True, timeout=GH_TIMEOUT_SECONDS)
    head, _, body = done.stdout.partition(b"\r\n\r\n")
    if not body and b"\n\n" in done.stdout:
        head, _, body = done.stdout.partition(b"\n\n")
    link = ""
    for line in head.decode(errors="replace").splitlines():
        if line.lower().startswith("link:"):
            link = line.split(":", 1)[1]
    return body, link


def _api_request(url: str, token: str) -> Tuple[bytes, str]:
    """One GET with the bearer token: the body and the `Link` header."""
    request = urllib.request.Request(url, headers={"Authorization": "Bearer " + token,
                                                   "Accept": "application/vnd.github+json",
                                                   "X-GitHub-Api-Version": "2022-11-28"})
    with _OPENER.open(request, timeout=GH_TIMEOUT_SECONDS) as response:
        return response.read(), response.headers.get("Link") or ""


class GitHub:
    """The API, with a disk cache for what cannot change any more: a completed run's jobs and metrics artifacts."""

    def __init__(self, repo: str, cache_dir: str = CACHE_DIR, token: Optional[str] = None) -> None:
        self.repo = repo
        self.cache_dir = os.path.join(cache_dir, repo.replace("/", "_"))
        self.token = token if token is not None else (os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN"))
        self.calls = 0

    def _url(self, path: str) -> str:
        path = path if path.startswith("repos/") else "repos/%s/%s" % (self.repo, path)
        return "https://api.github.com/" + path

    def _get(self, url: str, raw: bool = False, attempts: int = 3) -> Page:
        """One GET, retried twice on a transient failure (a 5xx, a reset, a timeout) after 2 s and 6 s."""
        for attempt in range(attempts - 1):
            try:
                return self._get_once(url, raw)
            except (subprocess.CalledProcessError, subprocess.TimeoutExpired, OSError) as error:
                status = getattr(error, "code", None)
                if status is not None and 400 <= status < 500 and status != 429:
                    raise
                time.sleep(2 * 3 ** attempt)
        return self._get_once(url, raw)

    def _get_once(self, url: str, raw: bool = False) -> Page:
        self.calls += 1
        body, link = _api_request(url, self.token) if self.token else _gh_cli(url)
        following = _NEXT_PAGE.search(link)
        return body if raw else json.loads(body.decode() or "null"), (following.group(1) if following else None)

    def api(self, path: str, raw: bool = False, pages: int = 1) -> Any:
        """One document; with pages > 1, a list of up to that many pages, following `Link: rel=next`."""
        url = self._url(path)
        if raw or pages == 1:
            return self._get(url, raw)[0]
        documents: List[Any] = []
        following: Optional[str] = url
        while following and len(documents) < pages:
            document, following = self._get(following)
            documents.append(document)
        return documents

    def cached(self, name: str, compute: Callable[[], Any]) -> Any:
        path = os.path.join(self.cache_dir, name)
        if os.path.exists(path):
            with open(path, encoding="utf-8") as handle:
                return json.load(handle)
        value = compute()
        try:
            os.makedirs(self.cache_dir, exist_ok=True)
            with open(path + ".tmp", "w", encoding="utf-8") as handle:
                json.dump(value, handle)
            os.replace(path + ".tmp", path)
        except OSError:
            pass
        return value

    def runs(self, workflow: Optional[str] = None, branch: Optional[str] = None, created: Optional[str] = None,
             limit: Optional[int] = None, status: str = "completed") -> List[Record]:
        """Runs newest first; `created` is GitHub's search syntax (`>=2026-11-01`, `a..b`)."""
        query = {"per_page": "100", "status": status}
        if branch:
            query["branch"] = branch
        if created:
            query["created"] = created
        base = ("actions/workflows/%s/runs" % workflow) if workflow else "actions/runs"
        found: List[Record] = []
        for page in self.api(base + "?" + urllib.parse.urlencode(query), pages=10):
            found += page["workflow_runs"]
            if limit and len(found) >= limit:
                break
        return found[:limit] if limit else found

    def run(self, run_id: Any) -> Record:
        return self.api("actions/runs/%s" % run_id)

    def jobs(self, run: Record) -> List[Record]:
        def fetch() -> List[Record]:
            pages = self.api("actions/runs/%s/attempts/%s/jobs?per_page=100" % (run["id"], run.get("run_attempt") or 1),
                             pages=5)
            return [job for page in pages for job in page["jobs"]]
        if run.get("status") == "completed":
            return self.cached("jobs-%s-%s.json" % (run["id"], run.get("run_attempt") or 1), fetch)
        return fetch()

    def _artifact_lines(self, artifact: Record) -> Optional[List[Record]]:
        """The metrics lines of one `ci-metrics-*` artifact; None when it is unreadable or names no runner."""
        try:
            blob = self.api("actions/artifacts/%s/zip" % artifact["id"], raw=True)
            with zipfile.ZipFile(io.BytesIO(blob)) as archive:
                text = archive.read("metrics.jsonl").decode(errors="replace")
        except Exception:  # noqa: BLE001 -- one unreadable artifact never loses the run
            return None
        lines = parse_metrics(text)
        return lines if any(line.get("runner_name") for line in lines) else None

    def metrics(self, run: Record) -> Dict[str, List[Record]]:
        """Every job's metrics lines from the run's `ci-metrics-*` artifacts, by artifact name (one runner can upload
        several: metrics_for picks a job's by its runner and start)."""
        def fetch() -> Dict[str, List[Record]]:
            found: Dict[str, List[Record]] = {}
            pages = self.api("actions/runs/%s/artifacts?per_page=100" % run["id"], pages=3)
            for artifact in [a for page in pages for a in page.get("artifacts", [])]:
                if artifact.get("expired") or not artifact["name"].startswith(METRICS_ARTIFACT_PREFIX):
                    continue
                lines = self._artifact_lines(artifact)
                if lines:
                    found[artifact["name"]] = lines
            return found
        if run.get("status") == "completed":
            return self.cached("metrics2-%s-%s.json" % (run["id"], run.get("run_attempt") or 1), fetch)
        return fetch()


def record_of(github: GitHub, run: Record, with_metrics: bool = True) -> Record:
    """The run's record: the API's jobs for every timing, the jobs' metrics artifacts for the rest."""
    jobs = github.jobs(run)
    metrics: Dict[str, List[Record]] = {}
    if with_metrics:
        try:
            metrics = github.metrics(run)
        except Exception:  # noqa: BLE001 -- the timings stand without them
            metrics = {}
    return run_record(run, jobs, metrics)


def latest_attempts(records: Iterable[Record]) -> List[Record]:
    kept: Dict[Any, Record] = {}
    for record in records:
        key = record["id"]
        if key not in kept or (record.get("attempt") or 1) > (kept[key].get("attempt") or 1):
            kept[key] = record
    return list(kept.values())


def job_windows(github: GitHub, run: Record) -> List[Tuple[float, float]]:
    """The (start, end) of every job of `run` that ran a step: what a run's slot saturation is counted from."""
    records = [job_record(job) for job in github.jobs(run)]
    return [(record["s"], record["e"]) for record in records if record]
