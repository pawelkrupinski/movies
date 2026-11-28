"""GitHub's REST API for the scheduler: the repository's queued jobs, read with a GitHub App's installation token and
ETags -- the requests through the Machine it is given."""
from __future__ import annotations

import json
import urllib.error
from email.message import Message
from typing import Any, Dict, List, Optional, Tuple

from .machine import Machine
from .model import Config

TOKEN_SECONDS = 45 * 60  # an installation token lives an hour: minted again after this


class GitHub:
    """GitHub's REST API with an installation token of a GitHub App (the token helper bitcashier's infra ships), and
    ETags: an unchanged list answers 304, which costs no rate limit."""

    API = "https://api.github.com"

    def __init__(self, machine: Machine, cfg: Config, app_id: str, key: str):
        self.machine = machine
        self.cfg, self.app_id, self.key = cfg, app_id, key
        self.token, self.minted = "", 0.0
        self.etags: Dict[str, Tuple[str, Any]] = {}
        self.remaining = 5000

    def _token(self) -> str:
        if not self.token or self.machine.now() - self.minted > TOKEN_SECONDS:
            self.token, self.minted = self._mint(), self.machine.now()
        return self.token

    def _mint(self) -> str:
        out = self.machine.run(["bash", "-c", f'. "{self.cfg.token_helper}" && github_app_token "$0" "$1" "$2"',
                                self.app_id, self.key, self.cfg.repo], timeout=60)
        if out.returncode != 0 or not out.stdout.strip():
            raise RuntimeError(f"no installation token for App {self.app_id}: {out.stderr.strip()[-200:]}")
        return out.stdout.strip()

    def request(self, method: str, url: str, body: Optional[bytes] = None, content_type: str = "application/json") -> Any:
        url = url if url.startswith("https://") else self.API + url
        cached = self.etags.get(url) if method == "GET" else None
        try:
            headers, data = self.machine.http(method, url, body, self.headers(content_type, cached), timeout=30)
        except urllib.error.HTTPError as e:
            return self.answer_error(e, method, cached)
        return self.answer(method, url, headers, data)

    def headers(self, content_type: str, cached: Optional[Tuple[str, Any]]) -> Dict[str, str]:
        headers = {"Authorization": f"Bearer {self._token()}", "Accept": "application/vnd.github+json",
                   "X-GitHub-Api-Version": "2022-11-28", "Content-Type": content_type}
        return headers | {"If-None-Match": cached[0]} if cached else headers

    def answer(self, method: str, url: str, headers: Message, data: bytes) -> Any:
        """The response's JSON (None for none), its ETag kept for the next GET of `url`, the rate budget left noted."""
        self.remaining = int(headers.get("X-RateLimit-Remaining", self.remaining))
        value = json.loads(data) if data else None
        if method == "GET" and headers.get("ETag"):
            self.etags[url] = (headers["ETag"], value)
        return value

    def answer_error(self, e: urllib.error.HTTPError, method: str, cached: Optional[Tuple[str, Any]]) -> Any:
        """304 is the cached value, a 404 of a GET or DELETE is nothing; every other error is raised."""
        if e.code == 304 and cached:
            return cached[1]
        if e.code == 404 and method in ("GET", "DELETE"):
            return None
        raise e

    def queued_jobs(self) -> List[Dict[str, Any]]:
        """Every queued job of the repository's queued and in-progress runs (a matrix row waits inside a running run)."""
        jobs: List[Dict[str, Any]] = []
        for status in ("queued", "in_progress"):
            runs = self.request("GET", f"/repos/{self.cfg.repo}/actions/runs?status={status}&per_page=100") or {}
            for run in runs.get("workflow_runs", []):
                page = self.request("GET", f"/repos/{self.cfg.repo}/actions/runs/{run['id']}/jobs?filter=latest&per_page=100")
                jobs += [j for j in (page or {}).get("jobs", []) if j.get("status") == "queued"]
        return jobs
