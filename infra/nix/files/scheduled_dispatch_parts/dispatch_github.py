"""GitHub as the dispatcher sees it: the App's token, the rate limits, the API calls and their errors.

A part of nix/files/scheduled_dispatch.py, which carries the argument for all of it (the header there).
"""
from __future__ import annotations
import base64
import datetime
import json
import subprocess
import urllib.error
import urllib.request
from typing import Any, Dict, List, Mapping, Optional, TypedDict
from dispatch_clock import epoch


API = "https://api.github.com"

# A GitHub API answer, read where each call needs it: its shape is GitHub's, not ours.
Json = Dict[str, Any]


class _RunFields(TypedDict):
    id: int
    event: str
    created_at: str


class WorkflowRun(_RunFields, total=False):
    """The fields of a workflow run the dispatcher reads: the first three always, the others once it has progressed."""
    status: str
    conclusion: Optional[str]


class ApiError(Exception):
    """A call GitHub would not answer. `retry_at` (a UTC datetime) when it refused for a rate limit: retrying before
    then is refused again."""

    def __init__(self, message: str, retry_at: Optional[datetime.datetime] = None) -> None:
        super().__init__(message)
        self.retry_at = retry_at


# A rate-limited call is retried this long after the reset GitHub names, and never sooner than a minute after the
# refusal (GitHub's advice for a secondary limit that names no time).
RATE_LIMIT_MARGIN_SECONDS = 5

RATE_LIMIT_MIN_WAIT_SECONDS = 60


# ---------------------------------------------------------------- GitHub

def _b64url(data: bytes) -> str:
    return base64.urlsafe_b64encode(data).rstrip(b"=").decode()


def _reset_epoch(header: Mapping[str, str], exhausted: bool, now: float) -> float:
    """The epoch a refused call may be retried at: `retry-after` seconds from now, or the primary limit's reset, and a
    minute from now when the headers name no usable time."""
    try:
        if "retry-after" in header:
            return now + int(header["retry-after"])
        if exhausted and "x-ratelimit-reset" in header:
            return int(header["x-ratelimit-reset"])
    except ValueError:
        pass
    return now + RATE_LIMIT_MIN_WAIT_SECONDS


def rate_limit_reset(status: int, headers: Optional[Mapping[str, str]], body: Optional[str],
                     now: float) -> Optional[datetime.datetime]:
    """When a refused call may be retried (a UTC datetime), or None when the refusal is not a rate limit. A primary
    limit is a 403/429 with `x-ratelimit-remaining: 0` and the reset epoch in `x-ratelimit-reset`; a secondary one
    carries `retry-after` seconds, or only its message."""
    if status not in (403, 429):
        return None
    header = {k.lower(): v for k, v in (headers or {}).items()}
    exhausted = header.get("x-ratelimit-remaining") == "0"
    if not (exhausted or "retry-after" in header or "rate limit" in (body or "").lower()):
        return None
    return datetime.datetime.fromtimestamp(_reset_epoch(header, exhausted, now), datetime.timezone.utc)


def _jwt_signing_input(app_id: str, now: float) -> bytes:
    """The unsigned JWT of the App: its header and claims, 10 minutes' validity."""
    head = _b64url(json.dumps({"alg": "RS256", "typ": "JWT"}, separators=(",", ":")).encode())
    body = _b64url(json.dumps({"iat": int(now) - 60, "exp": int(now) + 540, "iss": str(app_id)},
                              separators=(",", ":")).encode())
    return f"{head}.{body}".encode()


def _signature(returncode: int, stdout: bytes, stderr: bytes) -> bytes:
    """What `openssl dgst -sign` printed, or the ApiError saying it could not sign."""
    if returncode != 0 or not stdout:
        raise ApiError(f"openssl could not sign with the App key: {stderr.decode(errors='replace')[:200]}")
    return stdout


def app_jwt(app_id: str, key_file: str, now: float) -> str:
    signing = _jwt_signing_input(app_id, now)
    signed = subprocess.run(["openssl", "dgst", "-sha256", "-sign", key_file], input=signing,
                            capture_output=True, timeout=30)
    return f"{signing.decode()}.{_b64url(_signature(signed.returncode, signed.stdout, signed.stderr))}"


def _api_error(method: str, path: str, e: BaseException, now: float) -> ApiError:
    """The ApiError of a failed call: GitHub's refusal with its status and body (a rate limit says when to retry), or
    the transport's own error."""
    if not isinstance(e, urllib.error.HTTPError):
        return ApiError(f"{method} {path}: {e}")
    body = e.read()[:500].decode(errors="replace")
    return ApiError(f"{method} {path}: HTTP {e.code} {body[:200]}",
                    retry_at=rate_limit_reset(e.code, dict(e.headers or {}), body, now))


def _request(url: str, method: str, bearer: str, body: Optional[Json]) -> urllib.request.Request:
    return urllib.request.Request(
        url, method=method, data=json.dumps(body).encode() if body is not None else None,
        headers={"Authorization": f"Bearer {bearer}", "Accept": "application/vnd.github+json",
                 "X-GitHub-Api-Version": "2022-11-28", "User-Agent": "kinowo-scheduled-dispatch",
                 "Content-Type": "application/json"})


def _answer(request: urllib.request.Request) -> bytes:
    """The body GitHub answered, `null` when it is empty."""
    with urllib.request.urlopen(request, timeout=30) as response:
        return response.read() or b"null"


class GitHub:
    """The calls the dispatcher makes, as an installation of a GitHub App."""

    def __init__(self, repository: str, app_id: str, key_file: str, api: str = API) -> None:
        self.repository, self.app_id, self.key_file, self.api = repository, app_id, key_file, api
        self._token, self._minted = "", 0.0

    def _http(self, method: str, path: str, bearer: str, body: Optional[Json] = None) -> Json:
        request = _request(f"{self.api}/{path}", method, bearer, body)
        try:
            return json.loads(_answer(request))
        except (urllib.error.URLError, OSError, ValueError) as e:
            raise _api_error(method, path, e, epoch()) from None

    def _mint(self) -> str:
        jwt = app_jwt(self.app_id, self.key_file, epoch())
        installation = self._http("GET", f"repos/{self.repository}/installation", jwt)["id"]
        granted = self._http("POST", f"app/installations/{installation}/access_tokens", jwt,
                             {"repositories": [self.repository.split("/", 1)[1]]})
        return granted["token"]

    def token(self) -> str:
        """An installation token, re-minted after 50 of its 60 minutes (the guard outlives one)."""
        now = epoch()
        if now - self._minted > 3000:
            self._token, self._minted = self._mint(), now
        return self._token

    def runs_since(self, workflow_file: str, since: datetime.datetime) -> List[WorkflowRun]:
        created = since.strftime("%Y-%m-%dT%H:%M:%SZ")
        answer = self._http("GET", f"repos/{self.repository}/actions/workflows/{workflow_file}/runs"
                                   f"?created=%3E%3D{created}&per_page=100", self.token())
        return answer.get("workflow_runs") or []

    def cancel(self, run_id: int) -> None:
        self._http("POST", f"repos/{self.repository}/actions/runs/{run_id}/cancel", self.token())

    def dispatch(self, event_type: str, payload: Json) -> None:
        self._http("POST", f"repos/{self.repository}/dispatches", self.token(),
                   {"event_type": event_type, "client_payload": payload})
