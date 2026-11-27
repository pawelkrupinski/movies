"""HTTP, INJECTED (scripts/lib; CLAUDE.md, "Design rules": the network is an injected effect since 2026-11-27): a tool
asks the web through the `Http` its composition root hands it -- `Http.here()` over urllib, `Http.in_memory(answer)`
in a unit test -- never `urllib` itself (GitHub stays `gh`'s: tool_github.py). `Http` holds every rule (a timeout
required, the query encoded into the URL, a JSON body encoded with its content type, header names in one case, a
redirect followed -- or not -- the same way, a body decoded by its charset); a `Transport` is only the boundary, one
request sent and its answer, NEVER following a redirect itself: `UrllibTransport` or `InMemoryTransport`, held alike by
tool_http_test.py's contract suite (the real one against a local server answering what the in-memory one answers).

  http.get(url, timeout, headers=None, query=None, follow_redirects=True) -> Response
  http.post(url, timeout, body=b"", json_body=None, headers=None, follow_redirects=True) -> Response
  Response.status / .ok (2xx) / .header(name) / .text() / .json()
  HttpFailed   no answer at all (refused, timed out, no such host); a 4xx or 5xx is an answer, never raised"""
from __future__ import annotations

import dataclasses
import json
import urllib.error
import urllib.parse
import urllib.request
from typing import Any, Callable, Dict, List, Mapping, Optional, Protocol, Tuple

REDIRECTS = (301, 302, 303, 307, 308)
MAX_REDIRECTS = 10


class HttpFailed(Exception):
    """No answer: the request never got a status back."""


@dataclasses.dataclass(frozen=True)
class Request:
    method: str
    url: str
    headers: Tuple[Tuple[str, str], ...]   # lower-case names, sorted
    body: Optional[bytes]
    timeout: float


@dataclasses.dataclass(frozen=True)
class Response:
    status: int
    body: bytes = b""
    headers: Tuple[Tuple[str, str], ...] = ()   # lower-case names

    @property
    def ok(self) -> bool:
        return 200 <= self.status < 300

    def header(self, name: str) -> Optional[str]:
        return next((v for k, v in self.headers if k == name.lower()), None)

    def text(self) -> str:
        kind = self.header("content-type") or ""
        charset = next((p.split("=", 1)[1].strip() for p in kind.split(";")[1:] if p.strip().startswith("charset=")),
                       "utf-8")
        return self.body.decode(charset, errors="replace")

    def json(self) -> Any:
        return json.loads(self.text())


def lowered(headers: Mapping[str, str]) -> Tuple[Tuple[str, str], ...]:
    return tuple(sorted((k.lower(), v) for k, v in headers.items()))


class Transport(Protocol):
    def send(self, request: Request) -> Response: ...   # HttpFailed when nothing answered; never follows a redirect


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, *args: Any, **kwargs: Any) -> None:
        return None


class UrllibTransport:
    def send(self, request: Request) -> Response:
        sent = urllib.request.Request(request.url, data=request.body, headers=dict(request.headers),
                                      method=request.method)
        opener = urllib.request.build_opener(NoRedirect)
        try:
            with opener.open(sent, timeout=request.timeout) as answer:
                return Response(answer.status, answer.read(), lowered(dict(answer.headers.items())))
        except urllib.error.HTTPError as error:
            with error:
                return Response(error.code, error.read(), lowered(dict(error.headers.items())))
        except (urllib.error.URLError, OSError, ValueError) as error:
            raise HttpFailed(f"{request.method} {request.url}: {error}") from None


class InMemoryTransport:
    """Answers each request with `answer(request)`, recording every one sent."""

    def __init__(self, answer: Callable[[Request], Response]) -> None:
        self.answer = answer
        self.sent: List[Request] = []

    def send(self, request: Request) -> Response:
        self.sent.append(request)
        return self.answer(request)


class Http:
    def __init__(self, transport: Transport) -> None:
        self.transport = transport

    @classmethod
    def here(cls) -> "Http":
        return cls(UrllibTransport())

    @classmethod
    def in_memory(cls, answer: Callable[[Request], Response]) -> "Http":
        return cls(InMemoryTransport(answer))

    def get(self, url: str, timeout: float, headers: Optional[Mapping[str, str]] = None,
            query: Optional[Mapping[str, str]] = None, follow_redirects: bool = True) -> Response:
        if query:
            url += ("&" if "?" in url else "?") + urllib.parse.urlencode(list(query.items()))
        return self.send("GET", url, timeout, dict(headers or {}), None, follow_redirects)

    def post(self, url: str, timeout: float, body: bytes = b"", json_body: Any = None,
             headers: Optional[Mapping[str, str]] = None, follow_redirects: bool = True) -> Response:
        sent: Dict[str, str] = dict(headers or {})
        if json_body is not None:
            body = json.dumps(json_body).encode("utf-8")
            sent.setdefault("Content-Type", "application/json")
        return self.send("POST", url, timeout, sent, body, follow_redirects)

    def send(self, method: str, url: str, timeout: float, headers: Dict[str, str], body: Optional[bytes],
             follow_redirects: bool) -> Response:
        if not timeout > 0:
            raise ValueError(f"a request needs a timeout, not {timeout!r}")
        request = Request(method, url, lowered(headers), body, timeout)
        for _ in range(MAX_REDIRECTS + 1):
            answer = self.transport.send(request)
            onward = redirected(request, answer) if follow_redirects else None
            if onward is None:
                return answer
            request = onward
        raise HttpFailed(f"{method} {url}: more than {MAX_REDIRECTS} redirects")


def redirected(request: Request, answer: Response) -> Optional[Request]:
    """The request a redirect answer sends on to (a 303, or a POST's 301 / 302, turned into a body-less GET), or None
    when the answer is no redirect."""
    where = answer.header("location")
    if answer.status not in REDIRECTS or not where:
        return None
    to_get = answer.status == 303 or (answer.status in (301, 302) and request.method == "POST")
    method = "GET" if to_get else request.method
    return Request(method, urllib.parse.urljoin(request.url, where), request.headers,
                   None if to_get else request.body, request.timeout)
