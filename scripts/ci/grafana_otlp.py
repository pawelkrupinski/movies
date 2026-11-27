"""METRICS TO GRAFANA OVER OTLP, the one way the CI reporters push them (scripts/ci; the sparse checkouts that run them
check out this directory): a JSON OTLP payload POSTed to <GRAFANA_URL>/ci/otlp/v1/metrics with the bearer token,
bounded, never following a redirect, never raising.

  post(payload, url, token)  None when pushed, else why not ("HTTP 302", the error)
  NoRedirect / OPEN          the handler and the opener it builds"""
from __future__ import annotations

import json
import urllib.request
from typing import Any, Callable, Optional


class NoRedirect(urllib.request.HTTPRedirectHandler):
    """A redirect here is the Google sign-in answering instead of the token gate; following it
    would read the login page's 200 as a successful push."""

    def redirect_request(self, *args: Any, **kwargs: Any) -> None:
        return None


def OPEN(*args: Any, **kwargs: Any) -> Any:
    """The opener that does not follow a redirect, opened (built per call: no network at import)."""
    return urllib.request.build_opener(NoRedirect).open(*args, **kwargs)


def post(payload: object, url: str, token: str, opener: Callable[..., Any] = OPEN) -> Optional[str]:
    request = urllib.request.Request(
        url.rstrip("/") + "/ci/otlp/v1/metrics", data=json.dumps(payload).encode(), method="POST",
        headers={"Content-Type": "application/json", "Authorization": "Bearer " + token})
    try:
        with opener(request, timeout=20) as response:
            status = response.status
    except Exception as error:  # noqa: BLE001 -- a push never fails its job: the caller says why
        return str(error)
    return None if status == 200 else "HTTP %s" % status
