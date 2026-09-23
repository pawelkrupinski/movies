#!/usr/bin/env python3
"""Assert the live sites' security headers, and exit non-zero naming every one that is wrong.

WHY IT EXISTS. Two regressions reached production with nothing watching the RESPONSES:
`/api/me` granted `Access-Control-Allow-Credentials: true` to any Origin, so any page on the web
could read a signed-in visitor's state with their cookie; and HSTS was one week, not a year. Both
were header values -- no test layer renders a response through Caddy and Cloudflare, and no alert
reads a header -- so the only place they can be checked is against the real hosts.

What it asserts, per host (kinowo.net, showtimes.cc):
  1. /api/me, GET and a PUT preflight, with a FOREIGN Origin: no `Access-Control-Allow-Credentials:
     true`. A wildcard `Access-Control-Allow-Origin: *` without credentials is fine -- that is the
     public API -- and browsers refuse to combine `*` with credentials anyway.
  2. `Strict-Transport-Security` with `max-age` of at least one year, on every response checked.
  3. The session cookie (`PLAY_SESSION`, set by /auth/google/start) carries `Secure` -- ENFORCED
     only when REQUIRE_SECURE_SESSION_COOKIE=true, because `KINOWO_SESSION_SECURE` is not live yet;
     until then a missing `Secure` is printed as a GitHub warning, so flipping the variable is
     the whole of turning it on.

AN UNREACHABLE ORIGIN IS A FAILURE, NOT A PASS. GitHub's runners are Cloudflare Bot-Fight-Mode
material on both zones (2026-09-15), and a challenge page carries none of the headers checked
here -- so "no ACAC header" would pass against a page that is not ours. Every request therefore
has an EXPECTED status (401 anonymous /api/me, 2xx preflight, 303 OAuth start), and anything else
fails with the status and `cf-mitigated` named. Requests go through the Decodo residential proxy
when KINOWO_PROXY_USER / KINOWO_PROXY_PASS are set, as the OG-card workflow's do.

Usage: check-security-headers.py [host ...]     (default: kinowo.net showtimes.cc)
The pure `evaluate_*` functions are unit-tested offline by test_check_security_headers.py.
"""
import os
import re
import sys
import urllib.error
import urllib.request

HOSTS = ("kinowo.net", "showtimes.cc")
ONE_YEAR = 365 * 24 * 3600
FOREIGN_ORIGIN = "https://security-check.invalid"
SESSION_COOKIE = "PLAY_SESSION"
PROXY_HOST = "isp.decodo.com:10001"


def header(headers, name):
    """Every value of `name` (case-insensitive) in a list of (name, value) pairs."""
    return [v for k, v in headers if k.lower() == name.lower()]


def reachability_failures(what, status, expected, headers):
    """A response that is not the origin answering the way it always does is not evidence."""
    if status in expected:
        return []
    mitigated = header(headers, "cf-mitigated")
    return ["%s: got HTTP %s, expected %s%s -- not the origin's answer, so nothing else about it "
            "can be checked" % (what, status, "/".join(map(str, sorted(expected))),
                                " (cf-mitigated: %s)" % mitigated[0] if mitigated else "")]


def hsts_failures(what, headers):
    values = header(headers, "strict-transport-security")
    if not values:
        return ["%s: no Strict-Transport-Security header" % what]
    match = re.search(r"max-age\s*=\s*\"?(\d+)", values[0], re.IGNORECASE)
    if not match:
        return ["%s: Strict-Transport-Security has no max-age: %r" % (what, values[0])]
    if int(match.group(1)) < ONE_YEAR:
        return ["%s: HSTS max-age=%s is under a year (%d)" % (what, match.group(1), ONE_YEAR)]
    return []


def evaluate_cors(what, status, expected, headers):
    """No credentials for a foreign Origin, on an origin-answered response with a year of HSTS."""
    failures = reachability_failures(what, status, expected, headers)
    if failures:
        return failures
    if any(v.strip().lower() == "true" for v in header(headers, "access-control-allow-credentials")):
        failures.append("%s: Access-Control-Allow-Credentials: true for foreign Origin %s (Allow-Origin: %s) "
                        "-- any site can read a signed-in visitor's state"
                        % (what, FOREIGN_ORIGIN, ", ".join(header(headers, "access-control-allow-origin")) or "-"))
    return failures + hsts_failures(what, headers)


def evaluate_session_cookie(what, status, headers, require_secure):
    """Returns (failures, warnings)."""
    failures = reachability_failures(what, status, {303}, headers)
    if failures:
        return failures, []
    failures = hsts_failures(what, headers)
    cookies = [c for c in header(headers, "set-cookie") if c.split("=", 1)[0].strip() == SESSION_COOKIE]
    if not cookies:
        return failures + ["%s: no %s Set-Cookie -- the check cannot see the session cookie any more"
                           % (what, SESSION_COOKIE)], []
    attributes = {a.split("=", 1)[0].strip().lower() for a in cookies[0].split(";")[1:]}
    if "secure" in attributes:
        return failures, []
    message = "%s: the %s cookie is not Secure" % (what, SESSION_COOKIE)
    return (failures + [message], []) if require_secure else (failures, [message])


class _NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, *args, **kwargs):
        return None


def _opener():
    handlers = [_NoRedirect()]
    user, password = os.environ.get("KINOWO_PROXY_USER", "").strip(), os.environ.get("KINOWO_PROXY_PASS", "").strip()
    if user and password:
        proxy = "http://%s:%s@%s" % (urllib.request.quote(user, safe=""), urllib.request.quote(password, safe=""), PROXY_HOST)
        handlers.append(urllib.request.ProxyHandler({"https": proxy, "http": proxy}))
    return urllib.request.build_opener(*handlers)


def fetch(opener, method, url, headers):
    request = urllib.request.Request(url, method=method, headers={"User-Agent": "kinowo-security-headers-check", **headers})
    try:
        with opener.open(request, timeout=30) as response:
            return response.status, list(response.headers.items())
    except urllib.error.HTTPError as error:
        return error.code, list(error.headers.items())
    except OSError as error:
        return "unreachable (%s)" % error, []


def check_host(opener, host, require_secure):
    origin = {"Origin": FOREIGN_ORIGIN}
    failures, warnings = [], []
    status, headers = fetch(opener, "GET", "https://%s/api/me" % host, origin)
    failures += evaluate_cors("%s GET /api/me" % host, status, {401}, headers)
    status, headers = fetch(opener, "OPTIONS", "https://%s/api/me/pl/hidden-films/x" % host,
                            dict(origin, **{"Access-Control-Request-Method": "PUT"}))
    failures += evaluate_cors("%s OPTIONS /api/me/… (preflight)" % host, status, {200, 204}, headers)
    status, headers = fetch(opener, "GET", "https://%s/auth/google/start" % host, {})
    session_failures, session_warnings = evaluate_session_cookie(
        "%s GET /auth/google/start" % host, status, headers, require_secure)
    return failures + session_failures, warnings + session_warnings


def main(argv):
    hosts = argv[1:] or HOSTS
    require_secure = os.environ.get("REQUIRE_SECURE_SESSION_COOKIE", "").strip().lower() == "true"
    opener = _opener()
    failures, warnings = [], []
    for host in hosts:
        host_failures, host_warnings = check_host(opener, host, require_secure)
        failures += host_failures
        warnings += host_warnings
    for warning in warnings:
        print("::warning::%s (set REQUIRE_SECURE_SESSION_COOKIE=true once KINOWO_SESSION_SECURE is live)" % warning)
    for failure in failures:
        print("::error::%s" % failure)
    if not failures:
        print("ok  %s: no credentialed CORS for a foreign origin, HSTS >= 1 year%s"
              % (", ".join(hosts), ", Secure session cookie" if require_secure else ""))
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
