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
     when REQUIRE_SECURE_SESSION_COOKIE=true, which the workflow sets now that
     `KINOWO_SESSION_SECURE` is live on both hosts; without it a missing `Secure` is only a
     GitHub warning (for a run against a host that has not turned it on).

AN UNREACHABLE ORIGIN IS A FAILURE, NOT A PASS. Datacenter IPs (GitHub's runners, then ci-runner)
are Cloudflare Bot-Fight-Mode material on both zones (2026-09-15), and a challenge page carries
none of the headers checked here -- so "no ACAC header" would pass against a page that is not
ours. Every request therefore has an EXPECTED status (401 anonymous /api/me, 2xx preflight, 303
OAuth start), and anything else fails with the status and `cf-mitigated` named.

DIRECT FIRST, FROM CI-RUNNER, WITH THE EDGE KEY. Runs #2239 on (2026-10-27): the check went
through the Decodo residential proxy only, and Decodo's account went dark (CONNECT 503 on every
port), so a header check of our own sites failed for a third party's reason. Bot Fight Mode has no
custom-rule skip on the Free plan; its one exception is an IP Access Rule, matched first. So both
zones Allow ci-runner's address (162.55.0.154), and a custom rule BLOCKS that address on
kinowo.net / showtimes.cc unless the request carries `X-Kinowo-Edge-Check: <EDGE_CHECK_KEY>` --
the bypass is the address AND the secret, nothing else (infra/bin/cloudflare-ci-edge-allow sets
both up). Every request goes direct with that header; only a direct answer that is not the
origin's (a 403, or no connection) is retried through the proxy, when DECODO_PROXY_USER /
DECODO_PROXY_PASS are set. The proxy is the fallback, never the verdict's excuse: a challenge
through either path is still a failure.

THE PROXY IS A POOL, AND A DEAD PORT IS NAMED AS THE PROXY'S. Decodo's ports are one static IP
each; the pool is the worker's, read from its committed residential-proxy.properties. Runs #2173
and #2188 (2026-10-27): the one port this check used to pin, 10001, answered `503` on the tunnel
for every request while both origins were healthy (retrying that same port three times changed
nothing). A proxy-side error (the CONNECT tunnel refused, or a timeout / refused / reset connection
while reaching the proxy) now moves the request to the next port, and the dead port is skipped for
the rest of the run; only when every port has failed does the check report `proxy unavailable
(Decodo ...)` with each port's error -- still a failure, since nothing about the origin was checked.

A THIRD PARTY'S FAILURE IS AN ALERT, NOT A RED RUN (ci-failures-2026-10-30, recommendation 2). A request the origin
never answered -- no connection, the proxy pool dark, a Cloudflare challenge (`cf-mitigated`) -- says nothing about our
headers, so it is no regression of ours: when every failure is of that kind the check exits 3 (UNREACHED), which the
workflow turns into an alert (one tracked issue, refreshed while it lasts, closed by the next clean run) and a green
run. Any header the origin DID answer wrongly exits 1 as before: that is our code.

Usage: check-security-headers.py [host ...]     (default: kinowo.net showtimes.cc)
Exit: 0 every check passed; 1 a header regression (ours); 3 only unreached origins (a third party's: an alert).
The pure `evaluate_*` functions are unit-tested offline by check_security_headers_test.py.
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
PROXY_PROPERTIES = os.path.join(os.path.dirname(os.path.abspath(__file__)), os.pardir, os.pardir,
                                "worker", "src", "main", "resources", "residential-proxy.properties")
PROXY_UNAVAILABLE = "proxy unavailable (Decodo"
EDGE_CHECK_HEADER = "X-Kinowo-Edge-Check"


def header(headers, name):
    """Every value of `name` (case-insensitive) in a list of (name, value) pairs."""
    return [v for k, v in headers if k.lower() == name.lower()]


def reachability_failures(what, status, expected, headers):
    """A response that is not the origin answering the way it always does is not evidence."""
    if status in expected:
        return []
    if str(status).startswith(PROXY_UNAVAILABLE):
        return ["%s: %s -- the origin was not reached, so nothing about it could be checked" % (what, status)]
    mitigated = header(headers, "cf-mitigated")
    return ["%s: got HTTP %s, expected %s%s -- not the origin's answer, so nothing else about it "
            "can be checked" % (what, status, "/".join(map(str, sorted(expected))),
                                " (cf-mitigated: %s)" % mitigated[0] if mitigated else "")]


def unreached(status, headers):
    """Whether the origin never answered: no connection, the proxy pool dark, or a Cloudflare challenge in its place."""
    return not isinstance(status, int) or bool(header(headers, "cf-mitigated"))


UNREACHED = 3


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


def _proxy_credentials():
    return os.environ.get("DECODO_PROXY_USER", "").strip(), os.environ.get("DECODO_PROXY_PASS", "").strip()


def _edge_check_key():
    return os.environ.get("EDGE_CHECK_KEY", "").strip()


def redact(text):
    """`text` with any URL userinfo, the proxy credentials and the edge key (raw or URL-quoted)
    masked. A failed fetch's text goes into an `::error::` line of a public run log, and the proxy
    URL carries the Decodo user and password."""
    text = re.sub(r"(\w+://)[^/@\s]+@", r"\1***@", text)
    for secret in _proxy_credentials() + (_edge_check_key(),):
        if secret:
            for form in {secret, urllib.request.quote(secret, safe="")}:
                text = text.replace(form, "***")
    return text


def proxy_pool(path=PROXY_PROPERTIES):
    """(host, [port, ...]) from the worker's residential-proxy.properties."""
    properties = {}
    with open(path) as source:
        for line in source:
            line = line.strip()
            if line and not line.startswith("#") and "=" in line:
                key, value = line.split("=", 1)
                properties[key.strip()] = value.strip()
    return properties["host"], [port.strip() for port in properties["ports"].split(",") if port.strip()]


class ProxyPoolUnavailable(Exception):
    pass


class ProxyPool:
    """An opener over every port of the pool: a request goes through the port the last one
    succeeded on; a proxy-side error retires that port for the run and moves on to the next."""

    def __init__(self, host, ports, open_via):
        self.host, self.ports, self.open_via = host, list(ports), open_via
        self.dead = {}

    def open(self, request, timeout):
        for port in [port for port in self.ports if port not in self.dead]:
            try:
                return self.open_via(port, request, timeout)
            except Exception as error:  # noqa: BLE001 -- sorted below: only a proxy-side error moves on
                if not is_proxy_side(error):
                    raise
                self.dead[port] = _describe(error)
                print("Decodo %s:%s retired for this run (%s %s): %s"
                      % (self.host, port, request.get_method(), request.full_url, self.dead[port]))
        raise ProxyPoolUnavailable("every port of %s failed: %s" % (self.host, "; ".join(
            "%s: %s" % (port, self.dead[port]) for port in self.ports)))


class DirectThenProxy:
    """Direct first; a direct answer that is not the origin's -- a 403 (Cloudflare's challenge or
    the edge-key rule's block) or no connection at all -- is tried once more through the proxy.
    Whatever the proxy answers is the verdict, so a challenge there still fails the check."""

    def __init__(self, direct, proxy):
        self.direct, self.proxy = direct, proxy

    def open(self, request, timeout):
        try:
            return self.direct.open(request, timeout=timeout)
        except urllib.error.HTTPError as error:
            if error.code != 403:
                raise
            print("direct %s %s: HTTP 403%s -- retrying through the proxy"
                  % (request.get_method(), request.full_url, "".join(
                      " (cf-mitigated: %s)" % v for v in header(list(error.headers.items()), "cf-mitigated"))))
        except urllib.error.URLError as error:
            print("direct %s %s: %s -- retrying through the proxy"
                  % (request.get_method(), request.full_url, _describe(error)))
        return self.proxy.open(request, timeout)


def _opener():
    direct = urllib.request.build_opener(_NoRedirect())
    user, password = _proxy_credentials()
    if not (user and password):
        return direct
    host, ports = proxy_pool()
    openers = {}

    def open_via(port, request, timeout):
        if port not in openers:
            proxy = "http://%s:%s@%s:%s" % (urllib.request.quote(user, safe=""), urllib.request.quote(password, safe=""), host, port)
            openers[port] = urllib.request.build_opener(_NoRedirect(), urllib.request.ProxyHandler({"https": proxy, "http": proxy}))
        return openers[port].open(request, timeout=timeout)
    return DirectThenProxy(direct, ProxyPool(host, ports, open_via))


def is_proxy_side(error):
    """urllib wraps what fails while connecting -- reaching the proxy, its CONNECT tunnel -- in a
    URLError; a tunnel the proxy refused reads `Tunnel connection failed: <status>`. A TLS error
    is the origin's certificate, through a tunnel that worked."""
    if not isinstance(error, urllib.error.URLError) or isinstance(error, urllib.error.HTTPError):
        return False
    reason = error.reason
    if isinstance(reason, (TimeoutError, ConnectionError)):
        return True
    return isinstance(reason, OSError) and str(reason).startswith("Tunnel connection failed")


def _describe(error):
    return redact("%s: %s" % (type(error).__name__, error))


def fetch(opener, method, url, headers):
    key = _edge_check_key()
    request = urllib.request.Request(url, method=method, headers={
        "User-Agent": "kinowo-security-headers-check", **({EDGE_CHECK_HEADER: key} if key else {}), **headers})
    try:
        with opener.open(request, timeout=30) as response:
            return response.status, list(response.headers.items())
    except urllib.error.HTTPError as error:
        return error.code, list(error.headers.items())
    except ProxyPoolUnavailable as error:
        return "%s): %s" % (PROXY_UNAVAILABLE, redact(str(error))), []
    except Exception as error:  # noqa: BLE001 -- any failure is "unreachable", never a traceback
        return "unreachable (%s)" % _describe(error), []


def check_host(opener, host, require_secure):
    """(failures, warnings, unreached): a request the origin never answered goes to `unreached`, judged no further."""
    origin = {"Origin": FOREIGN_ORIGIN}
    failures, warnings, missed = [], [], []

    def answered(what, status, expected, headers):
        if status not in expected and unreached(status, headers):
            missed.extend(reachability_failures(what, status, expected, headers))
            return False
        return True

    what = "%s GET /api/me" % host
    status, headers = fetch(opener, "GET", "https://%s/api/me" % host, origin)
    if answered(what, status, {401}, headers):
        failures += evaluate_cors(what, status, {401}, headers)
    what = "%s OPTIONS /api/me/… (preflight)" % host
    status, headers = fetch(opener, "OPTIONS", "https://%s/api/me/pl/hidden-films/x" % host,
                            dict(origin, **{"Access-Control-Request-Method": "PUT"}))
    if answered(what, status, {200, 204}, headers):
        failures += evaluate_cors(what, status, {200, 204}, headers)
    what = "%s GET /auth/google/start" % host
    status, headers = fetch(opener, "GET", "https://%s/auth/google/start" % host, {})
    if answered(what, status, {303}, headers):
        session_failures, session_warnings = evaluate_session_cookie(what, status, headers, require_secure)
        failures += session_failures
        warnings += session_warnings
    return failures, warnings, missed


def main(argv):
    hosts = argv[1:] or HOSTS
    require_secure = os.environ.get("REQUIRE_SECURE_SESSION_COOKIE", "").strip().lower() == "true"
    opener = _opener()
    failures, warnings, missed = [], [], []
    for host in hosts:
        host_failures, host_warnings, host_missed = check_host(opener, host, require_secure)
        failures += host_failures
        warnings += host_warnings
        missed += host_missed
    for warning in warnings:
        print("::warning::%s (REQUIRE_SECURE_SESSION_COOKIE is not set, so this is not enforced)" % warning)
    for failure in failures:
        print("::error::%s" % failure)
    for miss in missed:
        print("::warning::UNREACHED %s" % miss)
    if failures:
        return 1
    if missed:
        print("ALERT: %d request(s) never reached the origin (a third party's failure, not a header regression)" % len(missed))
        return UNREACHED
    print("ok  %s: no credentialed CORS for a foreign origin, HSTS >= 1 year%s"
          % (", ".join(hosts), ", Secure session cookie" if require_secure else ""))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
