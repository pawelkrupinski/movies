#!/usr/bin/env python3
"""check-security-headers.py's verdicts, driven with header sets recorded from the live hosts.

Offline on purpose: the workflow runs this BEFORE the live check, so a checker that has stopped
being able to fail is caught on the run that would otherwise have passed on it.

Run: python3 scripts/ci/check_security_headers_test.py
"""
import os
import ssl
import sys
import unittest
import urllib.error

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import script_module  # noqa: E402

check = script_module.load(os.path.join(HERE, "check-security-headers.py"))

# kinowo.net GET /api/me with a foreign Origin, as served on 2026-09-23 after the CORS fix.
API_ME_TODAY = [
    ("access-control-allow-origin", "*"),
    ("strict-transport-security", "max-age=31536000"),
    ("vary", "Accept-Encoding,Origin"),
]
# The shape before it: the Origin echoed back WITH credentials.
API_ME_CREDENTIALED = [
    ("access-control-allow-origin", "https://security-check.invalid"),
    ("access-control-allow-credentials", "true"),
    ("strict-transport-security", "max-age=31536000"),
]
# A Cloudflare Bot-Fight challenge: none of the headers checked, so it must NOT pass.
CHALLENGE = [("cf-mitigated", "challenge"), ("content-type", "text/html")]
# /auth/google/start before KINOWO_SESSION_SECURE went live: session cookie without Secure.
SESSION_WITHOUT_SECURE = [
    ("strict-transport-security", "max-age=31536000"),
    ("set-cookie", "PLAY_SESSION=eyJ0; Max-Age=7776000; Expires=Tue, 22 Dec 2026 14:45:27 GMT; SameSite=Lax; Path=/; HTTPOnly"),
]
SESSION_SECURE = [
    ("strict-transport-security", "max-age=31536000; includeSubDomains"),
    ("set-cookie", "PLAY_SESSION=eyJ0; Max-Age=7776000; SameSite=Lax; Path=/; Secure; HTTPOnly"),
]


class Cors(unittest.TestCase):
    def test_todays_wildcard_without_credentials_passes(self):
        self.assertEqual(check.evaluate_cors("GET /api/me", 401, {401}, API_ME_TODAY), [])

    def test_credentials_for_a_foreign_origin_fail(self):
        failures = check.evaluate_cors("GET /api/me", 401, {401}, API_ME_CREDENTIALED)
        self.assertEqual(len(failures), 1)
        self.assertIn("Access-Control-Allow-Credentials: true", failures[0])

    def test_a_challenge_page_fails_instead_of_passing_for_want_of_headers(self):
        failures = check.evaluate_cors("GET /api/me", 403, {401}, CHALLENGE)
        self.assertEqual(len(failures), 1)
        self.assertIn("cf-mitigated: challenge", failures[0])

    def test_an_unreachable_host_fails(self):
        self.assertTrue(check.evaluate_cors("GET /api/me", "unreachable (timed out)", {401}, []))


class Hsts(unittest.TestCase):
    def test_one_week_fails(self):
        failures = check.hsts_failures("x", [("Strict-Transport-Security", "max-age=604800")])
        self.assertEqual(len(failures), 1)
        self.assertIn("under a year", failures[0])

    def test_one_year_passes_with_directives_around_it(self):
        self.assertEqual(check.hsts_failures("x", [("strict-transport-security", "includeSubDomains; max-age=31536000; preload")]), [])

    def test_missing_fails(self):
        self.assertTrue(check.hsts_failures("x", []))

    def test_short_hsts_fails_the_cors_check_too(self):
        headers = [("access-control-allow-origin", "*"), ("strict-transport-security", "max-age=604800")]
        self.assertTrue(check.evaluate_cors("GET /api/me", 401, {401}, headers))


class SessionCookie(unittest.TestCase):
    def test_not_secure_is_a_warning_until_required(self):
        failures, warnings = check.evaluate_session_cookie("start", 303, SESSION_WITHOUT_SECURE, require_secure=False)
        self.assertEqual(failures, [])
        self.assertEqual(len(warnings), 1)

    def test_not_secure_fails_once_required(self):
        failures, warnings = check.evaluate_session_cookie("start", 303, SESSION_WITHOUT_SECURE, require_secure=True)
        self.assertEqual(len(failures), 1)
        self.assertIn("not Secure", failures[0])

    def test_secure_passes(self):
        self.assertEqual(check.evaluate_session_cookie("start", 303, SESSION_SECURE, require_secure=True), ([], []))

    def test_a_missing_session_cookie_fails_rather_than_passing(self):
        failures, _ = check.evaluate_session_cookie("start", 303, SESSION_WITHOUT_SECURE[:1], require_secure=True)
        self.assertEqual(len(failures), 1)

    def test_a_non_redirect_fails(self):
        failures, _ = check.evaluate_session_cookie("start", 403, CHALLENGE, require_secure=False)
        self.assertEqual(len(failures), 1)


class ProxyCredentialsNeverPrinted(unittest.TestCase):
    """The proxy URL carries the Decodo user and password, and a failed fetch's text goes
    straight into an `::error::` line in a public run log. Whatever an exception says about the
    proxy, and whatever kind of exception it is, the credentials must not come out."""

    USER, PASSWORD = "decodo-user", "s3cr3t/pass"

    def _failing_opener(self, error):
        class Opener:
            def open(self, request, timeout):
                raise error
        return Opener()

    def _fetched(self, error):
        env = {"DECODO_PROXY_USER": self.USER, "DECODO_PROXY_PASS": self.PASSWORD}
        old = {k: os.environ.get(k) for k in env}
        os.environ.update(env)
        try:
            status, _ = check.fetch(self._failing_opener(error), "GET", "https://kinowo.net/api/me", {})
        finally:
            for k, v in old.items():
                if v is None:
                    os.environ.pop(k, None)
                else:
                    os.environ[k] = v
        return str(status)

    def assertRedacted(self, text):
        for secret in (self.USER, self.PASSWORD, "s3cr3t%2Fpass"):
            self.assertNotIn(secret, text)

    def test_an_os_error_naming_the_proxy_url_is_redacted(self):
        text = self._fetched(OSError("Tunnel via http://decodo-user:s3cr3t%2Fpass@isp.decodo.com:10001 failed"))
        self.assertIn("unreachable", text)
        self.assertRedacted(text)

    def test_the_raw_credentials_are_redacted_too(self):
        self.assertRedacted(self._fetched(OSError("407 for decodo-user / s3cr3t/pass")))

    def test_an_exception_that_is_not_an_os_error_is_still_a_redacted_result(self):
        text = self._fetched(ValueError("proxy http://decodo-user:s3cr3t%2Fpass@isp.decodo.com:10001 rejected"))
        self.assertIn("unreachable", text)
        self.assertRedacted(text)


class ProxyFailures(unittest.TestCase):
    """Runs #2173 and #2188: the Decodo port the check pinned (10001) answered `503` on the tunnel
    for all six requests while both origins were healthy, and retrying that same port changed
    nothing. A proxy-side error moves the request to the pool's next port; a dead port is not tried
    again; only a pool with no port left is reported, as the proxy's, with every port's error."""

    TUNNEL_503 = urllib.error.URLError(OSError("Tunnel connection failed: 503 Service Unavailable"))
    PROXY_TIMEOUT = urllib.error.URLError(TimeoutError("timed out"))
    ANSWER = (401, [("strict-transport-security", "max-age=31536000")])

    class Response:
        def __init__(self, status, headers):
            self.status, self.headers = status, dict(headers)

        def __enter__(self):
            return self

        def __exit__(self, *exc):
            return False

    def _pool(self, by_port):
        """A pool whose port answers with by_port[port]: an exception is raised, a tuple returned."""
        calls = []

        def open_via(port, request, timeout):
            calls.append(port)
            outcome = by_port[port]
            if isinstance(outcome, BaseException):
                raise outcome
            return self.Response(*outcome)
        return check.ProxyPool("isp.decodo.com", list(by_port), open_via), calls

    def _fetch(self, pool):
        return check.fetch(pool, "GET", "https://kinowo.net/api/me", {})

    def test_the_pool_is_the_workers(self):
        host, ports = check.proxy_pool()
        self.assertEqual(host, "isp.decodo.com")
        self.assertGreater(len(ports), 1)
        self.assertIn("10001", ports)

    def test_a_dead_port_moves_the_request_on_and_is_not_tried_again(self):
        pool, calls = self._pool({"10001": self.TUNNEL_503, "10002": self.PROXY_TIMEOUT, "10003": self.ANSWER})
        self.assertEqual(self._fetch(pool), self.ANSWER)
        self.assertEqual(self._fetch(pool), self.ANSWER)
        self.assertEqual(calls, ["10001", "10002", "10003", "10003"])

    def test_a_pool_with_every_port_dead_is_named_as_the_proxy_not_the_origin(self):
        pool, calls = self._pool({"10001": self.TUNNEL_503, "10002": self.PROXY_TIMEOUT})
        status, _ = self._fetch(pool)
        self.assertTrue(str(status).startswith("proxy unavailable (Decodo"), status)
        self.assertIn("10001: URLError: <urlopen error Tunnel connection failed: 503", status)
        self.assertIn("10002: URLError: <urlopen error timed out>", status)
        failures = check.evaluate_cors("kinowo.net GET /api/me", status, {401}, [])
        self.assertEqual(len(failures), 1)
        self.assertIn("proxy unavailable (Decodo", failures[0])
        self.assertNotIn("not the origin's answer", failures[0])
        self._fetch(pool)
        self.assertEqual(calls, ["10001", "10002"])

    def test_an_origin_side_error_does_not_retire_the_port(self):
        pool, calls = self._pool({"10001": urllib.error.URLError(ssl.SSLError("certificate verify failed")),
                                  "10002": self.ANSWER})
        status, _ = self._fetch(pool)
        self.assertTrue(str(status).startswith("unreachable"), status)
        self.assertEqual((calls, pool.dead), (["10001"], {}))

    def test_an_origin_answer_with_an_error_status_is_the_origins(self):
        error = urllib.error.HTTPError("https://kinowo.net/api/me", 403, "Forbidden", {"cf-mitigated": "challenge"}, None)
        pool, calls = self._pool({"10001": error, "10002": self.ANSWER})
        self.assertEqual(self._fetch(pool), (403, [("cf-mitigated", "challenge")]))
        self.assertEqual(calls, ["10001"])


class DirectThenProxyTest(unittest.TestCase):
    """Run #2239 on (2026-10-27): Decodo dark, so the proxy-only check failed for a third party's
    reason. Direct (ci-runner, IP Access Allow + the edge key) answers first; only a direct answer
    that is not the origin's goes to the proxy, and the proxy's answer -- a challenge included --
    is the verdict."""

    CHALLENGE_403 = urllib.error.HTTPError("https://kinowo.net/api/me", 403, "Forbidden", {"cf-mitigated": "challenge"}, None)
    ANSWER = (401, [("strict-transport-security", "max-age=31536000")])

    class Leg:
        def __init__(self, outcome):
            self.outcome, self.requests = outcome, []

        def open(self, request, timeout):
            self.requests.append(request)
            if isinstance(self.outcome, BaseException):
                raise self.outcome
            return ProxyFailures.Response(*self.outcome)

    def _fetch(self, direct, proxy):
        return check.fetch(check.DirectThenProxy(self.Leg(direct), proxy), "GET", "https://kinowo.net/api/me", {})

    def test_a_direct_origin_answer_never_touches_the_proxy(self):
        proxy = self.Leg(self.ANSWER)
        self.assertEqual(self._fetch(self.ANSWER, proxy), self.ANSWER)
        self.assertEqual(proxy.requests, [])

    def test_a_direct_challenge_falls_back_to_the_proxy(self):
        proxy = self.Leg(self.ANSWER)
        self.assertEqual(self._fetch(self.CHALLENGE_403, proxy), self.ANSWER)
        self.assertEqual(len(proxy.requests), 1)

    def test_a_challenge_on_both_paths_still_fails(self):
        status, headers = self._fetch(self.CHALLENGE_403, self.Leg(self.CHALLENGE_403))
        failures = check.evaluate_cors("kinowo.net GET /api/me", status, {401}, headers)
        self.assertEqual(len(failures), 1)
        self.assertIn("cf-mitigated: challenge", failures[0])

    def test_a_direct_challenge_with_the_proxy_dark_fails_naming_the_proxy(self):
        dark = check.ProxyPool("isp.decodo.com", ["10001"], lambda port, request, timeout: (_ for _ in ()).throw(
            urllib.error.URLError(OSError("Tunnel connection failed: 503 Service Unavailable"))))
        status, _ = self._fetch(self.CHALLENGE_403, dark)
        self.assertTrue(str(status).startswith("proxy unavailable (Decodo"), status)

    def test_a_direct_origin_error_status_is_not_retried(self):
        proxy = self.Leg(self.ANSWER)
        error = urllib.error.HTTPError("https://kinowo.net/api/me", 500, "Server Error", {}, None)
        self.assertEqual(self._fetch(error, proxy), (500, []))
        self.assertEqual(proxy.requests, [])


class EdgeKeyTest(unittest.TestCase):
    """The edge key rides on every request (the custom rule blocks ci-runner without it), and never
    reaches a log line."""

    def setUp(self):
        self.saved = os.environ.get("EDGE_CHECK_KEY")
        os.environ["EDGE_CHECK_KEY"] = "s3cr3t-edge/key"

    def tearDown(self):
        if self.saved is None:
            os.environ.pop("EDGE_CHECK_KEY", None)
        else:
            os.environ["EDGE_CHECK_KEY"] = self.saved

    def test_every_request_carries_the_key(self):
        leg = DirectThenProxyTest.Leg(DirectThenProxyTest.ANSWER)
        check.fetch(leg, "OPTIONS", "https://kinowo.net/api/me", {"Origin": "https://x.invalid"})
        self.assertEqual(leg.requests[0].get_header("X-kinowo-edge-check"), "s3cr3t-edge/key")
        self.assertEqual(leg.requests[0].get_header("Origin"), "https://x.invalid")

    def test_the_key_is_redacted(self):
        self.assertNotIn("s3cr3t", check.redact("failed with s3cr3t-edge/key and s3cr3t-edge%2Fkey"))


if __name__ == "__main__":
    unittest.main(verbosity=2)
