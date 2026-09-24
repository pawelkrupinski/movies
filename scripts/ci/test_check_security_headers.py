#!/usr/bin/env python3
"""check-security-headers.py's verdicts, driven with header sets recorded from the live hosts.

Offline on purpose: the workflow runs this BEFORE the live check, so a checker that has stopped
being able to fail is caught on the run that would otherwise have passed on it.

Run: python3 scripts/ci/test_check_security_headers.py
"""
import importlib.util
import os
import unittest

_spec = importlib.util.spec_from_file_location(
    "check_security_headers", os.path.join(os.path.dirname(os.path.abspath(__file__)), "check-security-headers.py"))
check = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(check)

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
        env = {"KINOWO_PROXY_USER": self.USER, "KINOWO_PROXY_PASS": self.PASSWORD}
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


if __name__ == "__main__":
    unittest.main(verbosity=2)
