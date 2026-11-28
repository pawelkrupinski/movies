#!/usr/bin/env python3
"""infra/bin/cloudflare-ci-edge-allow, black-box: the vault key, its GitHub secret, both zones' custom rule and IP
Access allow, then the check from ci-runner -- each step skipped when already done, and --dry-run changing nothing.
Cloudflare is a fake HTTP server of the test's own (CLOUDFLARE_API_URL), `secrets`, `gh`, `openssl` and `ssh` are
fakes on PATH; the real API and the real vault are never reached. The same cases hold the shell tool and its Python
rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import json
import os
import sys
import threading
import unittest
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from typing import Any, Dict, List, Optional, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))),
                                "scripts", "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("infra/bin/cloudflare_ci_edge_allow.py", "infra/bin/cloudflare-ci-edge-allow")
KEY = "f" * 64
IP = "162.55.0.154"
ZONES = {"kinowo.net": "zone-k", "showtimes.cc": "zone-s"}
ENTRYPOINT = "/rulesets/phases/http_request_firewall_custom/entrypoint"

# The vault: one file per key under FAKE_VAULT; `run` exports the named keys and execs the command.
SECRETS = """
import subprocess
vault = env["FAKE_VAULT"]
command, rest = argv[0], argv[1:]
def path(key):
    return os.path.join(vault, key)
if command == "unlock":
    exit(0)
if command == "list":
    for key in sorted(os.listdir(vault)):
        if not key.endswith(".description"):
            out(key + "  (a description)")
    exit(0)
if command in ("set", "describe"):
    with open(path(rest[1]) + (".description" if command == "describe" else ""), "w") as f:
        f.write(sys.stdin.read().strip())
    exit(0)
if command == "get":
    with open(path(rest[1])) as f:
        out(f.read())
    exit(0)
if command == "run":
    split = rest.index("--")
    child = dict(env)
    for key in rest[1:split]:
        with open(path(key)) as f:
            child[key] = f.read()
    os.execvpe(rest[split + 1], rest[split + 1:], child)
exit(9)
"""
GH = """
with open(os.path.join(env["FAKE_VAULT"], "..", "gh-stdin"), "a") as f:
    f.write(sys.stdin.read())
"""
SSH = """
with open(os.path.join(env["FAKE_VAULT"], "..", "ssh-stdin"), "w") as f:
    f.write(sys.stdin.read())
out("  kinowo.net  no key: HTTP/2 403")
"""


class FakeCloudflare:
    """The four Cloudflare endpoints the tool reads and writes, over per-zone state; every request recorded."""

    def __init__(self) -> None:
        self.entrypoints: Dict[str, Optional[Dict[str, Any]]] = {zone: None for zone in ZONES.values()}
        self.access: Dict[str, List[Dict[str, Any]]] = {zone: [] for zone in ZONES.values()}
        self.requests: List[Tuple[str, str, Any]] = []
        self.refuse: Optional[str] = None  # a method answered 403
        fake = self

        class Handler(BaseHTTPRequestHandler):
            def log_message(self, *_: Any) -> None:
                pass

            def do_GET(self) -> None:
                fake.answer(self)

            do_PUT = do_POST = do_PATCH = do_GET

        self.server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        threading.Thread(target=self.server.serve_forever, daemon=True).start()
        self.url = "http://127.0.0.1:%d/client/v4" % self.server.server_address[1]

    def close(self) -> None:
        self.server.shutdown()
        self.server.server_close()

    def writes(self) -> List[Tuple[str, str, Any]]:
        return [r for r in self.requests if r[0] != "GET"]

    def answer(self, handler: BaseHTTPRequestHandler) -> None:
        length = int(handler.headers.get("Content-Length") or 0)
        body = json.loads(handler.rfile.read(length)) if length else None
        path = handler.path[len("/client/v4"):]
        self.requests.append((handler.command, path, body))
        assert handler.headers["Authorization"] == "Bearer cf-token", handler.headers["Authorization"]
        code, payload = self.route(handler.command, path, body)
        data = json.dumps(payload).encode()
        handler.send_response(code)
        handler.send_header("Content-Type", "application/json")
        handler.send_header("Content-Length", str(len(data)))
        handler.end_headers()
        handler.wfile.write(data)

    def route(self, method: str, path: str, body: Any) -> Tuple[int, Any]:
        if method == self.refuse:
            return 403, {"success": False, "errors": [{"code": 10000, "message": "Authentication error"}]}
        if path.startswith("/zones?name="):
            return 200, {"result": [{"id": ZONES[path.split("=", 1)[1]], "plan": {"name": "Free Website"}}]}
        zone = path.split("/")[2]
        if path.endswith(ENTRYPOINT):
            return self.entrypoint(method, zone, body)
        if "/firewall/access_rules/rules" in path:
            if method == "POST":
                self.access[zone].append(dict(body, id="access-1"))
            return 200, {"result": self.access[zone]}
        return self.rule(method, zone, path, body)

    def entrypoint(self, method: str, zone: str, body: Any) -> Tuple[int, Any]:
        if method == "PUT":
            self.entrypoints[zone] = {"id": "set-" + zone, "rules": [dict(r, id="rule-new") for r in body["rules"]]}
        if self.entrypoints[zone] is None:
            return 404, {"success": False, "errors": [{"code": 10003, "message": "not found"}], "result": None}
        return 200, {"result": self.entrypoints[zone]}

    def rule(self, method: str, zone: str, path: str, body: Any) -> Tuple[int, Any]:
        rules = (self.entrypoints[zone] or {"rules": []})["rules"]
        if method == "POST":
            rules.insert(body["position"]["index"] - 1, dict(body, id="rule-new"))
        else:
            rules[:] = [dict(body, id=r["id"]) if path.endswith("/" + r["id"]) else r for r in rules]
        return 200, {"result": self.entrypoints[zone]}


def expression(hosts: str, key: str = KEY) -> str:
    return ('(ip.src eq %s and http.host in {%s} and not any(http.request.headers["x-kinowo-edge-check"][*] eq "%s"))'
            % (IP, hosts, key))


KINOWO = expression('"kinowo.net" "www.kinowo.net"')
SHOWTIMES = expression('"showtimes.cc" "www.showtimes.cc"')


class CloudflareCiEdgeAllowTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.cloudflare = FakeCloudflare()
        self.addCleanup(self.cloudflare.close)
        self.vault = self.path("vault")
        os.makedirs(self.vault)
        self.write("vault/CLOUDFLARE_API_TOKEN", "cf-token")
        self.fakes.program("secrets", SECRETS)
        self.fakes.program("gh", GH)
        self.fakes.program("ssh", SSH)
        self.fakes.script("openssl", "echo " + KEY)

    def run_edge(self, *args: str) -> cli_harness.Done:
        return self.run_tool(*args, env={"FAKE_VAULT": self.vault, "CLOUDFLARE_API_URL": self.cloudflare.url})

    def current(self) -> None:
        """Both zones as a finished apply leaves them, with the vault key set."""
        self.write("vault/EDGE_CHECK_KEY", KEY)
        for zone, text in (("zone-k", KINOWO), ("zone-s", SHOWTIMES)):
            self.cloudflare.entrypoints[zone] = {"id": "set-" + zone, "rules": [
                {"id": "rule-1", "ref": "ci-runner-edge-key", "action": "block", "enabled": True, "expression": text}]}
            self.cloudflare.access[zone] = [{"id": "a", "mode": "whitelist"}]

    def test_a_first_apply_mints_the_key_pushes_it_and_writes_both_zones_block_first(self) -> None:
        done = self.run_edge()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(KEY, self.read("vault/EDGE_CHECK_KEY"))
        self.assertIn("lets ci-runner's Security headers check past Cloudflare", self.read("vault/EDGE_CHECK_KEY.description"))
        self.assertIn("vault: movies/EDGE_CHECK_KEY created", done.stdout)
        self.assertEqual([["secret", "set", "EDGE_CHECK_KEY", "-R", "pawelkrupinski/movies"]], self.fakes.calls("gh"))
        self.assertEqual(KEY, self.read("gh-stdin").strip())
        self.assertIn("  pawelkrupinski/movies EDGE_CHECK_KEY: pushed from EDGE_CHECK_KEY", done.stdout)
        writes = self.cloudflare.writes()
        self.assertEqual(["PUT", "POST", "PUT", "POST"], [w[0] for w in writes])
        self.assertEqual("/zones/zone-k" + ENTRYPOINT, writes[0][1])
        rule = writes[0][2]["rules"][0]
        self.assertEqual(("ci-runner-edge-key", "block", True, KINOWO),
                         (rule["ref"], rule["action"], rule["enabled"], rule["expression"]))
        self.assertEqual(SHOWTIMES, writes[2][2]["rules"][0]["expression"])
        self.assertEqual({"mode": "whitelist", "configuration": {"target": "ip", "value": IP},
                          "notes": "ci-runner: Security headers check; gated by custom rule ci-runner-edge-key "
                                   "(infra/bin/cloudflare-ci-edge-allow)"}, writes[1][2])
        for line in ("kinowo.net: plan Free Website", "kinowo.net: create the custom-rules entrypoint with "
                     "ci-runner-edge-key", "kinowo.net: IP Access allow 162.55.0.154", "showtimes.cc: plan Free Website"):
            self.assertIn(line + "\n", done.stdout)
        self.assertIn("verify from ci-runner (expect 403 without the key, not a challenge; 401 with it):", done.stdout)
        self.assertIn("  kinowo.net  no key: HTTP/2 403", done.stdout)
        ssh = self.fakes.calls("ssh")
        self.assertEqual(1, len(ssh))
        self.assertEqual("ci-runner", ssh[0][0])
        self.assertIn("for h in kinowo.net showtimes.cc", ssh[0][1])
        self.assertEqual(KEY, self.read("ssh-stdin").strip())

    def test_a_dry_run_before_the_first_apply_writes_nothing_anywhere(self) -> None:
        done = self.run_edge("--dry-run")
        self.assertEqual(0, done.code, done.output)
        self.assertFalse(os.path.exists(self.path("vault/EDGE_CHECK_KEY")))
        self.assertIn("WOULD create vault key movies/EDGE_CHECK_KEY\n", done.stdout)
        self.assertIn("  pawelkrupinski/movies EDGE_CHECK_KEY: vault key EDGE_CHECK_KEY not set yet -- skipped",
                      done.stdout)
        self.assertEqual([], self.fakes.calls("gh"))
        self.assertEqual([], self.fakes.calls("openssl"))
        self.assertEqual([], self.cloudflare.writes())
        self.assertIn("WOULD kinowo.net: create the custom-rules entrypoint with ci-runner-edge-key\n", done.stdout)
        self.assertIn("WOULD showtimes.cc: IP Access allow 162.55.0.154\n", done.stdout)
        self.assertEqual([], self.fakes.calls("ssh"))
        self.assertNotIn("verify from ci-runner", done.stdout)

    def test_a_dry_run_after_an_apply_says_what_it_would_push_and_that_both_zones_are_current(self) -> None:
        self.current()
        done = self.run_edge("--dry-run")
        self.assertEqual(0, done.code, done.output)
        self.assertNotIn("WOULD create vault key", done.stdout)
        self.assertIn("  pawelkrupinski/movies EDGE_CHECK_KEY: WOULD push from EDGE_CHECK_KEY", done.stdout)
        self.assertEqual([], self.fakes.calls("gh"))
        self.assertEqual([], self.cloudflare.writes())
        self.assertNotIn("WOULD kinowo", done.stdout)
        self.assertEqual([], self.fakes.calls("ssh"))

    def test_an_apply_over_a_finished_one_changes_nothing_but_still_verifies(self) -> None:
        self.current()
        done = self.run_edge()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([], self.fakes.calls("openssl"))
        self.assertNotIn("created", done.stdout)
        self.assertEqual([], self.cloudflare.writes())
        for zone in ZONES:
            self.assertIn("%s: custom rule ci-runner-edge-key current\n" % zone, done.stdout)
            self.assertIn("%s: IP Access allow 162.55.0.154 present\n" % zone, done.stdout)
        self.assertEqual(1, len(self.fakes.calls("ssh")))

    def test_a_missing_rule_is_added_first_and_a_stale_one_is_patched_in_place(self) -> None:
        self.current()
        self.cloudflare.entrypoints["zone-k"] = {"id": "set-k", "rules": [{"id": "other", "ref": "x", "expression": "y"}]}
        stale = self.cloudflare.entrypoints["zone-s"]
        assert stale is not None
        stale["rules"][0]["expression"] = expression('"showtimes.cc" "www.showtimes.cc"', key="old")
        done = self.run_edge()
        self.assertEqual(0, done.code, done.output)
        writes = self.cloudflare.writes()
        self.assertEqual([("POST", "/zones/zone-k/rulesets/set-k/rules"),
                          ("PATCH", "/zones/zone-s/rulesets/set-zone-s/rules/rule-1")], [w[:2] for w in writes])
        self.assertEqual({"index": 1}, writes[0][2]["position"])
        self.assertEqual(KINOWO, writes[0][2]["expression"])
        self.assertEqual(SHOWTIMES, writes[1][2]["expression"])
        self.assertNotIn("position", writes[1][2])
        self.assertIn("kinowo.net: add custom rule ci-runner-edge-key first\n", done.stdout)
        self.assertIn("showtimes.cc: update custom rule ci-runner-edge-key\n", done.stdout)

    def test_a_disabled_rule_is_patched_too(self) -> None:
        self.current()
        entry = self.cloudflare.entrypoints["zone-k"]
        assert entry is not None
        entry["rules"][0]["enabled"] = False
        done = self.run_edge()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([("PATCH", "/zones/zone-k/rulesets/set-zone-k/rules/rule-1")],
                         [w[:2] for w in self.cloudflare.writes()])

    def test_a_refused_write_fails_the_run_naming_the_call_and_never_verifies(self) -> None:
        self.write("vault/EDGE_CHECK_KEY", KEY)
        self.cloudflare.refuse = "PUT"
        done = self.run_edge()
        self.assertEqual(1, done.code, done.output)
        self.assertIn("Cloudflare PUT /zones/zone-k%s: HTTP 403 [{'code': 10000, 'message': 'Authentication error'}]"
                      % ENTRYPOINT, done.stderr)
        self.assertNotIn("showtimes.cc", done.stdout)
        self.assertEqual([], self.fakes.calls("ssh"))

    def test_a_failed_push_stops_before_cloudflare(self) -> None:
        self.write("vault/EDGE_CHECK_KEY", KEY)
        self.fakes.script("gh", "exit 4")
        done = self.run_edge()
        self.assertEqual(4, done.code, done.output)
        self.assertEqual([], self.cloudflare.requests)
        self.assertEqual([], self.fakes.calls("ssh"))


if __name__ == "__main__":
    unittest.main()
