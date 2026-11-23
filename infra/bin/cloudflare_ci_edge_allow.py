#!/usr/bin/env python3
"""Let ci-runner's Security headers check (.github/workflows/security-headers.yml) past Cloudflare's Bot Fight Mode on
kinowo.net and showtimes.cc WITHOUT the Decodo proxy, and only when it carries a secret.

    infra/bin/cloudflare-ci-edge-allow             # apply (idempotent), then verify from ci-runner
    infra/bin/cloudflare-ci-edge-allow --dry-run   # say what it would change, change nothing

WHY THIS SHAPE. Bot Fight Mode (Free plan, `fight_mode: true` on both zones) runs outside the ruleset engine: a WAF
custom rule's Skip / Allow has no effect on it (developers.cloudflare.com/bots/get-started/bot-fight-mode,
"Limitations"). Its one exception is an IP Access Rule that matches first -- the same hand step that lets monitoring-1
probe the sites (infra/nix/modules/roles/synthetic-probes.nix). An address alone would let ANY job on ci-runner
through, so a custom rule, which still runs for an allowed address (monitoring-1's showtimes.cc /movie/ exemption shows
it), BLOCKS ci-runner on the sites' own hosts unless the request carries `X-Kinowo-Edge-Check: <EDGE_CHECK_KEY>`. The
block rule goes in BEFORE the allow, so there is never a moment the address is let through without the key.

Steps, each skipped when already done:
  1. EDGE_CHECK_KEY: a fresh random value into the `movies` vault (never argv, never printed);
  2. it into the GitHub Actions secret EDGE_CHECK_KEY (infra/bin/push-gh-secrets);
  3. per zone: the custom rule `ci-runner-edge-key` (first in the zone's custom rules), then the IP Access Rule
     allowing 162.55.0.154 -- run under `secrets run`, so the API token is only ever in that child's environment;
  4. verify on ci-runner: without the key 403 (blocked, NOT `cf-mitigated: challenge`), with it 401.
Rotating the key: `secrets delete movies EDGE_CHECK_KEY` and run this again (it rewrites the rule).
Stdlib only. CLOUDFLARE_API_URL replaces the API's address (a test's fake Cloudflare)."""
from __future__ import annotations

import json
import os
import subprocess
import sys
import urllib.error
import urllib.request
from typing import Any, Dict, List, Optional

HERE = os.path.dirname(os.path.realpath(__file__))
CI_RUNNER_IP = "162.55.0.154"
REF = "ci-runner-edge-key"
HOSTS = {"kinowo.net": ["kinowo.net", "www.kinowo.net"], "showtimes.cc": ["showtimes.cc", "www.showtimes.cc"]}
ENTRYPOINT = "/zones/%s/rulesets/phases/http_request_firewall_custom/entrypoint"
ZONES_STEP = "--zones-step"  # the re-run of this file under `secrets run`, which holds the token
VERIFY = r'''read -r k; for h in kinowo.net showtimes.cc; do
  printf "  %s  no key: " $h; curl -s -o /dev/null -D - -m 20 https://$h/api/me | grep -iE "^(HTTP|cf-mitigated)" | tr -d "\r" | tr "\n" " "; echo
  printf "  %s  key:    " $h; curl -s -o /dev/null -w "%{http_code}\n" -m 20 -K <(printf "header = \"X-Kinowo-Edge-Check: %s\"\n" "$k") https://$h/api/me
done'''


class Failed(Exception):
    """A step that failed: its exit status."""

    def __init__(self, code: int) -> None:
        super().__init__(code)
        self.code = code


def run(argv: List[str], stdin: Optional[str] = None, capture: bool = False) -> str:
    done = subprocess.run(argv, input=stdin, text=True, stdout=subprocess.PIPE if capture else None)
    if done.returncode != 0:
        raise Failed(done.returncode)
    return done.stdout or ""


def vault_has_key() -> bool:
    listing = run(["secrets", "list", "movies"], capture=True)
    return "EDGE_CHECK_KEY" in [line.split()[0] for line in listing.splitlines() if line.split()]


def mint_key(dry: bool) -> None:
    if vault_has_key():
        return
    if dry:
        print("WOULD create vault key movies/EDGE_CHECK_KEY", flush=True)
        return
    run(["secrets", "set", "movies", "EDGE_CHECK_KEY"], stdin=run(["openssl", "rand", "-hex", "32"], capture=True))
    run(["secrets", "describe", "movies", "EDGE_CHECK_KEY"],
        stdin="X-Kinowo-Edge-Check value: lets ci-runner's Security headers check past Cloudflare "
              "(infra/bin/cloudflare-ci-edge-allow)\n")
    print("vault: movies/EDGE_CHECK_KEY created", flush=True)


def apply(dry: bool) -> None:
    run(["secrets", "unlock", "movies"], capture=True)
    mint_key(dry)
    run([os.path.join(HERE, "push-gh-secrets"), *(["--dry-run"] if dry else []), "EDGE_CHECK_KEY"])
    keys, env = ["CLOUDFLARE_API_TOKEN", "EDGE_CHECK_KEY"], dict(os.environ, DRY_RUN="1" if dry else "0")
    if not vault_has_key():  # --dry-run before the first apply
        keys, env["EDGE_CHECK_KEY"] = ["CLOUDFLARE_API_TOKEN"], "dry-run-placeholder"
    sys.stdout.flush()
    done = subprocess.run(["secrets", "run", "movies", *keys, "--", sys.executable, "-I", os.path.realpath(__file__),
                           ZONES_STEP], env=env)
    if done.returncode != 0:
        raise Failed(done.returncode)
    if dry:
        return
    print("verify from ci-runner (expect 403 without the key, not a challenge; 401 with it):", flush=True)
    run(["ssh", "ci-runner", VERIFY], stdin=run(["secrets", "get", "movies", "EDGE_CHECK_KEY"], capture=True))


class Zones:
    """Step 3 over Cloudflare's API, with the token and the key from the environment `secrets run` gave."""

    def __init__(self, env: Dict[str, str]) -> None:
        self.token, self.key = env["CLOUDFLARE_API_TOKEN"], env["EDGE_CHECK_KEY"]
        self.dry = env.get("DRY_RUN") == "1"
        self.api_url = env.get("CLOUDFLARE_API_URL", "https://api.cloudflare.com/client/v4")

    def api(self, method: str, path: str, body: Any = None) -> Any:
        request = urllib.request.Request(self.api_url + path, method=method,
                                         data=None if body is None else json.dumps(body).encode(),
                                         headers={"Authorization": "Bearer " + self.token,
                                                  "Content-Type": "application/json"})
        try:
            with urllib.request.urlopen(request, timeout=30) as response:
                return json.load(response)
        except urllib.error.HTTPError as error:
            payload = json.load(error)
            if method == "GET" and error.code == 404:
                return payload
            sys.exit("Cloudflare %s %s: HTTP %s %s" % (method, path, error.code, payload.get("errors")))

    def change(self, what: str, method: str, path: str, body: Any) -> None:
        print(("WOULD " if self.dry else "") + what, flush=True)
        if not self.dry:
            self.api(method, path, body)

    def rule(self, hosts: List[str]) -> Dict[str, Any]:
        expression = ('(ip.src eq %s and http.host in {%s} and not any(http.request.headers["x-kinowo-edge-check"][*]'
                      ' eq "%s"))' % (CI_RUNNER_IP, " ".join('"%s"' % h for h in hosts), self.key))
        return {"ref": REF, "action": "block", "enabled": True, "expression": expression,
                "description": "ci-runner without the edge key (its IP Access allow is for the Security headers "
                               "check only)"}

    def custom_rule(self, zone_name: str, zone_id: str, rule: Dict[str, Any]) -> None:
        entry = self.api("GET", ENTRYPOINT % zone_id).get("result")
        if not entry:
            self.change("%s: create the custom-rules entrypoint with %s" % (zone_name, REF), "PUT",
                        ENTRYPOINT % zone_id, {"rules": [rule]})
            return
        rules_path = "/zones/%s/rulesets/%s/rules" % (zone_id, entry["id"])
        existing = [r for r in entry.get("rules", []) if r.get("ref") == REF]
        if not existing:
            self.change("%s: add custom rule %s first" % (zone_name, REF), "POST", rules_path,
                        dict(rule, position={"index": 1}))
        elif existing[0]["expression"] != rule["expression"] or existing[0]["action"] != "block" \
                or not existing[0].get("enabled"):
            self.change("%s: update custom rule %s" % (zone_name, REF), "PATCH",
                        "%s/%s" % (rules_path, existing[0]["id"]), rule)
        else:
            print("%s: custom rule %s current" % (zone_name, REF), flush=True)

    def access_rule(self, zone_name: str, zone_id: str) -> None:
        path = "/zones/%s/firewall/access_rules/rules" % zone_id
        rules = self.api("GET", "%s?configuration.value=%s" % (path, CI_RUNNER_IP))["result"]
        if any(r["mode"] == "whitelist" for r in rules):
            print("%s: IP Access allow %s present" % (zone_name, CI_RUNNER_IP), flush=True)
            return
        self.change("%s: IP Access allow %s" % (zone_name, CI_RUNNER_IP), "POST", path,
                    {"mode": "whitelist", "configuration": {"target": "ip", "value": CI_RUNNER_IP},
                     "notes": "ci-runner: Security headers check; gated by custom rule %s "
                              "(infra/bin/cloudflare-ci-edge-allow)" % REF})

    def apply(self) -> None:
        for zone_name, hosts in HOSTS.items():
            zone = self.api("GET", "/zones?name=" + zone_name)["result"][0]
            print("%s: plan %s" % (zone_name, zone["plan"]["name"]), flush=True)
            self.custom_rule(zone_name, zone["id"], self.rule(hosts))
            self.access_rule(zone_name, zone["id"])


def main(argv: List[str]) -> int:
    if argv[:1] == [ZONES_STEP]:
        Zones(dict(os.environ)).apply()
        return 0
    try:
        apply(dry=argv[:1] == ["--dry-run"])
    except Failed as failed:
        return failed.code
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
