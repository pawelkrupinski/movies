#!/usr/bin/env python3
"""END TO END: infra/bin/push-k8s-secrets, which reaches the cluster's Secrets, over a map file, a stub vault (`secrets`)
and a stub k3s server behind `ssh` (scripts/e2e/e2e_world.py); the real jq and shasum run. No real vault, host or
cluster is reachable.

  - --dry-run says what WOULD change (a stale value, a value of the other vault namespace), skips a key the vault does
    not hold, and sends the server nothing but its read of the current sha256s;
  - a real run patches ONLY the changed keys, through ssh's stdin as base64 (`--patch-file /dev/stdin`); no vault value
    is in any argv of any call; a rerun finds everything up to date and sends no patch.
Mutations (scripts/e2e/mutations): the equal-sha check never equal (a rerun patches again); --dry-run patching anyway;
the patch given to kubectl as an argument (secrets in argv)."""
import base64
import json
import os
import sys
import unittest
from typing import Dict, List

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))
import e2e_world  # noqa: E402

VAULT = {"movies": {"WEB_APP": "new-app-secret", "GOOGLE_ID": "gid-same"}, "filmowo": {"TOKEN": "film-token"}}
MAP = """# toy map
kinowo/web-secrets   APPLICATION_SECRET   WEB_APP
kinowo/web-secrets   GOOGLE_CLIENT_ID     GOOGLE_ID
kinowo/web-secrets   NOT_YET              NOT_IN_VAULT
filmowo/app          TOKEN                filmowo:TOKEN
"""
SERVER = {"kinowo/web-secrets": {"APPLICATION_SECRET": "old-app-secret", "GOOGLE_CLIENT_ID": "gid-same"},
          "filmowo/app": {"TOKEN": "stale-token"}}
SECRETS = """
vault = json.load(open(os.environ["E2E_VAULT"]))
if argv[:1] == ["unlock"]:
    sys.exit(0)
if argv[:1] == ["list"]:
    for key in vault[argv[1]]:
        out(key + " set")
elif argv[:1] == ["get"]:
    out(vault[argv[1]][argv[2]])
else:
    err("secrets shim: " + repr(argv)); sys.exit(97)
"""
# the k3s server behind ssh: its read answers `key sha256` lines of the Secret named in the command; a patch is kept
SSH = """
import re
command = argv[-1]
state = json.load(open(os.environ["E2E_SERVER"]))
if "patch secret" in command:
    with open(os.environ["E2E_PATCHES"], "a") as kept:
        kept.write(json.dumps({"command": command, "patch": stdin}) + "\\n")
    sys.exit(0)
ns, name = re.search(r"-n (\\S+) get secret (\\S+)", command).groups()
for key, value in state[ns + "/" + name].items():
    out(key + " " + __import__("hashlib").sha256(value.encode()).hexdigest())
"""


class PushK8sSecretsE2ETest(e2e_world.Scenario):
    def setUp(self) -> None:
        super().setUp()
        self.map = self.w.write(self.w.root, "secrets.map", MAP)
        self.vault, self.server = (os.path.join(self.w.root, n) for n in ("vault.json", "server.json"))
        self.patches = os.path.join(self.w.root, "patches.jsonl")
        for path, data in ((self.vault, VAULT), (self.server, SERVER)):
            self.w.write(self.w.root, os.path.basename(path), json.dumps(data))
        self.w.shim("secrets", SECRETS, reads_stdin=False)
        self.w.shim("ssh", SSH)

    def push(self, *args: str) -> e2e_world.cli_harness.Done:
        return self.w.run([self.w.tool("infra/bin/push-k8s-secrets"), *args], self.w.root,
                          env={"K8S_SECRETS_MAP": self.map, "E2E_VAULT": self.vault, "E2E_SERVER": self.server,
                               "E2E_PATCHES": self.patches, "K8S_SERVER_HOST": "kube-e2e"})

    def patched(self) -> List[Dict[str, str]]:
        return [json.loads(line) for line in self.w.read(self.patches).splitlines()] if os.path.exists(self.patches) else []

    def apply(self, patches: List[Dict[str, str]]) -> None:
        """What the server does with the patches: the Secret's data now holds them."""
        state = json.loads(self.w.read(self.server))
        for p in patches:
            target = "kinowo/web-secrets" if "web-secrets" in p["command"] else "filmowo/app"
            for key, b64 in json.loads(p["patch"])["data"].items():
                state[target][key] = base64.b64decode(b64).decode()
        self.w.write(self.w.root, "server.json", json.dumps(state))

    def test_dry_run_changes_nothing_and_a_real_run_patches_only_what_differs(self) -> None:
        done = self.push("--dry-run")
        self.assertRan(done, 0, "kinowo/web-secrets GOOGLE_CLIENT_ID: up to date",
                       "NOT_IN_VAULT not set yet -- skipped", "kinowo/web-secrets: WOULD update APPLICATION_SECRET",
                       "filmowo/app: WOULD update TOKEN")
        self.assertEqual([], self.patched(), "a dry run patched the cluster")
        done = self.push()
        self.assertRan(done, 0, "kinowo/web-secrets: updated APPLICATION_SECRET (Reloader restarts its pods)",
                       "filmowo/app: updated TOKEN")
        patches = self.patched()
        self.assertEqual(2, len(patches))
        sent = {k: base64.b64decode(v).decode() for p in patches for k, v in json.loads(p["patch"])["data"].items()}
        self.assertEqual({"APPLICATION_SECRET": "new-app-secret", "TOKEN": "film-token"}, sent)
        every_argv = json.dumps([c["argv"] for name in ("ssh", "secrets", "kubectl") for c in self.w.calls(name)])
        for value in ("new-app-secret", "film-token", "gid-same", base64.b64encode(b"new-app-secret").decode()):
            self.assertNotIn(value, every_argv, "a vault value travelled in an argv")
        self.apply(patches)
        again = self.push()
        self.assertRan(again, 0, "kinowo/web-secrets: nothing to change", "filmowo/app: nothing to change")
        self.assertEqual(2, len(self.patched()), "a rerun patched again")


if __name__ == "__main__":
    unittest.main()
