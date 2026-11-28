#!/usr/bin/env python3
"""infra/bin/flux-rotate-deploy-keys, black-box: a dry run touches nothing; --apply mints both keys, hands them to
Flux's Secrets and only then removes the old keys; it refuses to rotate for sources not on GitHub, or for a github.com
host key GitHub does not publish. gh, ssh (the cluster's kubectl runs over ssh) and ssh-keygen are fakes on PATH: the
real tool is never run against GitHub or the cluster. Ported from flux-rotate-deploy-keys-test.sh case for case; the
same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import json
import os
import sys
import unittest
import zlib
from typing import Any, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))),
                                "scripts", "lib"))
from harness import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("infra/bin/flux_rotate_deploy_keys.py", "infra/bin/flux-rotate-deploy-keys")
URL = "ssh://git@github.com/pawelkrupinski/movies-gitops.git"
# A key's fingerprint is a checksum of its text, so the fake /meta can publish the one the fake keyscan's key has.
FINGERPRINT = "256 SHA256:%d comment (ED25519)"
SSH_KEYGEN = '''
import random, zlib
if "-lf" in argv:
    target = argv[-1]
    text = sys.stdin.read() if target == "-" else open(target).read()
    fields = text.split()
    key = next((fields[i + 1] for i, f in enumerate(fields[:-1]) if f.startswith("ssh-")), "")
    out("256 SHA256:%d comment (ED25519)" % zlib.crc32(key.encode()))
else:
    target = argv[-1]
    open(target, "w").write("private\\n")
    open(target + ".pub", "w").write("ssh-ed25519 KEY%s%d comment\\n" % (os.path.basename(target), random.randrange(10 ** 5)))
'''
GH = '''
head = " ".join(argv[:2])
if head == "auth status":
    exit(0)
if head == "repo deploy-key":
    if argv[2] == "list":
        out("17\\tflux-git-read (kinowo k3s) 20261031T150000Z\\t(read_only true)")
elif head == "api meta":
    out(open(env["META"]).read(), end="")
elif head == "api repos/pawelkrupinski/movies-gitops":
    out("  repo pawelkrupinski/movies-gitops")
elif head == "api -H":
    if "gotk-sync.yaml" in argv[3]:
        out("url: %s\\nname: flux-git-read" % env["URL"])
    elif "automation.yaml" in argv[3]:
        out("url: %s\\nname: flux-git-write" % env["URL"])
'''
SSH = '''
cmd = argv[-1]
if "ssh-keyscan" in cmd:
    out(open(env["KEYSCAN"]).read(), end="")
elif "get gitrepository movies-write" in cmd:
    out(env["URL"] + " flux-git-write")
elif "get gitrepository movies " in cmd:
    out(env.get("MOVIES_SOURCE") or env["URL"] + " flux-git-read")
elif "apply -f -" in cmd:
    with open(env["APPLIED"], "a") as f:
        f.write(sys.stdin.read() + "\\n")
'''


def fingerprint(key: str) -> int:
    return zlib.crc32(key.encode())


class FluxRotateDeployKeysTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.keyscan = self.write("keyscan", "github.com ssh-ed25519 HOSTKEY\n")
        self.meta = self.write("meta", f"{fingerprint('HOSTKEY')}\n")
        self.applied = self.path("applied")
        self.fakes.program("ssh-keygen", SSH_KEYGEN)
        self.fakes.program("gh", GH)
        self.fakes.program("ssh", SSH)

    def rotate(self, *args: str, source: Optional[str] = None) -> cli_harness.Done:
        self.fakes.clear()
        if os.path.exists(self.applied):
            os.remove(self.applied)
        return self.run_tool(*args, env={"URL": URL, "META": self.meta, "KEYSCAN": self.keyscan,
                                         "APPLIED": self.applied, "MOVIES_SOURCE": source})

    def called(self, name: str, text: str) -> int:
        return sum(1 for c in self.fakes.calls(name) if text in " ".join(c))

    def applied_secrets(self) -> List[Any]:
        if not os.path.exists(self.applied):
            return []
        text, decoder, found, at = self.read("applied"), json.JSONDecoder(), [], 0
        while True:
            at = len(text) - len(text[at:].lstrip())
            if at >= len(text):
                break
            doc, at = decoder.raw_decode(text, at)
            found.append(doc)
        return found

    def test_a_dry_run_exits_0_and_registers_deletes_and_asks_the_cluster_nothing(self) -> None:
        done = self.rotate()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual((0, 0, 0), (self.called("gh", "deploy-key add"), self.called("gh", "deploy-key delete"),
                                     len(self.fakes.calls("ssh"))))

    def test_a_dry_run_names_the_old_key_it_would_remove(self) -> None:
        done = self.rotate()
        self.assertIn("would: gh repo deploy-key delete 17", done.output)

    def test_apply_registers_a_read_and_a_write_key(self) -> None:
        done = self.rotate("--apply")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual((2, 1), (self.called("gh", "deploy-key add"), self.called("gh", "--allow-write")))

    def test_apply_hands_both_keys_to_fluxs_secrets_and_moves_no_source(self) -> None:
        self.rotate("--apply")
        names = [s["metadata"]["name"] for s in self.applied_secrets()]
        self.assertEqual(["flux-git-read", "flux-git-write"], sorted(names))
        self.assertEqual([], [s for s in self.applied_secrets() if s["kind"] == "GitRepository"])
        self.assertEqual(0, self.called("ssh", "patch gitrepository"))

    def test_apply_removes_the_old_key_only_after_both_secrets_are_applied(self) -> None:
        self.rotate("--apply")
        with open(self.fakes.log) as f:
            logged = [json.loads(line) for line in f if line.strip()]
        steps = [(c["name"], " ".join(c["argv"])) for c in logged]
        last_apply = max(i for i, (n, a) in enumerate(steps) if n == "ssh" and "apply -f -" in a)
        removal = next(i for i, (n, a) in enumerate(steps) if "deploy-key delete 17" in a)
        self.assertGreater(removal, last_apply)

    def test_a_source_not_on_githubs_url_is_refused_before_any_key_is_minted(self) -> None:
        done = self.rotate("--apply", source="https://forge.example/movies-gitops.git flux-git-read")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(0, self.called("gh", "deploy-key add"))

    def test_a_github_host_key_github_does_not_publish_is_refused_before_any_key_is_registered(self) -> None:
        self.write("keyscan", "github.com ssh-ed25519 OTHERKEY\n")
        done = self.rotate("--apply")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(0, self.called("gh", "deploy-key add"))

    def test_an_unknown_argument_is_a_usage_error(self) -> None:
        done = self.rotate("--bogus")
        self.assertEqual(2, done.code, done.output)
        self.assertEqual([], self.fakes.calls())


if __name__ == "__main__":
    unittest.main()
