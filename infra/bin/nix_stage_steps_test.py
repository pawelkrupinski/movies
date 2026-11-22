#!/usr/bin/env python3
"""nix_stage_steps.py: nix-stage-closures.yml's two decision steps, black-box over a scratch infra/ with a fake `nix`
and a fake bin/stage-nixos-closures. `evaluate`: every host the flake declares is instantiated (never built), each one
that does not evaluate named, and a flake with no host is no pass. `stage`: each missing key said as itself, nothing
staged; the signing key a mode-600 file the tool is pointed at and removed after, the ssh key where `nix copy`'s own ssh
looks, the ssh options the tool's direct calls use; the tool's exit kept, 2 and the rest each annotated. Written as the
workflow's inline steps were and held to them: CLI_TOOL_UNDER_TEST=<a step's block as a script taking the subcommand
as $1> runs these cases against the blocks themselves."""
from __future__ import annotations

import os
import shutil
import stat
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))),
                                "scripts", "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("infra/bin/nix_stage_steps.py")
# nix: the hosts FAKE_HOSTS lists (JSON as `nix eval --json --apply builtins.attrNames` prints it); a host in
# FAKE_BROKEN fails to evaluate
NIX = """
if "--json" in argv:
    out("[" + ",".join(f'"{h}"' for h in env.get("FAKE_HOSTS", "").split()) + "]")
    exit(0)
target = [a for a in argv if a.startswith(".#nixosConfigurations.")][0]
host = target.split(".")[2]
if host in env.get("FAKE_BROKEN", "").split():
    err(f"error: {host} does not evaluate")
    exit(1)
out(f"/nix/store/{host}.drv", end="")
exit(0)
"""
# the staging tool (a Python script, run as `python3 bin/stage-nixos-closures`): what it was handed, then FAKE_STAGE_EXIT
STAGE = """import os, stat, sys
key = os.environ["NIX_SIGNING_KEY_FILE"]
seen = os.environ["FAKE_SEEN"]
with open(os.path.join(os.environ["HOME"], ".ssh/id_ed25519")) as f:
    ssh = f.read().strip()
with open(key) as f, open(seen, "w") as out:
    out.write("opts=%s key=%s mode=%o ssh=%s\\n" % (os.environ["NIX_SSHOPTS"], f.read().strip(),
                                                    stat.S_IMODE(os.stat(key).st_mode), ssh))
with open(seen + ".path", "w") as out:
    out.write(key)
sys.exit(int(os.environ.get("FAKE_STAGE_EXIT", "0")))
"""


class NixStageStepsTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        self.infra = self.path("checkout/infra")
        self.tool = self.copy(TOOL, os.path.join(self.infra, "bin", os.path.basename(TOOL)))
        lib = os.path.join(cli_harness.ROOT, "scripts/lib")
        shutil.copytree(lib, self.path("checkout/scripts/lib"), ignore=shutil.ignore_patterns("__pycache__", "*_test.py"))
        stage = self.write("checkout/infra/bin/stage-nixos-closures", STAGE)
        os.chmod(stage, 0o755)
        self.fakes.program("nix", NIX)
        os.makedirs(self.path("home"))

    def copy(self, source: str, target: str) -> str:
        os.makedirs(os.path.dirname(target), exist_ok=True)
        shutil.copy2(source, target)
        return target

    def step(self, name: str, env: Optional[Dict[str, Optional[str]]] = None) -> cli_harness.Done:
        given: Dict[str, Optional[str]] = {"HOME": self.path("home"), "RUNNER_TEMP": self.path("tmp"),
                                           "FAKE_SEEN": self.path("seen"), **(env or {})}
        os.makedirs(self.path("tmp"), exist_ok=True)
        return cli_harness.run_bounded([self.tool, name], env=self.env(given), cwd=self.infra, stdin=None)

    def evaluated(self, done: cli_harness.Done) -> List[str]:
        return [line.strip() for line in done.stdout.splitlines() if line.strip().endswith(": evaluates")]

    def test_every_host_evaluates(self) -> None:
        done = self.step("evaluate", {"FAKE_HOSTS": "web-1 db-1"})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["web-1: evaluates", "db-1: evaluates"], self.evaluated(done))
        self.assertIn("::group::web-1\n/nix/store/web-1.drv\n", done.stdout)
        self.assertEqual(["eval", "--raw", ".#nixosConfigurations.db-1.config.system.build.toplevel.drvPath"],
                         self.fakes.calls("nix")[-1])

    def test_a_host_that_does_not_evaluate_is_named_the_rest_still_tried(self) -> None:
        done = self.step("evaluate", {"FAKE_HOSTS": "web-1 db-1 gw-1", "FAKE_BROKEN": "db-1"})
        self.assertEqual(1, done.code)
        self.assertIn("::error title=A host does not evaluate::db-1 -- main will not build for it, so "
                      "bin/stage-nixos-closures cannot stage it and the host cannot self-heal.", done.stdout)
        self.assertEqual(["web-1: evaluates", "gw-1: evaluates"], self.evaluated(done))
        self.assertEqual(3, done.stdout.count("::endgroup::"))

    def test_a_flake_with_no_host_is_no_pass(self) -> None:
        done = self.step("evaluate", {"FAKE_HOSTS": ""})
        self.assertEqual(1, done.code)
        self.assertIn("::error title=Nothing was evaluated::This flake declares no nixosConfigurations, which is NOT the "
                      "same as every host being fine.", done.stdout)

    def test_each_missing_key_is_said_as_itself_and_nothing_staged(self) -> None:
        done = self.step("stage", {"NIX_STAGE_SIGNING_KEY": "", "NIX_STAGE_SSH_KEY": "s"})
        self.assertEqual(1, done.code)
        self.assertIn("::error title=No signing key::secrets.NIX_STAGE_SIGNING_KEY is unset", done.stdout)
        done = self.step("stage", {"NIX_STAGE_SIGNING_KEY": "k", "NIX_STAGE_SSH_KEY": ""})
        self.assertEqual(1, done.code)
        self.assertIn("::error title=No deploy key::secrets.NIX_STAGE_SSH_KEY is unset, so no host could be reached. "
                      "Nothing was staged.", done.stdout)
        self.assertFalse(os.path.exists(self.path("seen")))

    def test_the_keys_become_files_with_modes_and_the_signing_key_is_removed(self) -> None:
        done = self.step("stage", {"NIX_STAGE_SIGNING_KEY": "sign-me", "NIX_STAGE_SSH_KEY": "ssh-me"})
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("opts=-o BatchMode=yes -o StrictHostKeyChecking=accept-new -o ConnectTimeout=10 key=sign-me "
                         "mode=600 ssh=ssh-me\n", self.read("seen"))
        self.assertFalse(os.path.exists(self.read("seen.path").strip()))
        ssh_key = self.path("home/.ssh/id_ed25519")
        self.assertEqual(0o600, stat.S_IMODE(os.stat(ssh_key).st_mode))

    def test_the_tools_exit_is_kept_and_annotated(self) -> None:
        keys: Dict[str, Optional[str]] = {"NIX_STAGE_SIGNING_KEY": "k", "NIX_STAGE_SSH_KEY": "s"}
        done = self.step("stage", {**keys, "FAKE_STAGE_EXIT": "2"})
        self.assertEqual(2, done.code)
        self.assertIn("::error title=Nothing was staged::bin/stage-nixos-closures exited 2 -- it could not read the "
                      "flake or found no hosts, which is NOT the same as every host being up to date.", done.stdout)
        done = self.step("stage", {**keys, "FAKE_STAGE_EXIT": "5"})
        self.assertEqual(5, done.code)
        self.assertIn("::error title=A host could not be staged::bin/stage-nixos-closures exited 5. A host that could "
                      "not be reached has NOT got the closure, and what it would run is unknown. Scroll up for which.",
                      done.stdout)


if __name__ == "__main__":
    unittest.main()
