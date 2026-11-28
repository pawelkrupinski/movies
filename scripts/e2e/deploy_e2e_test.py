#!/usr/bin/env python3
"""END TO END: infra/bin/deploy <host>'s PREVIEW, from a REAL git checkout with a bare origin, against a STUB host --
`ssh` and `nix` are shims answering as a NixOS host and its flake would (scripts/e2e/e2e_world.py); no real host, flake
or nix store is reached, and nothing is ever applied.

  - at origin main: the preview -- running, candidate, the units dry-activate would start / restart -- exit 0, with the
    PROBE the only script sent to the host (no switch unit launched, no profile set) and `preview only` said;
  - a checkout BEHIND origin main: REFUSED (deploying it rolls the host back) before any ssh or nix call;
  - a host answering without the end marker (the bare IP's forced-command key): COULD NOT MEASURE, exit 2, never read
    as the host's state.
Mutations (scripts/e2e/mutations): the staleness check reading a checkout behind main as current; the end marker
not required."""
import json
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.realpath(__file__)))
import e2e_world  # noqa: E402

HOST, ADDRESS = "web-e2e-1", "10.9.8.7"
OLD, NEW = "/nix/store/0000-nixos-system-old", "/nix/store/1111-nixos-system-new"
DRY = ("would stop the following units: legacy.service\nwould restart the following units: kinowo-web.service\n"
       "would start the following units: kinowo-new.service")
# The flake, as `nix eval <flake>#nixosConfigurations.<attr>` answers it
NIX = f"""
attr = next((a.split("#nixosConfigurations.", 1)[1] for a in argv if "#nixosConfigurations." in a), "")
answers = {{"{HOST}.config.fleet.deployAddress": "{ADDRESS}", "{HOST}.config.system.build.toplevel.outPath": "{NEW}",
           "{HOST}.config.system.configurationRevision": "e2e-rev"}}
if argv[:1] != ["eval"] or attr not in answers:
    err(f"nix shim: no answer for {{argv}}"); sys.exit(1)
out(answers[attr], end="")
"""
# The host: `ssh -G` resolves the alias named like it to its address; a script on stdin is answered as the PROBE does
SSH = f"""
if argv[:1] == ["-G"]:
    out("hostname {ADDRESS}"); out("user root"); sys.exit(0)
host = json.load(open(os.environ["E2E_HOST"]))
if "@@ END" not in stdin or "switch-to-configuration" not in stdin or "systemd-run" in stdin:
    err("ssh shim: only the PROBE is answered here"); sys.exit(255)
cand = argv[-1].split("bash -s -- ", 1)[-1].strip("'")
out("user=root\\nhostname={HOST}\\nrunning={OLD}\\nbooted={OLD}\\npinned={NEW}\\nrunning_rev=old-rev\\nfailed=")
if cand == "{NEW}":
    out("candidate_present=1\\ncandidate_rev=e2e-rev")
    for c in ("kernel", "initrd", "kernel-modules", "systemd"):
        out(f"boot_{{c}}=same")
    out("@@ DRY rc=0\\n" + {DRY!r} + "\\n@@ DRYEND")
    out("state kinowo-web.service active")
else:
    out("candidate_present=0")
if host.get("end_marker", True):
    out("@@ END")
"""


class DeployE2ETest(e2e_world.Scenario):
    def setUp(self) -> None:
        super().setUp()
        files = {rel: self.w.read(self.w.tool(rel)) for rel in ("infra/bin/deploy", "infra/bin/nix_path.py", "infra/bin/deploy_decide.py",
                                                                      "infra/bin/deploy_parse.py", "infra/bin/deploy_scripts.py")}
        self.repo = self.w.toy_repo("fleet", dict(files, **{"infra/flake.nix": "{ outputs = _: { }; }\n"}))
        self.host_file = os.path.join(self.w.root, "host.json")
        self.host({})
        self.w.shim("nix", NIX)
        self.w.shim("ssh", SSH)

    def host(self, facts: dict) -> None:
        with open(self.host_file, "w") as f:
            json.dump(facts, f)

    def deploy(self) -> e2e_world.cli_harness.Done:
        return self.w.run([os.path.join(self.repo.clone, "infra/bin/deploy"), HOST], self.repo.clone,
                          env={"E2E_HOST": self.host_file, "SSH_USER": None})

    def test_the_preview_shows_the_plan_and_switches_nothing(self) -> None:
        done = self.deploy()
        self.assertRan(done, 0, f"== {HOST}  via ssh {HOST}", "checkout: at origin main", f"running   {OLD}",
                       f"candidate {NEW}  rev e2e-rev", "restart   kinowo-web.service (active)",
                       "start     kinowo-new.service", "stop      legacy.service", "unchanged (no reboot needed)",
                       f"preview only. To switch: bin/deploy {HOST} --apply")
        scripts = [c["stdin"] for c in self.w.calls("ssh") if c["argv"][:1] != ["-G"]]
        self.assertEqual(1, len(scripts), "the preview sent the host more than its one probe")
        self.assertNotIn("nix-env", scripts[0].split("dry-activate")[0], "the preview set a profile")

    def test_a_checkout_behind_origin_main_is_refused_before_reaching_the_host(self) -> None:
        other = os.path.join(self.w.root, "other")
        self.w.git(self.w.root, "clone", "-q", self.repo.bare, other)
        self.w.write(other, "infra/hosts.txt", "moved on\n")
        self.w.commit(other, "main moves on")
        self.w.git(other, "push", "-q", "origin", "HEAD:main")
        done = self.deploy()
        self.assertRan(done, 1, "REFUSED: this checkout is 1 commit(s) behind origin main")
        self.assertEqual([], self.w.calls("ssh") + self.w.calls("nix"), "a stale checkout reached the host")

    def test_a_reply_without_the_end_marker_is_no_measurement(self) -> None:
        self.host({"end_marker": False})
        self.assertRan(self.deploy(), 2, "COULD NOT MEASURE", "did not answer as a shell (no end marker)")


if __name__ == "__main__":
    unittest.main()
