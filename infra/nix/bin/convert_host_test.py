#!/usr/bin/env python3
"""infra/nix/bin/convert-host, black-box: its refusals (usage, no --yes, no SSH key, a layout naming a Hetzner volume,
a declared disk absent or resolving to a volume, a machine that never becomes the rescue system) and the conversion's
calls in order -- the Hetzner API through curl with the token on stdin only, three consecutive `rescue` answers, the
disk checks, nixos-anywhere, the wait for NixOS and the host-key refresh. Every outside program is a fake on PATH
(curl, ssh, nix, ssh-keygen, ssh-keyscan, secrets); no machine or API is reached. CONVERT_HOST_POLL_SECONDS=0 makes
the polls instant. The same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import json
import os
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__))))), "scripts", "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("infra/nix/bin/convert_host.py", "infra/nix/bin/convert-host")
FLAKE = os.path.dirname(os.path.dirname(os.path.dirname(os.path.realpath(TOOL))))
IP = "178.105.221.61"
ROOT_DISK = "/dev/disk/by-id/scsi-0QEMU_QEMU_HARDDISK_123"
API = "https://api.hetzner.cloud/v1"

# The Hetzner API: each call's stdin (curl's -K - config) appended to curl-stdin; the answer by URL.
CURL = """
state = env["FAKE_STATE"]
with open(os.path.join(state, "curl-stdin"), "a") as f:
    f.write(sys.stdin.read())
url = argv[-1]
fail = os.path.join(state, "curl-fail")
if os.path.exists(fail) and open(fail).read().strip() in url:
    err("curl: (22) The requested URL returned error: 403")
    exit(22)
if "/ssh_keys?" in url:
    out(open(os.path.join(state, "ssh-keys")).read())
else:
    out('{"action": {"id": 1}}')
"""
# ssh root@<ip> <command>: `hostname` answers the next line of `hostnames` (the last one again once they run out).
SSH = """
state = env["FAKE_STATE"]
at = [i for i, a in enumerate(argv) if a.startswith("root@")][0]
command = " ".join(argv[at + 1:])
def read(name, default=""):
    path = os.path.join(state, name)
    return open(path).read() if os.path.exists(path) else default
if command == "hostname":
    names = read("hostnames").split()
    seen = int(read("hostname-calls", "0"))
    open(os.path.join(state, "hostname-calls"), "w").write(str(seen + 1))
    out(names[min(seen, len(names) - 1)])
elif command.startswith("readlink -f"):
    sys.stdout.write(read("resolved"))
elif command.startswith("ls -l /dev/disk/by-id/"):
    sys.stdout.write(read("volumes"))
elif command.startswith("lsblk"):
    out("sda 40G 123")
elif command == "test -e /etc/NIXOS":
    exit(int(read("nixos", "0")))
else:
    exit(99)
"""
NIX = """
if argv[:3] == ["--extra-experimental-features", "nix-command flakes", "eval"]:
    sys.stdout.write(open(os.path.join(env["FAKE_STATE"], "declared")).read())
"""
SECRETS = """
assert argv[:4] == ["run", "movies", "HETZNER_API_TOKEN", "--"], argv
os.execvpe(argv[4], argv[4:], dict(env, HETZNER_API_TOKEN="vault-token"))
"""


class ConvertHostTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.state = self.path("state")
        os.makedirs(self.state)
        os.makedirs(self.path("home", ".ssh"))
        self.write("state/ssh-keys", json.dumps({"ssh_keys": [{"id": 4242}]}))
        self.write("state/hostnames", "rescue")
        self.write("state/declared", ROOT_DISK)
        self.write("state/resolved", "/dev/sda\n")
        self.write("state/volumes", "/dev/sdb\n")
        self.fakes.program("curl", CURL)
        self.fakes.program("ssh", SSH)
        self.fakes.program("nix", NIX)
        self.fakes.program("secrets", SECRETS)
        self.fakes.script("ssh-keygen", "exit 0")
        self.fakes.script("ssh-keyscan", 'echo "$3 ssh-ed25519 AAAAfresh"')

    def convert(self, *args: str, token: Optional[str] = "env-token") -> cli_harness.Done:
        env: Dict[str, Optional[str]] = {"FAKE_STATE": self.state, "HOME": self.path("home"),
                                         "CONVERT_HOST_POLL_SECONDS": "0", "HETZNER_API_TOKEN": token}
        return self.run_tool(*args, env=env)

    def curl_urls(self) -> List[str]:
        return [call[-1] for call in self.fakes.calls("curl")]

    def ssh_commands(self) -> List[str]:
        return [" ".join(c[[i for i, a in enumerate(c) if a.startswith("root@")][0] + 1:]) for c in self.fakes.calls("ssh")]

    def test_no_host_or_an_unknown_one_is_the_usage(self) -> None:
        for args in ((), ("mongo-9", "--yes")):
            done = self.convert(*args)
            self.assertEqual(1, done.code, done.output)
            self.assertEqual("usage: convert-host <mongo-1|monitoring-1|k3s-worker-1> --yes\n", done.stderr)
        self.assertEqual([], self.fakes.calls("curl"))

    def test_without_yes_it_refuses_naming_the_machine_it_would_wipe(self) -> None:
        done = self.convert("monitoring-1")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("convert-host: refusing to run without --yes.", done.stderr)
        self.assertIn("This will REPARTITION /dev/sda on monitoring-1 (128.140.49.167, Hetzner server 164234644)",
                      done.stderr)
        self.assertEqual([], self.fakes.calls("curl"))
        self.assertEqual([], self.fakes.calls("ssh"))

    def test_with_no_token_it_runs_itself_again_under_the_vault_first(self) -> None:
        done = self.convert("k3s-worker-1", token=None)
        self.assertEqual(1, done.code, done.output)
        call = self.fakes.calls("secrets")
        self.assertEqual(1, len(call))
        self.assertEqual(["run", "movies", "HETZNER_API_TOKEN", "--"], call[0][:4])
        self.assertEqual(["k3s-worker-1"], call[0][-1:])
        self.assertIn("k3s-worker-1 (2.28.47.31, Hetzner server 164234589)", done.stderr)

    def test_a_conversion_runs_every_step_in_order_and_keeps_the_token_out_of_argv(self) -> None:
        self.write("state/hostnames", "ubuntu rescue rescue ubuntu rescue rescue rescue")
        done = self.convert("mongo-1", "--yes", token="vault-token")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([API + "/ssh_keys?name=pawel.krupinski%40gmail.com",
                          API + "/servers/164241785/actions/enable_rescue",
                          API + "/servers/164241785/actions/reset"], self.curl_urls())
        rescue = self.fakes.calls("curl")[1]
        self.assertEqual(["-X", "POST", "-d", '{"type":"linux64","ssh_keys":[4242]}'], rescue[5:9])
        for call in self.fakes.calls("curl"):
            self.assertEqual(["-fsS", "-K", "-", "-H", "Content-Type: application/json"], call[:5])
            self.assertNotIn("vault-token", " ".join(call))
        self.assertEqual('header = "Authorization: Bearer vault-token"\n' * 3, self.read("state/curl-stdin"))
        self.assertEqual(["hostname"] * 7 + ["readlink -f '%s' 2>/dev/null || true" % ROOT_DISK]
                         + ["ls -l /dev/disk/by-id/ 2>/dev/null | grep -E 'scsi-0HC_Volume_[0-9]+ ' | "
                            "awk '{print $NF}' | sed 's#\\.\\./\\.\\./#/dev/#'", "test -e /etc/NIXOS"],
                         self.ssh_commands())
        self.assertIn("ServerAliveInterval=5", self.fakes.calls("ssh")[0])
        self.assertEqual([["--extra-experimental-features", "nix-command flakes", "eval", "--raw",
                           FLAKE + "#nixosConfigurations.mongo-1.config.disko.devices.disk.main.device"],
                          ["run", "nixpkgs#nixos-anywhere", "--", "--flake", FLAKE + "#mongo-1", "--build-on-remote",
                           "root@" + IP]], self.fakes.calls("nix"))
        self.assertEqual([["-R", IP]], self.fakes.calls("ssh-keygen"))
        self.assertEqual(IP + " ssh-ed25519 AAAAfresh\n", self.read("home/.ssh/known_hosts"))
        for line in ("==> enabling rescue on mongo-1 (164241785)", "==> resetting mongo-1 into rescue",
                     "==> waiting for the rescue system on " + IP, "    layout names: " + ROOT_DISK,
                     "    resolves to /dev/sda, which is not a volume -- proceeding",
                     "==> installing NixOS on mongo-1 from %s#mongo-1" % FLAKE,
                     "==> mongo-1 converted.", "       sops updatekeys infra/nix/secrets/mongo-1.yaml",
                     "Then deploy for real:  colmena apply --on mongo-1"):
            self.assertIn(line + "\n", done.stdout)

    def test_no_operator_ssh_key_stops_before_the_rescue(self) -> None:
        self.write("state/ssh-keys", '{"ssh_keys": []}')
        done = self.convert("mongo-1", "--yes")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("convert-host: no Hetzner SSH key named pawel.krupinski@gmail.com in this project", done.stderr)
        self.assertEqual(1, len(self.fakes.calls("curl")))

    def test_a_refused_api_call_stops_the_run_with_curls_status(self) -> None:
        self.write("state/curl-fail", "enable_rescue")
        done = self.convert("mongo-1", "--yes")
        self.assertEqual(22, done.code, done.output)
        self.assertEqual(2, len(self.fakes.calls("curl")))
        self.assertEqual([], self.fakes.calls("ssh"))

    def test_a_machine_that_never_becomes_the_rescue_system_fails_after_sixty_probes(self) -> None:
        self.write("state/hostnames", "rescue rescue ubuntu")
        done = self.convert("mongo-1", "--yes")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("convert-host: %s never came up as the rescue system (waited 5 minutes)." % IP, done.stderr)
        self.assertEqual(["hostname"] * 60, self.ssh_commands())
        self.assertEqual([], self.fakes.calls("nix"))

    def test_a_layout_naming_a_volume_is_refused_before_the_disk_is_read(self) -> None:
        self.write("state/declared", "/dev/disk/by-id/scsi-0HC_Volume_777")
        done = self.convert("mongo-1", "--yes")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("convert-host: REFUSING -- mongo-1's disko layout names a Hetzner VOLUME "
                      "(/dev/disk/by-id/scsi-0HC_Volume_777).", done.stderr)
        self.assertEqual(["hostname"] * 3, self.ssh_commands())

    def test_a_declared_disk_that_is_absent_is_refused_listing_the_disks_there(self) -> None:
        self.write("state/resolved", "")
        done = self.convert("mongo-1", "--yes")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("convert-host: REFUSING -- %s does not exist on mongo-1.\n  Disks actually present there:\n"
                      "sda 40G 123\n" % ROOT_DISK, done.stderr)
        self.assertEqual(1, len(self.fakes.calls("nix")))

    def test_a_declared_disk_resolving_to_a_volume_is_refused(self) -> None:
        self.write("state/resolved", "/dev/sdb\n")
        self.write("state/volumes", "/dev/sdc\n/dev/sdb\n")
        done = self.convert("mongo-1", "--yes")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("convert-host: REFUSING -- %s resolves to /dev/sdb on mongo-1, which is a Hetzner VOLUME."
                      % ROOT_DISK, done.stderr)
        self.assertEqual(1, len(self.fakes.calls("nix")))

    def test_a_host_that_never_answers_as_nixos_still_finishes_and_warns_of_an_empty_keyscan(self) -> None:
        self.write("state/nixos", "1")
        self.fakes.script("ssh-keyscan", "exit 0")
        done = self.convert("mongo-1", "--yes")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(60, self.ssh_commands().count("test -e /etc/NIXOS"))
        self.assertIn("    WARNING: ssh-keyscan returned nothing for %s." % IP, done.stderr)
        self.assertIn("ssh-keygen -R %s && ssh-keyscan -t ed25519 %s >> ~/.ssh/known_hosts" % (IP, IP), done.stderr)
        self.assertFalse(os.path.exists(self.path("home/.ssh/known_hosts")))
        self.assertIn("==> mongo-1 converted.", done.stdout)


if __name__ == "__main__":
    unittest.main()
