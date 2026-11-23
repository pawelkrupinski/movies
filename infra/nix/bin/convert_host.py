#!/usr/bin/env python3
"""Convert one stock-Ubuntu Hetzner Cloud server into a NixOS host from this flake.

    infra/nix/bin/convert-host mongo-1 --yes

THIS REPARTITIONS THE TARGET'S ROOT DISK AND IS IRREVERSIBLE. Everything on /dev/sda is destroyed; an attached Hetzner
Volume is NOT touched (disko only owns the disks its layout names -- see nix/hosts/*/disko.nix), which is precisely why
mongo-1's database lives on one.

HOW IT WORKS, and why it is not bitcashier's route. bitcashier re-images onto a prebuilt `role=nixos-base` snapshot
and then rebuilds; that snapshot lives in its Hetzner project and this project has none, and building one needs a
Linux nix builder this estate does not have. So this takes bitcashier's OTHER path, the one its
RUNBOOK-borg-ovh-1-nixos.md uses for bare metal: boot the machine into Hetzner's rescue system and let nixos-anywhere
install over it. The rescue system is a live Linux with our SSH key already injected by the API, which is the only
foothold needed -- and it is the reason this works on a host we have NEVER been able to log into, which is the state
all three of these machines were in (they were created without the operator's key).

It is safe to re-run. A second conversion is simply another install. Stdlib only; CONVERT_HOST_POLL_SECONDS replaces
the 5 s between probes (a test's 0)."""
from __future__ import annotations

import json
import os
import subprocess
import sys
import time
from typing import List, Optional, Tuple

HERE = os.path.dirname(os.path.realpath(__file__))
FLAKE_DIR = os.path.dirname(os.path.dirname(HERE))
API = "https://api.hetzner.cloud/v1"
# The server ids and addresses are written here rather than looked up by name, because the lookup and the destruction
# would otherwise share a failure mode: a typo that matches no server is an error, but a typo that matches the WRONG
# server silently wipes it. An explicit table cannot resolve to a machine nobody named.
HOSTS = {"mongo-1": ("164241785", "178.105.221.61"), "monitoring-1": ("164234644", "128.140.49.167"),
         "k3s-worker-1": ("164234589", "2.28.47.31")}
# The host key CHANGES when the machine boots into rescue, and changes again when it boots into the installed NixOS.
# Pinning it would mean two known-hosts edits per conversion, so this deliberately does not verify it -- the exposure
# is one SSH session to a machine we are about to overwrite entirely, on a connection whose only payload is a public
# flake.
#
# ServerAliveInterval IS NOT BELT AND BRACES HERE, it is the difference between this tool failing and it HANGING FOR
# EVER. `ConnectTimeout` bounds only the CONNECT; a session that establishes and then dies silently -- which is exactly
# what a rescue system does when it is reset or its network blips -- leaves ssh waiting on a socket nothing will ever
# answer. That happened converting mongo-2 on 2026-09-02: the `readlink -f` probe sat open for eleven minutes against
# a machine that was up and idle. Three missed five-second probes and ssh gives up, which turns a hang into an error.
UNPINNED = ["-o", "StrictHostKeyChecking=no", "-o", "UserKnownHostsFile=/dev/null", "-o", "ConnectTimeout=5"]
SSH_OPTIONS = UNPINNED + ["-o", "ServerAliveInterval=5", "-o", "ServerAliveCountMax=3", "-o", "BatchMode=yes"]
PROBES = 60
VOLUMES = ("ls -l /dev/disk/by-id/ 2>/dev/null | grep -E 'scsi-0HC_Volume_[0-9]+ ' | awk '{print $NF}' | "
           "sed 's#\\.\\./\\.\\./#/dev/#'")


class Stop(Exception):
    """The run ends here with `code`, after `message` (if any) on stderr."""

    def __init__(self, code: int, message: str = "") -> None:
        super().__init__(message)
        self.code, self.message = code, message


def say(text: str) -> None:
    print(text, flush=True)


def pause() -> None:
    time.sleep(float(os.environ.get("CONVERT_HOST_POLL_SECONDS", "5")))


def output(argv: List[str], stdin: Optional[str] = None, quiet: bool = False) -> str:
    """`argv`'s stdout without its trailing newlines (as `$(...)`); a failure ends the run with its status."""
    done = subprocess.run(argv, input=stdin, text=True, stdout=subprocess.PIPE,
                          stderr=subprocess.DEVNULL if quiet else None)
    if done.returncode != 0:
        raise Stop(done.returncode)
    return done.stdout.rstrip("\n")


def api(*args: str) -> str:
    """curl to the Hetzner API. The bearer header goes in through a curl config on stdin, so the token is never in
    argv (ps)."""
    config = 'header = "Authorization: Bearer %s"\n' % os.environ["HETZNER_API_TOKEN"]
    return output(["curl", "-fsS", "-K", "-", "-H", "Content-Type: application/json", *args], stdin=config)


def ssh(ip: str, command: str) -> List[str]:
    return ["ssh", *SSH_OPTIONS, "root@" + ip, command]


def rescue(host: str, server: str) -> None:
    """The operator SSH key, looked up by NAME (its id changes every time the key is rotated), injected into the
    rescue system -- NOT into the installed system, which gets its keys from nix/modules/fleet/accounts.nix -- then a
    HARD RESET into it: a machine that declines to shut down cleanly would otherwise sit in the old system
    indefinitely, and there is nothing on these disks to lose."""
    keys = json.loads(api(API + "/ssh_keys?name=pawel.krupinski%40gmail.com")).get("ssh_keys") or [{}]
    key = keys[0].get("id")
    if key is None:
        raise Stop(1, "convert-host: no Hetzner SSH key named pawel.krupinski@gmail.com in this project")
    say("==> enabling rescue on %s (%s)" % (host, server))
    api("-X", "POST", "-d", '{"type":"linux64","ssh_keys":[%s]}' % key, "%s/servers/%s/actions/enable_rescue"
        % (API, server))
    say("==> resetting %s into rescue" % host)
    api("-X", "POST", "%s/servers/%s/actions/reset" % (API, server))


def wait_for_rescue(ip: str) -> None:
    """THREE CONSECUTIVE `rescue` ANSWERS from `hostname` (Hetzner's rescue system is literally called `rescue`, which
    no Ubuntu image matches by accident). The reset is asynchronous, so the first connection is usually still the old
    system; and the rescue system starts sshd early and carries on initialising, so one answer is routinely followed by
    the connection dropping (mongo-2, 2026-09-02). A miss RESETS the count: two answers either side of a reboot are not
    evidence of stability."""
    say("==> waiting for the rescue system on " + ip)
    stable = 0
    for _ in range(PROBES):
        probe = subprocess.run(ssh(ip, "hostname"), text=True, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        stable = stable + 1 if probe.stdout.rstrip("\n") == "rescue" else 0
        if stable >= 3:
            return
        pause()
    raise Stop(1, "convert-host: %s never came up as the rescue system (waited 5 minutes).\n"
                  "  Check the machine in the Hetzner console; it may have booted the installed system\n"
                  "  instead, which means enable_rescue did not take before the reset." % ip)


def declared_disk(host: str) -> str:
    declared = output(["nix", "--extra-experimental-features", "nix-command flakes", "eval", "--raw",
                       "%s#nixosConfigurations.%s.config.disko.devices.disk.main.device" % (FLAKE_DIR, host)])
    say("    layout names: " + declared)
    if "/scsi-0HC_Volume_" in declared:
        raise Stop(1, "convert-host: REFUSING -- %s's disko layout names a Hetzner VOLUME (%s).\n"
                      "  Installing there would repartition the data disk. Point it at the root disk's own\n"
                      "  scsi-0QEMU_QEMU_HARDDISK_<serial>; read it with: lsblk -dno NAME,SIZE,SERIAL"
                   % (host, declared))
    return declared


def check_root_disk(host: str, ip: str) -> None:
    """REFUSE TO INSTALL ONTO A HETZNER VOLUME -- the failure that cost mongo-1 a rebuild on 2026-08-29: its layout
    named `/dev/sda`, an attached volume won the race for that name, and disko formatted the DATABASE'S DISK. The
    layouts now name the root disk by its own serial (scsi-0QEMU_QEMU_HARDDISK_<serial>), which a volume can never be
    assigned (volumes are scsi-0HC_Volume_<id>). This checks from the rescue system, BEFORE anything is written, that
    the declared device is present there (absent: the install would fail late and confusingly) and is not a volume
    (one: it would succeed catastrophically)."""
    say("==> checking the declared root disk on " + host)
    declared = declared_disk(host)
    resolved = output(ssh(ip, "readlink -f '%s' 2>/dev/null || true" % declared))
    if not resolved:
        print("convert-host: REFUSING -- %s does not exist on %s.\n  Disks actually present there:" % (declared, host),
              file=sys.stderr, flush=True)
        subprocess.run(ssh(ip, "lsblk -dno NAME,SIZE,SERIAL"), stdout=sys.stderr)
        raise Stop(1)
    if resolved in output(ssh(ip, VOLUMES)).split():
        raise Stop(1, "convert-host: REFUSING -- %s resolves to %s on %s, which is a Hetzner VOLUME.\n"
                      "  This is the mongo-1 failure of 2026-08-29. Installing would destroy that volume's data."
                   % (declared, resolved, host))
    say("    resolves to %s, which is not a volume -- proceeding" % resolved)


def install(host: str, ip: str) -> None:
    """--build-on-remote because the operator's machine is aarch64-darwin and cannot produce an x86_64-linux closure
    at all; the rescue system has enough disk and RAM to do it. nixos-anywhere REBOOTS as its last act, so this waits
    for the machine to come back BEFORE touching known_hosts: a keyscan racing that reboot records the rescue system's
    key or nothing at all (monitoring-1, 2026-08-29)."""
    say("==> installing NixOS on %s from %s#%s" % (host, FLAKE_DIR, host))
    output(["nix", "run", "nixpkgs#nixos-anywhere", "--", "--flake", "%s#%s" % (FLAKE_DIR, host), "--build-on-remote",
            "root@" + ip])
    say("==> waiting for %s to come back up as NixOS" % host)
    for _ in range(PROBES):
        if subprocess.run(["ssh", *UNPINNED, "-o", "BatchMode=yes", "root@" + ip, "test -e /etc/NIXOS"],
                          stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL).returncode == 0:
            return
        pause()


def refresh_host_key(ip: str) -> None:
    """THE HOST KEY HAS CHANGED, and every later tool checks it strictly: the machine minted a fresh
    ssh_host_ed25519_key on first boot, so the old entry now looks exactly like a man-in-the-middle and stops
    `nixos-rebuild --target-host`. Re-learning it here is safe: we installed this machine ourselves, seconds ago. A
    keyscan that returns nothing is said loudly -- its silence is what produced the monitoring-1 confusion."""
    say("==> refreshing the host key for %s (it changed during the install)" % ip)
    subprocess.run(["ssh-keygen", "-R", ip], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    scan = subprocess.run(["ssh-keyscan", "-t", "ed25519", ip], text=True, stdout=subprocess.PIPE,
                          stderr=subprocess.DEVNULL).stdout
    if not any(scan.splitlines()):
        print("    WARNING: ssh-keyscan returned nothing for %s.\n    Re-run once it answers:  ssh-keygen -R %s && "
              "ssh-keyscan -t ed25519 %s >> ~/.ssh/known_hosts" % (ip, ip, ip), file=sys.stderr, flush=True)
        return
    with open(os.path.join(os.environ["HOME"], ".ssh", "known_hosts"), "a") as known_hosts:
        known_hosts.write(scan)


def finished(host: str, ip: str) -> str:
    return """
==> {host} converted.

Two things are NOT done yet and the host cannot decrypt its secrets until both are:

  1. Derive the new host age key -- the machine minted a fresh SSH host key on this boot, so the
     old recipient (if any) is now worthless:

       ssh root@{ip} cat /etc/ssh/ssh_host_ed25519_key.pub | nix run nixpkgs#ssh-to-age

  2. Add it to infra/.sops.yaml under the {host} anchor, then:

       sops updatekeys infra/nix/secrets/{host}.yaml

Then deploy for real:  colmena apply --on {host}""".format(host=host, ip=ip)


def chosen(argv: List[str]) -> Tuple[str, str, str]:
    host = argv[0] if argv else ""
    if host not in HOSTS:
        raise Stop(1, "usage: convert-host <mongo-1|monitoring-1|k3s-worker-1> --yes")
    server, ip = HOSTS[host]
    if argv[1:2] != ["--yes"]:
        raise Stop(1, "convert-host: refusing to run without --yes.\n\n"
                      "  This will REPARTITION /dev/sda on %s (%s, Hetzner server %s)\n"
                      "  and install NixOS over whatever is there. It is irreversible.\n\n"
                      "  Attached Hetzner Volumes are not touched." % (host, ip, server))
    return host, server, ip


def main(argv: List[str]) -> int:
    # The Hetzner token comes from the operator's Touch ID vault (`movies/HETZNER_API_TOKEN`), never from a file:
    # missing from the environment, the tool re-runs itself under `secrets run` (one Touch ID). Done FIRST, before the
    # irreversible-confirmation prompt, so nobody is asked to confirm twice.
    if not os.environ.get("HETZNER_API_TOKEN"):
        os.execvp("secrets", ["secrets", "run", "movies", "HETZNER_API_TOKEN", "--", os.path.realpath(__file__), *argv])
    try:
        host, server, ip = chosen(argv)
        rescue(host, server)
        wait_for_rescue(ip)
        check_root_disk(host, ip)
        install(host, ip)
        refresh_host_key(ip)
    except Stop as stop:
        if stop.message:
            print(stop.message, file=sys.stderr)
        return stop.code
    say(finished(host, ip))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
