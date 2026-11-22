"""infra/bin/check's steps that ask the flake about each host: its toplevel derivation, then three questions `nix eval`
proving each host builds cannot answer -- where the StartLimit* keys landed, whether a log-shipping host accepts a
vector restart, and which hosts may CREATE the replica set."""
from __future__ import annotations

from typing import List

from check_gate import Gate, indented, tail

HOSTS = ("mongo-1", "monitoring-1", "k3s-worker-1")
NIX = ["nix", "--extra-experimental-features", "nix-command flakes"]
# StartLimitIntervalSec / StartLimitBurst are [Unit] directives: under serviceConfig they land in [Service], where
# systemd ignores them and quietly applies the default limit.
PLACEMENT_APPLY = """svcs:
       let bad = builtins.filter (n:
             let sc = (svcs.${n}.serviceConfig or {}); in
             sc ? StartLimitIntervalSec || sc ? StartLimitBurst
             || sc ? startLimitIntervalSec || sc ? startLimitBurst)
           (builtins.attrNames svcs);
       in builtins.concatStringsSep " " bad"""
ORIGIN_APPLY = 'svcs: if svcs ? mongodb-init-replicaset then "yes" else "no"'


def services(host: str) -> str:
    return f".#nixosConfigurations.{host}.config.systemd.services"


def toplevels(gate: Gate) -> None:
    """EVERY host's toplevel derivation: the whole module tree type-checked and every `assertions` entry run."""
    for host in HOSTS:
        gate.step(f"nix eval {host}")
        code, out = gate.output(NIX + ["eval", f".#nixosConfigurations.{host}.config.system.build.toplevel.drvPath"])
        out = out.rstrip("\n")
        if code == 0:
            gate.say("  ok  " + out.rsplit("/", 1)[-1])
        else:
            gate.fail(*indented(tail(out, 20)))


def key_placement(gate: Gate) -> None:
    gate.step("systemd key placement")
    misplaced: List[str] = []
    for host in HOSTS:
        code, offenders = gate.stdout_of(NIX + ["eval", "--raw", services(host), "--apply", PLACEMENT_APPLY])
        if code != 0 or offenders == "EVALFAIL":
            gate.fail(f"  FAILED could not evaluate {host} (see its nix eval step above)")
        elif offenders:
            misplaced.append(f"{host}: {offenders}")
    if not misplaced:
        gate.say("  ok  no StartLimit* directives are hidden in a [Service] section")
        return
    gate.fail("  FAILED StartLimit* under serviceConfig, where systemd ignores it:",
              *["           " + entry for entry in misplaced],
              "         Move them to the unit level (beside wantedBy), not into serviceConfig.")


def shipper_restart(gate: Gate, here: str) -> None:
    """vector.service carries its rendered config in restartTriggers, and nixos-auto-apply is default-deny: a host
    that has not accepted that bounce refuses the WHOLE closure, with nothing said."""
    gate.step("a log shipper may be restarted unattended")
    flake = f"{here}/../nix#nixosConfigurations"
    refusing: List[str] = []
    for host in HOSTS:
        if gate.stdout_of(NIX + ["eval", f"{flake}.{host}.config.fleet.logs.enable"])[1] != "true":
            continue
        _, accepts = gate.stdout_of(NIX + ["eval", "--json", f"{flake}.{host}.config.fleet.autoApply.restartableUnits"])
        if '"vector.service"' not in accepts:
            refusing.append(host)
    if not refusing:
        gate.say("  ok  every host that ships logs accepts a vector restart")
    else:
        gate.fail("  FAILED these hosts ship logs but would refuse the switch that changes the shipper:",
                  *["         " + host for host in refusing])


def replica_set_origin(gate: Gate) -> None:
    """EXACTLY ONE HOST MAY CREATE THE REPLICA SET. An evaluation failure is not an answer: a host that cannot be
    evaluated is named, never counted as carrying no initiator."""
    gate.step("replica-set origin")
    origins: List[str] = []
    unevaluated: List[str] = []
    for host in HOSTS:
        code, has_unit = gate.stdout_of(NIX + ["eval", "--raw", services(host), "--apply", ORIGIN_APPLY])
        if code != 0:
            unevaluated.append(host)
        elif has_unit == "yes":
            origins.append(host)
    if unevaluated:
        gate.fail(f"  FAILED could not evaluate: {' '.join(unevaluated)}",
                  "         The nix eval steps above say why. Until every host evaluates, this check cannot",
                  "         know which of them would initiate the replica set.")
    say_origins(gate, origins)


def say_origins(gate: Gate, origins: List[str]) -> None:
    if len(origins) == 1:
        gate.say(f"  ok  only {origins[0]} may initiate the replica set")
    elif not origins:
        gate.fail("  FAILED no host would initiate the replica set.",
                  "         A fleet built from scratch would come up with mongod running and no set at all,",
                  "         so the app's change streams -- which need an oplog -- would never work.",
                  "         Exactly one mongo host needs fleet.mongodb.initiateReplicaSet = true.")
    else:
        gate.fail(f"  FAILED {len(origins)} hosts would each CREATE their own replica set:",
                  *["           " + host for host in origins],
                  "         Set fleet.mongodb.initiateReplicaSet = false on every host that JOINS an existing",
                  "         set and let the current primary add it -- see",
                  "         infra/ansible/playbooks/migrate-mongo-replica.yml.")

