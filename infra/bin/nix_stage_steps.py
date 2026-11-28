#!/usr/bin/env python3
"""THE TWO DECISION STEPS OF nix-stage-closures.yml, run from infra/ (the steps' working directory):

  nix_stage_steps.py evaluate   "Every host in the flake evaluates": every host the flake declares instantiated
                                WITHOUT building (`drvPath` forces the whole module system, assertions included, and
                                compiles nothing), each one that does not named; a flake with no host is no pass.
  nix_stage_steps.py stage      "Build, sign, copy and pin -- activating nothing": the keys made files with modes, then
                                bin/stage-nixos-closures, its exit kept and annotated.

THE ROSTER IS THE FLAKE, never a list in this file -- the rule bin/stage-nixos-closures follows and for the same reason:
a hand-kept list of hosts silently stops matching the fleet. THE TOOL IS THE POLICY: which hosts are covered, what
counts as a failure and what an operator is told to run next live in bin/stage-nixos-closures, so the same behaviour is
available from a laptop and this workflow cannot quietly diverge from it.

The steps' inline blocks until 2026-11-22 (the shell-to-Python migration's workflow blocks); nix_stage_steps_test.py
holds both."""
from __future__ import annotations

import json
import os
import sys
import tempfile
from typing import Dict, List, Mapping, TextIO, cast

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(HERE)), "scripts", "lib"))
from processes import machine  # noqa: E402
from cli import tool_actions  # noqa: E402
import tool_process  # noqa: E402

# What `nix copy` passes to its own ssh. It must AGREE with the options bin/stage-nixos-closures uses for its direct
# calls -- otherwise its "is this host already current" probe succeeds under one set of rules and the copy fails under
# another, which reads as an unreachable host that answered a moment ago.
SSH_OPTIONS = "-o BatchMode=yes -o StrictHostKeyChecking=accept-new -o ConnectTimeout=10"
NOT_STAGED = ("bin/stage-nixos-closures exited 2 -- it could not read the flake or found no hosts, which is NOT the same"
              " as every host being up to date.")


class Steps:
    def __init__(self, infra: str, env: Mapping[str, str], runner: tool_process.Runner, actions: tool_actions.Actions,
                 out: TextIO) -> None:
        self.infra = infra
        self.env = env
        self.runner = runner
        self.actions = actions
        self.out = out

    def hosts(self) -> List[str]:
        listed = self.runner.capture(tool_process.Command(
            ["nix", "eval", "--json", ".#nixosConfigurations", "--apply", "builtins.attrNames"], self.env, cwd=self.infra))
        self.out.write(listed.stderr)
        try:
            names: object = json.loads(listed.stdout) if listed.code == 0 else []
        except ValueError:
            names = []
        return [str(n) for n in cast(List[object], names)] if isinstance(names, list) else []

    def evaluate(self) -> int:
        hosts = self.hosts()
        if not hosts:
            self.actions.annotate("error", "Nothing was evaluated", "This flake declares no nixosConfigurations, which is"
                                  " NOT the same as every host being fine.")
            return 1
        overall = 0
        for host in hosts:
            self.actions.group(host)
            target = f".#nixosConfigurations.{host}.config.system.build.toplevel.drvPath"
            if self.runner.run(tool_process.Command(["nix", "eval", "--raw", target], self.env, cwd=self.infra)) == 0:
                self.actions.say(f"\n  {host}: evaluates")
            else:
                self.actions.annotate("error", "A host does not evaluate", f"{host} -- main will not build for it, so"
                                      " bin/stage-nixos-closures cannot stage it and the host cannot self-heal.")
                overall = 1
            self.actions.end_group()
        return overall

    def missing_key(self) -> bool:
        """EACH PREREQUISITE IS REPORTED AS ITSELF: "there is no signing key" and "a host refused the key" are
        different problems with different owners."""
        if not self.env.get("NIX_STAGE_SIGNING_KEY"):
            self.actions.annotate("error", "No signing key", "secrets.NIX_STAGE_SIGNING_KEY is unset, so closures could"
                                  " not be signed -- and the hosts accept this job's paths ONLY because they are signed,"
                                  " since the deploy account is deliberately not a trusted nix user. Nothing was staged.")
            return True
        if not self.env.get("NIX_STAGE_SSH_KEY"):
            self.actions.annotate("error", "No deploy key", "secrets.NIX_STAGE_SSH_KEY is unset, so no host could be"
                                  " reached. Nothing was staged.")
            return True
        return False

    def stage(self) -> int:
        if self.missing_key():
            return 1
        # THE SECRETS BECOME FILES, WITH MODES, AND ARE REMOVED ON EVERY EXIT PATH: a private key in a child's
        # environment is visible there, and OpenSSH REFUSES a group- or world-readable one ("Permission denied
        # (publickey)", which looks like the host rejecting it).
        with tempfile.TemporaryDirectory(prefix="nix-stage.", dir=self.env.get("RUNNER_TEMP") or None) as workspace:
            os.chmod(workspace, 0o700)
            signing = private_file(os.path.join(workspace, "signing.key"), self.env["NIX_STAGE_SIGNING_KEY"])
            # THE SSH KEY GOES WHERE A PLAIN `ssh` LOOKS FOR IT: `nix copy --to ssh://...` spawns its OWN ssh, so a key
            # named only on the tool's command lines would not be found by the copy -- the one step that matters.
            ssh_dir = os.path.join(self.env.get("HOME") or os.path.expanduser("~"), ".ssh")
            os.makedirs(ssh_dir, exist_ok=True)
            private_file(os.path.join(ssh_dir, "id_ed25519"), self.env["NIX_STAGE_SSH_KEY"])
            env: Dict[str, str] = dict(self.env, NIX_SSHOPTS=SSH_OPTIONS, NIX_SIGNING_KEY_FILE=signing)
            status = self.runner.run(tool_process.Command(
                [sys.executable, os.path.join(self.infra, "bin/stage-nixos-closures")], env, cwd=self.infra))
        self.annotate_staging(status)
        return status

    def annotate_staging(self, status: int) -> None:
        if status == 2:
            self.actions.annotate("error", "Nothing was staged", NOT_STAGED)
        elif status != 0:
            self.actions.annotate("error", "A host could not be staged", f"bin/stage-nixos-closures exited {status}. A"
                                  " host that could not be reached has NOT got the closure, and what it would run is"
                                  " unknown. Scroll up for which.")


def private_file(path: str, text: str) -> str:
    """`text` and a newline into `path`, readable by its owner alone from the moment it exists."""
    descriptor = os.open(path, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600)
    with os.fdopen(descriptor, "w") as f:
        f.write(text + "\n")
    os.chmod(path, 0o600)
    return path


def main(argv: List[str]) -> int:
    """The composition point: infra/ (this file's parent's), the environment, the real runner and step commands."""
    steps = Steps(os.path.dirname(HERE), machine.environment(), tool_process.SubprocessRunner(),
                  tool_actions.Actions(machine.environment(), sys.stdout), sys.stdout)
    commands = {"evaluate": steps.evaluate, "stage": steps.stage}
    if len(argv) != 1 or argv[0] not in commands:
        print("usage: nix_stage_steps.py evaluate|stage", file=sys.stderr)
        return 64
    return commands[argv[0]]()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
