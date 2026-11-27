"""infra/bin/check's offline steps: terraform, the dashboard's plists and its lint + tests, and the test/ scripts (what
each one checks: its comment in SCRIPTS and the script's own header). Each prints its ok and failure lines exactly as
the shell gate did."""
from __future__ import annotations

import glob
import os
import re
import shutil
from dataclasses import dataclass
from typing import Callable, Dict, List

from check_gate import Gate, indented, lines, ok_count, tail

PLIST_LOAD = 'import plistlib,sys; plistlib.load(open(sys.argv[1],"rb"))'
TESTS_PASSED = re.compile(r"Tests +[0-9]+ passed")


def terraform_fmt(gate: Gate) -> None:
    gate.step("terraform fmt")
    if gate.run(["terraform", "fmt", "-check", "-recursive", "terraform"]) == 0:
        gate.say("  ok")
    else:
        gate.fail("  FAILED -- run: terraform fmt -recursive infra/terraform")


def terraform_validate(gate: Gate) -> None:
    """`-backend=false` so this needs no Terraform Cloud credentials: validate checks the configuration, not the state,
    and requiring a login here would make the gate unrunnable in CI."""
    gate.step("terraform validate")
    if (gate.silently(["terraform", "-chdir=terraform", "init", "-backend=false", "-input=false"]) == 0
            and gate.run(["terraform", "-chdir=terraform", "validate"]) == 0):
        gate.say("  ok")
    else:
        gate.fail("  FAILED")


def plist_syntax(gate: Gate) -> None:
    """XML FORBIDS A DOUBLE HYPHEN INSIDE A COMMENT: launchctl rejects such a plist whole, so the agent never runs."""
    gate.step("plist syntax")
    for plist in sorted(glob.glob(os.path.join(gate.cwd, "version-dashboard", "*.plist"))):
        name = os.path.relpath(plist, gate.cwd)
        if gate.run(["python3", "-c", PLIST_LOAD, name]) == 0:
            gate.say(f"  ok  {name}")
        else:
            gate.fail(f"  FAILED {name} (XML forbids -- inside a comment; that is the usual cause)")


def dashboard(gate: Gate) -> None:
    """THE DASHBOARD IS NOT COVERED BY ANY OTHER CHECK HERE (TypeScript outside the flake). Skips, saying so, when npm
    or the dependencies are missing."""
    gate.step("version-dashboard lint + tests")
    if not shutil.which("npm", path=gate.env.get("PATH")):
        gate.say("  skipped: no npm on PATH")
    elif not dependencies(gate):
        gate.say("  skipped: version-dashboard dependencies are not installed and npm ci failed")
    else:
        lint_and_test(gate)


def dependencies(gate: Gate) -> bool:
    folder = os.path.join(gate.cwd, "version-dashboard")
    if os.path.isdir(os.path.join(folder, "node_modules")):
        return True
    return os.path.isdir(folder) and gate.silently(["npm", "ci", "--silent"], cwd="version-dashboard") == 0


def lint_and_test(gate: Gate) -> None:
    code, out = gate.output(["npm", "run", "--silent", "lint"], cwd="version-dashboard")
    if code == 0:
        code, tested = gate.output(["npm", "test", "--silent"], cwd="version-dashboard")
        out += tested
    out = out.rstrip("\n")
    if code == 0:
        passed = [line.lstrip(" ") for line in lines(out) if TESTS_PASSED.search(line)]
        gate.say("  ok  " + "\n".join(passed))
    else:
        gate.fail(*indented(tail(out, 30)))


@dataclass(frozen=True)
class Script:
    """A test/ script: `shows` how its success prints (echo: all of it; tail1: its last line indented; last: its last
    line; count / count-any: `ok  N test(s)` of its `... ok$` / `... ok` lines; spec-count: `[spec] N cases pass`;
    else a fixed line), `missing` what a
    missing one does (said: a FAILED line; silent: nothing; runs: run anyway, and fail), `failure` how its failure
    prints (echo, indent, tail25)."""
    step: str
    script: str
    shows: str
    missing: str
    failure: str


def run_script(gate: Gate, script: Script) -> None:
    gate.step(script.step)
    path = f"test/{script.script}"
    if script.missing != "runs" and not os.path.isfile(os.path.join(gate.cwd, path)):
        if script.missing == "said":
            gate.fail(f"  FAILED {path} is missing")
        return
    code, out = gate.output(["python3" if path.endswith(".py") else "bash", path])
    out = out.rstrip("\n")
    if code == 0:
        gate.say(*SHOWS.get(script.shows, lambda _: [script.shows])(out))
    else:
        gate.fail(*FAILURES[script.failure](out))


SHOWS: Dict[str, Callable[[str], List[str]]] = {
    "echo": lines,
    "tail1": lambda out: indented(tail(out, 1)),
    "last": lambda out: tail(out, 1),
    "count": lambda out: [f"  ok  {ok_count(out)} test(s)"],
    "count-any": lambda out: [f"  ok  {ok_count(out, anchored=False)} test(s)"],
    "spec-count": lambda out: [f"  [spec] {ok_count(out)} cases pass"],
}
FAILURES: Dict[str, Callable[[str], List[str]]] = {
    "echo": lines,
    "indent": lambda out: indented(lines(out)),
    "tail25": lambda out: indented(tail(out, 25)),
}

# Before the per-host `nix eval` steps. Each script's header says what it checks and the incident behind it.
SCRIPTS_BEFORE_EVAL = (
    # the provisioned Grafana JSON: structure, and the host/job/mountpoint literals its queries pin
    Script("grafana dashboard tests", "test_dashboards.py", "count-any", "silent", "tail25"),
    # each alerting rule fed a series and asked what it does
    Script("alerting rule tests", "test_alert_rules.sh", "echo", "said", "echo"),
    # bin/backtest-alerts' decisions, offline
    Script("alert backtest logic", "test_backtest_alerts.py", "count", "said", "tail25"),
    # nix/files/identity-regressions.py: net losses and flips from the take-change log
    Script("identity regression watch", "test_identity_regressions.py", "count", "runs", "tail25"),
    # every writePython3Bin script linted by the flake8 its build runs (nix eval never builds them)
    Script("python writers' build-time flake8", "test_nix_python_writers.py", "count", "said", "tail25"),
    # which receiver an alert's labels reach (first-match-wins routing)
    Script("alertmanager routing", "test_alertmanager.sh", "echo", "said", "echo"),
    # the vector transform run by the real vector (INFRA_CHECK_BUILD_VECTOR=1 forces the build)
    Script("pod log fields", "test_log_fields.sh", "echo", "said", "echo"),
    # the patched exporter against MongoDB 8.3's opLatencies histogram shape
    Script("mongodb_exporter patch", "test_mongodb_exporter_patch.sh", "echo", "runs", "echo"),
    Script("synthetic probes", "test_synthetic_probes.sh", "last", "runs", "echo"),
    # the real vhost asked of the real Caddy: the facet throttle and the share-preview crawler
    Script("public proxy behaviour", "test_public_proxy.sh", "echo", "said", "echo"),
    # the fleet's own resolvers, never Hetzner DHCP's (2026-11-05 outage)
    Script("host dns resolvers", "test_host_dns.sh", "tail1", "runs", "indent"),
    # fleet.reboot.waitFor: mongo-1's nightly mongodump (2026-11-10)
    Script("reboot waits for in-flight work", "test_reboot_wait_for.sh", "tail1", "runs", "indent"),
    # every account with an ssh key lingers (the 2026-11-10 half-applied switch)
    Script("ssh accounts linger", "test_ssh_accounts_linger.sh", "tail1", "said", "indent"),
    # both operator Macs' keys reach root and pawel on every host
    Script("operator keys", "test_operator_keys.sh", "tail1", "runs", "indent"),
    # every operator Mac's dashboard key under monitoring-1's forced Prometheus read
    Script("dashboard read-only keys", "test_dashboard_readonly_keys.sh", "tail1", "runs", "indent"),
    # a rule edit reloads prometheus.service, a scrape or flag edit restarts it
    Script("prometheus reload", "test_prometheus_reload.sh", "tail1", "runs", "indent"),
    # zram, vm.swappiness and the kubelet's reserves, from each host's evaluated config
    Script("host memory safety", "test_host_memory.sh", "tail1", "said", "indent"),
)
SCRIPTS_AFTER_EVAL = (
    # nixos-auto-apply's classifier over recorded switch output
    Script("auto-apply classifier", "test_auto_apply_classify.py", "spec-count", "said", "echo"),
    Script("bin/deploy preview and stale-tree logic", "test_deploy.py", "  [spec] deploy logic passes", "runs", "echo"),
    # bin/fleet-ssh and bin/fleet_hosts.py, against a fake flake
    Script("fleet-ssh address resolution", "test_fleet_hosts.py", "count", "said", "tail25"),
    # nixos_reboot_required's decision (nix/files/reboot-required.sh)
    Script("reboot-required decision", "test_reboot_required.sh", "tail1", "said", "indent"),
    # the heap-dump prune every kinowo pod runs (nix/files/heap-dumps.sh)
    Script("heap-dump budget", "test_heap_dumps.sh", "tail1", "said", "indent"),
    # the off-box copy of mongo-1's dump (nix/files/mongodb-offbox-pull.sh)
    Script("mongodb off-box pull", "test_mongodb_offbox_pull.sh", "tail1", "said", "indent"),
)
