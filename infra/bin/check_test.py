#!/usr/bin/env python3
"""infra/bin/check, THE GATE FOR infra/, black-box over a scratch infra/ tree: fake `nix`, `terraform` and `npm` first
on its PATH, and every test/ script it runs written as a stand-in that prints a known output and exits as told. Each
step's ok and failure lines, the missing-script lines, the skipped dashboard paths, the nix eval commands it asks
(argv and cwd), the three cross-host checks (StartLimit placement, the log shipper's restart, replica-set origin), the
verdict line and the exit status. It takes no flags: an argument changes nothing. Runs in seconds, no network, no
real nix."""
from __future__ import annotations

import json
import os
import shlex
import shutil
import sys
import unittest
from typing import Dict, List, Optional, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))),
                                "scripts", "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("infra/bin/check.py", "infra/bin/check")
HOSTS = ("mongo-1", "monitoring-1", "k3s-worker-1")
FLAGS = ["--extra-experimental-features", "nix-command flakes"]
PLACEMENT_APPLY = """svcs:
       let bad = builtins.filter (n:
             let sc = (svcs.${n}.serviceConfig or {}); in
             sc ? StartLimitIntervalSec || sc ? StartLimitBurst
             || sc ? startLimitIntervalSec || sc ? startLimitBurst)
           (builtins.attrNames svcs);
       in builtins.concatStringsSep " " bad"""
ORIGIN_APPLY = 'svcs: if svcs ? mongodb-init-replicaset then "yes" else "no"'
# nix: ANSWERS (host, question) -> [stdout, exit]; a question unanswered gets the healthy default
NIX = """
answers = json.load(open(ANSWERS)) if os.path.exists(ANSWERS) else {}
target = [a for a in argv if "nixosConfigurations." in a][0]
host = target.split("nixosConfigurations.")[1].split(".")[0]
if target.endswith("toplevel.drvPath"): kind, default = "drv", ["/nix/store/abc-nixos-system-" + host + ".drv\\n", 0]
elif "concatStringsSep" in " ".join(argv): kind, default = "placement", ["", 0]
elif "--apply" in argv: kind, default = "origin", ["yes" if host == "mongo-1" else "no", 0]
elif target.endswith("logs.enable"): kind, default = "logs", ["true\\n", 0]
else: kind, default = "accepts", ['["sshd.service","vector.service"]\\n', 0]
text, code = answers.get(host + " " + kind, default)
sys.stdout.write(text)
sys.stdout.flush()
if code: err("error: " + host + " " + kind + " does not evaluate")
exit(code)
"""
TERRAFORM = """
codes = json.load(open(ANSWERS)) if os.path.exists(ANSWERS) else {}
verb = [a for a in argv if not a.startswith("-chdir")][0]
if verb == "validate": out("Success! The configuration is valid.")
if verb == "fmt" and codes.get("fmt"): out("terraform/main.tf")
exit(codes.get(verb, 0))
"""
NPM = """
codes = json.load(open(ANSWERS)) if os.path.exists(ANSWERS) else {}
verb = [a for a in argv if not a.startswith("-") and a != "run"][0]
if verb == "lint": out("lint clean" if not codes.get("lint") else "\\n".join("lint error %d" % i for i in range(40)))
if verb == "test": out(" Test Files  3 passed (3)\\n      Tests  12 passed (12)")
if verb == "ci": os.makedirs("node_modules", exist_ok=not codes.get("ci"))
exit(codes.get(verb, 0))
"""
SH_OUT, PY_OUT = "  alpha\n  omega\n", "a ... ok\nb ... ok\nc ... ok then\n"
# Each test/ script: its step, how its success prints (echo: all of it; tail1: the last line indented; last: the last
# line; count / count-any: `ok  N test(s)` of its `... ok$` / `... ok` lines; a [spec] line), whether a missing one
# is said as missing (the rest run anyway: a missing script is then a failure of its own), and how its failure prints.
SCRIPTS: Tuple[Tuple[str, str, str, str, str], ...] = (
    ("test_dashboards.py", "grafana dashboard tests", "count-any", "silent", "tail25"),
    ("test_alert_rules.sh", "alerting rule tests", "echo", "said", "echo"),
    ("test_backtest_alerts.py", "alert backtest logic", "count", "said", "tail25"),
    ("test_identity_regressions.py", "identity regression watch", "count", "runs", "tail25"),
    ("test_alertmanager.sh", "alertmanager routing", "echo", "said", "echo"),
    ("test_log_fields.sh", "pod log fields", "echo", "said", "echo"),
    ("test_mongodb_exporter_patch.sh", "mongodb_exporter patch", "echo", "runs", "echo"),
    ("test_synthetic_probes.sh", "synthetic probes", "last", "runs", "echo"),
    ("test_public_proxy.sh", "public proxy behaviour", "echo", "said", "echo"),
    ("test_host_dns.sh", "host dns resolvers", "tail1", "runs", "indent"),
    ("test_reboot_wait_for.sh", "reboot waits for in-flight work", "tail1", "runs", "indent"),
    ("test_ssh_accounts_linger.sh", "ssh accounts linger", "tail1", "said", "indent"),
    ("test_operator_keys.sh", "operator keys", "tail1", "runs", "indent"),
    ("test_dashboard_readonly_keys.sh", "dashboard read-only keys", "tail1", "runs", "indent"),
    ("test_prometheus_reload.sh", "prometheus reload", "tail1", "runs", "indent"),
    ("test_host_memory.sh", "host memory safety", "tail1", "said", "indent"),
    ("test_auto_apply_classify.py", "auto-apply classifier", "  [spec] 2 cases pass", "said", "echo"),
    ("test_deploy.py", "bin/deploy preview and stale-tree logic", "  [spec] deploy logic passes", "runs", "echo"),
    ("test_fleet_hosts.py", "fleet-ssh address resolution", "count", "said", "tail25"),
    ("test_reboot_required.sh", "reboot-required decision", "tail1", "said", "indent"),
    ("test_heap_dumps.sh", "heap-dump budget", "tail1", "said", "indent"),
    ("test_mongodb_offbox_pull.sh", "mongodb off-box pull", "tail1", "said", "indent"),
)
NIX_EVAL_AFTER = "host memory safety"  # the per-host `nix eval` steps come right after this one


def head(name: str) -> str:
    return f"\n\033[1m==> {name}\033[0m\n"


def ok_lines(script: str, how: str) -> str:
    output = PY_OUT if script.endswith(".py") else SH_OUT
    return {"echo": output, "tail1": "    omega\n", "last": "  omega\n", "count": "  ok  2 test(s)\n",
            "count-any": "  ok  3 test(s)\n"}.get(how, how + "\n")


def green_steps() -> List[Tuple[str, str]]:
    steps = [("terraform fmt", "  ok\n"), ("terraform validate", "Success! The configuration is valid.\n  ok\n"),
             ("plist syntax", "  ok  version-dashboard/agent.plist\n"),
             ("version-dashboard lint + tests", "  ok  Tests  12 passed (12)\n")]
    for script, step, how, _, _ in SCRIPTS:
        steps.append((step, ok_lines(script, how)))
        if step == NIX_EVAL_AFTER:
            steps += [(f"nix eval {h}", f"  ok  abc-nixos-system-{h}.drv\n") for h in HOSTS]
    return steps + [
        ("systemd key placement", "  ok  no StartLimit* directives are hidden in a [Service] section\n"),
        ("a log shipper may be restarted unattended", "  ok  every host that ships logs accepts a vector restart\n"),
        ("replica-set origin", "  ok  only mongo-1 may initiate the replica set\n")]


GREEN = "\n\033[1;32minfra/bin/check: all green\033[0m\n"
RED = "\n\033[1;31minfra/bin/check: FAILED\033[0m\n"


class InfraCheckTest(cli_harness.ToolCase):
    def setUp(self) -> None:
        super().setUp()
        self.infra = self.path("checkout/infra")
        bin_dir = os.path.join(self.infra, "bin")
        os.makedirs(bin_dir)
        source = os.path.dirname(TOOL)
        for name in os.listdir(source):
            if name == "nix_path.py" or (name.startswith("check") and name.endswith(".py")
                                         and not name.endswith("_test.py")):
                shutil.copy2(os.path.join(source, name), bin_dir)
        self.tool = os.path.join(bin_dir, os.path.basename(TOOL))
        shutil.copy2(TOOL, self.tool)
        os.makedirs(self.path("checkout/scripts"))
        os.symlink(os.path.join(cli_harness.ROOT, "scripts/lib"), self.path("checkout/scripts/lib"))
        for name, body in (("nix", NIX), ("terraform", TERRAFORM), ("npm", NPM)):
            self.fakes.program(name, f"ANSWERS = {self.path(name + '.json')!r}\n" + body)
        self.write("checkout/infra/version-dashboard/agent.plist", PLIST)
        os.makedirs(os.path.join(self.infra, "version-dashboard/node_modules"))
        for script, _, _, _, _ in SCRIPTS:
            self.script(script)

    def script(self, name: str, output: Optional[str] = None, code: int = 0) -> None:
        text = (PY_OUT if name.endswith(".py") else SH_OUT) if output is None else output
        if name.endswith(".py"):
            body = f"import sys\nsys.stderr.write({text!r})\nsys.exit({code})\n"
        else:
            body = f"printf '%s' {shlex.quote(text)}\nexit {code}\n"
        self.write(f"checkout/infra/test/{name}", body)

    def answer(self, fake: str, answers: Dict[str, object]) -> None:
        self.write(f"{fake}.json", json.dumps(answers))

    def check(self, *args: str, env: Optional[Dict[str, Optional[str]]] = None) -> cli_harness.Done:
        given: Dict[str, Optional[str]] = {"PATH": "/usr/bin:/bin", "NIX_PATH_CANDIDATES": self.path("no/nix"),
                                           **(env or {})}
        return cli_harness.run_bounded([self.tool, *args], env=self.env(given), cwd=self.work, stdin=None)

    def section(self, done: cli_harness.Done, step: str) -> str:
        """What `step` printed: from after its header to the next header (or the verdict)."""
        start = done.stdout.index(head(step)) + len(head(step))
        end = done.stdout.find("\n\033[1", start)
        return done.stdout[start:end]

    def assertFailedWith(self, done: cli_harness.Done, step: str, lines: str) -> None:
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(lines, self.section(done, step), done.stdout)
        self.assertTrue(done.stdout.endswith(RED), done.stdout)

    # -- the whole run ---------------------------------------------------------------------------------------------

    def test_all_green_prints_every_step_in_order_and_exits_0(self) -> None:
        done = self.check()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("".join(head(s) + body for s, body in green_steps()) + GREEN, done.stdout)
        self.assertEqual("", done.stderr)

    def test_an_argument_changes_nothing(self) -> None:
        self.assertEqual(self.check().stdout, self.check("--help", "extra").stdout)

    def test_the_nix_evaluations_it_asks_and_where(self) -> None:
        self.check()
        drv = [["eval", f".#nixosConfigurations.{h}.config.system.build.toplevel.drvPath"] for h in HOSTS]
        placement = [["eval", "--raw", f".#nixosConfigurations.{h}.config.systemd.services", "--apply",
                      PLACEMENT_APPLY] for h in HOSTS]
        nix = os.path.join(self.infra, "bin/../nix")
        shipper = [q for h in HOSTS for q in (
            ["eval", f"{nix}#nixosConfigurations.{h}.config.fleet.logs.enable"],
            ["eval", "--json", f"{nix}#nixosConfigurations.{h}.config.fleet.autoApply.restartableUnits"])]
        origin = [["eval", "--raw", f".#nixosConfigurations.{h}.config.systemd.services", "--apply", ORIGIN_APPLY]
                  for h in HOSTS]
        self.assertEqual([FLAGS + c for c in drv + placement + shipper + origin], self.fakes.calls("nix"))
        with open(self.fakes.log) as f:
            cwds = {e["cwd"] for e in map(json.loads, f) if e["name"] in ("nix", "terraform")}
        self.assertEqual({self.infra}, cwds)

    def test_terraform_is_asked_from_infra(self) -> None:
        self.check()
        self.assertEqual([["fmt", "-check", "-recursive", "terraform"],
                          ["-chdir=terraform", "init", "-backend=false", "-input=false"],
                          ["-chdir=terraform", "validate"]], self.fakes.calls("terraform"))

    # -- nix itself ------------------------------------------------------------------------------------------------

    def test_no_nix_anywhere_fails_before_any_step(self) -> None:
        os.remove(os.path.join(self.fakes.bin, "nix"))
        done = self.check()
        self.assertEqual(1, done.code, done.output)
        self.assertEqual("", done.stdout)
        self.assertIn("nix not found on PATH or at " + self.path("no/nix"), done.stderr)
        self.assertTrue(done.stderr.endswith("\nFAILED -- the nix eval steps below cannot run\n"), done.stderr)
        self.assertEqual([], self.fakes.calls("terraform"))

    def test_nix_at_an_install_location_is_found_and_used(self) -> None:
        installed = self.path("installed/bin")
        os.makedirs(installed)
        os.rename(os.path.join(self.fakes.bin, "nix"), os.path.join(installed, "nix"))
        done = self.check(env={"NIX_PATH_CANDIDATES": os.path.join(installed, "nix")})
        self.assertEqual(0, done.code, done.output)
        self.assertIn(head("nix eval mongo-1") + "  ok  abc-nixos-system-mongo-1.drv\n", done.stdout)

    # -- terraform, plist, dashboard -------------------------------------------------------------------------------

    def test_terraform_fmt_drift_names_the_fix(self) -> None:
        self.answer("terraform", {"fmt": 3})
        self.assertFailedWith(self.check(), "terraform fmt",
                              "terraform/main.tf\n  FAILED -- run: terraform fmt -recursive infra/terraform\n")

    def test_terraform_init_failing_skips_validate(self) -> None:
        self.answer("terraform", {"init": 1})
        self.assertFailedWith(self.check(), "terraform validate", "  FAILED\n")
        self.assertNotIn(["-chdir=terraform", "validate"], self.fakes.calls("terraform"))

    def test_terraform_validate_failing(self) -> None:
        self.answer("terraform", {"validate": 1})
        self.assertFailedWith(self.check(), "terraform validate", "Success! The configuration is valid.\n  FAILED\n")

    def test_a_malformed_plist_is_named_beside_the_good_one(self) -> None:
        self.write("checkout/infra/version-dashboard/broken.plist", "<plist><!-- a -- b --></plist>")
        done = self.check()
        self.assertFailedWith(done, "plist syntax", "  ok  version-dashboard/agent.plist\n  FAILED version-dashboard/"
                              "broken.plist (XML forbids -- inside a comment; that is the usual cause)\n")
        self.assertIn("Traceback", done.stderr)

    def test_no_plist_prints_nothing_for_the_step(self) -> None:
        os.remove(os.path.join(self.infra, "version-dashboard/agent.plist"))
        done = self.check()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("", self.section(done, "plist syntax"))

    def test_dashboard_skipped_without_npm(self) -> None:
        os.remove(os.path.join(self.fakes.bin, "npm"))
        done = self.check()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("  skipped: no npm on PATH\n", self.section(done, "version-dashboard lint + tests"))

    def test_dashboard_dependencies_installed_when_missing(self) -> None:
        os.rmdir(os.path.join(self.infra, "version-dashboard/node_modules"))
        done = self.check()
        self.assertEqual("  ok  Tests  12 passed (12)\n", self.section(done, "version-dashboard lint + tests"))
        self.assertEqual([["ci", "--silent"], ["run", "--silent", "lint"], ["test", "--silent"]],
                         self.fakes.calls("npm"))

    def test_dashboard_skipped_when_npm_ci_fails(self) -> None:
        os.rmdir(os.path.join(self.infra, "version-dashboard/node_modules"))
        self.answer("npm", {"ci": 1})
        done = self.check()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual("  skipped: version-dashboard dependencies are not installed and npm ci failed\n",
                         self.section(done, "version-dashboard lint + tests"))

    def test_dashboard_lint_failure_shows_its_last_30_lines_and_skips_the_tests(self) -> None:
        self.answer("npm", {"lint": 1})
        expected = "".join(f"  lint error {i}\n" for i in range(10, 40))
        self.assertFailedWith(self.check(), "version-dashboard lint + tests", expected)
        self.assertNotIn(["test", "--silent"], self.fakes.calls("npm"))

    def test_dashboard_test_failure(self) -> None:
        self.answer("npm", {"test": 1})
        self.assertFailedWith(self.check(), "version-dashboard lint + tests",
                              "  lint clean\n   Test Files  3 passed (3)\n        Tests  12 passed (12)\n")

    # -- the test/ scripts -----------------------------------------------------------------------------------------

    def test_each_script_failing_prints_its_way(self) -> None:
        many = "".join(f"line {i}\n" for i in range(30))
        for script, _, _, _, _ in SCRIPTS:
            self.script(script, many, 1)
        done = self.check()
        self.assertEqual(1, done.code, done.output)
        lines = many.splitlines()
        for script, step, _, _, fail in SCRIPTS:
            with self.subTest(script=script):
                expected = {"echo": lines, "indent": ["  " + x for x in lines],
                            "tail25": ["  " + x for x in lines[-25:]]}[fail]
                self.assertEqual("".join(x + "\n" for x in expected), self.section(done, step))

    def test_each_missing_script_is_said_skipped_or_fails(self) -> None:
        for script, _, _, _, _ in SCRIPTS:
            os.remove(os.path.join(self.infra, "test", script))
        done = self.check()
        self.assertEqual(1, done.code, done.output)
        for script, step, _, missing, _ in SCRIPTS:
            with self.subTest(script=script):
                if missing == "said":
                    self.assertEqual(f"  FAILED test/{script} is missing\n", self.section(done, step))
                elif missing == "silent":
                    self.assertEqual("", self.section(done, step))
                else:
                    self.assertIn(f"test/{script}", self.section(done, step))

    def test_a_silent_script_still_prints_its_empty_line(self) -> None:
        silent = (("test_host_dns.sh", "  \n"), ("test_alert_rules.sh", "\n"), ("test_synthetic_probes.sh", "\n"),
                  ("test_fleet_hosts.py", "  ok  0 test(s)\n"), ("test_deploy.py", "  [spec] deploy logic passes\n"))
        for script, _ in silent:
            self.script(script, "")
        done = self.check()
        for script, out in silent:
            with self.subTest(script=script):
                step = [s for name, s, _, _, _ in SCRIPTS if name == script][0]
                self.assertEqual(out, self.section(done, step))

    def test_scripts_run_from_infra(self) -> None:
        self.write("checkout/infra/test/test_operator_keys.sh", 'echo "in $(pwd)"\n')
        self.assertEqual(f"  in {self.infra}\n", self.section(self.check(), "operator keys"))

    # -- the nix-evaluated checks ----------------------------------------------------------------------------------

    def test_a_host_that_does_not_evaluate(self) -> None:
        self.answer("nix", {"monitoring-1 drv": ["", 1], "monitoring-1 placement": ["", 1],
                            "monitoring-1 origin": ["", 1]})
        done = self.check()
        self.assertFailedWith(done, "nix eval monitoring-1", "  error: monitoring-1 drv does not evaluate\n")
        self.assertEqual("  FAILED could not evaluate monitoring-1 (see its nix eval step above)\n"
                         "  ok  no StartLimit* directives are hidden in a [Service] section\n",
                         self.section(done, "systemd key placement"))
        self.assertEqual("  FAILED could not evaluate: monitoring-1\n"
                         "         The nix eval steps above say why. Until every host evaluates, this check cannot\n"
                         "         know which of them would initiate the replica set.\n"
                         "  ok  only mongo-1 may initiate the replica set\n", self.section(done, "replica-set origin"))

    def test_nix_eval_failure_shows_its_last_20_lines(self) -> None:
        self.answer("nix", {"k3s-worker-1 drv": ["".join(f"trace {i}\n" for i in range(25)), 1]})
        expected = "".join(f"  trace {i}\n" for i in range(6, 25)) + "  error: k3s-worker-1 drv does not evaluate\n"
        self.assertFailedWith(self.check(), "nix eval k3s-worker-1", expected)

    def test_start_limit_under_service_config(self) -> None:
        self.answer("nix", {"mongo-1 placement": ["mongodb-exporter backup\n", 0], "k3s-worker-1 placement": ["k3s", 0]})
        self.assertFailedWith(self.check(), "systemd key placement",
                              "  FAILED StartLimit* under serviceConfig, where systemd ignores it:\n"
                              "           mongo-1: mongodb-exporter backup\n           k3s-worker-1: k3s\n"
                              "         Move them to the unit level (beside wantedBy), not into serviceConfig.\n")

    def test_a_shipping_host_refusing_a_vector_restart(self) -> None:
        self.answer("nix", {"mongo-1 accepts": ['["sshd.service"]', 0], "k3s-worker-1 accepts": ["", 1],
                            "monitoring-1 logs": ["false", 0]})
        self.assertFailedWith(self.check(), "a log shipper may be restarted unattended",
                              "  FAILED these hosts ship logs but would refuse the switch that changes the shipper:\n"
                              "         mongo-1\n         k3s-worker-1\n")
        self.assertNotIn(FLAGS + ["eval", "--json", os.path.join(self.infra, "bin/../nix") +
                                  "#nixosConfigurations.monitoring-1.config.fleet.autoApply.restartableUnits"],
                         self.fakes.calls("nix"))

    def test_hosts_not_shipping_or_unreadable_are_not_asked(self) -> None:
        self.answer("nix", {h + " logs": ["", 1] for h in HOSTS})
        done = self.check()
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(4 * len(HOSTS), len(self.fakes.calls("nix")))  # drv, placement, logs, origin

    def test_no_replica_set_origin(self) -> None:
        self.answer("nix", {"mongo-1 origin": ["no", 0]})
        self.assertFailedWith(self.check(), "replica-set origin",
                              "  FAILED no host would initiate the replica set.\n"
                              "         A fleet built from scratch would come up with mongod running and no set at "
                              "all,\n         so the app's change streams -- which need an oplog -- would never "
                              "work.\n         Exactly one mongo host needs fleet.mongodb.initiateReplicaSet = true.\n")

    def test_two_replica_set_origins(self) -> None:
        self.answer("nix", {"k3s-worker-1 origin": ["yes\n", 0]})
        self.assertFailedWith(self.check(), "replica-set origin",
                              "  FAILED 2 hosts would each CREATE their own replica set:\n"
                              "           mongo-1\n           k3s-worker-1\n"
                              "         Set fleet.mongodb.initiateReplicaSet = false on every host that JOINS an "
                              "existing\n         set and let the current primary add it -- see\n"
                              "         infra/ansible/playbooks/migrate-mongo-replica.yml.\n")

    def test_failures_add_up_to_one_verdict(self) -> None:
        self.answer("terraform", {"fmt": 1})
        self.script("test_deploy.py", "boom\n", 2)
        done = self.check()
        self.assertEqual(1, done.code)
        self.assertEqual(1, done.stdout.count("infra/bin/check: FAILED"))
        self.assertIn(head("replica-set origin") + "  ok  only mongo-1 may initiate the replica set\n" + RED,
                      done.stdout)


PLIST = """<?xml version="1.0" encoding="UTF-8"?>
<plist version="1.0"><dict><key>Label</key><string>agent</string></dict></plist>
"""


if __name__ == "__main__":
    unittest.main()
