#!/usr/bin/env python3
"""Tests for bin/deploy's pure logic: the stale-tree verdict, the ssh target choice, the probe and
dry-activate parsing, the preview and the post-switch verdict, the LAUNCH / SWITCH_UNIT / SWITCH_STATUS /
VERIFY_STATES remote scripts under stub sudo, systemd-run and systemctl, following a detached switch
across dropped sessions, and --apply end to end against a fake git, nix and ssh. No network, no nix, no host.

    python3 infra/test/test_deploy.py   (also run by infra/bin/check and nix-stage-closures.yml)
"""
import importlib.machinery
import importlib.util
import os
import unittest
from unittest import mock

_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "bin", "deploy")
_loader = importlib.machinery.SourceFileLoader("deploy_tool", _path)
_spec = importlib.util.spec_from_loader("deploy_tool", _loader)
deploy = importlib.util.module_from_spec(_spec)
_loader.exec_module(deploy)

# Shaped like a real switch-to-configuration-ng dry-activate (nixpkgs 26.05) on ci-runner.
DRY = """Not checking switch inhibitors (action = dry-activate)
would stop the following units: old-exporter.service
would NOT restart the following changed units: gitea-runner-ubuntu.service, nixos-auto-apply.service
would restart the following units: forgejo-runner-drain.service, vswitch-watchdog.timer
would start the following units: ci-docker-hygiene.timer
would reload the following units: dbus-broker.service
would restart the following user units: foo.service
would activate the configuration...
sops-install-secrets: Imported /etc/ssh/ssh_host_ed25519_key as age key with fingerprint age1xyz
would add secret: alertmanager/smtp-password
would restart systemd"""


class Staleness(unittest.TestCase):
    def test_behind_main_is_refused(self):
        ok, message = deploy.staleness_verdict(behind=8, ahead=0, dirty=False)
        self.assertFalse(ok)
        self.assertIn("8 commit(s) behind", message)
        self.assertIn("roll the host back", message)

    def test_behind_and_ahead_is_still_refused(self):
        self.assertFalse(deploy.staleness_verdict(behind=1, ahead=3, dirty=True)[0])

    def test_at_main_is_fine(self):
        self.assertEqual(deploy.staleness_verdict(0, 0, False), (True, "checkout: at origin main"))

    def test_ahead_and_dirty_are_allowed_and_said(self):
        ok, message = deploy.staleness_verdict(0, 2, True)
        self.assertTrue(ok)
        self.assertIn("2 commit(s) ahead", message)
        self.assertIn("-dirty", message)


class Target(unittest.TestCase):
    def test_an_alias_named_like_the_host_wins_over_the_restricted_bare_ip(self):
        # monitoring-1: the bare IP binds the dashboard's forced-command key.
        self.assertEqual(deploy.choose_target("monitoring-1", "128.140.49.167", "128.140.49.167"),
                         "monitoring-1")

    def test_an_alias_pointing_at_a_stale_address_is_refused(self):
        with self.assertRaises(deploy.Refused):
            deploy.choose_target("monitoring-1", "128.140.49.167", "10.9.9.9")

    def test_no_alias_uses_the_flake_address(self):
        # `ssh -G name` echoes the name back as hostname when no Host block matches.
        self.assertEqual(deploy.choose_target("dev-postgres", "10.0.0.12", "dev-postgres"), "10.0.0.12")
        self.assertEqual(deploy.choose_target("dev-postgres", "10.0.0.12", "dev-postgres", "pawel"),
                         "pawel@10.0.0.12")

    def test_no_alias_and_no_address_cannot_be_reached(self):
        with self.assertRaises(deploy.Unmeasured):
            deploy.choose_target("ghost", "", "ghost")


class Candidate(unittest.TestCase):
    def test_this_checkouts_closure_wins_when_present(self):
        self.assertEqual(deploy.choose_candidate("/e", True, "/p", "r" * 40, True)[0], "/e")

    def test_the_pin_is_used_when_its_tree_is_identical(self):
        # movies, 2026-10-23: HEAD a35c783 touched no infra file, so CI never staged its closure;
        # the pin (ccca772) is the same configuration.
        got = deploy.choose_candidate("/e", False, "/p", "c" * 40, True)
        self.assertEqual(got[0], "/p")
        self.assertIn("same flake tree", got[1])

    def test_a_pin_from_a_different_tree_is_never_used(self):
        self.assertIsNone(deploy.choose_candidate("/e", False, "/p", "c" * 40, False))
        self.assertIsNone(deploy.choose_candidate("/e", False, "/p", "", True))
        self.assertIsNone(deploy.choose_candidate("/e", False, "", "", True))

    def test_an_unreadable_revision_is_not_the_same_tree(self):
        self.assertFalse(deploy.same_flake_tree("unknown"))
        self.assertFalse(deploy.same_flake_tree(""))


class DryActivateParsing(unittest.TestCase):
    def setUp(self):
        self.p = deploy.parse_dry_activate(DRY)

    def test_every_kind_lands_in_its_bucket(self):
        self.assertEqual(self.p.stop, ["old-exporter.service"])
        self.assertEqual(self.p.restart, ["forgejo-runner-drain.service", "vswitch-watchdog.timer"])
        self.assertEqual(self.p.start, ["ci-docker-hygiene.timer"])
        self.assertEqual(self.p.reload, ["dbus-broker.service"])
        self.assertEqual(self.p.skipped, ["gitea-runner-ubuntu.service", "nixos-auto-apply.service"])

    def test_runner_units_changed_but_not_restarted_are_drains(self):
        self.assertEqual(self.p.drains, ["gitea-runner-ubuntu.service"])

    def test_user_units_are_kept_apart(self):
        self.assertEqual(self.p.user, ["restart foo.service"])

    def test_noise_is_dropped_and_everything_else_is_shown(self):
        # A line the preview does not understand must be SHOWN, never silently dropped.
        self.assertEqual(self.p.other, ["would add secret: alertmanager/smtp-password",
                                        "would restart systemd"])

    def test_a_no_op_is_empty(self):
        p = deploy.parse_dry_activate("Not checking switch inhibitors (action = dry-activate)\n"
                                      "would activate the configuration...")
        self.assertEqual(p, deploy.Preview([], [], [], [], [], [], [], []))


PROBE_OUT = """user=pawel
hostname=ci-runner
running=/nix/store/aaa-nixos-system
booted=/nix/store/bbb-nixos-system
pinned=/nix/store/ccc-nixos-system
running_rev=abc
failed=foo.service
candidate_present=1
candidate_rev=def
boot_kernel=same
boot_initrd=differs
boot_kernel-modules=same
boot_systemd=same
@@ DRY rc=0
would restart the following units: vswitch-watchdog.timer
@@ DRYEND
state vswitch-watchdog.timer active
unitfile idle-oneshot.service
unitfile vswitch-watchdog.timer
unitfile multi-user.target.wants
@@ END
"""


class Probe(unittest.TestCase):
    def test_facts_dry_text_and_states(self):
        f = deploy.parse_probe(PROBE_OUT)
        self.assertEqual(f["running"], "/nix/store/aaa-nixos-system")
        self.assertEqual(f["dry_rc"], 0)
        self.assertEqual(f["dry"], "would restart the following units: vswitch-watchdog.timer")
        self.assertEqual(f["states"], {"vswitch-watchdog.timer": "active"})
        self.assertEqual(deploy.boot_changes(f), ["initrd"])

    def test_a_reply_without_the_end_marker_is_not_the_host(self):
        # What the restricted bare-IP key answers: a dashboard reading, exit 0, no marker.
        original = deploy.run
        deploy.run = lambda *a, **k: type("R", (), {"stdout": "dashboard: ok\n", "returncode": 0})()
        try:
            with self.assertRaises(deploy.Unmeasured):
                deploy.remote("128.140.49.167", deploy.PROBE)
        finally:
            deploy.run = original


class RunnersAJobCanLose(unittest.TestCase):
    """A switch that STOPS CI runner units cuts off the jobs they run: 2026-11-09 22:58Z, a nixpkgs bump on ci-runner
    reactivated the sysinit units, and every runner unit -- each Requires sysinit.target, the movies ones docker too --
    stopped mid-job, X-RestartIfChanged=false or not (Android #443, Tool self-tests #181, Landing check build cache
    #248: "The self-hosted runner lost communication with the server"). The preview names them, and --apply refuses
    while one is busy."""

    SYSINIT = """would NOT restart the following changed units: movies-runner@3.service, github-runner-bitcashier-gh-builder-1.service
would restart the following units: sysinit-reactivation.target, vswitch-watchdog.timer"""

    def facts(self, runners):
        return {"states": {}, "runners": runners}

    def test_ci_runners_changed_but_not_restarted_drain(self):
        p = deploy.parse_dry_activate(self.SYSINIT)
        self.assertEqual(p.drains, ["movies-runner@3.service", "github-runner-bitcashier-gh-builder-1.service"])

    def test_a_sysinit_reactivation_stops_every_active_runner(self):
        p = deploy.parse_dry_activate(self.SYSINIT)
        stopped = deploy.runners_stopped(p, self.facts("movies-runner@3.service github-runner-bitcashier-gh-builder-1.service"))
        self.assertEqual(stopped, ["github-runner-bitcashier-gh-builder-1.service", "movies-runner@3.service"])

    def test_a_runner_the_switch_restarts_is_stopped_too(self):
        p = deploy.parse_dry_activate("would restart the following units: movies-runner@5.service")
        self.assertEqual(deploy.runners_stopped(p, self.facts("")), ["movies-runner@5.service"])

    def test_a_switch_that_leaves_the_runners_alone_stops_none(self):
        p = deploy.parse_dry_activate("would restart the following units: vswitch-watchdog.timer")
        self.assertEqual(deploy.runners_stopped(p, self.facts("movies-runner@3.service")), [])

    def test_apply_refuses_while_a_runner_it_would_stop_is_busy_and_says_which(self):
        p = deploy.parse_dry_activate(self.SYSINIT)
        why = deploy.runner_stop_refusal(p, self.facts("movies-runner@3.service"), allowed=False)
        self.assertIn("movies-runner@3.service", why)
        self.assertIn("--allow-runner-stop", why)
        self.assertIsNone(deploy.runner_stop_refusal(p, self.facts("movies-runner@3.service"), allowed=True))
        self.assertIsNone(deploy.runner_stop_refusal(p, self.facts(""), allowed=False))

    def test_the_preview_names_the_runners_it_would_stop(self):
        facts = dict(deploy.parse_probe(PROBE_OUT), runners="movies-runner@3.service")
        text = deploy.render_preview("ci-runner", "pawel@10.1.0.3", facts, deploy.parse_dry_activate(self.SYSINIT),
                                     "/nix/store/ddd", "def", "checkout: at origin main", "bootstrap")
        self.assertIn("runners STOPPED          movies-runner@3.service", text)

    def test_the_probe_reads_the_active_runners(self):
        self.assertEqual(deploy.parse_probe("runners=movies-runner@3.service \n@@ END\n")["runners"], "movies-runner@3.service ")
        self.assertIn("runners=", deploy.PROBE)


class Rendering(unittest.TestCase):
    def test_the_preview_names_drains_and_the_reboot(self):
        facts = deploy.parse_probe(PROBE_OUT)
        text = deploy.render_preview("ci-runner", "pawel@10.1.0.3", facts,
                                     deploy.parse_dry_activate(DRY), "/nix/store/ddd", "def",
                                     "checkout: at origin main", "bootstrap")
        self.assertIn("runner drains           gitea-runner-ubuntu.service", text)
        self.assertIn("finishes its running jobs", text)
        self.assertIn("vswitch-watchdog.timer (active)", text)
        self.assertIn("CHANGES initrd -> a REBOOT is needed", text)
        self.assertIn("CI's pin differs", text)
        self.assertIn("already failed before the switch: foo.service", text)
        # Idle units dry-activate does not mention are still shown; acted-on ones are not repeated.
        self.assertIn("unit files changed, unit idle  idle-oneshot.service  (", text)

    def test_a_stop_plus_start_is_shown_as_one_restart(self):
        p = deploy.parse_dry_activate("would stop the following units: systemd-sysctl.service\n"
                                      "would start the following units: systemd-sysctl.service")
        text = deploy.render_preview("h", "h", {"running": "/a"}, p, "/b", "", "", "")
        self.assertIn("restart   systemd-sysctl.service [stop+start]", text)
        self.assertIn("start     -", text)

    def test_nothing_to_do(self):
        facts = {"running": "/nix/store/x"}
        self.assertIn("NOTHING TO DO", deploy.render_preview("h", "h", facts, None, "/nix/store/x",
                                                             "", "", ""))


class Verify(unittest.TestCase):
    def test_a_clean_switch_passes(self):
        self.assertEqual(deploy.verify_verdict(
            "/nix/store/new", "/nix/store/new", "rev1", "rev1", {"old.service"}, {"old.service"},
            {"a.timer": ["active", "success", ""],
             "setup.service": ["inactive", "success", "oneshot"]}), [])

    def test_each_failure_is_named(self):
        problems = deploy.verify_verdict(
            "/nix/store/new", "/nix/store/old", "rev1", "rev0", set(), {"x.service"},
            {"svc.service": ["failed", "exit-code", "simple"]})
        self.assertEqual(len(problems), 4, problems)
        self.assertIn("not /nix/store/new", problems[0])
        self.assertIn("expected rev1", problems[1])
        self.assertIn("x.service", problems[2])
        self.assertIn("svc.service", problems[3])

    def test_unstamped_hosts_skip_the_revision(self):
        self.assertEqual(deploy.verify_verdict("/p", "/p", "", "", set(), set(), {}), [])


class ApplyScript(unittest.TestCase):
    """The LAUNCH, SWITCH_UNIT and VERIFY_STATES scripts themselves, under stub sudo / systemd-run /
    systemctl and a stub candidate closure. The systemd-run stub, like the real one, can hand its
    transient unit systemd's own PATH and not the login shell's: a bare `nix-env` is not found there,
    and every --apply exited 127 with the host unchanged (mongo-1, 2026-10-24)."""

    def setUp(self):
        import shutil, tempfile
        work = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, work)
        self.work, self.stubs = work, os.path.join(work, "bin")
        self.cand, self.log = os.path.join(work, "system"), os.path.join(work, "log")
        self.status = os.path.join(work, "switch.status")
        for d in (self.stubs, os.path.join(self.cand, "bin"), os.path.join(self.cand, "sw", "bin")):
            os.makedirs(d)
        self.stub(os.path.join(self.stubs, "sudo"), '[ "$1" = -n ] && shift; exec "$@"')

    def stub(self, path, body):
        with open(path, "w") as f:
            f.write("#!/bin/sh\n" + body + "\n")
        os.chmod(path, 0o755)

    def run_script(self, script, *args, path=None):
        import subprocess
        return subprocess.run(["/bin/sh", "-c", script, "script", *args], capture_output=True, text=True,
                              env={"PATH": path or self.stubs + ":/usr/bin:/bin", "BIN_DEPLOY_DIR": self.work})

    def logged(self):
        with open(self.log) as f:
            return f.read().splitlines()

    def switch_status(self):
        with open(self.status) as f:
            return f.read().strip()

    def test_the_switch_finds_nix_env_without_the_login_path(self):
        # What the transient unit runs, with systemd's bare PATH: nix-env by the candidate's path.
        self.stub(os.path.join(self.cand, "sw", "bin", "nix-env"), 'echo "nix-env $*" >> ' + self.log)
        self.stub(os.path.join(self.cand, "bin", "switch-to-configuration"), 'echo "switch $*" >> ' + self.log)

        out = self.run_script(deploy.SWITCH_UNIT, self.cand, self.status, path="/usr/bin:/bin")
        self.assertEqual(out.returncode, 0, out.stdout + out.stderr)
        self.assertEqual(self.switch_status(), "0")
        self.assertEqual(self.logged(), ["nix-env --profile /nix/var/nix/profiles/system --set " + self.cand,
                                         "switch switch"])

    def test_a_failed_switch_leaves_its_exit_status(self):
        self.stub(os.path.join(self.cand, "sw", "bin", "nix-env"), "exit 0")
        self.stub(os.path.join(self.cand, "bin", "switch-to-configuration"), "exit 4")
        out = self.run_script(deploy.SWITCH_UNIT, self.cand, self.status)
        self.assertEqual(out.returncode, 4)
        self.assertEqual(self.switch_status(), "4")

    def test_the_launch_starts_the_switch_detached_and_returns(self):
        # systemd-run without --wait or --pipe: nothing of the switch rides on the ssh session, which
        # the switch may kill by restarting sshd / systemd-networkd (monitoring-1, 2026-11-10). The stub
        # records its options and then runs the unit's command as systemd would, with systemd's PATH.
        self.stub(os.path.join(self.stubs, "systemd-run"),
                  'echo "systemd-run $*" >> ' + self.log + '\n'
                  'while [ "$1" != -- ]; do shift; done; shift; env -i PATH=/usr/bin:/bin "$@" >/dev/null')
        self.stub(os.path.join(self.cand, "sw", "bin", "nix-env"), "exit 0")
        self.stub(os.path.join(self.cand, "bin", "switch-to-configuration"), 'echo "switch $*" >> ' + self.log)

        out = self.run_script(deploy.LAUNCH, self.cand, "deploy-switch-x")
        self.assertIn("@@ LAUNCHED rc=0", out.stdout, out.stdout + out.stderr)
        run_line, switched = self.logged()
        self.assertIn("--unit deploy-switch-x", run_line)
        self.assertNotIn("--wait", run_line)
        self.assertNotIn("--pipe", run_line)
        self.assertEqual(switched, "switch switch")
        with open(os.path.join(self.work, "deploy-switch-x.status")) as f:
            self.assertEqual(f.read().strip(), "0")

    def test_the_launch_refuses_a_candidate_not_on_the_host(self):
        out = self.run_script(deploy.LAUNCH, os.path.join(self.work, "absent"), "deploy-switch-x")
        self.assertIn("is not on this host", out.stdout)
        self.assertNotIn("@@ LAUNCHED", out.stdout)

    def test_the_status_reads_the_exit_status_and_the_journal(self):
        self.stub(os.path.join(self.stubs, "systemctl"), "echo inactive")
        self.stub(os.path.join(self.stubs, "journalctl"), 'printf "restarting sshd.service\\nFailed to start foo.service\\n"')
        with open(os.path.join(self.work, "deploy-switch-x.status"), "w") as f:
            f.write("4\n")
        out = self.run_script(deploy.SWITCH_STATUS, "deploy-switch-x")
        facts = deploy.parse_switch_status(out.stdout)
        self.assertEqual(facts["status"], 4, out.stdout + out.stderr)
        self.assertEqual(facts["state"], "inactive")
        self.assertEqual(facts["journal"], ["restarting sshd.service", "Failed to start foo.service"])

    def lock_held_for(self, failures):
        """A switch-to-configuration that exits 11, "Could not acquire lock", `failures` times."""
        self.stub(os.path.join(self.stubs, "sleep"), "exit 0")
        self.stub(os.path.join(self.cand, "sw", "bin", "nix-env"), "exit 0")
        self.stub(os.path.join(self.cand, "bin", "switch-to-configuration"),
                  'echo switch >> ' + self.log + '; [ "$(wc -l < ' + self.log + ')" -gt ' + str(failures)
                  + ' ] && exit 0; echo "Could not acquire lock" >&2; exit 11')

    def test_a_switch_lock_held_by_the_applier_is_waited_out(self):
        # Writing the pin wakes nixos-auto-apply, which holds switch-to-configuration's lock for a
        # second while it judges the pin: the deploy's own switch exited 11, "Could not acquire lock".
        self.lock_held_for(1)
        out = self.run_script(deploy.SWITCH_UNIT, self.cand, self.status)
        self.assertEqual(self.switch_status(), "0", out.stdout + out.stderr)
        self.assertEqual(self.logged(), ["switch", "switch"])

    def test_a_lock_that_stays_held_is_reported_not_retried_for_ever(self):
        # The retry waits out a moment's lock; one held past it is the switch's own exit code, for
        # the verdict to judge, never a success.
        self.lock_held_for(1000)
        out = self.run_script(deploy.SWITCH_UNIT, self.cand, self.status)
        self.assertEqual(self.switch_status(), "11", out.stdout + out.stderr)
        self.assertEqual(len(self.logged()), 21)

    def test_the_unit_states_are_read_in_the_order_the_verdict_expects(self):
        # `systemctl show -p A,B,C --value` prints systemd's own property order (ActiveState, Type,
        # Result), not the order asked for: a oneshot that had exited cleanly read as unhealthy.
        self.stub(os.path.join(self.stubs, "systemctl"),
                  'case "$3" in\n'
                  '  ActiveState) echo inactive ;; Result) echo success ;; Type) echo oneshot ;;\n'
                  '  *) printf "inactive\\noneshot\\nsuccess\\n" ;;\n'
                  'esac')
        out = self.run_script(deploy.VERIFY_STATES, "systemd-fsck-root.service")
        line = next(l for l in out.stdout.splitlines() if l.startswith("unit "))
        self.assertTrue(deploy.unit_ok(line.split()[2:]), line)


class FollowSwitch(unittest.TestCase):
    """follow_switch against a scripted host: each reply is a SWITCH_STATUS answer or a dropped ssh."""

    def follow(self, replies, **bounds):
        import contextlib, io
        clock = {"t": 0.0}
        calls = []

        def fake_remote(target, script, args=(), timeout=0):
            calls.append(args)
            reply = replies.pop(0) if replies else replies_tail
            if reply is None:
                raise deploy.Unmeasured(f"{target} did not answer as a shell (no end marker)")
            return reply

        replies_tail = replies[-1] if replies else None
        sleep = lambda s: clock.__setitem__("t", clock["t"] + s)
        out = io.StringIO()
        with mock.patch.object(deploy, "remote", fake_remote), contextlib.redirect_stdout(out):
            outcome = deploy.follow_switch("monitoring-1", "deploy-switch-x", clock=lambda: clock["t"],
                                           sleep=sleep, **bounds)
        return outcome, out.getvalue()

    @staticmethod
    def status(state, journal=(), status=None):
        return (f"state={state}\n" + (f"status={status}\n" if status is not None else "")
                + "now=/nix/store/new\n@@ JOURNAL\n" + "".join(l + "\n" for l in journal) + "@@ END\n")

    def test_the_host_dropping_mid_switch_is_waited_out_and_the_result_read(self):
        outcome, out = self.follow([self.status("active", ["starting the following units: nix-daemon.service"]),
                                    None, None, None,
                                    self.status("inactive", ["starting the following units: nix-daemon.service",
                                                             "the following new units were started: foo.service"], 0)])
        self.assertEqual(outcome.status, 0)
        self.assertEqual(out.count("not answering"), 3)
        # Every journal line printed once, across the reconnect.
        self.assertEqual(out.count("starting the following units"), 1, out)
        self.assertIn("new units were started", out)
        self.assertIsNone(deploy.switch_problem("deploy-switch-x", outcome))

    def test_a_failed_switch_returns_its_status_and_journal(self):
        outcome, _ = self.follow([self.status("failed", ["Failed to start foo.service"], 4)])
        self.assertEqual(outcome, deploy.SwitchOutcome(4, ["Failed to start foo.service"]))
        self.assertIn("exited 4", deploy.switch_problem("deploy-switch-x", outcome))

    def test_a_unit_gone_without_a_status_is_a_failure(self):
        outcome, _ = self.follow([self.status("inactive", ["Killed"])])
        self.assertIsNone(outcome.status)
        self.assertIn("without an exit status", deploy.switch_problem("deploy-switch-x", outcome))

    def test_an_unreachable_host_is_bounded_and_says_so(self):
        with self.assertRaisesRegex(deploy.Unmeasured, "unreachable for over 60s"):
            self.follow([None], unreachable=60, every=5)

    def test_a_switch_that_never_ends_is_bounded(self):
        with self.assertRaisesRegex(deploy.Unmeasured, "had not ended after 30s"):
            self.follow([self.status("active")], deadline=30, every=5)


class ApplyEndToEnd(unittest.TestCase):
    """`deploy monitoring-1 --apply` against a fake git, nix and ssh: the host's switch restarts sshd and
    systemd-networkd, so the ssh session open while it runs DIES (and the next few cannot connect),
    while the switch itself completes -- monitoring-1, 2026-11-10 ~07:00Z: the output stopped mid
    "starting the following units: ...", the tool hung ~22 min and exited 2; the host had switched."""

    OLD, CAND, REV = "/nix/store/old-nixos-system", "/nix/store/new-nixos-system", "a" * 40

    def setUp(self):
        self.running, self.failed = self.OLD, ""
        self.ssh_argvs, self.unreachable, self.switched = [], 0, False
        self.switch_status, self.journal = 0, ["starting the following units: nix-daemon.service, sshd.service",
                                               "the following new units were started: sshd.service"]

    def probe(self, cand):
        out = [f"user=root", "hostname=monitoring-1", f"running={self.running}", f"booted={self.OLD}",
               f"pinned={self.CAND if self.switched else self.OLD}", f"running_rev={self.REV if self.running == self.CAND else 'b' * 40}",
               f"failed={self.failed}", "runners="]
        if cand:
            out += ["candidate_present=1", f"candidate_rev={self.REV}"] + [f"boot_{c}=same" for c in deploy.BOOT_CHAIN]
            out += ["@@ DRY rc=0", "would restart the following units: sshd.service, systemd-networkd.service",
                    "@@ DRYEND", "state sshd.service active", "state systemd-networkd.service active"]
        return "\n".join(out) + "\n@@ END\n"

    def switch(self):
        """The host's switch, run to completion whatever happens to the session that started it."""
        self.switched, self.running = True, self.CAND
        if self.switch_status:
            self.failed = "foo.service"
        self.unreachable = 3  # sshd and the network restarting: the next connections fail

    def ssh(self, argv, script):
        import shlex
        self.ssh_argvs.append(argv)
        args = shlex.split(argv[-1])[3:]
        if self.unreachable:
            self.unreachable -= 1
            return "", 255
        if script == deploy.PROBE:
            return self.probe(args[0] if args else ""), 0
        if script == getattr(deploy, "LAUNCH", None):
            self.switch()  # detached: it carries on without this session
            return f"· pinned {self.CAND}\n@@ LAUNCHED rc=0\n@@ END\n", 0
        if script == getattr(deploy, "SWITCH_STATUS", None):
            return (f"state=inactive\nstatus={self.switch_status}\nnow={self.running}\n@@ JOURNAL\n"
                    + "".join(l + "\n" for l in self.journal) + "@@ END\n"), 0
        if script == getattr(deploy, "APPLY", None):
            # Before the fix the switch streamed through this session: it dies mid-line.
            self.switch()
            return f"· pinned {self.CAND}\nstarting the following units: nix-daemon.service, ss", 255
        if script == deploy.VERIFY_STATES:
            return "".join(f"unit {u} active success notify\n" for u in args) + "@@ END\n", 0
        raise AssertionError(f"unexpected remote script: {script[:80]!r}")

    def fake_run(self, argv, cwd=None, input=None, text=True, timeout=None, capture_output=True):
        import subprocess
        def done(out, rc=0):
            return subprocess.CompletedProcess(argv, rc, out, "")
        if argv[0] == "ssh" and argv[1] == "-G":
            return done("hostname 203.0.113.7\n")
        if argv[0] == "ssh":
            out, rc = self.ssh(argv, input)
            return done(out, rc)
        if argv[0] == "git" and argv[1:2] == ["rev-list"]:
            return done("0\n")
        if argv[0] == "git" and argv[1:2] == ["rev-parse"]:
            return done(deploy.REPO + "\n" if "--show-toplevel" in argv else "false\n")
        if argv[0] == "git":
            return done("")
        if argv[:2] == ["nix", "eval"]:
            attr = argv[-1].split("#", 1)[1]
            return {"nixosConfigurations.monitoring-1.config.fleet.publicAddress": done("203.0.113.7"),
                    "nixosConfigurations.monitoring-1.config.system.build.toplevel.outPath": done(self.CAND),
                    "nixosConfigurations.monitoring-1.config.system.configurationRevision": done(self.REV),
                    }.get(attr, done("", 1))
        raise AssertionError(f"unexpected command: {argv}")

    def apply(self):
        import contextlib, io, types
        test = self

        class FakePopen:  # the streamed switch of the code before the fix
            def __init__(self, argv, **_):
                self.argv, self.stdin, self.returncode = argv, io.StringIO(), None
                self.stdin.close = lambda: None

            @property
            def stdout(self):
                out, self.returncode = test.ssh(self.argv, self.stdin.getvalue())
                return iter(out.splitlines(keepends=True))

            def wait(self, timeout=None):
                return self.returncode

        fake = types.SimpleNamespace(run=self.fake_run, Popen=FakePopen, PIPE=-1, STDOUT=-2,
                                     TimeoutExpired=TimeoutError, CompletedProcess=None)
        out, err = io.StringIO(), io.StringIO()
        with mock.patch.object(deploy, "subprocess", fake), mock.patch.object(deploy.time, "sleep"), \
                contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
            rc = deploy.main(["monitoring-1", "--apply"])
        return rc, out.getvalue() + err.getvalue()

    def test_a_switch_that_restarts_sshd_under_the_session_is_read_back_as_a_success(self):
        rc, out = self.apply()
        self.assertEqual(rc, 0, out)
        self.assertIn(f"verified: monitoring-1 runs {self.CAND} at {self.REV}", out)
        self.assertIn("the following new units were started: sshd.service", out)

    def test_a_switch_that_genuinely_fails_says_so_with_its_journal(self):
        self.switch_status = 4
        self.journal = ["starting the following units: foo.service", "Job for foo.service failed"]
        rc, out = self.apply()
        self.assertEqual(rc, 1, out)
        self.assertIn("VERIFY FAILED", out)
        self.assertIn("exited 4", out)
        self.assertIn("Job for foo.service failed", out)
        self.assertIn("newly failed units: foo.service", out)

    def test_every_ssh_notices_a_dead_session_in_about_a_minute(self):
        # Without keepalives the dropped session of 2026-11-10 hung for ~22 minutes.
        self.apply()
        sessions = [a for a in self.ssh_argvs if a[1] != "-G"]
        self.assertTrue(sessions)
        for argv in sessions:
            self.assertIn("ServerAliveInterval=15", argv)
            self.assertIn("ServerAliveCountMax=4", argv)


if __name__ == "__main__":
    unittest.main(verbosity=1)
