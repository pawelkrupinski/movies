#!/usr/bin/env python3
"""Tests for bin/deploy's pure logic: the stale-tree verdict, the ssh target choice, the probe and
dry-activate parsing, the preview and the post-switch verdict. No network, no nix, no host.

    python3 infra/test/test_deploy.py   (also run by infra/bin/check and nix-stage-closures.yml)
"""
import importlib.machinery
import importlib.util
import os
import unittest

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
        self.assertEqual(deploy.staleness_verdict(0, 0, False), (True, "checkout: at forgejo main"))

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


class Rendering(unittest.TestCase):
    def test_the_preview_names_drains_and_the_reboot(self):
        facts = deploy.parse_probe(PROBE_OUT)
        text = deploy.render_preview("ci-runner", "pawel@10.1.0.3", facts,
                                     deploy.parse_dry_activate(DRY), "/nix/store/ddd", "def",
                                     "checkout: at forgejo main", "bootstrap")
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
    """The APPLY script itself, run under a systemd-run that, like the real one, hands its transient
    unit systemd's own PATH and not the login shell's: a bare `nix-env` is not found there, and every
    --apply exited 127 with the host unchanged (mongo-1, 2026-10-24)."""

    def test_the_switch_finds_nix_env_without_the_login_path(self):
        import shutil, subprocess, tempfile
        work = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, work)
        stubs, cand, log = os.path.join(work, "bin"), os.path.join(work, "system"), os.path.join(work, "log")
        os.makedirs(stubs); os.makedirs(os.path.join(cand, "bin")); os.makedirs(os.path.join(cand, "sw", "bin"))

        def script(path, body):
            with open(path, "w") as f:
                f.write("#!/bin/sh\n" + body + "\n")
            os.chmod(path, 0o755)

        script(os.path.join(stubs, "sudo"), '[ "$1" = -n ] && shift; exec "$@"')
        # systemd-run: skip its options up to `--`, then run the command with systemd's bare PATH.
        script(os.path.join(stubs, "systemd-run"),
               'while [ "$1" != -- ]; do shift; done; shift; exec env -i PATH=/usr/bin:/bin "$@"')
        script(os.path.join(cand, "sw", "bin", "nix-env"), 'echo "nix-env $*" >> ' + log)
        script(os.path.join(cand, "bin", "switch-to-configuration"), 'echo "switch $*" >> ' + log)

        out = subprocess.run(["/bin/sh", "-c", deploy.APPLY, "apply", cand], capture_output=True, text=True,
                             env={"PATH": stubs + ":/usr/bin:/bin"})
        self.assertIn("@@ SWITCH rc=0", out.stdout, out.stdout + out.stderr)
        with open(log) as f:
            self.assertEqual(f.read().splitlines(),
                             ["nix-env --profile /nix/var/nix/profiles/system --set " + cand, "switch switch"])

    def test_a_switch_lock_held_by_the_applier_is_waited_out(self):
        # Writing the pin wakes nixos-auto-apply, which holds switch-to-configuration's lock for a
        # second while it judges the pin: the deploy's own switch exited 11, "Could not acquire lock".
        import shutil, subprocess, tempfile
        work = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, work)
        stubs, cand, log = os.path.join(work, "bin"), os.path.join(work, "system"), os.path.join(work, "log")
        os.makedirs(stubs); os.makedirs(os.path.join(cand, "bin")); os.makedirs(os.path.join(cand, "sw", "bin"))

        def script(path, body):
            with open(path, "w") as f:
                f.write("#!/bin/sh\n" + body + "\n")
            os.chmod(path, 0o755)

        script(os.path.join(stubs, "sudo"), '[ "$1" = -n ] && shift; exec "$@"')
        script(os.path.join(stubs, "sleep"), "exit 0")
        script(os.path.join(stubs, "systemd-run"), 'while [ "$1" != -- ]; do shift; done; shift; exec "$@"')
        script(os.path.join(cand, "sw", "bin", "nix-env"), "exit 0")
        held = os.path.join(work, "held")
        open(held, "w").close()
        script(os.path.join(cand, "bin", "switch-to-configuration"),
               'echo switch >> ' + log + '; if [ -e ' + held + ' ]; then rm ' + held
               + '; echo "Could not acquire lock" >&2; exit 11; fi; exit 0')

        out = subprocess.run(["/bin/sh", "-c", deploy.APPLY, "apply", cand], capture_output=True, text=True,
                             env={"PATH": stubs + ":/usr/bin:/bin"})
        self.assertIn("@@ SWITCH rc=0", out.stdout, out.stdout + out.stderr)
        with open(log) as f:
            self.assertEqual(f.read().splitlines(), ["switch", "switch"])

    def test_the_unit_states_are_read_in_the_order_the_verdict_expects(self):
        # `systemctl show -p A,B,C --value` prints systemd's own property order (ActiveState, Type,
        # Result), not the order asked for: a oneshot that had exited cleanly read as unhealthy.
        import shutil, subprocess, tempfile
        work = tempfile.mkdtemp()
        self.addCleanup(shutil.rmtree, work)
        stubs = os.path.join(work, "bin")
        os.makedirs(stubs)
        with open(os.path.join(stubs, "systemctl"), "w") as f:
            f.write('#!/bin/sh\n'
                    'case "$3" in\n'
                    '  ActiveState) echo inactive ;; Result) echo success ;; Type) echo oneshot ;;\n'
                    '  *) printf "inactive\\noneshot\\nsuccess\\n" ;;\n'
                    'esac\n')
        os.chmod(os.path.join(stubs, "systemctl"), 0o755)
        out = subprocess.run(["/bin/sh", "-c", deploy.VERIFY_STATES, "verify", "systemd-fsck-root.service"],
                             capture_output=True, text=True, env={"PATH": stubs + ":/usr/bin:/bin"})
        line = next(l for l in out.stdout.splitlines() if l.startswith("unit "))
        self.assertTrue(deploy.unit_ok(line.split()[2:]), line)


if __name__ == "__main__":
    unittest.main(verbosity=1)
