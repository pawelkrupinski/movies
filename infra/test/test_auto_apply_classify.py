#!/usr/bin/env python3
"""`classify_output` from nix/files/nixos-auto-apply.py, driven with recorded switch output.

WHY THIS EXISTS. The classifier decides, for every staged closure on this fleet, whether the switch
would disturb anything -- and it decides it by MATCHING TEXT that `switch-to-configuration
dry-activate` prints. That is a contract with a program nobody here controls: nixpkgs is free to
reword a line, pluralise it, or add one, and every such change lands as a pattern that silently
stops matching. The module's own header says the file "fails open into 'nothing would happen' if
its patterns ever stop matching", and the header of `classify_output` says in as many words that it
was split out so a prover could drive it with recorded output -- but `bin/prove-nixos-auto-apply`
was never ported, so nothing did.

THE CASE THAT FORCED IT. On 2026-09-05 monitoring-1 refused a closure with
`blocked (unrecognised_output): ... would add secret: alertmanager/smtp-password`. The benign rule
for that line had been written on 2026-08-27 from an observation of `would add secretS:` -- plural,
because that switch happened to add more than one -- and sops-nix pluralises against the count. One
secret, one missing character, one host that stopped taking deploys. Every string below is recorded
verbatim from a real host for exactly that reason: a case somebody made up cannot catch a reword.

Run: python3 infra/test/test_auto_apply_classify.py   (also run by infra/bin/check)
"""

import contextlib
import importlib.util
import io
import os
import pathlib
import shutil
import tempfile
import unittest

HERE = pathlib.Path(os.path.dirname(os.path.abspath(__file__)))
APPLIER = HERE / ".." / "nix" / "files" / "nixos-auto-apply.py"

# The file is a script, not a module on the path; load it by location. It is also EMBEDDED into the
# nix module by `builtins.readFile` with its first line dropped, which is why importing the file
# itself -- rather than a copy -- is the only way to test what actually ships.
_spec = importlib.util.spec_from_file_location("nixos_auto_apply", APPLIER)
applier = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(applier)


class BenignLines(unittest.TestCase):
    """Lines that mean "this switch changes files and disturbs no running process"."""

    def test_a_single_new_secret_is_benign(self):
        # VERBATIM from monitoring-1, 2026-09-05, the line that blocked the host.
        got = applier.classify_output("would add secret: alertmanager/smtp-password")
        self.assertEqual(got.unrecognised, [],
                         "sops-nix pluralises this line against the number of secrets; the "
                         "singular form must be recognised or a one-secret change blocks the host")
        self.assertEqual(got.disruptive, [])
        self.assertEqual(got.reloads, [])

    def test_several_new_secrets_are_benign(self):
        # The plural the original rule was written from, which must keep working.
        got = applier.classify_output("would add secrets: grafana/admin-password, k3s/cluster-token")
        self.assertEqual(got.unrecognised, [])
        self.assertEqual(got.disruptive, [])

    def test_the_ordinary_preamble_is_benign(self):
        got = applier.classify_output(
            "Not checking switch inhibitors (action = dry-activate)\n"
            "would activate the configuration...")
        self.assertEqual(got.unrecognised, [])
        self.assertEqual(got.disruptive, [])


class SecretsThatAreNotBenign(unittest.TestCase):
    """The distinction the benign rule is careful about, pinned so a future widening cannot blur it.

    A secret ARRIVING cannot change the behaviour of anything already running -- no process today
    reads a file that did not exist. A secret CHANGING or being REMOVED can, out from under a
    process that already read it, and stays a person's decision.
    """

    def test_a_changed_secret_is_not_waved_through(self):
        got = applier.classify_output("would modify secret: alertmanager/telegram-bot-token")
        self.assertEqual(got.unrecognised, ["would modify secret: alertmanager/telegram-bot-token"],
                         "an existing secret changing value is not the 'arrived from nothing' shape")

    def test_a_removed_secret_is_not_waved_through(self):
        got = applier.classify_output("would remove secret: prometheus/fly-token")
        self.assertEqual(got.unrecognised, ["would remove secret: prometheus/fly-token"])


class UnitLines(unittest.TestCase):
    """The half that decides whether a switch is disruptive at all."""

    def test_a_restart_is_disruptive_unless_forgiven(self):
        line = "would restart the following units: alertmanager.service"
        self.assertTrue(applier.classify_output(line).disruptive)
        forgiven = applier.classify_output(line, restartable=("alertmanager.service",))
        self.assertEqual(forgiven.disruptive, [])
        self.assertEqual(forgiven.forgiven, ("restart alertmanager.service",))

    def test_never_disturb_beats_the_allow_list(self):
        # `neverDisturbUnits` is checked BEFORE either allow-list, so naming a unit in both must
        # still refuse -- the floor (sshd, mongodb, k3s) has to survive a `*` allow-list.
        got = applier.classify_output(
            "would restart the following units: mongodb.service",
            restartable=("*",), never=("mongodb.service",))
        self.assertEqual(got.forgiven, ())
        self.assertTrue(got.disruptive)

    def test_a_changed_but_skipped_unit_is_disruptive(self):
        # It disturbs nothing TODAY, which is exactly why it is worth refusing: the running service
        # no longer matches its definition, and that must be a person's decision.
        got = applier.classify_output(
            "would NOT restart the following changed units: nixos-auto-apply.service")
        self.assertTrue(got.disruptive)
        self.assertEqual(got.unrecognised, [])


class AnythingElse(unittest.TestCase):
    def test_an_unknown_line_blocks_rather_than_passing(self):
        # THE WHOLE SAFETY PROPERTY. A line nobody has a rule for must land in `unrecognised`,
        # which blocks -- never be silently dropped, which would read as "nothing would happen".
        got = applier.classify_output("would reticulate the following splines: 3")
        self.assertEqual(got.unrecognised, ["would reticulate the following splines: 3"])
        self.assertEqual(got.disruptive, [])


class SwitchExitingFour(unittest.TestCase):
    """`switch-to-configuration switch` exits 4 for ANY failure it saw -- including a per-user
    activation for a user whose systemd manager was going away underneath it.

    THE CASE THAT FORCED IT. monitoring-1, 2026-09-25 05:48Z (and the same shape 2026-09-02 07:03Z):
    CI's `nixdeploy` ssh session starts this unit and disconnects; logind stops `user@998` ten
    seconds later (`UserStopDelaySec`), which landed exactly while the switch was "reloading user
    units" for that user. Every system unit came up (prometheus restarted, the probe-target job
    finished), but the switch exited 4, this script called that `could_not_measure`, the unit
    failed and `SystemdUnitFailed` paged -- for a deploy that had fully landed. The head of each
    output below is VERBATIM from the journal; the tail was cut off by the old 400-character head
    truncation, so it is the text the pinned nixpkgs' switch-to-configuration-ng prints on that
    path (`warning: user activation for {name} failed`), not a recording.
    """

    HEAD_2026_09_25 = (
        "Checking switch inhibitors... done\n"
        "sops-install-secrets: Imported /etc/ssh/ssh_host_ed25519_key as age key with fingerprint "
        "age1gzk07n7anpg2nxdlfm5waledsu6xvhmg0s93tcr3474mlcx5jufsuxjpg8\n"
        "stopping the following units: prometheus.service, synthetic-probe-targets.service\n"
        "activating the configuration...\n"
        "reloading user units for root...\n"
        "restarting the following user units: nixos\n"
        "updating GRUB 2 menu...\n")
    USER_ACTIVATION_RACE = (
        "reloading user units for nixdeploy...\n"
        "Error: Failed to open dbus connection\n"
        "warning: user activation for nixdeploy failed\n"
        "restarting sysinit-reactivation.target\n"
        "starting the following units: prometheus.service, synthetic-probe-targets.service\n")
    # 2026-09-02, verbatim up to where the old truncation cut it ("Failed to start user uni").
    USER_UNIT_RACE_2026_09_02 = (
        "reloading user units for root...\n"
        "restarting the following user units: nixos-activation.service\n"
        "Failed to start user unit dbus.socket: Connection reset by peer\n"
        "warning: user activation for root failed\n")

    def switch(self, output: str, *, failed_after=frozenset()):
        """Drive the real `activate` with the switch exiting 4 and `output` on stderr."""
        # Stubbed at the process boundary, so the script's own `run` wrapper is what is tested.
        def fake_subprocess_run(argv, **_kwargs):
            code = 4 if argv[-1] == "switch" else 0
            return applier.subprocess.CompletedProcess(argv, code, "", output if code else "")

        states = iter([set(), set(failed_after)])
        saved = applier.subprocess.run, applier.failed_units, applier.time.sleep
        applier.subprocess.run = fake_subprocess_run
        applier.failed_units = lambda: next(states)
        applier.time.sleep = lambda _s: None
        self.journal = io.StringIO()
        try:
            with contextlib.redirect_stderr(self.journal):
                return applier.activate(applier.pathlib.Path("/nix/store/x-candidate"),
                                        rollback_on_failure=False)
        finally:
            applier.subprocess.run, applier.failed_units, applier.time.sleep = saved

    def test_a_user_manager_going_away_mid_switch_is_not_a_failed_deploy(self):
        failure = self.switch(self.HEAD_2026_09_25 + self.USER_ACTIVATION_RACE)
        self.assertEqual(failure, "",
                         "only a per-user activation failed; every system unit came up, so the "
                         "deploy landed and must not be reported as unmeasurable")
        self.assertIn("warning: user activation for nixdeploy failed", self.journal.getvalue(),
                      "the tolerated failure is still named in the journal")

    def test_the_2026_09_02_user_unit_shape_is_tolerated_too(self):
        failure = self.switch(self.USER_UNIT_RACE_2026_09_02)
        self.assertEqual(failure, "")

    def test_a_system_unit_failing_still_stops_the_pass_and_says_which(self):
        output = (self.HEAD_2026_09_25 + self.USER_ACTIVATION_RACE
                  + "warning: the following units failed: prometheus.service\n")
        with self.assertRaises(applier.SwitchFailed) as raised:
            self.switch(output)
        self.assertIn("warning: the following units failed: prometheus.service",
                      str(raised.exception),
                      "the failure lines are at the END of the output; reporting only its head "
                      "is what hid the cause of both incidents")

    def test_a_system_start_failure_is_not_mistaken_for_a_user_one(self):
        output = ("activating the configuration...\n"
                  "warning: user activation for nixdeploy failed\n"
                  "Failed to start grafana.service: Unit grafana.service is masked.\n")
        with self.assertRaises(applier.SwitchFailed):
            self.switch(output)

    def test_exit_four_with_no_user_activation_failure_is_not_tolerated(self):
        with self.assertRaises(applier.SwitchFailed):
            self.switch(self.HEAD_2026_09_25)

    def test_a_system_unit_that_failed_afterwards_is_still_caught_by_verification(self):
        # The tolerance only skips switch-to-configuration's verdict on USER scope; this script's
        # own before/after comparison of failed system units still runs.
        failure = self.switch(self.HEAD_2026_09_25 + self.USER_ACTIVATION_RACE,
                                 failed_after={"prometheus.service"})
        self.assertIn("prometheus.service", failure)


class SwitchFailedMidway(unittest.TestCase):
    """A switch that exits non-zero once it has started activating is a FAILED SWITCH, never
    `could_not_measure`: the host is half on the new closure.

    THE CASE THAT FORCED IT. monitoring-1, 2026-11-10 08:29:09Z, closure 2kicckppx0vq (rev
    cc8ad36eed). switch-to-configuration-ng lists logind's users and then reads each one's GID
    over D-Bus with `?` -- so a user whose manager logind tears down between the two calls (root,
    whose forced-command ssh sessions come and go every 10-15 s) aborts the WHOLE switch with exit
    1 before its start phase. /run/current-system already pointed at the new closure;
    local-fs.target and remote-fs.target stayed stopped (left in /run/nixos/start-list), and this
    script called it `undetermined (could_not_measure)`. The output below is VERBATIM from that
    pass's journal (stderr, then stdout, as `activate` joins them).
    """

    LOGIND_USER_VANISHED_2026_11_10 = (
        "Checking switch inhibitors... done\n"
        "updating GRUB 2 menu...\n"
        "activating the configuration...\n"
        "sops-install-secrets: Imported /etc/ssh/ssh_host_ed25519_key as age key with fingerprint "
        "age1gzk07n7anpg2nxdlfm5waledsu6xvhmg0s93tcr3474mlcx5jufsuxjpg8\n"
        "reloading user units for nixdeploy...\n"
        "restarting the following user units: nixos-activation.service\n"
        "Error: Failed to get GID for root\n"
        "\n"
        "Caused by:\n"
        "    Unknown object '/org/freedesktop/login1/user/_0'.\n"
        "setting up /etc...\n"
        "sops-install-secrets: Imported /etc/ssh/ssh_host_rsa_key as GPG key with fingerprint "
        "256c8924c924bcab5fa0eb54508d65cc59591fb2\n")
    BEFORE_ACTIVATING = (
        "Checking switch inhibitors... done\n"
        "Error: There are changes to critical components of the system: dbus\n")

    def setUp(self):
        self.root = pathlib.Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, self.root)
        self.candidate = self.root / "candidate"
        self.candidate.mkdir()
        (self.root / "previous").mkdir()
        self.current = self.root / "current-system"
        self.profile = self.root / "profile"
        self.profile.symlink_to(self.candidate)
        saved = (applier.subprocess.run, applier.failed_units, applier.time.sleep,
                 applier.CURRENT_SYSTEM, applier.SYSTEM_PROFILE, applier.SWITCH_STATE_DIRECTORY)
        def restore():
            (applier.subprocess.run, applier.failed_units, applier.time.sleep,
             applier.CURRENT_SYSTEM, applier.SYSTEM_PROFILE, applier.SWITCH_STATE_DIRECTORY) = saved
        self.addCleanup(restore)
        applier.CURRENT_SYSTEM = str(self.current)
        applier.SYSTEM_PROFILE = str(self.profile)
        applier.SWITCH_STATE_DIRECTORY = str(self.root / "run-nixos")
        applier.failed_units = lambda: set()
        applier.time.sleep = lambda _s: None

    def switch(self, *results, current_is_candidate=True):
        """Drive the real `activate`; each `switch` call takes the next (exit code, output)."""
        self.current.symlink_to(self.candidate if current_is_candidate else self.root / "previous")
        outcomes = iter(results)
        self.switches = 0
        def fake_subprocess_run(argv, **_kwargs):
            if argv[-1] != "switch":
                return applier.subprocess.CompletedProcess(argv, 0, "", "")
            self.switches += 1
            code, output = next(outcomes)
            return applier.subprocess.CompletedProcess(argv, code, "", output)
        applier.subprocess.run = fake_subprocess_run
        self.journal = io.StringIO()
        with contextlib.redirect_stderr(self.journal):
            return applier.activate(self.candidate, rollback_on_failure=False)

    def test_the_logind_race_is_retried_once_and_a_clean_retry_lands(self):
        failure = self.switch((1, self.LOGIND_USER_VANISHED_2026_11_10), (0, ""))
        self.assertEqual(failure, "")
        self.assertEqual(self.switches, 2, "the half-applied host completes its own switch")
        self.assertIn("Unknown object '/org/freedesktop/login1/user/_0'", self.journal.getvalue(),
                      "the retried race is still named in the journal")

    def test_a_retry_that_fails_again_is_a_failed_switch_with_its_tail(self):
        with self.assertRaises(applier.SwitchFailed) as raised:
            self.switch((1, self.LOGIND_USER_VANISHED_2026_11_10),
                        (1, self.LOGIND_USER_VANISHED_2026_11_10))
        self.assertEqual(self.switches, 2, "ONE retry, never a loop")
        self.assertIn("Failed to get GID for root", str(raised.exception))
        self.assertNotIsInstance(raised.exception, applier.Undetermined,
                                 "a half-applied host is a verdict, not could_not_measure")

    def test_no_retry_unless_the_host_is_on_the_candidate(self):
        with self.assertRaises(applier.SwitchFailed):
            self.switch((1, self.LOGIND_USER_VANISHED_2026_11_10), (0, ""),
                        current_is_candidate=False)
        self.assertEqual(self.switches, 1,
                         "/run/current-system and the profile must both name the candidate")

    def test_another_failure_after_activating_is_a_failed_switch_and_not_retried(self):
        output = ("activating the configuration...\n"
                  "Error: Failed to open dbus connection\n")
        with self.assertRaises(applier.SwitchFailed):
            self.switch((1, output), (0, ""))
        self.assertEqual(self.switches, 1)

    def test_a_switch_refused_before_activating_changed_nothing(self):
        with self.assertRaises(applier.Undetermined) as raised:
            self.switch((1, self.BEFORE_ACTIVATING), current_is_candidate=False)
        self.assertNotIsInstance(raised.exception, applier.SwitchFailed)

    def test_an_interrupted_switch_is_named_from_its_leftover_lists(self):
        state = pathlib.Path(applier.SWITCH_STATE_DIRECTORY)
        self.assertEqual(applier.interrupted_switch(), [], "no directory, no leftovers")
        state.mkdir()
        (state / "start-list").write_text("local-fs.target\nremote-fs.target\n")
        (state / "restart-list").write_text("")
        self.assertEqual(applier.interrupted_switch(),
                         ["start local-fs.target", "start remote-fs.target"])

    def test_leftover_lists_of_a_switch_still_running_are_not_a_stopped_switch(self):
        import fcntl
        state = pathlib.Path(applier.SWITCH_STATE_DIRECTORY)
        state.mkdir()
        (state / "start-list").write_text("local-fs.target\n")
        with open(state / "switch-to-configuration.lock", "w") as held:
            fcntl.flock(held, fcntl.LOCK_EX)
            with self.assertRaises(applier.LockContended):
                applier.interrupted_switch()
        self.assertEqual(applier.interrupted_switch(), ["start local-fs.target"],
                         "a released lock leaves the lists as a stopped switch's")

    def test_a_pass_on_a_half_applied_host_reports_switch_failed(self):
        # The NEXT pass after 2026-11-10's: candidate == /run/current-system, so it used to say
        # up_to_date (or, once a newer closure was staged, classifier_disagreement) while the
        # host still owed local-fs.target and remote-fs.target.
        self.current.symlink_to(self.candidate)
        state = pathlib.Path(applier.SWITCH_STATE_DIRECTORY)
        state.mkdir()
        (state / "start-list").write_text("local-fs.target\nremote-fs.target\n")
        saved = applier.staged_system, applier.configuration_revision
        applier.staged_system = lambda _pin: self.candidate
        applier.configuration_revision = lambda _system: "cc8ad36eed"
        try:
            verdict = applier.classify_and_apply(
                type("Args", (), {"staged_system": "/unused"})(), metrics=None)
        finally:
            applier.staged_system, applier.configuration_revision = saved
        self.assertEqual((verdict.state, verdict.reason), ("blocked", "switch_failed"))
        self.assertIn("start local-fs.target", verdict.detail)


class SystemdReexecAfterStoreOptimise(unittest.TestCase):
    """`would restart systemd` when PID 1 already runs the candidate's systemd, byte for byte.

    THE INCIDENT. 2026-09-28/29: k3s-worker-1 (4.5h) then mongo-1 and monitoring-1 (18h) refused
    every closure as `classifier_disagreement`, structural test inert, dry-activate saying
    `restart-systemd`. Nothing had changed systemd: every generation on every host carried the same
    systemd-260.4 and the same system.conf. switch-to-configuration-ng decides on a re-exec by
    comparing `canonicalize("/proc/1/exe")` with the candidate's binary, and falls back to the path
    "/unknown" when that canonicalize FAILS -- which it does once the nightly `nix-store --optimise`
    has hard-linked a duplicate over the running binary's path: the kernel then reports PID 1's exe
    as "<path> (deleted)". The first optimise after the 09-27 nixpkgs bump (1.1-1.3 GiB freed) did
    exactly that, and only a hand switch (re-exec, 09-28 09:23 and 09-29 10:10) cleared it.
    """

    # VERBATIM from the blocked hosts' journals, re-joined into the dry-activate lines it summarised.
    RECORDED = ("Not checking switch inhibitors (action = dry-activate)\n"
                "would activate the configuration...\n"
                "would restart systemd\n"
                "would reload the following units: dbus-broker.service")

    def test_a_reexec_of_the_identical_systemd_is_not_disruptive(self):
        got = applier.classify_output(self.RECORDED, pid1_unchanged=True)
        self.assertEqual(got.disruptive, [],
                         "PID 1 runs the candidate's systemd byte for byte; re-exec changes nothing")
        self.assertEqual(got.reloads, ["dbus-broker.service"])

    def test_a_reexec_into_a_different_systemd_still_blocks(self):
        got = applier.classify_output(self.RECORDED)
        self.assertEqual(got.disruptive, ["restart-systemd"])

    def _tree(self, binary: bytes, conf: str):
        root = pathlib.Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, root)
        (root / "systemd" / "lib" / "systemd").mkdir(parents=True)
        (root / "systemd" / "lib" / "systemd" / "systemd").write_bytes(binary)
        (root / "etc" / "systemd").mkdir(parents=True)
        (root / "etc" / "systemd" / "system.conf").symlink_to(conf)
        return root

    def _files(self, binary: bytes, conf_text: str):
        d = pathlib.Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, d)
        (d / "pid1").write_bytes(binary)
        (d / "system.conf").write_text(conf_text)
        return d / "pid1", d / "system.conf"

    def test_identical_bytes_at_another_path_and_the_same_config_is_inert(self):
        pid1, conf = self._files(b"systemd-260.4", "[Manager]\n")
        candidate = self._tree(b"systemd-260.4", str(conf))
        self.assertTrue(applier.pid1_reexec_is_inert(candidate, pid1, conf))

    def test_a_different_binary_is_not_inert(self):
        pid1, conf = self._files(b"systemd-260.3", "[Manager]\n")
        candidate = self._tree(b"systemd-260.4", str(conf))
        self.assertFalse(applier.pid1_reexec_is_inert(candidate, pid1, conf))

    def test_a_changed_system_conf_is_not_inert(self):
        pid1, conf = self._files(b"systemd-260.4", "[Manager]\n")
        other = conf.parent / "other.conf"
        other.write_text("[Manager]\nDefaultTimeoutStopSec=5s\n")
        candidate = self._tree(b"systemd-260.4", str(other))
        self.assertFalse(applier.pid1_reexec_is_inert(candidate, pid1, conf))

    def test_an_unreadable_pid1_is_not_inert(self):
        pid1, conf = self._files(b"systemd-260.4", "[Manager]\n")
        candidate = self._tree(b"systemd-260.4", str(conf))
        self.assertFalse(applier.pid1_reexec_is_inert(candidate, pid1.parent / "absent", conf))



class Leniency(unittest.TestCase):
    """NEW units and ROUTINE units (timers, exporters, the applier itself), added 2026-10-23.

    monitoring-1 refused a closure that only ADDED units as `units_would_change`, and the person ran
    nix-env + switch-to-configuration by hand. These drive the shipped chain -- `leniency_for` ->
    `classify_output` -> `structural_changes` -- with NO restartable list, and require BOTH halves to
    agree: equal answers are the only ones that are not `classifier_disagreement`.
    """

    ROUTINE = ("*.timer", "prometheus-*-exporter.service", "nixos-auto-apply.service")
    RUNNING = {"sshd.service": "/nix/store/s", "multi-user.target.wants/sshd.service": "/nix/store/s",
               "mongodump.timer": "/nix/store/t1", "mongodump.service": "/nix/store/d1",
               "nixos-auto-apply.service": "/nix/store/a1", "mongodb.service": "/nix/store/m1"}

    def setUp(self):
        self.base = pathlib.Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, self.base)
        self.running = self._tree("running", {})

    def _tree(self, name, changes):
        units = dict(self.RUNNING)
        for key, value in changes.items():
            if value is None:
                units.pop(key, None)
            else:
                units[key] = value
        unit_dir = self.base / name / "etc" / "systemd" / "system"
        unit_dir.mkdir(parents=True)
        for unit, target in units.items():
            (unit_dir / unit).parent.mkdir(parents=True, exist_ok=True)
            (unit_dir / unit).symlink_to(target)
        return self.base / name

    def _both(self, changes, text, start_new=True, routine=(), never=(), inactive=()):
        """(structural_noop, dry_noop), each computed by the code that ships."""
        candidate = self._tree(f"c{len(list(self.base.iterdir()))}", changes)
        leniency = applier.leniency_for(applier.unit_tree(self.running),
                                        applier.unit_tree(candidate), start_new, tuple(routine),
                                        tuple(inactive))
        dry = applier.classify_output(text, (), never, leniency=leniency)
        changed = applier.structural_changes(self.running, candidate, dry, (), (), never, leniency)
        return not changed.blocked, not dry.disruptive and not dry.unrecognised

    def test_a_new_timer_applies(self):
        changes = {"gc.timer": "/nix/store/g", "gc.service": "/nix/store/gs",
                   "timers.target.wants/gc.timer": "/nix/store/g"}
        self.assertEqual(self._both(changes, "would start the following units: gc.timer"),
                         (True, True))
        self.assertEqual(self._both(changes, "would start the following units: gc.timer",
                                    start_new=False), (False, False))

    def test_a_new_wants_directory_needs_every_entry_new(self):
        only_new = {"exporter.service": "/nix/store/e",
                    "metrics.target.wants/exporter.service": "/nix/store/e"}
        self.assertEqual(self._both(only_new, "would start the following units: exporter.service"),
                         (True, True))
        with_existing = dict(only_new, **{"metrics.target.wants/sshd.service": "/nix/store/s"})
        self.assertEqual(
            self._both(with_existing, "would start the following units: exporter.service")[0], False)

    def test_new_stateful_units_and_mounts_are_still_refused(self):
        self.assertEqual(self._both({"mongod.service": "/nix/store/x"},
                                    "would start the following units: mongod.service"),
                         (False, False))
        self.assertEqual(self._both({"mnt-data.mount": "/nix/store/x"},
                                    "would start the following units: mnt-data.mount"),
                         (False, False))

    def test_a_new_drop_in_for_an_existing_unit_is_not_new(self):
        self.assertEqual(self._both({"sshd.service.d/override.conf": "/nix/store/o"},
                                    "would restart the following units: sshd.service"),
                         (False, False))

    def test_a_routine_timer_and_its_idle_service_apply(self):
        changes = {"mongodump.timer": "/nix/store/t2", "mongodump.service": "/nix/store/d2"}
        line = "would restart the following units: mongodump.timer"
        self.assertEqual(self._both(changes, line, routine=self.ROUTINE), (True, True))
        self.assertEqual(self._both(changes, line), (False, False))

    def test_a_routine_timers_running_service_is_refused_by_both(self):
        changes = {"mongodump.timer": "/nix/store/t2", "mongodump.service": "/nix/store/d2"}
        line = "would restart the following units: mongodump.service, mongodump.timer"
        self.assertEqual(self._both(changes, line, routine=self.ROUTINE), (False, False))

    def test_the_applier_applies_itself_only_when_routine_names_it(self):
        changes = {"nixos-auto-apply.service": "/nix/store/a2"}
        line = "would NOT restart the following changed units: nixos-auto-apply.service"
        self.assertEqual(self._both(changes, line, routine=self.ROUTINE), (True, True))
        self.assertEqual(self._both(changes, "", routine=self.ROUTINE), (True, True),
                         "a direct invocation, where dry-activate is silent about the unit")
        self.assertEqual(self._both(changes, line, routine=("*.timer",)), (False, False))

    def test_routine_star_cannot_restart_a_database(self):
        self.assertEqual(self._both({"mongodb.service": "/nix/store/m2"},
                                    "would restart the following units: mongodb.service",
                                    routine=("*",)), (False, False))

    def test_never_disturb_outranks_routine(self):
        changes = {"mongodump.timer": "/nix/store/t2"}
        self.assertEqual(self._both(changes, "would restart the following units: mongodump.timer",
                                    routine=self.ROUTINE, never=("mongodump.*",)), (False, False))

    def test_a_removed_unit_is_not_new(self):
        self.assertEqual(self._both({"mongodump.timer": None, "mongodump.service": None},
                                    "would stop the following units: mongodump.timer"),
                         (False, False))

    def test_an_idle_changed_unit_applies(self):
        # monitoring-1, 2026-11-02: a nixpkgs bump changed generate-shutdown-ramfs.service, which
        # runs only at shutdown; dry-activate said nothing of it and the unit tree refused it, so
        # every pass was `classifier_disagreement`.
        self.RUNNING = dict(self.RUNNING, **{"generate-shutdown-ramfs.service": "/nix/store/r1"})
        self.running = self._tree("running-ramfs", {})
        changes = {"generate-shutdown-ramfs.service": "/nix/store/r2"}
        self.assertEqual(self._both(changes, "", inactive=("generate-shutdown-ramfs.service",)),
                         (True, True))
        self.assertEqual(self._both(changes, ""), (False, True),
                         "an active unit dry-activate is silent about is still the dangerous case")

    def test_an_idle_unit_dry_activate_would_restart_is_refused_by_both(self):
        changes = {"mongodump.service": "/nix/store/d2"}
        line = "would restart the following units: mongodump.service"
        self.assertEqual(self._both(changes, line, inactive=("mongodump.service",)), (False, False))

    def test_idle_cannot_reach_a_stateful_unit_or_a_never_disturb_one(self):
        self.assertEqual(self._both({"mongodb.service": "/nix/store/m2"}, "",
                                    inactive=("mongodb.service",))[0], False)
        self.assertEqual(self._both({"mongodump.service": "/nix/store/d2"}, "",
                                    inactive=("mongodump.service",), never=("mongodump.*",))[0],
                         False)

    def test_idle_covers_only_changed_services_already_running_tree(self):
        leniency = applier.leniency_for(
            {"a.service": "x", "b.socket": "y", "c.service": "z"},
            {"a.service": "x2", "b.socket": "y2", "c.service": "z", "n.service": "n"},
            False, (), ("a.service", "b.socket", "c.service", "n.service"))
        self.assertEqual(leniency.idle, frozenset({"a.service"}))

    def test_inactive_units_reads_systemd_and_fails_closed(self):
        import subprocess
        def answering(stdout, code=0):
            return lambda argv, **_: subprocess.CompletedProcess(argv, code, stdout, "")
        original = applier.run
        self.addCleanup(setattr, applier, "run", original)
        applier.run = answering("active\ninactive\n")
        self.assertEqual(applier.inactive_units({"b.service", "a.service"}), {"b.service"})
        applier.run = answering("inactive\n")
        self.assertEqual(applier.inactive_units({"a.service", "b.service"}), set(),
                         "an answer that does not line up forgives nothing")
        applier.run = answering("inactive\ninactive\n", code=1)
        self.assertEqual(applier.inactive_units({"a.service", "b.service"}), set())

    def test_user_units_are_never_lenient(self):
        got = applier.classify_output(
            "would restart the following user units: gc.timer",
            leniency=applier.Leniency(routine=("*.timer",)))
        self.assertTrue(got.disruptive)

class RebootAlreadyOwed(unittest.TestCase):
    """A reboot the RUNNING system already owes is not this switch's doing, and dry-activate cannot see it.

    2026-11-09/10, k3s-worker-1 and mongo-1: nixpkgs c56a87d (kernel 6.18.60, a rebuilt systemd)
    was switched by hand at 23:31 -- pid 1 re-executed into the new systemd, the kernel waited for a
    reboot. The next closure (7ecf116149, same nixpkgs) changed nothing but files, and every pass
    refused it as `classifier_disagreement`: the boot chain, compared against /run/booted-system,
    still differed, and was counted as "not inert" -- while dry-activate, which never looks at the
    kernel and saw pid 1 already on the candidate's systemd, said only "reload dbus-broker". The
    owner switched both by hand; the switch reloaded dbus-broker and nothing else.
    """

    # VERBATIM from mongo-1, 2026-11-10: dry-activate of the 23:31 generation from the 02:37 one --
    # the same two closures as the refused pair, in reverse, so the same dbus-broker reload.
    MONGO_1 = (
        "Not checking switch inhibitors (action = dry-activate)\n"
        "would activate the configuration...\n"
        "sops-install-secrets: Imported /etc/ssh/ssh_host_rsa_key as GPG key with fingerprint "
        "cb5023b4e2881e4a5c1902e97414ed99ad9aaf77\n"
        "sops-install-secrets: Imported /etc/ssh/ssh_host_ed25519_key as age key with fingerprint "
        "age1cdw35s3ezp0ekt3wf5766flefmelngsr36kx57crwj3xan6xkdnsrwml9w\n"
        "would reload the following units: dbus-broker.service\n")
    ALL = list(applier.BOOT_CHAIN)
    RELOADABLE = ("dbus-broker.service",)

    def setUp(self):
        self.base = pathlib.Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, self.base)

    def _system(self, name, **components):
        system = self.base / name
        system.mkdir()
        for component in applier.BOOT_CHAIN:
            target = self.base / "store" / f"{component}-{components.get(component, 'old')}"
            target.mkdir(parents=True, exist_ok=True)
            (system / component).symlink_to(target)
        return system

    def test_the_boot_chain_splits_what_the_switch_introduces_from_what_is_already_owed(self):
        new = dict.fromkeys(applier.BOOT_CHAIN, "new")
        booted = self._system("booted")
        running = self._system("running", **new)
        chain = applier.boot_chain(booted, running, self._system("candidate", **new))
        self.assertEqual((chain.owed, chain.introduced), (self.ALL, []))
        chain = applier.boot_chain(booted, booted, self._system("bump", kernel="new"))
        self.assertEqual((chain.owed, chain.introduced), (["kernel"], ["kernel"]))
        chain = applier.boot_chain(booted, running, booted)
        self.assertEqual(chain.owed, [], "a switch back to the booted system owes nothing")

    def test_a_files_only_switch_while_a_reboot_is_already_owed_applies(self):
        dry = applier.classify_output(self.MONGO_1)
        chain = applier.BootChain(owed=self.ALL, introduced=[])
        self.assertIsNone(applier.refusal([], chain, dry, self.RELOADABLE),
                          "the 2026-11-10 pair must apply, not raise classifier_disagreement")

    def test_a_new_kernel_dry_activate_is_silent_about_is_reboot_owed_not_a_disagreement(self):
        dry = applier.classify_output(self.MONGO_1)
        chain = applier.BootChain(owed=["kernel"], introduced=["kernel"])
        self.assertEqual(applier.refusal([], chain, dry, self.RELOADABLE), "reboot_owed")

    def test_a_new_systemd_is_seen_by_both_halves(self):
        dry = applier.classify_output(self.MONGO_1 + "would restart systemd\n")
        chain = applier.BootChain(owed=["systemd"], introduced=["systemd"])
        self.assertEqual(applier.refusal([], chain, dry, self.RELOADABLE), "reboot_owed")
        silent = applier.classify_output(self.MONGO_1)
        self.assertEqual(applier.refusal([], chain, silent, self.RELOADABLE),
                         "classifier_disagreement",
                         "a new systemd dry-activate does not re-execute is a real disagreement")

    def test_a_changed_unit_dry_activate_is_silent_about_still_disagrees(self):
        dry = applier.classify_output(self.MONGO_1)
        chain = applier.BootChain(owed=self.ALL, introduced=[])
        self.assertEqual(applier.refusal(["caddy.service"], chain, dry, self.RELOADABLE),
                         "classifier_disagreement")
        self.assertEqual(applier.refusal([], chain, dry, ()), "classifier_disagreement",
                         "a reload the host does not permit is still refused by dry-activate")


class PublishedMetrics(unittest.TestCase):
    """The textfile node_exporter reads. ONE malformed line drops EVERY series in it.

    2026-11-10, monitoring-1: a `reboot_owed` verdict for four components wrote `# HELP
    nixos_auto_apply_reboot_component` four times; node_exporter refused the file ("second HELP
    line for metric name"), so the host published no auto-apply series at all and the fleet
    dashboard showed it "not covered" while the applier ran every 30 minutes.
    """

    def test_every_metric_family_is_described_once(self):
        directory = pathlib.Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, directory)
        metrics = applier.Metrics(directory / "nixos-auto-apply.prom", "monitoring-1")
        verdict = applier.Verdict("blocked", "reboot_owed", "detail", "rev", "/nix/store/c",
                                  True, True, tuple(applier.BOOT_CHAIN))
        applier.publish(metrics, verdict, 1, deployment_written=True)
        lines = (directory / "nixos-auto-apply.prom").read_text().splitlines()
        described = [line.split()[2] for line in lines if line.startswith("# HELP ")]
        self.assertEqual(len(described), len(set(described)), described)
        components = [line for line in lines if line.startswith("nixos_auto_apply_reboot_component{")]
        self.assertEqual(len(components), len(applier.BOOT_CHAIN))


if __name__ == "__main__":
    unittest.main(verbosity=2)
