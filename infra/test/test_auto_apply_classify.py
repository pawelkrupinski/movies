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

import importlib.util
import os
import pathlib
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


if __name__ == "__main__":
    unittest.main(verbosity=2)
