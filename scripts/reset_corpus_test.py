#!/usr/bin/env python3
"""scripts/reset-corpus.sh, black-box: which prod database a country's reset targets and what it scales around the wipe.
No vault, ssh or Mongo: `secrets`, `ssh`, `mongosh` and `kubectl` are fakes on PATH (the fake ssh listens on the port the
tool forwards, as a tunnel would). Ported from reset-corpus-test.sh case for case; the same cases hold the shell tool and
its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import re
import sys
import unittest
from typing import List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/reset_corpus.py", "scripts/reset-corpus.sh")
SECRETS = '''
shift 2
while [ "$1" != "--" ]; do export "${1%%=*}=mongodb://u:p@10.20.0.13:27017/?authSource=admin"; shift; done
shift
exec "$@"
'''
SSH = '''
import signal, socket
if env.get("SSH_FAILS"):
    exit(255)
listener = socket.socket()
listener.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
listener.bind(("127.0.0.1", int(argv[argv.index("-L") + 1].split(":")[0])))
listener.listen(5)
signal.pause()
'''
MONGOSH = '''
if env.get("DROP_FAILS") and any("getSiblingDB" in a for a in argv):
    exit(1)
out("0")
'''


class ResetCorpusTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.fakes.script("secrets", SECRETS)
        self.fakes.program("ssh", SSH)
        self.fakes.program("mongosh", MONGOSH)
        self.fakes.script("kubectl", "exit 0")

    def reset(self, country: str, database: Optional[str], *args: str, **extra: str) -> cli_harness.Done:
        env = {"MONGODB_URI": None, "MONGODB_DB": database, "KINOWO_RESET_COUNTRY": country, **extra}
        return self.run_tool(*(args or ("--dry-run",)), env=env)

    @staticmethod
    def target(done: cli_harness.Done) -> str:
        found = re.search(r"target db '([^']*)'", done.output)
        return found.group(1) if found else ""

    def scaled(self) -> List[str]:
        return [f"{c[3].removeprefix('deploy/')} {c[4]}" for c in self.fakes.calls("kubectl") if "scale" in c]

    def test_polands_corpus_is_the_kinowo_database(self) -> None:
        self.assertEqual("kinowo", self.target(self.reset("pl", None)))

    def test_germanys_corpus_is_kinowo_de_never_polands(self) -> None:
        self.assertEqual("kinowo_de", self.target(self.reset("de", None)))

    def test_an_exported_database_naming_another_countrys_is_refused_before_anything_is_targeted(self) -> None:
        done = self.reset("de", "kinowo")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual("", self.target(done))

    def test_a_matching_exported_database_is_accepted(self) -> None:
        self.assertEqual("kinowo_uk", self.target(self.reset("uk", "kinowo_uk")))

    def test_the_prod_uris_password_never_reaches_mongoshs_command_line(self) -> None:
        self.reset("pl", None)
        self.assertTrue(self.fakes.calls("mongosh"))
        self.assertEqual([], [c for c in self.fakes.calls("mongosh") if "u:p@" in " ".join(c)])

    def test_a_dry_run_scales_nothing(self) -> None:
        self.reset("pl", None)
        self.assertEqual([], self.scaled())

    def test_a_reset_scales_both_down_wipes_and_scales_both_back_up(self) -> None:
        done = self.reset("pl", None, "--yes")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(["web-pl --replicas=0", "worker-pl --replicas=0", "web-pl --replicas=1",
                          "worker-pl --replicas=1"], self.scaled())

    def test_prod_unreachable_is_refused_before_anything_is_scaled(self) -> None:
        done = self.reset("pl", None, "--yes", SSH_FAILS="1")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual([], self.scaled())

    def test_a_wipe_that_fails_half_way_still_scales_both_back_up(self) -> None:
        done = self.reset("pl", None, "--yes", DROP_FAILS="1")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(["web-pl --replicas=0", "worker-pl --replicas=0", "web-pl --replicas=1",
                          "worker-pl --replicas=1"], self.scaled())

    def test_a_worker_that_will_not_scale_up_never_leaves_web_at_zero(self) -> None:
        self.fakes.script("kubectl", 'case "$*" in *worker-pl*--replicas=1) exit 1;; esac')
        done = self.reset("pl", None, "--yes")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual(["web-pl --replicas=0", "worker-pl --replicas=0", "web-pl --replicas=1",
                          "worker-pl --replicas=1", "web-pl --replicas=1", "worker-pl --replicas=1"], self.scaled())
        self.assertIn("could not scale back up: worker-pl", done.stderr)

    def test_an_unknown_argument_is_refused(self) -> None:
        done = self.reset("pl", None, "--bogus")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("unknown argument: --bogus", done.stderr)

    def test_an_unconfirmed_wipe_aborts_before_anything_is_scaled(self) -> None:
        done = self.run_tool(env={"MONGODB_URI": None, "KINOWO_RESET_COUNTRY": "pl"}, stdin="no\n")
        self.assertEqual(1, done.code, done.output)
        self.assertEqual([], self.scaled())


if __name__ == "__main__":
    unittest.main()
