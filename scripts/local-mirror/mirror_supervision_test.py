#!/usr/bin/env python3
# script-test-reads: scripts/local-mirror/mirror* scripts/local-mirror/*.js scripts/local-mirror/prod-tunnel.sh scripts/local-mirror/local-mongo.sh scripts/local-mirror/log-path.sh scripts/local-mirror/mongosh-at.sh scripts/lib/prod_tunnel.py scripts/lib/mongo_local.py scripts/jvm-gate.sh scripts/vault-env.sh
"""scripts/local-mirror/mirror.sh black-box (see mirror_fixture.py): the supervised tailer ending, failing, drifting or hanging.
Run: python3 scripts/local-mirror/mirror_supervision_test.py
"""
from __future__ import annotations

import glob
import json
import os
import subprocess
import time
import unittest

from mirror_fixture import HERE, MirrorCase, NODE, STAMPED, gone
import cli_harness  # noqa: E402  (on the path once mirror_fixture is imported)


class SupervisionTest(MirrorCase):
    def test_the_staleness_gate_reseeds_on_3_alone_whatever_fences_the_machine_holds(self) -> None:
        self.fence("kinowo_a", "what-if-machine", str(int(time.time()) + 600), "run",
                   root="home/.cache/kinowo/mirror-fence")
        self.answer("staleness", "3", db="kinowo_a")
        self.answer("staleness", "0", db="kinowo_b")
        self.answer("staleness", "1", db="kinowo_c")
        self.start(KINOWO_MIRROR_DBS="kinowo_a kinowo_b kinowo_c")
        for db in ("kinowo_a", "kinowo_b", "kinowo_c"):
            self.until_tailing(db=db)
        self.assertEqual(["kinowo_a"], [c["db"] for c in self.mongosh_calls("seed")])
        port = self.local_mongo.getsockname()[1]
        self.assertIn(f"var DST='mongodb://127.0.0.1:{port}/kinowo_prod_mirror?directConnection=true'",
                      str(self.mongosh_calls("seed")[0]["evals"]))
        self.assertEqual(1, len(self.lines("kinowo_c: staleness check could not run (exit 1) — tailing anyway")))

    def test_a_tailer_whose_resume_token_expired_reseeds_at_once_without_a_backoff(self) -> None:
        self.answer("tail", "2")
        self.start()
        self.until_tailing(2)
        kinds = [c["kind"] for c in self.mongosh_calls() if c["kind"] in ("seed", "tail")]
        self.assertEqual(["tail", "seed", "tail"], kinds)
        self.assertEqual(1, len(self.lines("kinowo: resume token expired — re-seeding…")))
        self.assertNotIn("2", self.sleeps())

    def test_a_stream_ending_over_and_over_backs_off_and_says_so_for_the_first_three(self) -> None:
        self.answer("tail", *["1"] * 30)
        self.start(FAKE_SLEEP_HOLD="2 15:6")
        self.until("no six backoffs", lambda: len([s for s in self.sleeps() if s in ("2", "15")]) >= 6)
        self.assertEqual(["2", "2", "2", "2", "15", "15"], [s for s in self.sleeps() if s in ("2", "15")])
        self.assertEqual(3, len(self.lines("kinowo: stream ended (exit 1, ")), self.output())
        self.assertEqual(1, len(self.lines("kinowo: stream ended (exit 1, 1 in a row) — recovering in 2s…")))

    def test_a_failed_seed_retries_on_the_next_cycle_instead_of_ending_the_supervisor(self) -> None:
        self.answer("seed", "1", "0")
        self.start("--reseed")
        self.until_tailing()
        self.assertEqual(2, len(self.mongosh_calls("seed")))
        self.assertEqual(1, len(self.lines("kinowo: seed failed (exit 1) — retrying in 30s")))
        self.assertIn("30", self.sleeps())

    def test_a_mirror_that_drifts_while_tailing_is_reseeded_and_its_tailer_stopped(self) -> None:
        self.answer("staleness", "0", "3", "3")
        self.start(KINOWO_MIRROR_AUDIT_POLLS="1")
        first = self.until_tailing(2)[0]
        self.assertEqual(1, len(self.lines("kinowo: drifted while tailing — restarting to re-seed")))
        kinds = [c["kind"] for c in self.mongosh_calls() if c["kind"] in ("seed", "tail")]
        self.assertEqual(["tail", "seed", "tail"], kinds)
        self.until("the drifted tailer was left streaming", lambda: gone(first))

    def test_a_staleness_check_that_never_returns_is_cut_off_and_reads_as_tail_anyway(self) -> None:
        self.answer("staleness", "hang")
        self.start(KINOWO_MIRROR_STALENESS_TIMEOUT="1")
        self.until_tailing()
        self.assertEqual(1, len(self.lines("kinowo: staleness check could not run (exit 124) — tailing anyway")))
        self.until("the hung check was left running", lambda: gone(int(str(self.mongosh_calls("staleness")[0]["pid"]))))
        self.assertEqual([], self.mongosh_calls("seed"))

    @unittest.skipUnless(NODE, "no node")
    def test_a_what_if_fence_in_force_holds_the_reseed_audit_of_its_database_alone(self) -> None:
        now = int(time.time())
        self.fence("kinowo_pl", "what-if-1", str(now + 600), "run")
        self.fence("kinowo_de", "what-if-2", str(now - 5), "run")
        self.fence("kinowo_uk", "what-if-3", str(now + 600), "linger")
        self.fence("kinowo_uk", "what-if-4", str(now + 600), "drain")
        self.fence("kinowo_fr", "what-if-5", "not-a-number", "run")
        self.fence("kinowo_es", "what-if-6", str(now + 600), "linger")
        dbs = ["kinowo_pl", "kinowo_de", "kinowo_uk", "kinowo_fr", "kinowo_es", "kinowo_it"]
        self.start(KINOWO_MIRROR_DBS=" ".join(dbs))
        for db in dbs:
            self.until_tailing(db=db)
        self.assertEqual(1, len(self.lines("kinowo_pl: held by the what-if fence what-if-1 — no re-seed audit")))
        self.assertEqual(1, len(self.lines("kinowo_es: held by the what-if fence what-if-6 — no re-seed audit")))
        self.assertEqual(2, len(self.lines("held by the what-if fence")), self.output())
        self.assertEqual({"kinowo_de", "kinowo_uk", "kinowo_fr", "kinowo_it"},
                         {str(c["db"]) for c in self.mongosh_calls("staleness")})
        self.assertEqual([], self.mongosh_calls("fence-check") + self.mongosh_calls("targets-fingerprint"))
        self.assertTrue(any(c[-1].endswith("fence-check.js") for c in self.fakes.calls("node")))

    @unittest.skipUnless(NODE, "no node")
    def test_steady_state_publishes_the_digest_audits_through_mongosh_and_dates_every_line(self) -> None:
        self.start(KINOWO_MIRROR_AUDIT_POLLS="1")
        self.until("no three audits", lambda: len(self.mongosh_calls("staleness")) >= 3)
        [published] = glob.glob(self.path("tmp", "kinowo-mirror-targets.*"))
        with open(published) as f:
            digest = f.read().strip()
        with open(os.path.join(cli_harness.ROOT, "common/src/main/resources/debug-mirror-collections.json")) as f:
            names = {c["name"] for c in json.load(f)["collections"]}
        self.assertRegex(digest, rf"^{len(names)}-[0-9a-f]{{8}}$")
        expected = subprocess.run([str(NODE), os.path.join(HERE, "nodb.js"), "--file", os.path.join(HERE, "mirror-targets.js"),
                                   "--file", os.path.join(HERE, "targets-fingerprint.js")],
                                  capture_output=True, text=True, check=True).stdout.strip()
        self.assertEqual(expected, digest)
        self.assertEqual({"ping", "staleness", "tail"}, {str(c["kind"]) for c in self.mongosh_calls()})
        undated = [line for line in self.output().splitlines() if not STAMPED.match(line)]
        self.assertEqual([], undated)


if __name__ == "__main__":
    unittest.main()
