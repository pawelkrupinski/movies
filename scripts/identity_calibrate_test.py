#!/usr/bin/env python3
"""scripts/identity-calibrate.sh, black-box: the sbt command each mode runs -- the calibration, --refit-signal,
--decorations-only, --unified, --takes and --refit -- in the en-US locale, its artefact versioned by the injected date
(TODAY); the inputs each mode needs; --extract-prod's READ-ONLY snapshot of every country database through the prod
tunnel, the vault asked for MONGODB_URI when it is not exported, and its stop when the tunnel cannot be opened. No
vault, no ssh, no sbt, no Mongo: `secrets`, `ssh`, `nc`, `mongosh` and `sbt` are fakes, and scripts/identity-refit a
stand-in, in a scratch copy of the scripts (the tool cds to its checkout and checks rows in there). Ported from
identity-calibrate-test.sh case for case, with the vault, a failed emit and a --refit-signal naming no signal besides;
the same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import shutil
import sys
import unittest
from typing import Dict, List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from harness import cli_harness  # noqa: E402

ROOT = cli_harness.ROOT
TOOL = cli_harness.tool_path("scripts/identity_calibrate.py", "scripts/identity-calibrate.sh")
LOCALE = ["-J-Duser.language=en", "-J-Duser.country=US"]
FAKE_URI = "mongodb://u:p@10.20.0.13:27017/kinowo?authSource=admin"
TUNNEL_URI = "mongodb://u:p@127.0.0.1:27017/kinowo?authSource=admin&directConnection=true"
DATABASES = ["kinowo", "kinowo_uk", "kinowo_de", "kinowo_es", "kinowo_us"]
UNIFIED = "tree/test/resources/fixtures/identity-unified"
KNOBS = ("CORPORA", "FIXTURES", "PROD", "REPORT", "EPSILON", "CASES", "WEIGHTS", "LABELS", "VERSION", "COUNTRIES",
         "RELEARN", "FAMILY_SEED", "POSTER_CACHE", "MONGODB_URI", "KINOWO_MONGO_SSH", "PROD_TUNNEL_PORT")
# sbt: the unified emit leaves its rows under --out; FAKE_SBT_FAIL fails every call
SBT = '''[ -z "$FAKE_SBT_FAIL" ] || exit 3
all="$*"
case "$all" in *IdentityUnifiedDataset*) out=${all##*--out }; mkdir -p "$out"; echo rows > "$out/training.tsv.gz"; echo takes > "$out/takes.tsv.gz" ;; esac'''
# the tunnel's ssh: up (and noted, to see it closed) unless NO_TUNNEL exists, when it exits as a refused login does
SSH = '''[ ! -e "$NO_TUNNEL" ] || exit 255
echo $$ >> "$SSH_PIDS"; touch "$TUNNEL_UP"; exec sleep 30'''
# mongosh: the extract prints its database's row and notes the URI it read; a ping answers while the tunnel is up
MONGOSH = '''case "$*" in
  *extract-prod.js*) echo "$KINOWO_CALIBRATE_DB $KINOWO_MONGOSH_URI" >> "$READS"; echo "{\\"db\\":\\"$KINOWO_CALIBRATE_DB\\"}" ;;
  *) [ -e "$TUNNEL_UP" ] ;;
esac'''
# `secrets run movies SPEC... -- CMD`: CMD with each SPEC's variable set to a fake in-cluster URI, never a real secret
SECRETS = f'''shift 2
while [ "$1" != "--" ]; do export "${{1%%=*}}={FAKE_URI}"; shift; done
shift
exec "$@"'''


class IdentityCalibrateTest(cli_harness.ToolCase):
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.copy_scripts("tree")
        shutil.copy2(TOOL, self.path("tree", "scripts", os.path.basename(TOOL)))
        mirror = os.path.join(ROOT, "scripts", "local-mirror")
        os.makedirs(self.path("tree", "scripts", "local-mirror"))
        for name in ("prod-tunnel.sh", "mongosh-at.sh", "no-app-registration.js"):
            shutil.copy2(os.path.join(mirror, name), self.path("tree", "scripts", "local-mirror", name))
        self.write("tree/scripts/identity-calibrate/extract-prod.js", "")
        os.makedirs(self.path(UNIFIED))
        # scripts/identity-refit, which --refit hands off to: a stand-in whose calls the fakes log
        refit = self.write("tree/scripts/identity-refit", '#!/bin/sh\nexec identity-refit-stand-in "$@"\n')
        os.chmod(refit, 0o755)
        self.fakes.script("identity-refit-stand-in", "")
        self.fakes.script("sbt", SBT)
        self.fakes.script("ssh", SSH)
        self.fakes.script("nc", '[ -e "$TUNNEL_UP" ]')
        self.fakes.script("mongosh", MONGOSH)
        self.fakes.script("secrets", SECRETS, shell="/usr/bin/env bash")

    def calibrate(self, *args: str, **knobs: str) -> cli_harness.Done:
        env: Dict[str, Optional[str]] = {name: None for name in KNOBS}
        env.update({"TODAY": "2001-02-03", "PROD_TUNNEL_PORT": "27017", "TUNNEL_UP": self.path("tunnel-up"),
                    "NO_TUNNEL": self.path("no-tunnel"), "SSH_PIDS": self.path("ssh-pids"),
                    "READS": self.path("reads"), "FAKE_SBT_FAIL": ""})
        env.update(knobs)
        self.fakes.clear()
        return self.run_tool(*args, env=env, tool=self.path("tree", "scripts", os.path.basename(TOOL)))

    def sbt(self) -> List[List[str]]:
        """Each sbt call, its one command argument's spacing made single."""
        return [[*call[:-1], " ".join(call[-1].split())] for call in self.fakes.calls("sbt")]

    def reads(self) -> List[str]:
        return self.read("reads").splitlines() if os.path.exists(self.path("reads")) else []

    def test_the_calibration_one_sbt_run_en_us_versioned_by_todays_date(self) -> None:
        done = self.calibrate(CORPORA="c", FIXTURES="f", PROD="p")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([[*LOCALE, "-J-Xmx12g", "-batch",
                           "worker/Test/runMain scripts.IdentityCalibrate --corpora c --fixtures f --prod p "
                           "--report target/identity-calibration --epsilon certified --version calibration-2001-02-03"]],
                         self.sbt())

    def test_every_knob_is_passed_on(self) -> None:
        done = self.calibrate(CORPORA="c", FIXTURES="f", PROD="p", VERSION="v9", REPORT="r", EPSILON="0.01",
                              COUNTRIES="pl,de", CASES="k", WEIGHTS="w", LABELS="l")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([[*LOCALE, "-J-Xmx12g", "-batch",
                           "worker/Test/runMain scripts.IdentityCalibrate --corpora c --fixtures f --prod p --report r "
                           "--epsilon 0.01 --version v9 --countries pl,de --cases k --weights w --labels l"]],
                         self.sbt())

    def test_a_missing_input_stops_it_naming_the_input_before_any_sbt(self) -> None:
        done = self.calibrate(CORPORA="c")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("set FIXTURES", done.output)
        self.assertEqual([], self.sbt())
        done = self.calibrate(CORPORA="c", FIXTURES="f")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("set PROD", done.output)
        self.assertEqual([], self.sbt())

    def test_decorations_only_needs_no_prod_and_is_versioned_by_todays_date(self) -> None:
        done = self.calibrate("--decorations-only", CORPORA="c", FIXTURES="f")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([[*LOCALE, "-J-Xmx12g", "-batch", "worker/Test/runMain scripts.IdentityDecorationsLearn "
                           "--corpora c --fixtures f --version decorations-2001-02-03"]], self.sbt())

    def test_refit_signal_refits_the_one_signal_named(self) -> None:
        done = self.calibrate("--refit-signal", "titleShape", CORPORA="c", FIXTURES="f", PROD="p", COUNTRIES="uk")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([[*LOCALE, "-J-Xmx12g", "-batch", "worker/Test/runMain scripts.IdentityCalibrate "
                           "--refit-signal titleShape --corpora c --fixtures f --prod p --countries uk"]], self.sbt())

    def test_refit_signal_naming_no_signal_stops_it_before_any_sbt(self) -> None:
        done = self.calibrate("--refit-signal", CORPORA="c", FIXTURES="f", PROD="p")
        self.assertEqual(1, done.code, done.output)
        self.assertIn("signal", done.output)
        self.assertEqual([], self.sbt())

    def test_refit_needs_no_inputs_its_report_beside_the_calibrations(self) -> None:
        done = self.calibrate("--refit")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([["--skip-checks", "--report", "target/identity-calibration/refit-report.md"]],
                         self.fakes.calls("identity-refit-stand-in"))
        done = self.calibrate("--refit", RELEARN="a,b", REPORT="r")
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([["--skip-checks", "--report", "r/refit-report.md", "--relearn", "a,b"]],
                         self.fakes.calls("identity-refit-stand-in"))
        self.assertEqual([], self.sbt())

    def test_unified_re_emits_the_rows_checks_them_in_and_refits_the_unified_weights(self) -> None:
        report = self.path("r")
        done = self.calibrate("--unified", CORPORA="c", FIXTURES="f", REPORT=report)
        self.assertEqual(0, done.code, done.output)
        emit, fit = self.sbt()
        self.assertEqual([*LOCALE, "-J-Xmx12g", "-batch",
                          f"worker/IntegrationTest/runMain integration.IdentityUnifiedDataset --out {report}/unified"],
                         emit)
        self.assertEqual([*LOCALE, "-J-Xmx8g", "-batch",
                          f"worker/Test/runMain scripts.IdentityUnifiedFit --report {report}/unified-report.md"], fit)
        self.assertEqual("rows\n", self.read(f"{UNIFIED}/training.tsv.gz"))
        self.assertEqual("takes\n", self.read(f"{UNIFIED}/takes.tsv.gz"))

    def test_takes_re_emits_the_rows_and_checks_in_only_the_takes(self) -> None:
        self.write(f"{UNIFIED}/training.tsv.gz", "fitted\n")
        done = self.calibrate("--takes", CORPORA="c", FIXTURES="f", REPORT=self.path("r"))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual(1, len(self.sbt()))
        self.assertEqual("takes\n", self.read(f"{UNIFIED}/takes.tsv.gz"))
        # the training rows the fit reads left as they were
        self.assertEqual("fitted\n", self.read(f"{UNIFIED}/training.tsv.gz"))

    def test_a_failed_emit_stops_it_its_status_kept_nothing_checked_in(self) -> None:
        done = self.calibrate("--unified", CORPORA="c", FIXTURES="f", REPORT=self.path("r"), FAKE_SBT_FAIL="1")
        self.assertEqual(3, done.code, done.output)
        self.assertEqual(1, len(self.sbt()))
        self.assertEqual([], os.listdir(self.path(UNIFIED)))

    def test_extract_prod_reads_every_country_database_through_the_tunnel_then_calibrates(self) -> None:
        prod = self.path("prod")
        done = self.calibrate("--extract-prod", CORPORA="c", FIXTURES="f", PROD=prod, MONGODB_URI=FAKE_URI)
        self.assertEqual(0, done.code, done.output)
        # every country database, each over the tunnelled URI, never the in-cluster one
        self.assertEqual([f"{db} {TUNNEL_URI}" for db in DATABASES], self.reads())
        for db in DATABASES:
            self.assertEqual(f'{{"db":"{db}"}}\n', self.read(f"prod/prod-{db}.jsonl"))
            self.assertIn(f"prod snapshot {db}: ", done.stdout)
        self.assertEqual([], self.fakes.calls("secrets"))
        # the tunnel closed by the end
        self.assertEqual(0, cli_harness.still_running(self.path("ssh-pids")))
        # and the calibration run over the snapshot
        [sbt] = self.sbt()
        self.assertIn(f"scripts.IdentityCalibrate --corpora c --fixtures f --prod {prod} ", sbt[-1])

    def test_extract_prod_asks_the_vault_for_an_unexported_mongodb_uri(self) -> None:
        done = self.calibrate("--extract-prod", CORPORA="c", FIXTURES="f", PROD=self.path("prod"))
        self.assertEqual(0, done.code, done.output)
        self.assertEqual([["run", "movies", "MONGODB_URI"]],
                         [call[:call.index("--")] for call in self.fakes.calls("secrets")])
        self.assertEqual([f"{db} {TUNNEL_URI}" for db in DATABASES], self.reads())
        self.assertEqual(1, len(self.sbt()))

    def test_a_tunnel_that_cannot_be_opened_stops_it_before_any_read_or_sbt(self) -> None:
        self.write("no-tunnel", "")
        done = self.calibrate("--extract-prod", CORPORA="c", FIXTURES="f", PROD=self.path("prod"), MONGODB_URI=FAKE_URI)
        self.assertEqual(1, done.code, done.output)
        self.assertEqual([], self.reads())
        self.assertEqual([], self.sbt())


if __name__ == "__main__":
    unittest.main()
