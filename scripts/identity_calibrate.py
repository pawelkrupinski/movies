#!/usr/bin/env python3
"""Regenerate the identity resolver's calibrated evidence model from data (docs/design/identity-resolver.md §14).


  identity/src/main/resources/identity-decorations.json     the learned venue decorations, with provenance (learned
                                                           first: the title shapes the weights are fitted on read them)
  identity/src/main/resources/identity-weights.json         weights, calibration map, thresholds, cannot-link rules
  test/resources/fixtures/identity/identity-labels.json.gz  the labelled set, with its train/calibration/test split
  $REPORT/calibration-report.{md,json}, contradicted-prod-resolutions.tsv, prod-cross-check-mismatches.tsv

Usage:
  CORPORA=… FIXTURES=… PROD=… [REPORT=…] [EPSILON=certified|<rate>] [CASES=<jsonl>] [WEIGHTS=… LABELS=…]
    scripts/identity-calibrate.sh [--extract-prod]
  … scripts/identity-calibrate.sh --refit | --unified | --takes | --decorations-only | --refit-signal <signal>

Inputs:
  CORPORA   dir holding cinema-scrapes-<cc>.json.gz (the recorder's full corpora, run 36153174348)
  FIXTURES  dir holding enrichment-<cc>/ (the matching recorded enrichment trees, real directories)
  PROD      dir of the production snapshot (prod-<db>.jsonl). With --extract-prod the snapshot is (re)read from
            production first: READ-ONLY find() over movies + movie_slots of every country database
            (scripts/identity-calibrate/extract-prod.js, by identity_calibrate_extract.py), through the local prod
            tunnel (scripts/local-mirror/prod-tunnel.sh's prod_tunnel_run) with MONGODB_URI -- exported, else the
            Touch ID vault's (the tool re-runs itself under `secrets run movies MONGODB_URI`), rewritten to the tunnel.
            Without PROD no listing has a proposal to corroborate, so nothing is labelled.
  CASES     {"listingKey","tmdbId","name"} lines whose probabilities the report shows under both the naive-Bayes and
            the joint model (evaluation only). WEIGHTS/LABELS redirect the two artefacts. COUNTRIES narrows.
  VERSION   names the artefact (default calibration-<TODAY>, decorations-<TODAY> for --decorations-only); TODAY
            (YYYY-MM-DD, default today in UTC) is the clock, injectable for the test.

The report formats numbers in the JVM locale: every sbt runs in en-US ("0.97", not "0,97"; scripts/lib/identity_jvm.py).

--refit re-tunes the shipped identity-weights.json on the unmatched-cluster ratchet (no CORPORA, FIXTURES or PROD: the
checked-in captures and identity-judgements.tsv) -- the weekly identity-refit.yml job's measure, run by hand
(scripts/identity-refit --skip-checks; scripts/identity-refit runs its checks too; §14.10). RELEARN=<signal,…> refits
those signals from the fresh units alone.

--unified re-emits the unified evidence model's rows (§20: integration.IdentityUnifiedDataset over the recorded corpora,
the unmatched-cluster fixture and its labels, offline -- FAMILY_SEED and POSTER_CACHE optional, MONGODB_URI a
throwaway) into test/resources/fixtures/identity-unified/training.tsv.gz (and takes.tsv.gz), then refits
identity-unified-weights.json from them (scripts.IdentityUnifiedFit), its report in $REPORT/unified-report.md.
--takes re-emits the same way but checks in only the takes (takes.tsv.gz, the rows today's stack takes, which
MatchedTakesLabelSpec grades): the training rows, and the weights and rules fitted from them, stay as they are.

--decorations-only relearns identity-decorations.json alone (CORPORA and FIXTURES only; PROD is not read) without
refitting the weights (scripts.IdentityDecorationsLearn).

--refit-signal <listing-film signal> refits that one table, and the cannot-links reading it, in the shipped artefact
(scripts.IdentityCalibrate.refitSignal): every other weight, rule, the calibration map and the cut stay.

Exit: sbt's (or identity-refit's) status; 1 when an input is missing, MONGODB_URI cannot be had or prod does not answer.
Tested by scripts/identity_calibrate_test.py against a fake sbt, ssh, mongosh and vault and a stand-in identity-refit."""
from __future__ import annotations

import datetime
import os
import shutil
import sys
from typing import Dict, List, Sequence

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
sys.path.insert(0, HERE)
import machine  # noqa: E402
import identity_calibrate_extract  # noqa: E402
import identity_jvm  # noqa: E402
import tool_process  # noqa: E402
import tool_python  # noqa: E402
import tool_shell  # noqa: E402

ROOT = os.path.dirname(HERE)
import vault_env  # noqa: E402

TAG = "identity-calibrate"
DEFAULT_REPORT = "target/identity-calibration"
UNIFIED = "test/resources/fixtures/identity-unified"
NEEDS = {"CORPORA": "set CORPORA to the dir of cinema-scrapes-<cc>.json.gz",
         "FIXTURES": "set FIXTURES to the dir of enrichment-<cc>/ trees",
         "PROD": "set PROD to the dir of the production snapshot (prod-<db>.jsonl)"}


class Missing(Exception):
    """An input the mode needs is not given: the tool stops, naming it."""


class Calibration:
    def __init__(self, env: Dict[str, str], runner: tool_process.Runner, today: str) -> None:
        self.env, self.runner, self.today = env, runner, today

    def value(self, name: str, default: str = "") -> str:
        return self.env.get(name) or default

    def need(self, name: str) -> str:
        if not self.env.get(name):
            raise Missing(f"{name}: {NEEDS[name]}")
        return self.env[name]

    def flag(self, flag: str, name: str) -> List[str]:
        """`--flag $NAME` when NAME is set (the shell's ${NAME:+--flag $NAME})."""
        return [flag, self.env[name]] if self.env.get(name) else []

    def sbt(self, heap: str, words: Sequence[str], env: Dict[str, str]) -> int:
        """One batch sbt, en-US, its command the words as one argument."""
        argv = [*identity_jvm.SBT, f"-J-Xmx{heap}", "-batch", " ".join(words)]
        return self.runner.run(tool_process.Command(argv, env))

    def refit(self) -> int:
        report = self.value("REPORT", DEFAULT_REPORT)
        argv = ["scripts/identity-refit", "--skip-checks", "--report", f"{report}/refit-report.md",
                *self.flag("--relearn", "RELEARN")]
        return self.runner.run(tool_process.Command(argv, self.env))

    def unified(self, corpora: str, fixtures: str, takes_only: bool) -> int:
        """The unified rows re-emitted, the takes checked in, and (not for --takes) the rows and their refit."""
        report = self.value("REPORT", DEFAULT_REPORT)
        out = f"{report}/unified"
        os.makedirs(out, exist_ok=True)
        env = {**self.env, "KINOWO_IDENTITY_FULL": self.value("COUNTRIES", "pl,uk,de,es,us"),
               "KINOWO_IDENTITY_CORPUS_DIR": corpora, "KINOWO_FIXTURE_ROOT": fixtures}
        for name in ("FAMILY_SEED", "POSTER_CACHE"):
            if self.env.get(name):
                env[f"KINOWO_IDENTITY_{name}"] = self.env[name]
        emitted = self.sbt("12g", ["worker/IntegrationTest/runMain integration.IdentityUnifiedDataset", "--out", out], env)
        if emitted != 0:
            return emitted
        shutil.copyfile(f"{out}/takes.tsv.gz", f"{UNIFIED}/takes.tsv.gz")
        if takes_only:
            return 0
        shutil.copyfile(f"{out}/training.tsv.gz", f"{UNIFIED}/training.tsv.gz")
        return self.sbt("8g", ["worker/Test/runMain scripts.IdentityUnifiedFit", "--report", f"{report}/unified-report.md"],
                        self.env)

    def decorations(self, corpora: str, fixtures: str) -> int:
        version = self.value("VERSION", f"decorations-{self.today}")
        return self.sbt("12g", ["worker/Test/runMain scripts.IdentityDecorationsLearn", "--corpora", corpora,
                                "--fixtures", fixtures, "--version", version], self.env)

    def refit_signal(self, signal: Sequence[str], inputs: List[str]) -> int:
        if not signal:
            print(f"{TAG}: --refit-signal needs the listing-film signal to refit", file=sys.stderr)
            return 1
        return self.sbt("12g", ["worker/Test/runMain scripts.IdentityCalibrate", "--refit-signal", signal[0], *inputs,
                                *self.flag("--countries", "COUNTRIES"), *self.flag("--weights", "WEIGHTS")], self.env)

    def extract_prod(self, argv: Sequence[str], prod: str) -> int:
        """Production's snapshot read again, READ-ONLY, through a prod tunnel held for the reads' length."""
        rerun = tool_python.isolated(os.path.abspath(__file__), *argv, python=sys.executable)
        if not vault_env.exec_under_vault(["MONGODB_URI"], rerun):
            print("[vault] MONGODB_URI not set, and no `secrets` CLI on PATH to read it from the vault", file=sys.stderr)
            return 1
        reads = tool_python.isolated(identity_calibrate_extract.__file__, prod, python=sys.executable)
        return self.runner.run(tool_shell.shell_command(
            self.env, "scripts/local-mirror/prod-tunnel.sh", "prod_tunnel_run",
            [TAG, self.env["MONGODB_URI"], identity_calibrate_extract.URI, *reads]))

    def calibrate(self, inputs: List[str]) -> int:
        return self.sbt("12g", ["worker/Test/runMain scripts.IdentityCalibrate", *inputs,
                                "--report", self.value("REPORT", DEFAULT_REPORT),
                                "--epsilon", self.value("EPSILON", "certified"),
                                "--version", self.value("VERSION", f"calibration-{self.today}"),
                                *self.flag("--countries", "COUNTRIES"), *self.flag("--cases", "CASES"),
                                *self.flag("--weights", "WEIGHTS"), *self.flag("--labels", "LABELS")], self.env)

    def main(self, argv: List[str]) -> int:
        mode = argv[0] if argv else ""
        if mode == "--refit":
            return self.refit()
        corpora, fixtures = self.need("CORPORA"), self.need("FIXTURES")
        if mode in ("--unified", "--takes"):
            return self.unified(corpora, fixtures, mode == "--takes")
        if mode == "--decorations-only":
            return self.decorations(corpora, fixtures)
        prod = self.need("PROD")
        inputs = ["--corpora", corpora, "--fixtures", fixtures, "--prod", prod]
        if mode == "--refit-signal":
            return self.refit_signal(argv[1:2], inputs)
        if mode == "--extract-prod":
            extracted = self.extract_prod(argv, prod)
            if extracted != 0:
                return extracted
        return self.calibrate(inputs)


def main(argv: List[str]) -> int:
    if argv[:1] in (["-h"], ["--help"]):
        print(__doc__)
        return 0
    os.chdir(ROOT)
    env = dict(machine.environment())
    today = env.get("TODAY") or datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%d")
    try:
        return Calibration(env, tool_process.SubprocessRunner(), today).main(argv)
    except Missing as missing:
        print(f"{TAG}: {missing}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
