#!/usr/bin/env python3
"""Run the race / concurrency harnesses many times over, to surface the interleavings a single CI run almost never meets.
The nightly stress workflow runs this with 50; a push runs each once.

A harness that fails 1 time in 50 is a real race that will redden some push, some day, and be "fixed" by a rerun. Here
it fails the night it can, with a count. Every iteration runs even after a failure, so the count is honest; the run
fails if any iteration did.

Usage: stress-races.sh <iterations> [unit|it|all]
  unit  the in-JVM harnesses (no Mongo)
  it    the Mongo-backed ones (MONGODB_URI / MONGODB_DB must point at a replica set)

The list is HERE, once (StressAndEmulatorScheduleSpec reads it). A listed harness whose class no longer exists fails the
run rather than being silently dropped; a new harness is added by adding its line.

Stdlib only. nightly-stress.yml runs it by the name scripts/ci/stress-races.sh; tested by stress_races_test.py
beside it."""
from __future__ import annotations

import collections
import os
import re
import shutil
import subprocess
import sys
from typing import Dict, List, Mapping, Sequence, TextIO

HERE = os.path.dirname(os.path.abspath(__file__))
OUT = "target/stress-races"
REPORTS = ("target/test-reports/unit", "target/test-reports/it")

# "<module>/<Config> <fully qualified class>"
UNIT = (
    "worker/Test tools.ConcurrentCandidateProbeSpec",
    "worker/Test services.sharecards.ShareCardConcurrencySpec",
    "common/Test tools.BoundedParallelSpec",
    "worker/Test tools.AdaptiveParallelSpec",
    "scrapers/Test tools.ParallelDetailFetchSpec",
    "worker/Test diagnostics.FilmwebDiffParallelFetchSpec",
)
IT = (
    "web/IntegrationTest integration.HiddenFilmsConcurrentWritesIntegrationSpec",
    "worker/IntegrationTest integration.TaskClaimsAcrossWorkersIntegrationSpec",
)
SCOPES = {"unit": UNIT, "it": IT, "all": UNIT + IT}


def missing(harnesses: Sequence[str]) -> List[str]:
    """An error for every listed class that is not where its line says."""
    errors: List[str] = []
    for line in harnesses:
        target, cls = line.split()
        module, config = target.split("/", 1)
        directory = f"{module}/src/it" if config == "IntegrationTest" else f"{module}/src/test"
        pattern = "class " + cls.rsplit(".", 1)[-1]
        if subprocess.run(["git", "grep", "-qw", "-e", pattern, "--", directory]).returncode != 0:
            errors.append(f"::error::stress-races.sh lists {cls} under {target}, but no such class is in {directory}")
    return errors


def commands(harnesses: Sequence[str]) -> List[str]:
    """One testOnly per module/config: "<target>/testOnly <class> <class>...", the targets sorted."""
    by_target: Dict[str, List[str]] = collections.defaultdict(list)
    for line in harnesses:
        target, cls = line.split()
        by_target[target].append(cls)
    return [f"{target}/testOnly {' '.join(by_target[target])}" for target in sorted(by_target)]


def failed_tests() -> List[str]:
    done = subprocess.run([sys.executable, os.path.join(HERE, "junit_failures.py"), *REPORTS], stdout=subprocess.PIPE,
                          text=True)
    return done.stdout.splitlines()


def iteration(number: int, total: int, runs: Sequence[str], failures: TextIO) -> bool:
    """Every command once; on a failure its log's tail, the failed tests and the reports kept. True when it passed."""
    passed = True
    log = os.path.join(OUT, "iteration.log")
    for command in runs:
        with open(log, "w") as sink:
            ok = subprocess.run(["sbt", "--client", command], stdout=sink, stderr=subprocess.STDOUT).returncode == 0
        if not ok:
            passed = False
            with open(log, errors="replace") as f:
                sys.stdout.write("".join(f.readlines()[-60:]))
    if not passed:
        failures.write("".join(f"{number}\t{test}\n" for test in failed_tests()))
        failures.flush()
        if os.path.isdir("target/test-reports"):
            shutil.copytree("target/test-reports", os.path.join(OUT, f"iteration-{number}"), dirs_exist_ok=True)
    print(f"iteration {number}/{total}: {'ok' if passed else 'FAILED'}", flush=True)
    return passed


def summary(iterations: int, scope: str, failed: int, failures: List[str]) -> str:
    lines = [f"### Race harnesses × {iterations} ({scope})", "", f"{iterations - failed}/{iterations} iterations passed."]
    if failures:
        counts = collections.Counter(line.split("\t", 1)[1] for line in failures)
        lines += ["", "| Failed in | Class | Test |", "|---|---|---|"]
        for test, count in sorted(counts.items(), key=lambda kv: (kv[1], kv[0]), reverse=True):
            cls, _, name = test.partition("\t")
            lines.append(f"| {count}/{iterations} | `{cls}` | {name} |")
    return "\n".join(lines) + "\n"


def stress(iterations: int, scope: str, env: Mapping[str, str]) -> int:
    harnesses = SCOPES[scope]
    errors = missing(harnesses)
    if errors:
        print("\n".join(errors), flush=True)
        return 1
    runs = commands(harnesses)
    for stale in (OUT, *REPORTS):
        shutil.rmtree(stale, ignore_errors=True)
    os.makedirs(OUT)
    failed = 0
    with open(os.path.join(OUT, "failures.tsv"), "w") as failures:
        for number in range(1, iterations + 1):
            failed += 0 if iteration(number, iterations, runs, failures) else 1
    subprocess.run(["sbt", "--client", "shutdown"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    with open(os.path.join(OUT, "failures.tsv")) as f:
        text = summary(iterations, scope, failed, f.read().splitlines())
    sys.stdout.write(text)
    if env.get("GITHUB_STEP_SUMMARY"):
        with open(env["GITHUB_STEP_SUMMARY"], "a") as f:
            f.write(text)
    return 0 if failed == 0 else 1


def main(argv: List[str], env: Mapping[str, str]) -> int:
    if not argv or not argv[0]:
        print("stress-races.sh: iterations", file=sys.stderr)
        return 1
    if not re.fullmatch(r"[1-9][0-9]{0,3}", argv[0]):
        print("iterations must be a number, 1-9999", file=sys.stderr)
        return 2
    scope = argv[1] if len(argv) > 1 else "all"
    if scope not in SCOPES:
        print("scope must be unit, it or all", file=sys.stderr)
        return 2
    top = subprocess.run(["git", "rev-parse", "--show-toplevel"], stdout=subprocess.PIPE, text=True)
    if top.returncode != 0:
        return 1
    os.chdir(top.stdout.strip())
    return stress(int(argv[0]), scope, env)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], os.environ))
