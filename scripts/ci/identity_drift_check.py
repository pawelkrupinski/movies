#!/usr/bin/env python3
"""THE INCREMENTAL = WHOLE GATE, for one country in its hermetic `takes-drift-1` / `takes-drift-2` rows: replay
production's incremental identity sequence (scripts.IdentityDrift: boot with answers missing, ticks, new scrapes,
re-scrapes, listings leaving and returning, deploys with and without a rules change, through the stores' own BSON)
over the pinned recording the row laid, on the row's FIXED seeds (test/resources/fixtures/drift-gate-seeds.tsv,
scripts/ci/drift_seeds.py), and judge two things:

  1. DIVERGENCES -- a listing whose take at a checkpoint differs from a whole resolve of the same listings -- fail
     the head outright, whatever its base did, unless test/resources/fixtures/identity-drift-allow.tsv names it with
     its reason: incremental = whole is a correctness property every commit must hold, not a flip.
  2. FLIPS -- a listing whose served take, recorded at every projection of the replay, goes back to a film it left,
     changes more than once, or changes where none of its deciding inputs did (services.identity.TakeFlips: prod's
     `kinowo_identity_listing_flips`) -- are judged DIFFERENTIALLY (the owner, 2026-11-04: "a gate that only fails on
     what the change adds"; docs/design/identity-no-drift.md, "The differential gate"): the row replays the same seeds
     over the same recording at the change's BASE, and fails only on a flip the head shows and the base does not
     (scripts/ci/drift_diff.py). A flip both show is PRE-EXISTING: printed as a `pre-existing (backlog)` line and never
     failed -- it is the backlog's (test/resources/fixtures/identity-flip-backlog.tsv), burnt down by its owner, and
     found by the nightly explorer (.github/workflows/identity-drift-explorer.yml), not a red on whichever commit
     replays its seed. A newborn check's TEMPORARY rows on identity-flips-allow.tsv still pass its own head-only flips.

THE BASE, per seed (identity_drift_check_base.py): the newest first-parent ancestor (DRIFT_BASE_DEPTH, 60) whose result
for that seed main STORED GREEN -- so a flip a red commit added stays the change's until it is fixed -- else the parent
(on main) or the merge-base with main (a branch); DRIFT_BASE names one outright. A base whose IDENTITY CODE (every input
the replay reads: identity_drift_check_store.replay_input) is the head's has the head's flips: nothing is replayed
twice, every flip is pre-existing -- so a commit touching no identity code can never be red on flips.
THE STORE (DRIFT_STORE, a release; empty: nothing stored or read; identity_drift_check_store.py): each green head on main
stores each seed's flips as `drift-green-<cc>-<seed>-<key>.tsv.gz`, and a base resolved here as `drift-calc-...` (any
ref: the key names the code, the recording and the seed, never the ref); the walk above reads only the green ones.
THE HEAD'S REPLAY IS A FUNCTION OF ITS INPUTS (DRIFT_TWICE proves it daily): stored as `drift-head-<cc>-<seeds>-<key>`,
its key everything the replay can read -- the replay's tree, the recording, the JDK and the heap -- a head whose inputs a
run already replayed reads that replay instead of running it again. Never read: DRIFT_TWICE (it exists to replay), the
explorer, DRIFT_FRESH=true (a profiled run), and no DRIFT_STORE (the A/A passes): those always replay.

THE SCENARIO GUARD: each replay writes what its harness DREW per seed (`scenario-<cc>.tsv`, scripts.IdentityDrift.Scenario),
and the base's is kept beside its flips. Before any flip is judged, head and base must have met the same scenario on each
seed (scripts/ci/drift_scenario.py): else the row is a HARNESS ERROR (exit 2, no verdict on flips), loudly. A base stored
before the guard has no scenario: said, not compared.

A RED HERE is fixed in the incremental state that diverges (a lag, a budget, queue order, a cache, a restart key), never
by a new resolver rule (the owner, 2026-10-29). docs/design/identity-no-drift.md, "Fixing a drift or flip red".

usage: identity-drift-check.sh <cc> <out dir>   (this file is its logic; the .sh is the name its callers run)
  env: DRIFT_SEED_LIST (the row's seeds, space- or comma-separated), DRIFT_SEED (one seed), DRIFT_SEEDS (seeds 1..N; 2 by
       default); DRIFT_BASE, DRIFT_BASE_DEPTH, DRIFT_BASE_STEPS, DRIFT_STORE, DRIFT_STORE_ASSETS; DRIFT_TWICE=true: replay
       the head's seeds a second time into <out>/again and fail when the two runs' TSVs differ
       (scripts/ci/drift-replays-compare.sh); HEAP (sbt -Xmx, default 6g); GITHUB_STEP_SUMMARY; GH_TOKEN for the store;
       DRIFT_EXPLORE (the explorer: the head alone, never red, <out>/explore-<cc>.tsv; DRIFT_EXPLORE_REF names the output).
Writes <out>/drift-<cc>.tsv and <out>/flips-<cc>.tsv (the head's), <out>/base/ (each seed's base flips and how it was
had), <out>/diff.txt, <out>/new-<seed>.tsv, <out>/seed (a red one first), <out>/reproduce.txt, <out>/report.txt,
<out>/scenario*.txt|tsv and <out>/verdict.txt: one line naming the divergences and the flips the change adds apart
(agentctl and the ::error read it: "... N flip(s) on the way ..."; a scenario mismatch: "harness error: ...").
Exit: 0 no divergence and no flip the change adds, 1 some, 2 it could not run or head and base met different scenarios.
Tested by identity_drift_check_test.py."""
from __future__ import annotations

import datetime
import hashlib
import os
import re
import sys
import tarfile
from typing import List, Mapping, Optional

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = [HERE, os.path.join(os.path.dirname(HERE), "lib")]
import identity_drift_check_base as base_  # noqa: E402
import identity_drift_check_judge as judge_  # noqa: E402
import identity_drift_check_replay as replay_  # noqa: E402
import identity_drift_check_store as store_  # noqa: E402
from cli import tool_actions  # noqa: E402
import tool_process  # noqa: E402

SUMMARY = re.compile(r"^## [a-z]+: ")
FLIPS = re.compile(r"^## [a-z]+ flips: ")
FLIPS_ANY = re.compile(r"^## [a-z]* flips: ")
REPORTED = re.compile(r"^(## |  [0-9]+ listing|      )")


def seeds_of(env: Mapping[str, str]) -> List[str]:
    if env.get("DRIFT_SEED_LIST"):
        return env["DRIFT_SEED_LIST"].replace(",", " ").split()
    if env.get("DRIFT_SEED"):
        return [env["DRIFT_SEED"]]
    return [str(n) for n in range(1, int(env.get("DRIFT_SEEDS") or "2") + 1)]


class Check:
    def __init__(self, cc: str, out: str, root: str, env: Mapping[str, str], runner: tool_process.Runner,
                 processes: replay_.Processes, actions: tool_actions.Actions) -> None:
        self.cc, self.out, self.root, self.env, self.actions = cc, out, root, env, actions
        self.heap = env.get("HEAP") or "6g"
        self.fixtures = os.path.join(root, "test/resources/fixtures")
        self.keys = store_.Keys(root, cc, runner, env)
        self.store = store_.Store(env.get("DRIFT_STORE", ""), root, cc, runner, env)
        self.replayer = replay_.Replayer(root, cc, self.heap, out, env, processes)
        self.bases = base_.Bases(self.keys, self.store, self.replayer, env, out)
        self.judge = judge_.Judge(root, cc, out, env, runner, actions)
        fixtures = self.fixtures
        self.head_options = ["--allow", f"{fixtures}/identity-drift-allow.tsv",
                             "--flips-allow", f"{fixtures}/identity-flips-allow.tsv",
                             "--labels", f"{fixtures}/identity-judgements.tsv"]

    def path(self, *parts: str) -> str:
        return os.path.join(self.out, *parts)

    @staticmethod
    def say(line: str, to_stderr: bool = False) -> None:
        print(line, file=sys.stderr if to_stderr else sys.stdout, flush=True)

    def run(self) -> int:
        os.makedirs(self.path("base"), exist_ok=True)
        self.seeds = seeds_of(self.env)
        if not self.seeds:
            self.say(f"identity drift ({self.cc}): no seed to replay", to_stderr=True)
            return 2
        self.seed_list, self.seed_said = ",".join(self.seeds), " ".join(self.seeds)
        with open(self.path("seed"), "w") as f:
            f.write("".join(s + "\n" for s in self.seeds))
        if not os.path.isfile(os.path.join(self.fixtures, f"corpus/cinema-scrapes-{self.cc}.json.gz")):
            self.say(f"identity drift ({self.cc}): no recorded corpus in {self.fixtures}/corpus", to_stderr=True)
            return 2
        self.keys.recording_key(self.fixtures)
        self.store.list(self.path("base", "assets.txt"))
        try:
            return self.judged()
        except replay_.Stop:
            return 2

    def judged(self) -> int:
        self.head_code = self.keys.code_of("HEAD")
        self.head_replay()
        open_divergences = self.read_report()
        if open_divergences is None:
            return 2
        if self.env.get("DRIFT_EXPLORE"):
            return self.explore()
        self.bases.choose(self.seeds, self.head_code)
        if self.judge.scenarios(self.seeds, self.seed_said):
            return 2
        code, result = self.judge.differential(self.seeds, self.seed_said)
        if code:
            return code
        self.judge.conclude(open_divergences, result, self.seed_said)
        if self.env.get("DRIFT_TWICE", "false") == "true":
            twice = self.twice()
            if twice:
                return twice
        red = open_divergences + result.new
        if red == 0 and self.env.get("GITHUB_REF", "") == "refs/heads/main":
            self.keep_green()
        return 1 if red else 0

    # ── the head's replay, or the stored replay of the same inputs ──────────────────────────

    def head_asset(self) -> str:
        key = hashlib.sha256("".join(f"{v}\n" for v in (self.seed_list, self.keys.recording, self.env.get("JAVA_HOME", ""),
                                                        self.heap, self.head_code)).encode()).hexdigest()[:24]
        return f"drift-head-{self.cc}-{self.seed_list.replace(',', '_')}-{key}.tar.gz"

    def memo(self) -> bool:
        return bool(self.store.release) and self.env.get("DRIFT_TWICE", "false") != "true" and \
            not self.env.get("DRIFT_EXPLORE") and self.env.get("DRIFT_FRESH", "false") != "true"

    def head_replay(self) -> None:
        asset = self.head_asset()
        if self.memo() and self.read_stored(asset):
            return
        self.replayer.replay(self.root, self.out, self.seed_list, self.head_options)
        if self.memo() and self.replay_judged():
            self.store_head(asset)

    def read_stored(self, asset: str) -> bool:
        packed = self.path("head-replay.tar")
        read_it = self.store.fetch(asset, packed) and unpacked(packed, self.out)
        if os.path.exists(packed):
            os.remove(packed)
        if read_it:
            self.say(f"identity drift ({self.cc}): the head's replay of seed(s) {self.seed_said} is stored under the same "
                     f"inputs ({asset}): read, not replayed")
        return read_it

    def replay_judged(self) -> bool:
        """The replay wrote its TSVs and both its verdict lines: worth storing."""
        log = read(self.path("sbt.log"))
        return self.wrote_tsvs(self.out) and has(SUMMARY, log) and has(FLIPS, log)

    def store_head(self, asset: str) -> None:
        """What the replay wrote, nothing the check adds after it."""
        tar = os.path.join(self.env.get("RUNNER_TEMP") or "/tmp", f"head-replay-{self.cc}.tar")
        with tarfile.open(tar, "w") as t:
            for name in ["sbt.log"] + sorted(n for n in os.listdir(self.out) if n.endswith(f"-{self.cc}.tsv")):
                t.add(self.path(name), arcname=name)
        self.store.keep(tar, asset)

    def wrote_tsvs(self, directory: str) -> bool:
        return all(os.path.isfile(os.path.join(directory, f"{k}-{self.cc}.tsv")) for k in ("drift", "flips"))

    def read_report(self) -> Optional[int]:
        """The head's report checked and printed up to its flips; its open divergences (None: no verdict, said)."""
        report = [line for line in read(self.path("sbt.log")).split("\n") if REPORTED.match(line)]
        with open(self.path("report.txt"), "w") as f:
            f.write("".join(line + "\n" for line in report))
        summary = first(SUMMARY, report)
        flips = first(FLIPS, report)
        why = self.no_verdict_why(summary, flips)
        if why:
            sys.stderr.write(replay_.tail(self.path("sbt.log")))
            self.say(f"identity drift ({self.cc}): {why}", to_stderr=True)
            return None
        found = int(re.sub(r".* ([0-9]+) divergence\(s\).*", r"\1", summary))
        allowed = int(re.sub(r".* ([0-9]+) allow-listed.*", r"\1", summary))
        # the head's own report up to its flips: its flip rows are not the verdict, the differential is
        before = report[:next(n for n, line in enumerate(report) if FLIPS_ANY.match(line))]
        sys.stdout.write("".join(line + "\n" for line in before))
        self.say(re.sub(r", [0-9]+ known$", "", flips))
        return found - allowed

    def no_verdict_why(self, summary: str, flips: str) -> str:
        if not self.wrote_tsvs(self.out):
            return "the replay wrote nothing"
        if not summary:
            return f"no verdict (sbt exit {self.replayer.code})"
        return "" if flips else f"no flips verdict (sbt exit {self.replayer.code})"

    # ── the explorer, the double run, the green head's store ────────────────────────────────

    def explore(self) -> int:
        target = self.path(f"explore-{self.cc}.tsv")
        done = tool_process.capture_loudly(self.keys.runner, tool_process.Command(
            ["python3", os.path.join(self.root, "scripts/ci/drift_explorer.py"), "new-flips", "--backlog",
             f"{self.fixtures}/identity-flip-backlog.tsv", "--date", datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%d"),
             "--ref", self.env.get("DRIFT_EXPLORE_REF", ""), self.path(f"flips-{self.cc}.tsv")], self.env))
        with open(target, "w") as f:
            f.write(done.stdout)
        if done.code != 0:
            return self.judge.no_verdict(f"identity drift ({self.cc}): the explorer could not read the head's flips")
        rows = done.stdout.split("\n")[1:]
        for row in (r.split("\t") for r in rows if r):
            self.say(f"      explored: {row[0]} | {row[1]} [seed {row[3]}] -- not in the backlog")
        said = (f"explorer ({self.cc}): {len(done.stdout.splitlines()) - 1} flip(s) no backlog row names, on seed(s) "
                f"{self.seed_said} (main, head alone; never red)")
        self.say(said)
        self.judge.verdict(said)
        self.actions.summary(f"### explorer ({self.cc}), seed(s) {self.seed_said}\n\n```\n{done.stdout}```\n")
        return 0

    def twice(self) -> int:
        again = self.path("again")
        self.replayer.replay(self.root, again, self.seed_list, self.head_options)
        if not self.wrote_tsvs(again):
            sys.stderr.write(replay_.tail(os.path.join(again, "sbt.log")))
            self.say(f"identity drift ({self.cc}): the second replay wrote nothing", to_stderr=True)
            return 2
        return self.keys.runner.run(tool_process.Command(
            [os.path.join(self.root, "scripts/ci/drift-replays-compare.sh"), self.cc, self.out, again,
             self.path("verdict.txt")], self.env))

    def keep_green(self) -> None:
        """A green head on main is the base of what follows: its flips (and scenario) stored per seed."""
        for s in self.seeds:
            for name, column, kind in ((f"flips-{self.cc}.tsv", 1, "green"), (f"scenario-{self.cc}.tsv", 0, "greenscenario")):
                source = self.path(name)
                if not os.path.isfile(source):
                    continue
                lines = read(source).split("\n")
                kept = [lines[0]] + [r for r in lines[1:] if r and r.split("\t")[column:column + 1] == [s]]
                target = self.path("base", f"head-{name.split('-')[0]}-{s}.tsv")
                with open(target, "w") as f:
                    f.write("".join(line + "\n" for line in kept))
                self.store.keep(target, self.keys.asset(kind, self.head_code, s))


def read(path: str) -> str:
    try:
        with open(path, errors="replace") as f:
            return f.read()
    except OSError:
        return ""


def first(pattern: "re.Pattern[str]", lines: List[str]) -> str:
    return next((line for line in lines if pattern.match(line)), "")


def has(pattern: "re.Pattern[str]", text: str) -> bool:
    return any(pattern.match(line) for line in text.split("\n"))


def unpacked(packed: str, into: str) -> bool:
    try:
        with tarfile.open(packed) as t:
            t.extractall(into, filter="data")  # no path outside `into`, no device, no link out
        return True
    except (OSError, tarfile.TarError):
        return False


def main(argv: List[str]) -> int:
    """The composition point: the checkout, the environment, the real runner, processes and step commands."""
    if len(argv) < 2:
        print("usage: identity-drift-check.sh <cc> <out dir>", file=sys.stderr)
        return 2
    os.makedirs(argv[1], exist_ok=True)
    out = os.path.abspath(argv[1])
    root = os.path.dirname(os.path.dirname(HERE))
    return Check(argv[0], out, root, os.environ, tool_process.SubprocessRunner(), replay_.SubprocessProcesses(),
                 tool_actions.Actions(os.environ, sys.stdout)).run()


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
