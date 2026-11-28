"""THE DRIFT CHECK'S JUDGEMENT (identity_drift_check.py): THE SCENARIO GUARD (head and base met the same scenario on each
seed, scripts/ci/drift_scenario.py, or no flip is judged: a HARNESS ERROR), THE DIFFERENTIAL (scripts/ci/drift_diff.py:
only a flip the head shows and its base does not fails), the verdict line, the red's seed and one-line reproducer, and
the step summary."""
from __future__ import annotations

import glob
import os
import re
import sys
from dataclasses import dataclass, field
from typing import List, Mapping, Tuple

from cli import tool_actions
import tool_process


def read(path: str) -> str:
    with open(path, errors="replace") as f:
        return f.read()


@dataclass
class Differential:
    new: int = 0
    pre: int = 0
    removed: int = 0
    red: List[str] = field(default_factory=lambda: [])
    calm: List[str] = field(default_factory=lambda: [])


class Judge:
    def __init__(self, root: str, cc: str, out: str, env: Mapping[str, str], runner: tool_process.Runner,
                 actions: tool_actions.Actions) -> None:
        self.root = root
        self.cc = cc
        self.out = out
        self.env = env
        self.runner = runner
        self.actions = actions
        self.fixtures = os.path.join(root, "test/resources/fixtures")

    def path(self, *parts: str) -> str:
        return os.path.join(self.out, *parts)

    def how(self, seed: str) -> str:
        return read(self.path("base", f"how-{seed}.txt")).strip()

    def python(self, script: str, args: List[str], log: str, append: bool = False) -> int:
        command = ["python3", os.path.join(self.root, "scripts/ci", script), *args]
        return self.runner.run(tool_process.Command(command, self.env, log=log, append=append))

    def verdict(self, said: str) -> None:
        with open(self.path("verdict.txt"), "w") as f:
            f.write(said + "\n")

    def no_verdict(self, said: str) -> int:
        print(said, flush=True)
        self.verdict(said)
        return 2

    # ── the scenario guard ──────────────────────────────────────────────────────────────────

    def scenarios(self, seeds: List[str], seed_said: str) -> int:
        """0 when every seed's head and base met one scenario; 2 (said) otherwise."""
        report = self.path("scenario.txt")
        open(report, "w").close()
        harness: List[str] = []
        for s in seeds:
            if self.how(s) == "same-code":
                with open(report, "a") as f:
                    f.write(f"scenario: the base's identity code and harness are the head's on seed {s} -- the same scenario\n")
                continue
            code = self.python("drift_scenario.py", ["compare", "--seed", s, "--head", self.path(f"scenario-{self.cc}.tsv"),
                                                     "--base", self.path("base", f"scenario-{s}.tsv")], report, append=True)
            if code == 1:
                harness.append(s)
            elif code != 0:
                sys.stdout.write(read(report))
                return self.no_verdict(f"identity drift ({self.cc}): the scenarios of seed {s} could not be read")
        text = read(report)
        sys.stdout.write("".join(line + "\n" for line in text.split("\n") if line.startswith("scenario: ")))
        return self.harness_error(harness, text) if harness else 0

    def harness_error(self, seeds: List[str], report: str) -> int:
        sys.stdout.write(report)
        said = (f"harness error: head and base replayed different scenarios on seed(s) {' '.join(seeds)} -- the drift "
                "harness couples its draws to what the code asks (scripts/ci/drift_scenario.py); no flip judged")
        self.verdict(said)
        self.actions.annotate("error", f"identity drift harness ({self.cc}, seed {seeds[0]})", said)
        self.actions.summary(f"### identity drift ({self.cc}): HARNESS ERROR\n\n```\n{report}```\n")
        print(f"identity drift ({self.cc}): {said}", flush=True)
        return 2

    # ── the differential ────────────────────────────────────────────────────────────────────

    def seed_diff(self, seed: str, result: Differential) -> int:
        base = read(self.path("base", f"base-{seed}.txt")).strip()
        how = self.how(seed)
        if how == "same-code":
            side, said = ["--same-code"], f"base {base[:12]} (the same identity code: nothing replayed twice)"
        else:
            side, said = ["--base", self.path("base", f"flips-{seed}.tsv")], f"base {base[:12]} ({how})"
        log = self.path(f"diff-{seed}.txt")
        code = self.diff_run(seed, side, said, log)
        if code > 1:
            sys.stdout.write(read(log))
            return self.no_verdict(f"identity drift ({self.cc}): the differential of seed {seed} could not be read")
        lines = read(log).rstrip("\n").split("\n")
        with open(self.path("diff.txt"), "a") as out:
            out.write("".join(line + "\n" for line in lines[:-1]))
        n, p, r = (int(re.sub(rf".*{k}=([0-9]+).*", r"\1", lines[-1])) for k in ("new", "pre-existing", "removed"))
        result.new, result.pre, result.removed = result.new + n, result.pre + p, result.removed + r
        (result.red if n > 0 else result.calm).append(seed)
        return 0

    def diff_run(self, seed: str, side: List[str], said: str, log: str) -> int:
        """drift_diff.py's stdout into `log`, its stderr this check's."""
        fixtures = self.fixtures
        argv = ["python3", os.path.join(self.root, "scripts/ci/drift_diff.py"), "--cc", self.cc, "--seed", seed, "--head",
                self.path(f"flips-{self.cc}.tsv"), *side, "--base-said", said,
                "--allow", f"{fixtures}/identity-flips-allow.tsv", "--backlog", f"{fixtures}/identity-flip-backlog.tsv",
                "--new-out", self.path(f"new-{seed}.tsv")]
        done = tool_process.capture_loudly(self.runner, tool_process.Command(argv, self.env))
        with open(log, "w") as out:
            out.write(done.stdout)
        return done.code

    def differential(self, seeds: List[str], seed_said: str) -> Tuple[int, Differential]:
        open(self.path("diff.txt"), "w").close()
        result = Differential()
        for s in seeds:
            if self.seed_diff(s, result):
                return 2, result
        sys.stdout.write(read(self.path("diff.txt")))
        with open(self.path("seed"), "w") as f:
            f.write("".join(s + "\n" for s in result.red + result.calm if s))
        if result.pre > 0:
            self.actions.annotate("notice", f"identity drift ({self.cc})",
                                  f"{result.pre} pre-existing (backlog) flip(s) on seed(s) {seed_said} -- in the base too, "
                                  "not failed (identity-flip-backlog.tsv; the explorer files new ones)")
        return 0, result

    # ── the verdict, the reproducer, the summary ────────────────────────────────────────────

    def conclude(self, open_divergences: int, result: Differential, seed_said: str) -> None:
        self.verdict(f"{open_divergences} divergence(s) from a whole resolve and {result.new} flip(s) on the way the "
                     f"change adds over its base, beyond the allow-lists; {result.pre} pre-existing (backlog) flip(s) not "
                     f"failed -- seed(s) {seed_said}")
        judged = " ".join(result.red) or seed_said
        repro = self.reproducer(judged.replace(" ", ","))
        with open(self.path("reproduce.txt"), "w") as f:
            f.write(repro + "\n")
        self.actions.summary(
            f"### identity drift ({self.cc}, seed(s) {seed_said}): incremental vs whole, and the flips the change adds "
            f"over its base\n**{read(self.path('verdict.txt')).rstrip()}**\n```\n{read(self.path('diff.txt'))}```\n"
            f"<details><summary>the head's full report</summary>\n\n```\n{read(self.path('report.txt'))}```\n</details>\n")
        if open_divergences + result.new > 0:
            print(f"identity drift ({self.cc}): red on seed(s) {judged} -- reproduce: {repro}", flush=True)
            self.actions.summary(f"**red on seed(s) {judged}** -- reproduce: `{repro}`\n")

    def reproducer(self, seeds: str) -> str:
        new = sorted(glob.glob(self.path("new-*.tsv"))) or [self.path("new-*.tsv")]
        done = self.runner.capture(tool_process.Command(
            ["python3", os.path.join(self.root, "scripts/ci/drift_seeds.py"), "slices", self.cc, self.fixtures,
             self.path(f"drift-{self.cc}.tsv"), *new], self.env))
        repro = f"IdentityDrift {self.cc} --seed {seeds}"
        for slice_path in (line for line in done.stdout.split("\n") if line):
            relative = slice_path[len(self.root) + 1:] if slice_path.startswith(self.root + "/") else slice_path
            repro += f"; slice: IdentityDrift {self.cc} --slice {relative} --seed {seeds}"
        return repro
