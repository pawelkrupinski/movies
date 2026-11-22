"""THE SKIP GATE and THE VERDICT of the identity takes check (identity_takes_check.py).

THE SKIP GATE (scripts/ci/takes_skip_gate.py; TAKES_SKIP_MODE off, shadow or enforce): whether this commit PROVABLY moves
no take -- a title-rule change alone, the base's every title reading (its trace under its exact inputs) read alike under
the head's rules. Fail-closed: anything it cannot prove is RUN. SHADOW: the takes run as ever, and the gate's answer is
held against them after (THE SHADOW). ENFORCE: a SKIP leaves out the takes diff -- except on every
TAKES_SKIP_SAMPLE_EVERY-th such commit (by its name, default 10), or while the daily full row's marker
(`takes-full-<cc>.txt` on the store, refreshed by every full takes diff on main) is missing or a day old.

THE VERDICT: the model's takes and the final ones diffed (identity_takes_diff.py), the corpus shape's findings
(corpus_shape_diff.py), the verdict's first line the commit status's description. A move or a new finding anywhere
fails (1); failing that, a half that could not run is no verdict (2), never a pass. THE A/A SELF-TEST compares the head
with itself (harness_aa.py takes): any difference is the harness's fault."""
from __future__ import annotations

import datetime
import os
import shutil
from typing import Callable, List, Optional

import identity_takes_check_context as ctx_module
import identity_takes_check_runs as runs
import identity_takes_check_sides as sides
import tool_process

Context = ctx_module.Context


class Gate:
    def __init__(self, ctx: Context, state: sides.State) -> None:
        self.ctx = ctx
        self.s = state
        self.mode = "off" if state.aa else ctx.setting("TAKES_SKIP_MODE", "off")
        self.answer = "RUN"
        self.line = ""
        self.skipped = False
        self.asked = self.mode in ("shadow", "enforce") and state.takes_moves

    def ask(self) -> None:
        if not self.asked:
            return
        os.makedirs(self.ctx.path("gate"), exist_ok=True)
        trace = self.base_trace()
        command = self.ctx.env.get("TAKES_SKIP_GATE_CMD") or os.path.join(self.ctx.here, "scripts/ci/takes_skip_gate.py")
        done = self.ctx.runner.capture(tool_process.Command(
            [command, self.s.base, self.ctx.head, trace, "--", os.path.join(self.ctx.here, "scripts/ci/takes-skip-proof.sh")],
            self.ctx.with_env({"TAKES_SKIP_MODE": self.mode}), timeout=self.ctx.seconds("TAKES_SKIP_GATE_TIMEOUT", "1200")))
        ctx_module.write(self.ctx.path("gate", "reasons.txt"), done.stderr)
        answer = done.stdout.split("\n") + [""]
        if answer[0] == "SKIP":
            self.answer, self.line = "SKIP", answer[1]
        self.ctx.say(f"{self.ctx.label}: skip gate ({self.mode}): {self.answer} -- {last_reason(done.stderr)}")
        if self.answer == "SKIP" and self.mode == "enforce":
            self.enforce()

    def base_trace(self) -> str:
        """The base's trace under its exact inputs, fetched for the gate (none stored: no file)."""
        trace = self.ctx.path("gate", "base-trace.jsonl.gz")
        exact = self.s.keys.exact_trace(self.s.base)
        if not exact or not self.s.store.fetch(exact, trace):
            runs.remove(trace)
        return trace

    def enforce(self) -> None:
        every = self.ctx.setting("TAKES_SKIP_SAMPLE_EVERY", "10")
        age = self.marker_age()
        if age is None or age >= 86400:
            shown = f"{age} s ago" if age is not None else "no marker"
            self.ctx.say(f"{self.ctx.label}: no full takes row of {self.ctx.cc} on main in the last 24 hours ({shown}): the "
                         "takes run in full, held against the gate")
        elif every.isdigit() and int(every) > 0 and int(self.ctx.head[:8], 16) % int(every) != 0:
            self.skipped = True
            self.s.takes_moves = False
            self.ctx.verdict(f"{self.ctx.label}: the takes diff skipped -- {self.line} under the head's rules: no take can "
                             "have moved")
        else:
            self.ctx.say(f"{self.ctx.label}: a sampled commit (1 in {every}): the takes run in full, held against the gate")

    def marker_age(self) -> Optional[int]:
        """The daily full row's marker's age in seconds; None when it cannot be read (the takes then run in full)."""
        store = self.ctx.env.get("TAKES_STORE", "")
        if not store:
            return None
        created = self.ctx.gh.read(["release", "view", store, "--json", "assets", "--jq",
                                    f'.assets[] | select(.name == "takes-full-{self.ctx.cc}.txt") | .createdAt'],
                                   self.ctx.seconds("IDENTITY_TAKES_GH_TIMEOUT", "120"), quiet=True)
        first = (created or "").split("\n")[0].strip()
        if not first:
            return None
        made = datetime.datetime.fromisoformat(first.replace("Z", "+00:00"))
        return int(self.ctx.clock() - made.timestamp())


class Verdict:
    def __init__(self, ctx: Context, state: sides.State, gate: Gate) -> None:
        self.ctx = ctx
        self.s = state
        self.gate = gate
        self.part = ctx.path("verdict.txt.part")  # beside the out dir, never in TMPDIR

    def python(self, script: str, args: List[str], tee: bool = True) -> int:
        done = self.ctx.runner.capture(tool_process.Command(
            ["python3", os.path.join(self.ctx.here, script), *args], self.ctx.env))
        if done.stderr:
            self.ctx.warn(done.stderr.rstrip("\n"))
        if tee and done.stdout:
            self.ctx.tee(done.stdout.rstrip("\n"), self.part)
        return done.code

    def named(self) -> List[str]:
        summary = self.ctx.env.get("GITHUB_STEP_SUMMARY", "")
        return ["--base-name", self.s.base[:9], "--head-name", self.ctx.head[:9], *(["--summary", summary] if summary else [])]

    def diff_takes(self, base: str, head: str, final: bool) -> int:
        return self.python("scripts/ci/identity_takes_diff.py", [self.ctx.cc, base, head, *(["--final"] if final else []),
                                                      "--judgements", self.ctx.fixtures("identity-judgements.tsv"),
                                                      *self.named()])

    def close(self) -> None:
        """The part, then the lines the check said along the way, become the verdict."""
        with open(self.part, "a") as f:
            f.write(ctx_module.read(self.ctx.path("verdict.txt")))
        shutil.move(self.part, self.ctx.path("verdict.txt"))

    def aa(self) -> int:
        ctx_module.write(self.part, "")
        if self.s.final != 0:
            self.ctx.tee(f"::error title=harness A/A ({self.ctx.cc})::a side's final takes could not be replayed "
                         f"({self.ctx.out}/*/ratchet-takes.tsv.log): nothing was judged", self.part)
            code = 2
        else:
            code = self.python("scripts/ci/harness_aa.py", ["takes", "--cc", self.ctx.cc, "--a", self.ctx.path("base"), "--b",
                                                 self.ctx.path("head")])
        self.close()
        return code

    def judge(self, refresh: Callable[[], None]) -> None:
        ctx_module.write(self.part, "")
        cc = self.ctx.cc
        if self.s.takes_moves:
            self.s.model = self.diff_takes(self.ctx.path("base", f"decisions-{cc}.jsonl"),
                                           self.ctx.path("head", f"decisions-{cc}.jsonl"), False)
            refresh()
            if self.s.final == 0:
                self.s.final = self.diff_takes(self.ctx.path("base", "ratchet-takes.tsv"),
                                               self.ctx.path("head", "ratchet-takes.tsv"), True)
            else:
                self.ctx.tee(f"::error::{self.ctx.label}: the final takes could not be replayed (the ratchet wrote no takes; "
                             f"{self.ctx.out}/*/ratchet-takes.tsv.log)", self.part)
        if self.s.shape_moves:
            if self.s.shape == 0:
                self.s.shape = self.python("scripts/ci/corpus_shape_diff.py", [cc, self.ctx.path("base", "shape"),
                                                                    self.ctx.path("head", "shape"), *self.named()])
            else:
                self.ctx.tee(f"::error::{self.ctx.label}: the corpus-shape specs wrote no findings (their log: "
                             f"{self.ctx.out}/*/shape.log)", self.part)
        self.close()

    def shadow(self) -> int:
        """THE SHADOW: the gate's answer held against the takes it was asked about; a SKIP where one moved fails (1)."""
        if not self.gate.asked or self.gate.skipped:
            return 0
        done = self.ctx.runner.capture(tool_process.Command(
            ["python3", os.path.join(self.ctx.here, "scripts/ci/harness_aa.py"), "moved", "--cc", self.ctx.cc, "--a",
             self.ctx.path("base"), "--b", self.ctx.path("head")], self.ctx.env))
        actual = done.stdout.strip() if done.code == 0 else "unknown"
        if 2 in (self.s.model, self.s.final):
            actual = "unknown"
        agree = not (self.gate.answer == "SKIP" and actual != "unmoved")
        self.ctx.verdict(f"takes-skip-shadow ({self.ctx.cc}): mode={self.gate.mode} gate={self.gate.answer} actual={actual} "
                         f"agree={'yes' if agree else 'no'} base={self.s.base[:9]} head={self.ctx.head[:9]}", summary=True)
        self.post(actual, agree)
        if agree:
            return 0
        self.ctx.verdict(f"::error title=takes skip gate ({self.ctx.cc})::the gate said SKIP but the takes were {actual} -- "
                         f"the gate is unsound; see {self.ctx.path('gate', 'reasons.txt')}")
        return 1

    def post(self, actual: str, agree: bool) -> None:
        repository, sha = self.ctx.env.get("GITHUB_REPOSITORY", ""), self.ctx.env.get("GITHUB_SHA", "")
        if not (repository and sha):
            return
        self.ctx.gh.succeeds(["api", "-X", "POST", f"repos/{repository}/statuses/{sha}", "-f",
                              f"state={'success' if agree else 'failure'}", "-f", f"context=takes skip gate ({self.ctx.cc})",
                              "-f", f"description={self.gate.mode}: gate {self.gate.answer}, takes {actual} -- "
                                    f"{'agree' if agree else 'DISAGREE'}"], self.ctx.seconds("IDENTITY_TAKES_GH_TIMEOUT", "120"))

    def code(self) -> int:
        results = (self.s.model, self.s.final, self.s.shape)
        if 1 in results:
            return 1
        return 2 if any(r != 0 for r in results) else 0


def last_reason(stderr: str) -> str:
    """The gate's last `takes-skip-gate: <ANSWER> -- <why>` line, its why."""
    reasons = [r for r in stderr.splitlines() if r.startswith("takes-skip-gate:")]
    if not reasons:
        return ""
    return reasons[-1].split(" -- ", 1)[1] if " -- " in reasons[-1] else reasons[-1]


def refresh_marker(ctx: Context) -> None:
    """THE DAILY FULL ROW: a full takes diff on main refreshes `takes-full-<cc>.txt` (best effort)."""
    store = ctx.env.get("TAKES_STORE", "")
    if not store or ctx.env.get("GITHUB_REF", "") != "refs/heads/main":
        return
    marker = os.path.join(ctx.temp, f"takes-full-{ctx.cc}.txt")
    now = datetime.datetime.fromtimestamp(ctx.clock(), datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    ctx_module.write(marker, f"{ctx.head} {now}\n")
    ctx.runner.capture(tool_process.Command([os.path.join(ctx.here, "scripts/ci/gh-release.sh"), "upload", store, marker,
                                             "--clobber"], ctx.env))
