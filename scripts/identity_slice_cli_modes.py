"""identity-slice's modes beside a slice of a recording (scripts/identity_slice_cli.py): --explain (the WHY trace of a
listing the unmatched-cluster capture holds, at one or two refs), --faithfulness capture and --faithfulness RUN (the
slice's agreement takes against a capture's, or a convergence leg's takes at its commit and pinned recording), and the
local prod mirror's read (--prod-mirror: its freshness checked against the recording's moment first, never prod)."""
from __future__ import annotations

import json
import os
import re
from typing import List, Optional

from identity_slice_cli_context import Context, Exit, cat, say
import tool_process  # (scripts/lib, on the path the context put it on)
from identity_slice_cli_keys import Keys
from identity_slice_cli_run import Slicer


def summary(path: str) -> List[str]:
    with open(path, encoding="utf-8") as f:
        return [line.rstrip("\n") for line in f if line.startswith(("== ", "agreement ", "TAKE "))]


def side_by_side(a: str, b: str, ra: str, rb: str, columns: str) -> None:
    """The two refs' outcomes side by side: each cluster's heading, agreement and takes."""
    half = int(columns) // 2 - 2

    def fit(s: str) -> str:
        return s if len(s) <= half else s[: half - 1] + "…"
    left, right = summary(a), summary(b)
    print(f"{fit(ra):<{half}} | {rb}")
    for i in range(max(len(left), len(right))):
        line_left = left[i] if i < len(left) else ""
        line_right = right[i] if i < len(right) else ""
        print(f"{fit(line_left):<{half}} | {fit(line_right)}")


def same_bytes(a: str, b: str) -> bool:
    with open(a, "rb") as fa, open(b, "rb") as fb:
        return fa.read() == fb.read()


class Modes:
    def __init__(self, ctx: Context, keys: Keys, slicer: Slicer) -> None:
        self.ctx, self.keys, self.slicer = ctx, keys, slicer

    def explain(self) -> int:
        o, ctx = self.ctx.o, self.ctx
        captured = ctx.answer("identity_capture", o.fixture, o.cc)
        if not os.path.isfile(f"{captured}/{o.cc}.json.gz"):
            say(f"no capture {captured}/{o.cc}.json.gz")
            return 1
        labels = ctx.answer("identity_judgements", captured)
        java = ["scripts.IdentityExplain", o.cc, "--fixture", f"{captured}/{o.cc}.json.gz", "--labels", labels, *o.passed]
        if not o.refs:
            return self.explain_at(ctx.repo, "-", java)
        for i, ref in enumerate(o.refs):
            if self.explain_at(ctx.tree_of(ref), ctx.path(f"{i}.txt"), java) != 0:
                cat(ctx.path(f"{i}.txt"))
                return 1
        if len(o.refs) == 1:
            cat(ctx.path("0.txt"))
            return 0
        self.compared(o.refs[0], o.refs[1])
        return 0

    def explain_at(self, tree: str, stdout: str, java: List[str]) -> int:
        cp = self.ctx.classpath(tree)
        return 1 if cp is None else self.ctx.java(tree, stdout, "-", ["-cp", cp, *java])

    def compared(self, ra: str, rb: str) -> None:
        """The two refs' outcomes side by side, then the traces' diff."""
        a, b = self.ctx.path("0.txt"), self.ctx.path("1.txt")
        side_by_side(a, b, ra, rb, self.ctx.knob("COLUMNS", "200"))
        print()
        if same_bytes(a, b):
            print("the two refs' traces are identical")
            return
        print(f"--- {ra}\n+++ {rb}")
        diff = self.ctx.capture(["diff", "-u", a, b]).stdout
        print("".join(diff.splitlines(keepends=True)[2:]), end="", flush=True)

    def faithfulness_of_capture(self) -> int:
        """The agreement-taken listings: the unmatched-cluster capture's, as this code's agreement takes them over
        prod's filed answers, on the recording the capture was made from -- what no hermetic leg takes."""
        o, ctx = self.ctx.o, self.ctx
        if o.families == "none" or not os.path.isfile(o.families):
            say("--faithfulness capture needs the capture (--families PATH)")
            return 2
        stem = o.families[:-len(".json.gz")] if o.families.endswith(".json.gz") else o.families
        o.run = o.run or recording_named(stem + ".inputs")
        if not o.run:
            say(f"no recording named beside {o.families} ({stem}.inputs): name it with --recording")
            return 1
        ctx.recording = ctx.recording_of(o.run, o.cc)
        self.use_mirror()
        ref = o.refs[0] if o.refs else "."
        ctx.log(f"faithfulness of the agreement takes: {o.families} at {ref} on recording {o.run}, {o.sample} listing(s)")
        return self.slicer.run_at(ctx.tree_of(ref), self.keys.sha_of(ref), ctx.path("faith.tsv"),
                                  ["--faithfulness", o.families, "--sample", o.sample, "--seed", o.seed])

    def faithfulness_of_run(self) -> int:
        """The slice's take of a sample of listings against what that convergence run's leg took, at its commit and
        its pinned recording."""
        o, ctx = self.ctx.o, self.ctx
        diag = f"{ctx.cache}/diagnostics/{o.faith}"
        takes = f"{diag}/{o.cc}-takes/{o.cc}.json.gz"
        if not os.path.isfile(takes) and ctx.shell("identity_locked", [f"diagnostics-{o.faith}-{o.cc}",
                                                                      "identity_takes_download", o.faith, o.cc, diag]).code:
            return 1
        o.run = o.run or self.pinned_recording()
        head = self.gh("KINOWO_IDENTITY_GH_TIMEOUT", "60", "run", "view", o.faith, "-R", self.repository(), "--json",
                       "headSha", "-q", ".headSha")
        if head is None or not head.strip():
            say(f"could not read run {o.faith}'s commit (gh failed or timed out)")
            return 1
        head = head.strip()
        ctx.recording = ctx.recording_of(o.run, o.cc)
        self.use_mirror()
        ctx.log(f"faithfulness: run {o.faith} (commit {head[:9]}) on recording {o.run}, {o.sample} listing(s)")
        return self.slicer.run_at(ctx.tree_of(head), head, ctx.path("faith.tsv"),
                                  ["--faithfulness", takes, "--sample", o.sample, "--seed", o.seed])

    def pinned_recording(self) -> str:
        """The recording the run's log names as its pinned corpus; the tool ends (exit 1) when it names none."""
        o = self.ctx.o
        text = self.gh("KINOWO_IDENTITY_DOWNLOAD_TIMEOUT", "1800", "run", "view", o.faith, "-R", self.repository(), "--log")
        if text is None:
            say(f"could not read run {o.faith}'s log (gh failed or timed out): name its recording with --recording")
            raise Exit(1)
        found = re.search(r"KINOWO_CONVERGENCE_PIN_CORPUS_RUN: ([0-9]+)", text)
        if not found:
            say(f"no pinned recording in run {o.faith}'s log: name it with --recording")
            raise Exit(1)
        return found.group(1)

    def repository(self) -> str:
        return self.ctx.env.get("KINOWO_REPO") or "pawelkrupinski/movies"

    def gh(self, bound: str, default: str, *args: str) -> Optional[str]:
        """identity-tools.sh's identity_gh: gh, ended once it outlasts its bound; None when it failed."""
        done = self.ctx.capture([self.ctx.knob("KINOWO_GH", "gh"), *args], timeout=float(self.ctx.knob(bound, default)))
        return done.stdout if done.code == 0 else None

    def use_mirror(self) -> None:
        """The local prod mirror, read only once its freshness is printed: WARNed loudly when it predates the recording
        or lags the clock (the slice would mix moments), refused when it is absent -- never a fall-back to prod."""
        o, ctx = self.ctx.o, self.ctx
        if not o.mirror:
            return
        recorded = recorded_at(f"{ctx.recording}/corpus/prod-coverage-{o.cc}.json")
        ctx.log(f"prod mirror: the local mirror's freshness (recording made {recorded or 'at an unknown time'}):")
        status = ctx.to_stderr([f"{ctx.repo}/scripts/local-mirror/freshness", o.cc, "--quiet", "--max-lag-hours",
                                ctx.knob("KINOWO_MIRROR_MAX_LAG_HOURS", "6"), "--need",
                                "identity_family_answers,identity_model_families,identity_traces,identity_agreements",
                                *(["--not-before", recorded] if recorded else [])])
        if status == 3:
            ctx.log("WARNING — THE PROD MIRROR IS NOT FRESH (above): prod's state it reads is not the recording's "
                    "moment; read every PROD line with that in mind")
        elif status != 0:
            ctx.log("no local prod mirror to read (above): scripts/local-mirror/service.sh install — --prod-mirror "
                    "never reads prod itself")
            raise Exit(2)
        ctx.mirror_args = ["--prod-mirror", self.mongo_uri()]
        self.deployed_worker()

    def mongo_uri(self) -> str:
        return self.ctx.shell("local_mongo_uri", [], "scripts/tool-conventions.sh").stdout.rstrip("\n")

    def deployed_worker(self) -> None:
        """Whose code wrote that state: the worker build prod runs (the deploy moves `deployed-worker` once it is live)."""
        ctx = self.ctx
        deployed = ctx.env.get("KINOWO_DEPLOYED_WORKER", "")
        if not deployed:
            remote = ctx.runner.capture(tool_process.Command(
                ["git", "-C", ctx.repo, "ls-remote", "origin", "refs/tags/deployed-worker"], ctx.env, timeout=15))
            deployed = "\n".join(line.split("\t")[0] for line in remote.stdout.splitlines())
        if deployed:
            ctx.log(f"prod's state was written by the deployed worker {deployed[:9]}: a PROD line of another ref's "
                    "slice mixes builds")
            ctx.mirror_args += ["--deployed", deployed]


def recording_named(inputs: str) -> str:
    """awk -F'\\t' '$1 == "recording" { print $2 }' <inputs>."""
    try:
        with open(inputs) as f:
            return "\n".join(row.rstrip("\n").split("\t")[1] for row in f
                             if row.split("\t")[0] == "recording" and "\t" in row)
    except OSError:
        return ""


def recorded_at(coverage: str) -> str:
    try:
        with open(coverage) as f:
            return str(json.load(f).get("recordedAt", ""))
    except Exception:  # noqa: BLE001 -- any unreadable coverage is "an unknown time"
        return ""
