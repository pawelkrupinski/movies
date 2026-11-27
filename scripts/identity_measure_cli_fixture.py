"""The fixture scripts/identity-measure's black-box tests share (identity_measure_cli_test.py,
identity_measure_cli_reuse_test.py): a scratch origin and checkout whose main learned the takes-only dispatch, the
downloaded takes artifacts of a run (`landable`, and `lost`: a right take lost), a fake gh answering as GitHub does
from the environment a case sets, and a git that can fail a measure/ push after origin took it. Ported from
identity-measure-test.sh's preamble; the same cases hold the shell tool and its Python rewrite (cli_harness.tool_path)."""
from __future__ import annotations

import os
import pwd
import re
import shutil
import subprocess
import sys
from typing import List, Mapping, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import machine  # noqa: E402
import cli_harness  # noqa: E402

TOOL = cli_harness.tool_path("scripts/identity_measure_cli.py", "scripts/identity-measure")
JUDGEMENTS = "test/resources/fixtures/identity-judgements.tsv"
# What can change a leg's verdict, in the matches.sh dialect: the resolver (rule.txt, engine/**) and the judgements.
CONVERGENCE_PATHS = ("# the paths\nrule.txt\nengine/**\ncommon/**\n.github/scripts/convergence-setup.sh\n"
                     "test/resources/fixtures/identity-judgements.tsv\n")
DECISION = ('{"key":"Helios|Lalka","venue":"Helios","rawTitle":"Lalka","film":%s,"filmTitle":%s,"basis":"%s",'
            '"explanation":[],"blocker":%s,"suspect":[]}\n')
# gh answering from $STUB (its counters and the marks a case reads), $ORIGIN (what was pushed), $ARTIFACT (a run's
# takes artifacts) and the switches each case sets; --jq applied through jq, as gh does
GH = r'''
import gzip, shutil, signal, subprocess, time
stub = env["STUB"]
joined = " " + " ".join(argv) + " "
def flag(name):
    return argv[argv.index(name) + 1] if name in argv else ""
def put(name, text):
    with open(os.path.join(stub, name), "w") as f:
        f.write(text)
def has(name):
    return os.path.exists(os.path.join(stub, name))
def count(name):
    n = (int(open(os.path.join(stub, name)).read()) if has(name) else 0) + 1
    put(name, str(n))
    return n
def jq(text):
    done = subprocess.run(["jq", "-r", flag("--jq")], input=text, capture_output=True, text=True)
    sys.stdout.write(done.stdout); sys.stderr.write(done.stderr); exit(done.returncode)
def view(status, conclusion=""):
    out(json.dumps({"databaseId": 777, "headSha": env.get("HEAD_SHA") or "abc1230000000000000000000000000000000000",
                    "headBranch": "measure/abc-1", "workflowName": "w", "status": status, "conclusion": conclusion}))
    exit(0)
def run_view(fields):
    if fields == "url":
        out("https://example.test/runs/777")
    elif "status,conclusion" in fields:
        if env.get("VIEW_FAILS"):
            exit(1)
        if env.get("VIEW_BLIPS") and not has("blipped"):
            put("blipped", ""); exit(1)
        if env.get("IN_PROGRESS"):
            put("watched", ""); view("in_progress")
        if count("view-pending") <= int(env.get("VIEW_PENDING") or 0):
            view("in_progress")
        view("completed", "cancelled" if count("reads") <= int(env.get("CANCELS") or 0) else "success")
    elif fields == "createdAt,startedAt,updatedAt,jobs":
        jq(json.dumps({"createdAt": "2026-10-15T06:42:54Z", "startedAt": "2026-10-15T06:42:54Z",
                       "updatedAt": "2026-10-15T07:23:38Z", "jobs": [
            {"name": "preflight", "startedAt": "2026-10-15T07:02:42Z", "completedAt": "2026-10-15T07:02:47Z",
             "conclusion": "success"},
            {"name": "leg (poland) / takes (poland)", "startedAt": "2026-10-15T07:02:49Z",
             "completedAt": "2026-10-15T07:12:31Z", "conclusion": "failure"}]}))
    elif fields == "status,headBranch,headSha":
        state = "in_progress" if count("pending") <= int(env.get("RUN_PENDING") or 0) else "completed"
        out(f"{state}\tmeasure/abc-1\t{env.get('HEAD_SHA', '')}")
    elif fields == "jobs":
        jq(json.dumps({"jobs": [
            {"name": "preflight", "status": "completed", "conclusion": "success"},
            {"name": "leg (poland) / takes-base (poland)", "status": "completed", "conclusion": "success"},
            {"name": "leg (poland) / takes (poland)", "status": env.get("TAKES_STATUS") or "in_progress",
             "conclusion": "failure"}]}))
    elif fields == "createdAt":
        out(env.get("RUN_CREATED") or time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()))
    exit(0)
def pushed():
    heads = os.path.join(env["ORIGIN"], "refs", "heads", "measure")
    for here, _, files in os.walk(heads):
        for name in files:
            put("branch", os.path.relpath(os.path.join(here, name), heads) + "\n")
            shutil.copyfile(os.path.join(here, name), os.path.join(stub, "pushed"))
    if has("pushed"):
        out("777")
def tool_ancestor():
    pid = os.getppid()
    for _ in range(6):
        ps = subprocess.run(["ps", "-o", "ppid=,command=", "-p", str(pid)], capture_output=True, text=True).stdout
        if "identity-measure" in ps or "identity_measure" in ps:
            return pid
        pid = int(ps.split()[0])
    exit(1)
verb = " ".join(argv[:2])
if verb == "workflow run":
    put("dispatch", " ".join(argv) + "\n")
    exit(1 if env.get("DISPATCH_FAILS") else 0)
if verb == "run list":
    if " status,conclusion,createdAt,updatedAt,headBranch " in joined:
        jq(env.get("ETA_RUNS") or "[]")
    if " --branch " in joined:
        pushed(); exit(0)
    out(env.get("LANE_BUSY") or "0"); exit(0)
if verb == "run view":
    run_view(flag("--json"))
if verb == "run download":
    if env.get("DOWNLOAD_FAILS"):
        err("HTTP 502: Bad Gateway"); exit(1)
    if env.get("DOWNLOAD_BLIPS") and not has("download-blipped"):
        put("download-blipped", ""); err("HTTP 500"); exit(1)
    shutil.copytree(env["ARTIFACT"], flag("--dir"), dirs_exist_ok=True); exit(0)
if verb == "release view":
    if not env.get("FOOTPRINT_ASSETS"):
        exit(1)
    out(env["FOOTPRINT_ASSETS"]); exit(0)
if verb == "release upload":
    if not env.get("RELEASE_DIR"):
        exit(1)
    shutil.copy(argv[3], env["RELEASE_DIR"]); exit(0)
if verb == "release download":
    target, pattern = flag("-O"), flag("-p")
    if pattern.startswith("measure-result-"):
        kept = os.path.join(env.get("RELEASE_DIR") or "/nonexistent", pattern)
        if not os.path.isfile(kept):
            exit(1)
        shutil.copyfile(kept, target); exit(0)
    with open(target, "wb") as f:
        f.write(gzip.compress(gzip.compress(b'{"m":"sanitize"}\n')))
    exit(0)
if verb == "run cancel":
    put("cancelled", "cancelled\n")
    if env.get("SIGNAL_ON_CANCEL"):  # a second signal while the tool cleans up (a second Ctrl-C)
        os.kill(tool_ancestor(), signal.SIGTERM)
    exit(0)
exit(1)
'''
# The real git, but a measure/ push that reports failure AFTER origin took it (a push timed out on its answer).
GIT = '''#!/usr/bin/env bash
if [ "$1" = push ] && [ -n "${PUSH_FAILS_AFTER:-}" ] && [[ " $* " == *refs/heads/measure/* ]] && [[ " $* " != *--delete* ]]; then
  "%s" "$@"; exit 1
fi
exec "%s" "$@"
'''


def user() -> str:
    return pwd.getpwuid(os.getuid()).pw_name


class MeasureCase(cli_harness.ToolCase):
    """A scratch origin + checkout: `old` (main before takes-only), `parent` (main learning it), `tip` (a resolver
    change); `measure` runs the tool from the checkout as identity-measure-test.sh's `measure` did."""
    TOOL = TOOL

    def setUp(self) -> None:
        super().setUp()
        self.origin, self.checkout = self.origin_and_checkout()
        self.write(f"repo/{JUDGEMENTS}", "country\tvenue\trawTitle\tfilm\tverdict\tstate\tscope\tfilmTitle\treason\t"
                   "judged\tevidence\npl\t*\tLalka\ttmdb:1\tright\topen\t\t\t\t2026-10-14\t\n")
        self.write("repo/.github/workflows/country-convergence-leg.yml", "phase: convergence\n")
        self.write("repo/.github/convergence-paths.txt", CONVERGENCE_PATHS)
        self.write("repo/scripts/ci/identity-takes-check.sh", "old\n")
        self.old = self.commit("old main, before takes-only")
        # Large, as the real one is: a reader that stops at the first match must not read it as lacking takes-only.
        self.write("repo/.github/workflows/country-convergence-leg.yml", "takes-only: {} takes-countries: {}\n" + 4000 *
                   "# a line of the leg workflow, padding it past a pipe buffer\n")
        self.write("repo/scripts/ci/identity-takes-check.sh", "new\n")
        self.write("repo/.github/workflows/identity-takes.yml", "name: Identity takes\n")
        self.parent = self.commit("main learns takes-only (and the takes rows' own workflow)")
        self.write("repo/rule.txt", "rule\n")
        self.tip = self.commit("a resolver change")
        self.git(self.checkout, "push", "-q", "origin", "main")
        self.artifact("landable", lose=False)
        self.artifact("lost", lose=True)
        self.fakes.program("gh", GH)
        real_git = shutil.which("git") or "git"
        with open(os.path.join(self.fakes.bin, "git"), "w") as f:
            f.write(GIT % (real_git, real_git))
        os.chmod(os.path.join(self.fakes.bin, "git"), 0o755)
        self.out = ""

    def commit(self, message: str) -> str:
        self.git(self.checkout, "add", "-A")
        self.git(self.checkout, "commit", "-qm", message)
        return self.git(self.checkout, "rev-parse", "HEAD")

    def artifact(self, name: str, lose: bool) -> None:
        """One artifact, identity-takes-pl: a right take ("Lalka") lost at the head when `lose`."""
        d = f"{name}/identity-takes-pl"
        self.write(f"{d}/verdict.txt", f"identity takes (pl): lost {1 if lose else 0}\n")
        self.write(f"{d}/base.txt", self.parent + "\n")
        self.write(f"{d}/base/decisions-pl.jsonl", DECISION % ("1", '"Lalka (2026)"', "OwnMatch", "null"))
        self.write(f"{d}/head/decisions-pl.jsonl", DECISION % ("null", "null", "none", '"no film"') if lose else
                   DECISION % ("1", '"Lalka (2026)"', "OwnMatch", "null"))
        self.write(f"{d}/timings.tsv", "phase\tseconds\thow\nhead-resolve\t546\tresolved\nbase-resolve\t3\treused\n")
        for side in ("base", "head"):
            self.write(f"{d}/{side}/ratchet-takes.tsv",
                       "status\tcountry\tvenue\trawTitle\tfilm\ttitle\tbasis\tids\twas\texplanation\n")

    def measure(self, art: str, *args: str, gate: bool = False, keep_cache: bool = False,
                env: Optional[Mapping[str, str]] = None) -> int:
        """The tool over the `art` artifacts, from the checkout: its exit; stdout and stderr in self.out. Every case
        but the gate's own passes the what-if gate (its override); `gate` leaves the gate to judge."""
        shutil.rmtree(self.path("stub"), ignore_errors=True)
        os.makedirs(self.path("stub"))
        if not keep_cache:
            shutil.rmtree(self.path("cache"), ignore_errors=True)
        self.fakes.clear()
        gate_args = [] if gate else ["--no-what-if", "the measure spec"]
        done = self.run_tool(*gate_args, *args, cwd=self.checkout, env={**self.stub_env(art), **(env or {})})
        self.out = done.stdout + done.stderr
        return done.code

    def stub_env(self, art: str) -> Mapping[str, str]:
        # HOME the case's own: no fake run is recorded with the real agentctl (identity_record_dispatch)
        return {"HOME": self.work, "STUB": self.path("stub"), "ORIGIN": self.origin, "ARTIFACT": self.path(art), "HEAD_SHA": "",
                "KINOWO_IDENTITY_CACHE": self.path("cache"), "IDENTITY_MEASURE_POLL": "0", "GH_DOWNLOAD_BACKOFF": "0"}

    def stub(self, name: str) -> str:
        """A mark the fake gh left in $STUB, as `$(cat ...)` reads it; "" when it left none."""
        path = self.path("stub", name)
        if not os.path.exists(path):
            return ""
        with open(path) as f:
            return f.read().rstrip("\n")

    def dispatched(self) -> bool:
        return os.path.exists(self.path("stub", "dispatch"))

    def measure_branches(self) -> List[str]:
        """Every measure/ branch on origin (they nest: measure/<user>/<sha>-<random>), as refs/heads/... names."""
        heads = os.path.join(self.origin, "refs", "heads")
        return [os.path.relpath(os.path.join(here, name), self.origin)
                for here, _, files in os.walk(os.path.join(heads, "measure")) for name in files]

    def drop_measure_branches(self) -> None:
        for ref in self.measure_branches():
            self.git(self.checkout, "push", "-q", "origin", "--delete", ref)

    def push_run_branch(self) -> None:
        """The branch a run of `tip` was dispatched on, for --run to collect."""
        self.git(self.checkout, "push", "-q", "origin", f"{self.tip}:refs/heads/measure/abc-1")

    def lines(self, pattern: str) -> int:
        """How many lines of the output match `pattern` (grep -c)."""
        return sum(1 for line in self.out.split("\n") if re.search(pattern, line))

    def judge_lalka_wrong(self) -> None:
        path = os.path.join(self.checkout, JUDGEMENTS)
        with open(path) as f:
            text = f.read()
        with open(path, "w") as f:
            f.write(text.replace("\tright\topen\t", "\twrong\topen\t"))

    def raw_git(self, *args: str) -> bytes:
        """git's stdout as it is (cli_harness's `git` strips it), for what hashes it."""
        return subprocess.run(["git", "-C", self.checkout, *args], env=cli_harness.isolated_env(machine.environment()),
                              check=True, capture_output=True).stdout
