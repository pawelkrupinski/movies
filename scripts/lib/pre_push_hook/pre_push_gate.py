"""RED BUILDS GATE THE PUSH (scripts/hooks/pre-push's header; CLAUDE.md, "Red builds gate every push to main"), and A
STRANDED TRAILER refused before it.

THE GATE (the owner, 2026-10-27: "if the latest main and convergence runs are green, just push then, remember that";
"if the latest completed run(s) is green, just check in"): exactly the newest COMPLETED run on main of Main, of Identity
model convergence and -- since its takes rows split off (the owner, 2026-11-08) -- of Identity takes; running, queued and
cancelled runs say nothing. A gate workflow new to the registry (.github/gating-checks.tsv, `unproven`) reports until its
first green run proves it. The nightly data's verdict is VERIFY's, never the capture's ("Capture, then verify"): Record
scrape fixtures goes red when it could not scrape or record a country -- a fact about the night, not about any push --
and Verify scrape recording no longer gates (`agentctl ci-why` lists its red, and any other, as information only).

A FIX MATCHES THE RED IT FIXES, NOT ONE RUN NUMBER (2026-11-05). Every push landing while main is red (tools-only commits
land freely) starts a new run failing the same way: a fix naming the run it was written against was refused once the
next one went red (Main #2164, then #2172, named while the gate read #2173, then #2174; main red ~5.5 h until the fix
was re-trailered by hand, twice). A `Fixes-Red:` counts when it names the newest red run, or an earlier run of the
same red EPISODE (the failed runs since the workflow's last green on main) that shares a failed job with the newest one
-- the aggregates (.github/gating-checks.tsv `aggregate`: they fail whenever anything does) set aside. A run of an older
episode (main went green since) never counts; jobs gh cannot read in time: it does not count. agentctl's waiting and
batch judge the same rule by its red signatures (~/bin AGENTCTL.md, episode_names).

THE GATE FAILS CLOSED: a verdict gh cannot give in time refuses every production commit; a tools-only push (no
production path: pre_push_paths.PRODUCTION; user, 2026-10-14) lands while red or unread.

Its state, `<i>:<conclusion>:<run>` ... (`<i>:none`, `<i>:unread`, `:report-only`), is a key of THE REUSE."""
from __future__ import annotations

import os
import re
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Protocol, Sequence, Tuple

from processes import machine
from pre_push_hook.plan import pre_push_paths as paths_of
from processes import tool_python
from pre_push_hook.plan.pre_push_repo import Repo, Say

# THE GATE's workflows, and the same by file (scripts/ci/gating_checks.py's registry keys on it and reads this line)
GATING_WORKFLOWS = ("Main", "Identity model convergence", "Identity takes")
GATING_WORKFLOW_FILES = ("main.yml", "identity-model-convergence.yml", "identity-takes.yml")
REGISTRY = ".github/gating-checks.tsv"
REPORT_ONLY = "scripts/ci/gating_checks.py"
STRANDED_KEYS = ("Fixes-Red", "Red-Unrelated", "New-Check", "Retires", "Pure-Move")
NOT_ON_MAIN = re.compile(r"could not find any workflows named", re.IGNORECASE)
GH_FIX = "brew install gh"
BOUNDED = os.path.join(os.path.dirname(os.path.dirname(os.path.realpath(__file__))), "processes", "tool_bounded.py")  # a process group ended whole


def verdict_jq(passed_over: Sequence[str]) -> str:
    """`<conclusion> <run id> <url> <episode>` of the newest decided run (success or a failure of any kind) not passed
    over, empty when none; <episode>: the decided runs since the last green, newest first, each `<run id>/<number>`."""
    skip = ",".join(f'"{r}"' for r in passed_over) or '""'
    return ('[.[] | select(.status == "completed" and (.conclusion | IN("cancelled", "skipped", "neutral", "") | not))\n'
            f'                   | select((.databaseId | tostring) | IN({skip}) | not)] as $d\n'
            '              | ($d[0] // empty) as $n\n'
            '              | ([first(range($d | length) as $i | select($d[$i].conclusion == "success") | $i)] | .[0] '
            '// ($d | length)) as $g\n'
            '              | "\\($n.conclusion) \\($n.databaseId) \\($n.url) \\([$d[:$g][] | '
            '"\\(.databaseId)/\\(.number // "")"] | join(" "))"')


FAILED_JOBS_JQ = '.jobs[] | select(.conclusion == "failure" or .conclusion == "timed_out") | .name'


@dataclass
class Verdict:
    conclusion: str
    run: str
    url: str
    episode: List[str]  # `<run id>/<run number>`, the newest red first


def parsed(text: str) -> Optional[Verdict]:
    words = (text.rstrip("\n").split("\n", 1)[0]).split(None, 3)
    if not words:
        return None
    words += [""] * (4 - len(words))
    return Verdict(words[0], words[1], words[2], words[3].split())


class Asking(Protocol):
    """What the gate asks GitHub (gh) and the registry's report-only: a command's (status, stdout, stderr), 124 when it
    outlasts its bound."""
    seconds: int

    def ask(self, argv: Sequence[str]) -> Tuple[int, str, str]: ...


@dataclass
class Bounded:
    """Each command under tool_bounded.py at the checkout, ended whole (a process group) past `seconds`."""
    repo: Repo
    seconds: int

    def ask(self, argv: Sequence[str]) -> Tuple[int, str, str]:
        return machine.both(tool_python.isolated(BOUNDED, str(self.seconds), *argv), self.repo.cwd, self.repo.env)


@dataclass
class Gate:
    """The gate over the checkout at `repo.cwd`, its gh calls `asking` (bounded by PREPUSH_GH_SECONDS), a verdict
    asked `attempts` times `retry` seconds apart before it counts as unread."""
    repo: Repo
    say: Say
    asking: Asking
    attempts: int = 3
    retry: float = 2.0

    @classmethod
    def of(cls, repo: Repo, say: Say) -> "Gate":
        env = repo.env
        return cls(repo, say, Bounded(repo, int(env.get("PREPUSH_GH_SECONDS") or 15)),
                   int(env.get("PREPUSH_GATE_ATTEMPTS") or 3), float(env.get("PREPUSH_GATE_RETRY_SECONDS") or 2))

    def bounded(self, argv: Sequence[str]) -> Tuple[int, str, str]:
        return self.asking.ask(argv)

    # ── gh's answers ──
    def newest(self, workflow: str, passed_over: Sequence[str]) -> Tuple[int, str, str]:
        """(status, verdict line, gh's errors) of a workflow's newest decided run on main. A GATE WORKFLOW NOT YET ON
        MAIN HAS NO RUN THERE (Identity takes, 2026-11-08: added to the gate in the push that adds it): gh's "could not
        find any workflows named" is no verdict -- like no run -- never an unread gate."""
        code, out, err = self.bounded(["gh", "run", "list", "--workflow", workflow, "--branch", "main", "--limit", "30",
                                       "--json", "databaseId,number,status,conclusion,url",
                                       "--jq", verdict_jq(passed_over)])
        if code != 0 and NOT_ON_MAIN.search(err):
            code = 0
        return code, out, err if code != 0 else ""

    def read_verdict(self, index: int, passed_over: Sequence[str] = ()) -> Tuple[int, str, str]:
        """newest(), asked up to `attempts` times; every failed attempt's errors, and the last status (124: cut off)."""
        errors, code = "", 0
        for attempt in range(1, self.attempts + 1):
            code, out, err = self.newest(GATING_WORKFLOWS[index], passed_over)
            if code == 0:
                return 0, out, errors
            errors += err
            if attempt < self.attempts:
                machine.sleep(self.retry)
        return code, "", errors

    def aggregates(self, workflow_file: str) -> List[str]:
        rows = [line.split("\t") for line in self.repo.files.read_text_or(REGISTRY, "").split("\n")]
        return [r[2] for r in rows if len(r) > 3 and r[0] == "job" and r[1] == workflow_file and r[3] == "aggregate"]

    def failed_jobs(self, run: str, workflow_file: str, everything: bool = False) -> Optional[List[str]]:
        """The run's failed jobs, sorted, the aggregates left out (`everything`: kept); None when gh cannot say."""
        code, out, _ = self.bounded(["gh", "run", "view", run, "--json", "jobs", "--jq", FAILED_JOBS_JQ])
        if code != 0:
            return None
        names = {line for line in out.split("\n") if line.strip()}
        if not everything:
            names -= set(self.aggregates(workflow_file))
        return sorted(names)

    def unproven_only(self, index: int, run: str) -> Optional[str]:
        """A CHECK GATES ONLY ONCE IT HAS BEEN GREEN (scripts/ci/gating_checks.py; ci-failures-2026-10-30 recommendation
        5): why a red run reports only -- its failed jobs all unproven, with the aggregates that follow them; None when
        it gates (the registry or the run's jobs unreadable included)."""
        if not (self.repo.files.is_file(REPORT_ONLY) and self.repo.files.is_file(REGISTRY)):
            return None
        code, out, _ = self.bounded(["python3", REPORT_ONLY, "report-only", "--workflow-file",
                                     GATING_WORKFLOW_FILES[index], "--run", run])
        return out.rstrip("\n") if code == 0 else None

    # ── the pushed commits ──
    def touches_production(self, commit: str) -> bool:
        return any(paths_of.is_production_path(p) for p in
                   self.repo.lines("diff-tree", "--no-commit-id", "--name-only", "-r", "--root", "-m", commit))

    def production_commits(self, ranges: Sequence[str]) -> List[str]:
        return [c for r in ranges for c in self.repo.lines("log", "--format=%H", r) if self.touches_production(c)]

    # ── A STRANDED TRAILER ──
    def stranded_trailers(self, ranges: Sequence[str]) -> bool:
        """Whether every pushed commit's trailer lines are where git reads them: more `<key>:` lines in a message than
        `%(trailers:key=<key>)` values is a line outside the closing trailer block, refused, said. Silent when none."""
        stranded = [(commit, key) for r in ranges for commit in self.repo.lines("rev-list", r)
                    for key in self.stranded_keys(commit)]
        for commit, key in stranded:
            self.say.warn(f"{commit[:9]}: a {key}: line outside the message's closing trailer block, where git reads "
                          "no trailer")
        if not stranded:
            return True
        self.say.warn("neither the red gate, agentctl nor the puremove check sees such a line. Put it in the message's "
                      "LAST paragraph, beside")
        self.say.warn("Co-Authored-By: with no blank line between them (git commit --amend / git rebase on unpushed "
                      "commits).")
        return False

    def stranded_keys(self, commit: str) -> List[str]:
        """The keys of STRANDED_KEYS with more `<key>:` lines in the commit's message than git reads trailers of."""
        message = self.repo.git_out("log", "-1", "--format=%B", commit).split("\n")
        keys: List[str] = []
        for key in STRANDED_KEYS:
            declared = sum(1 for line in message if re.match(f"{key}:", line, re.IGNORECASE))
            if declared and len(self.repo.lines("log", "-1", f"--format=%(trailers:key={key},valueonly,unfold)",
                                                commit)) < declared:
                keys.append(key)
        return keys

    # ── the gate ──
    def judge(self, ranges: Sequence[str]) -> Tuple[bool, str]:
        """(whether the push may land, the state read)."""
        if not gh_found(self.repo.env.get("PATH")):
            self.say.warn(f"gh not found: the redgate check cannot run, so the push is BLOCKED (never skipped). Fix: "
                          f"{GH_FIX} -- then push again (or PREPUSH_SKIP=redgate, named in your report).")
            return False, ""
        reading = Reading()
        for index in range(len(GATING_WORKFLOWS)):
            self.read_workflow(index, reading)
        if reading.unread:
            return self.unread(ranges, "; ".join(reading.unread)), reading.state
        if not reading.reds:
            return True, reading.state
        return self.fixes(ranges, reading), reading.state

    def read_workflow(self, index: int, reading: "Reading") -> None:
        workflow = GATING_WORKFLOWS[index]
        code, out, errors = self.read_verdict(index)
        if code != 0:
            if code == 124:
                self.say.warn(f"gh did not answer {workflow}'s verdict within {self.asking.seconds}s ({self.attempts} attempts):"
                              " cut off")
            else:
                self.say.warn(f"could not read {workflow}'s verdict from gh ({errors.split(chr(10), 1)[0]}) in "
                              f"{self.attempts} attempts")
            reading.unread.append(f"{workflow}'s verdict unread")
            reading.state += f" {index}:unread"
            return
        verdict = self.complete(index, parsed(out))
        if verdict is None:
            reading.state += f" {index}:none"
            return
        reading.state += f" {index}:{verdict.conclusion}:{verdict.run}"
        if verdict.conclusion == "success":
            return
        only = self.unproven_only(index, verdict.run)
        if only is not None:
            self.say.warn(f"{workflow}'s run {verdict.run} is red only in checks no green run has proven yet: it "
                          f"REPORTS, it does not gate ({only}; {REGISTRY})")
            reading.state += ":report-only"
            return
        reading.reds.append(verdict.run)
        reading.episodes[index] = verdict.episode
        self.say.warn(f"{workflow} is RED on main: run {verdict.run} ({verdict.conclusion}) {verdict.url}")
        if len(verdict.episode) > 1:
            numbers = "".join("#" + tok.rsplit("/", 1)[-1] + " " if "/" in tok else tok + " " for tok in verdict.episode)
            self.say.log(f"...its red episode since the last green: {numbers}(a Fixes-Red: naming one failing the same "
                         "way counts)", True)

    def complete(self, index: int, verdict: Optional[Verdict]) -> Optional[Verdict]:
        """AN INCOMPLETE RUN IS NO VERDICT (the owner, 2026-11-08: read main's verdict as agentctl does). A red run whose
        failed jobs are aggregates alone -- a run whose takes rows did not all decide, failing only `verdict complete`
        (Identity model convergence #1063) -- is passed over, and the complete run before it decides (at most three).
        Jobs gh cannot say: it stays red (the gate fails closed)."""
        passed: List[str] = []
        file = GATING_WORKFLOW_FILES[index]
        while verdict is not None and verdict.conclusion != "success" and len(passed) < 3:
            everything = self.failed_jobs(verdict.run, file, everything=True)
            if not everything or self.failed_jobs(verdict.run, file) != []:
                break
            self.say.log(f"{GATING_WORKFLOWS[index]}'s run {verdict.run} is NO VERDICT: only an aggregate failed (an "
                         "incomplete run); the complete run before it decides", True)
            passed.append(verdict.run)
            code, out, _ = self.read_verdict(index, passed)
            verdict = parsed(out) if code == 0 else None
        return verdict

    def unread(self, ranges: Sequence[str], unread: str) -> bool:
        production = self.production_commits(ranges)
        if not production:
            self.say.log(f"red-build gate UNREAD ({unread}): every pushed commit is tools-only, which lands while red",
                         True)
            return True
        self.say.warn(f"red-build gate UNREAD ({unread}): the push is REFUSED -- a gate that cannot be read may be red, "
                      "and")
        self.say.warn("production commits land only on a green (or Fixes-Red:) main: "
                      + "".join(c[:9] + " " for c in production))
        self.say.warn("GitHub did not answer: check the network and `gh auth status`, then push again.")
        return False

    def fixes(self, ranges: Sequence[str], reading: "Reading") -> bool:
        """Whether every pushed production commit names a run of a red episode in a `Fixes-Red:` trailer."""
        bad = False
        reds = "".join(" " + r for r in reading.reds)
        for r in ranges:
            for line in self.repo.lines("log", "--format=%H%x09%(trailers:key=Fixes-Red,valueonly,unfold,"
                                        "separator=%x2C)", r):
                commit, _, named = line.partition("\t")
                if not self.touches_production(commit):
                    self.say.log(f"{commit[:9]} is tools-only (touch no production path): lands while red", True)
                    continue
                if any(self.episode_fix(value, index, episode) for value in re.split(r"[\s,;]+", named) if value
                       for index, episode in sorted(reading.episodes.items())):
                    continue
                self.say.warn(f"{commit[:9]} carries no Fixes-Red: trailer naming a run of the red episode (red:{reds})")
                bad = True
        if not bad:
            self.say.log("every pushed production commit is declared a fix for a red run")
            return True
        self.say.warn("main is red: only a fix for it lands. Add 'Fixes-Red: <run id>' to each commit's message (git "
                      "commit --amend")
        self.say.warn("on unpushed commits), or bypass with PREPUSH_SKIP=redgate.")
        return False

    def episode_fix(self, value: str, index: int, episode: List[str]) -> bool:
        """Whether `Fixes-Red: <value>` names a run of the workflow's red episode failing as its newest red does; says
        why when the value is of the episode and still does not count."""
        value = value[1:] if value.startswith("#") else value
        newest = episode[0].split("/", 1)[0] if episode else ""
        for token in episode:
            run, _, number = token.partition("/") if "/" in token else (token, "", token)
            if value in (run, number):
                return run == newest or self.fails_alike(value, index, run, newest)
        return False

    def fails_alike(self, value: str, index: int, run: str, newest: str) -> bool:
        """Whether an earlier run of the episode shares a failed job (no aggregate) with the newest red, said."""
        workflow, file = GATING_WORKFLOWS[index], GATING_WORKFLOW_FILES[index]
        mine, theirs = self.failed_jobs(run, file), self.failed_jobs(newest, file)
        if mine is None or theirs is None:
            self.say.warn(f"Fixes-Red: {value} is a run of {workflow}'s red episode, but gh did not say its failed "
                          "jobs: not counted")
            return False
        if set(mine) & set(theirs):
            self.say.log(f"Fixes-Red: {value}: a run of {workflow}'s red episode failing as run {newest} does", True)
            return True
        self.say.warn(f"Fixes-Red: {value} is a run of {workflow}'s red episode that failed another way than run "
                      f"{newest}: not counted")
        return False


def no_words() -> List[str]:
    return []


def no_episodes() -> Dict[int, List[str]]:
    return {}


@dataclass
class Reading:
    """What the gate read of its workflows: the state (a key of THE REUSE), the unread ones, the red runs and each
    red workflow's episode."""
    state: str = ""
    unread: List[str] = field(default_factory=no_words)
    reds: List[str] = field(default_factory=no_words)
    episodes: Dict[int, List[str]] = field(default_factory=no_episodes)


def gh_found(path: Optional[str]) -> bool:
    return any(os.access(os.path.join(d, "gh"), os.X_OK) for d in (path or "").split(os.pathsep) if d)

