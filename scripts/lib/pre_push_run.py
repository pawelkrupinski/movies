"""THE PRE-PUSH HOOK'S RUN (scripts/hooks/pre-push execs `scripts/pre_push.py <hook> --run [args]`; its header says
what every step guards): where it runs (pre_push_tree), what is pushed and the checks its paths and messages select
(pre_push_push, the selection pre_push_modes answers), the red gate and the stranded trailers (pre_push_gate), the
known-red baseline, the reuse and a remote pass (pre_push_reuse), then each check (pre_push_checks: pre_push_registry's,
and the hook's `check_<name>` functions) and the verdict. pre_push_run_test.py holds it black-box."""
from __future__ import annotations

import os
import signal
from types import FrameType
from typing import IO, List, MutableMapping, Optional, Sequence

import machine
import pre_push_checks as running
import pre_push_gate as gating
import pre_push_modes as modes
import pre_push_push as pushed
import pre_push_reuse as reuse
import pre_push_tree as where
from pre_push_repo import Repo, Say, Terminal
from tool_files import Files, LocalDisk

GIT_VARIABLES = ("GIT_DIR", "GIT_INDEX_FILE", "GIT_WORK_TREE", "GIT_OBJECT_DIRECTORY",
                 "GIT_ALTERNATE_OBJECT_DIRECTORIES", "GIT_COMMON_DIR", "GIT_PREFIX")
# THE JOB'S SPLIT IS THIS RUN'S ALONE: the checks read them as the hook's shell variables, never exported to what they run
SPLIT_VARIABLES = ("PREPUSH_ONLY", "PREPUSH_ELSEWHERE", "PREPUSH_HEAVY_ROWS", "PREPUSH_HEAVY_MONGODB_URI",
                   "PREPUSH_HEAVY_MONGODB_DB", "PREPUSH_IDENTITY_SHARD", "PREPUSH_IDENTITY_PLAN")


class Ended(Exception):
    """The run is over, with this exit status."""

    def __init__(self, code: int) -> None:
        super().__init__(code)
        self.code = code


def ended_by(signum: int, _frame: Optional[FrameType]) -> None:
    raise Ended(128 + signum)


class Run:
    """One run of the hook over the checkout at `repo.cwd`."""

    def __init__(self, hook: str, args: List[str], repo: Repo, say: Say, stdin: IO[str]) -> None:
        self.hook, self.args, self.repo, self.say, self.stdin = hook, args, repo, say, stdin
        self.files = repo.files
        self.env = repo.env
        self.started = machine.monotonic()
        self.tmp = machine.output(["mktemp", "-d"], env=self.env)[1].strip()
        self.shell = running.Shell(hook, self.files, self.env, repo.cwd, f"{self.tmp}/state.sh")
        self.functions = running.Python(self.shell)
        self.state = self.shell.state
        self.state.update({"root": repo.cwd, "tmp": self.tmp})
        self.push = pushed.Push()
        self.paths: List[str] = []
        self.drift: List[str] = []
        self.checks: List[str] = []
        self.head = self.base = self.base_commit = self.baseline = self.baseline_note = ""
        self.gate_state = "not-main"
        self.tree: Optional[str] = None
        self.keep_tmp = False

    # ── the selection, asked of pre_push_modes in this process ──
    def ask(self, mode: str, paths: Sequence[str]) -> Optional[List[str]]:
        checkout = modes.Checkout(self.files, os.path.dirname(self.hook), self.env, self.selection_run)
        code, out = modes.answer(checkout, mode, [], "".join(p + "\n" for p in paths))
        return [line for line in out.split("\n") if line] if code == 0 else None

    def selection_run(self, argv: Sequence[str], text: str) -> "tuple[int, str]":
        return machine.output(argv, text, cwd=self.repo.cwd, env=self.env)

    def plan(self, paths: List[str]) -> Optional[List[str]]:
        return self.ask("--plan", paths)

    def skipped(self, check: str) -> bool:
        return check in self.env.get("PREPUSH_SKIP", "").split()

    # ── what is pushed ──
    def read_push(self) -> None:
        if not (self.args[:1] and self.args[0] in pushed.DRY_RUNS):
            self.push = pushed.from_stdin(self.repo, self.say, self.stdin.read())
            self.squash_guard()
            return
        if len(self.args) < 2:
            self.say.raw(f"{self.hook}: line 1: 2: {self.args[0]} A..B\n")
            raise Ended(1)
        self.push = pushed.dry_run(self.args)

    def squash_guard(self) -> None:
        """THE SQUASH GUARD: agentctl refuses a push to main returning files to an older main (where it is installed)."""
        agentctl = f"{self.env.get('HOME', '')}/bin/agentctl"
        if self.push.to_main and os.access(agentctl, os.X_OK):
            remote = self.args[0] if self.args else "origin"
            url = self.args[1] if len(self.args) > 1 else ""
            self.say.flush()
            code = machine.run([agentctl, "prepush-check", "--hook", remote, url],
                               machine.Ran(self.repo.cwd, self.env), self.push.pushed)[0]
            if code != 0:
                raise Ended(1)

    def nothing_pushed(self) -> None:
        if self.push.ranges:
            return
        if self.push.measure_only and not self.push.other:
            self.say.log("measure/ or check/ branch: a CI run's ref, never landed -- nothing to check")
        if self.push.tag_only and not self.push.other:
            self.say.log("tag push: a tag changes nothing main builds -- nothing to check")
        raise Ended(0)

    # ── the red gate (pre_push_gate) ──
    def gate(self) -> None:
        if not self.push.to_main:
            return
        lint = f"{os.path.dirname(self.hook)}/../ci/trailers.py"
        if self.files.is_file(lint):
            ranges = [w for r in self.push.ranges for w in ("--range", r)]
            self.repo.status(["python3", lint, "lint", *ranges],
                             timeout=float(self.env.get("PREPUSH_TRAILER_LINT_SECONDS", "20")))
        if self.skipped("trailers"):
            self.say.warn("trailers SKIPPED (PREPUSH_SKIP)")
        elif not gating.Gate.of(self.repo, self.say).stranded_trailers(self.push.ranges):
            raise Ended(1)
        self.gate_state = self.judge_gate()

    def judge_gate(self) -> str:
        p = self.push
        if self.skipped("redgate"):
            self.say.warn("redgate SKIPPED (PREPUSH_SKIP)")
            return "skipped"
        for flag, mode, when in ((p.verify_only, "verify-only", "the change is"), (p.fast_only, "fast-only",
                                  "the batch is"), (p.checks_only, "checks-only", "these commits are")):
            if flag:
                self.say.log(f"--{mode}: the red gate is not judged (it is when {when} pushed)", True)
                return mode
        self.say.flush()
        landing, state = gating.Gate.of(self.repo, self.say).judge(p.ranges)
        if not landing:
            raise Ended(1)
        return state

    # ── the paths and the checks they select ──
    def read_changed(self) -> None:
        found = pushed.changed(self.repo, self.say, self.push.ranges)
        if found is None:
            raise Ended(1)
        if not found.drift:
            raise Ended(0)
        self.paths, self.drift = found.paths, found.drift
        code, head = self.repo.git("rev-parse", "--verify", self.push.ranges[-1].split("..", 1)[1] + "^{commit}")
        if code != 0:
            raise Ended(1)
        self.head, self.base = head.strip(), self.push.ranges[0].split("..", 1)[0]

    def read_baseline(self) -> None:
        folder = self.env.get("PREPUSH_BASELINE_DIR", "")
        if not folder:
            return
        commit = self.repo.git_out("rev-parse", "--verify", "-q", f"{self.base}^{{commit}}").strip()
        self.base_commit = commit
        path = f"{folder}/{commit}"
        if len(self.push.ranges) != 1:
            self.baseline_note = (f"no known-red baseline for a push of {len(self.push.ranges)} ranges: every failing "
                                  "check blocks")
        elif not commit or not os.access(path, os.R_OK):
            self.baseline_note = f"no known-red baseline for base {commit[:10]} in {folder}: every failing check blocks"
        elif self.files.read_text_or(path, "").split("\n", 1)[0] != f"pre-push baseline {commit}":
            self.baseline_note = f"{path} does not start 'pre-push baseline {commit}': every failing check blocks"
        else:
            self.baseline = path

    def select(self) -> None:
        if self.paths:
            planned = self.plan(self.paths)
            if planned is None:
                self.say.warn("the selection (scripts/pre_push.py --plan) failed: the push is BLOCKED")
                raise Ended(1)
            self.checks = planned
        if self.push.fast_only:
            self.checks = pushed.fast_select(self.repo, self.plan, self.checks, self.paths, self.head)
        self.verify_extras()
        self.checks = pushed.with_trailer_checks(self.repo, self.push.ranges, self.checks)
        self.checks = pushed.without_moved_areas(self.repo, self.say, self.plan, self.push.ranges, self.checks)
        self.drift_check()
        self.checks = pushed.split(self.say, self.env, self.checks)

    def verify_extras(self) -> None:
        """--verify-only: THE REPO-WIDE SELF-TESTS a changed path's readers hold, and the touched modules' unit tests."""
        self.state["scripttests_readers"] = ""
        if not self.push.verify_only:
            return
        runner = "scripts/run-script-tests"
        if "scripttests" not in self.checks and self.paths and "--readers" in self.files.read_text_or(runner, ""):
            globs = machine.output([runner, "--reads"], cwd=self.repo.cwd, env=self.env)[1].split("\n")
            if pushed.readers_selected(globs, self.paths):
                self.state["scripttests_readers"] = "1"
                self.checks.append("scripttests")
        if self.ask("--unit-plan", self.paths) and "sbt" not in self.checks:
            self.checks.insert(0, "sbt")

    def drift_check(self) -> None:
        claims = pushed.drift_claims(self.repo, self.say, self.files.is_file, self.push, self.drift)
        if claims is None:
            raise Ended(1)
        self.state["drift_reds"], self.state["drift_birth"] = claims
        if claims[0] or claims[1]:
            self.checks.append("identitydrift")

    # ── the reuse and a remote pass ──
    def verdict_of(self, gate: str, checks: List[str]) -> reuse.Verdict:
        return reuse.Verdict(self.head, self.base, gate, self.baseline, self.env.get("PREPUSH_SKIP", ""), checks,
                             self.paths)

    def short_tree(self) -> str:
        return self.repo.git_out("rev-parse", "--short", f"{self.head}^{{tree}}").strip()

    def reuse_dir(self) -> str:
        return self.env.get("PREPUSH_REUSE_DIR", f"{self.env.get('HOME', '')}/.cache/kinowo-prepush")

    def passed_before(self, key: Optional[str]) -> Optional[str]:
        path = f"{self.reuse_dir()}/{key}"
        return self.files.read_text_or(path, "").rstrip("\n") if key and self.files.is_file(path) else None

    def reused(self) -> Optional[str]:
        """The reuse key of this run; Ended(0) when a pass answers for it."""
        one = len(self.push.ranges) == 1 and not self.env.get("PREPUSH_NO_REUSE")
        if one and self.env.get("PREPUSH_REUSE_FAST") and not self.push.fast_only:
            fast = pushed.fast_select(self.repo, self.plan, self.checks, self.paths, self.head)
            when = self.passed_before(reuse.key(self.repo, self.files, self.hook, self.verdict_of("fast-only", fast)))
            if when is not None:
                self.say.log(f"the waiting batch's fast check passed for tree {self.short_tree()} on this base {when}: "
                             "reused (each change was checked alone before it joined, and this check covered the "
                             "combined tree)", True)
                raise Ended(0)
        self.remote_passed()
        if not one:
            return None
        key = reuse.key(self.repo, self.files, self.hook, self.verdict_of(self.gate_state, self.checks))
        when = self.passed_before(key)
        if when is not None:
            self.say.log(f"same tree {self.short_tree()}, base, hook and red-gate state as a push that passed {when}: "
                         f"{' '.join(self.checks)}reused, not run again (PREPUSH_NO_REUSE=1 runs them)", True)
            raise Ended(0)
        return key

    def remote_passed(self) -> None:
        run = self.env.get("PREPUSH_REMOTE_RUN", "")
        if not run or len(self.push.ranges) != 1 or not self.push.to_main:
            return
        if reuse.remote_pass(self.repo, run, self.head, float(self.env.get("PREPUSH_GH_SECONDS", "15"))):
            self.say.log(f"the checks passed in CI for {self.head[:10]} (precommit-check.yml run {run}, confirmed with "
                         "GitHub): not run again here", True)
            raise Ended(0)
        self.say.warn(f"PREPUSH_REMOTE_RUN={run} does not vouch for {self.head[:10]} (GitHub: not a successful "
                      "precommit-check.yml run of it): every check runs here")

    # ── running them ──
    def place(self) -> str:
        """The tree the checks run in (THE PUSHED COMMITS ARE JUDGED)."""
        sbt_asked = any(c in ("sbt", "identity") for c in self.checks) and not self.skipped("sbt")
        why, client = where.isolation(self.repo, self.say, self.head, self.paths, sbt_asked)
        area = "identity" in self.checks
        self.state["area_tests"] = "identity " if area else ""
        self.state["sbt_client"] = "1" if client or (not why and area) else "0"
        if not why:
            return self.repo.cwd
        self.tree = where.temporary_tree(self.repo, self.say, self.tmp, self.head, why)
        if self.tree is None:
            raise Ended(1)
        return self.tree

    def check_state(self) -> None:
        p = self.push
        self.state.update({
            "root": self.repo.cwd, "tmp": self.tmp, "head_sha": self.head, "base_sha": self.base,
            "base_commit": self.base_commit, "changed": "\n".join(self.paths), "drift_changed": "\n".join(self.drift),
            "ranges": p.ranges, "to_main": "1" if p.to_main else "0", "checks_only": "1" if p.checks_only else "",
            "fast_only": "1" if p.fast_only else "", "verify_only": "1" if p.verify_only else "",
            "checks": "\n".join(self.checks), "gate_state": self.gate_state, "baseline": self.baseline,
            "tree": self.tree or ""})

    def hold_split(self) -> None:
        """THE JOB'S SPLIT IS THIS RUN'S ALONE: kept as the checks' shell variables, never exported to what they run."""
        for name in SPLIT_VARIABLES:
            if name in self.env:
                self.state[name] = self.env.pop(name)

    def run_checks(self, key: Optional[str]) -> int:
        self.check_state()
        cwd = self.place()
        self.state["tree"] = self.tree or ""
        self.shell.cwd = cwd
        scripts = os.path.realpath(f"{os.path.dirname(self.hook)}/..")
        watch = f"{scripts}/stall-watch.sh"
        loop = running.Loop(self.functions, self.say, self.files, cwd, self.env, self.tmp,
                            watch if self.files.is_file(watch) else f"{self.repo.cwd}/scripts/stall-watch.sh",
                            self.env.get("PREPUSH_STALL_SECONDS", "1200"),
                            self.files.read_text_or(self.baseline, "") if self.baseline else "", self.base_commit,
                            self.temp_roots())
        self.say.log(f"{' '.join(self.checks)} — for {len(self.paths) or 1} changed path(s)")
        running.run_all(loop, self.checks, self.env.get("PREPUSH_SKIP", "").split())
        return self.verdict(loop, key)

    def temp_roots(self) -> List[str]:
        tmpdir = self.env.get("TMPDIR", "")
        return [self.tmp.rsplit("/", 1)[0] + "/", (tmpdir[:-1] if tmpdir.endswith("/") else tmpdir) + "/" if tmpdir
                else ""]

    def seconds(self) -> int:
        return int(machine.monotonic() - self.started)

    def verdict(self, loop: running.Loop, key: Optional[str]) -> int:
        if loop.failed:
            if self.baseline_note:
                self.say.warn(self.baseline_note)
            self.say.warn(f"blocked by:{''.join(' ' + c for c in loop.failed)} ({self.seconds()}s). Fix, or bypass "
                          f"with PREPUSH_SKIP=\"{' '.join(loop.failed)}\" / --no-verify.")
            self.keep_tmp = True
            return 75 if loop.stalled else 1
        self.store(key)
        if loop.known:
            self.say.log(f"all clear but main's own known red:{''.join(' ' + c for c in loop.known)} "
                         f"({self.seconds()}s)")
        else:
            self.say.log(f"all clear ({self.seconds()}s)")
        self.long_run_warning()
        return 0

    def store(self, key: Optional[str]) -> None:
        folder = self.reuse_dir()
        if not key or machine.output(["mkdir", "-p", folder])[0] != 0:
            return
        when = machine.output(["date", "-u", "+%Y-%m-%dT%H:%MZ"])[1].strip()
        self.files.replace_text(f"{folder}/{key}", f"{when} ({self.head[:9]})\n")
        machine.output(["find", folder, "-type", "f", "-mtime", "+14", "-delete"])

    def long_run_warning(self) -> None:
        if self.args[:1] and self.args[0] in ("--range", "--checks-only"):
            return
        took = self.seconds()
        if took < int(self.env.get("PREPUSH_LONG_SECONDS", "300")):
            return
        remote = self.args[0] if self.args else "<remote>"
        self.say.warn(f"this hook ran {took}s with the push's connection open (git connects first): if the push now "
                      "dies with \"closed by remote host\" / \"Broken pipe\" / \"unexpected disconnect\", run the same "
                      "git push again -- this pass is stored and is reused in seconds. Next time check first: "
                      f"scripts/hooks/pre-push --range {remote}/main..HEAD, then git push")

    def go(self) -> int:
        self.read_push()
        self.nothing_pushed()
        self.gate()
        self.read_changed()
        self.read_baseline()
        self.select()
        if self.env.get("PREPUSH_LIST") == "1":
            self.say.raw("".join(c + "\n" for c in self.checks if c), False)
            return 0
        if not self.checks and not self.push.fast_only:
            return 0
        self.hold_split()
        return self.run_checks(self.reused())

    def finish(self, at_start: str) -> None:
        where.on_exit(self.repo, self.say, self.tree, "identity" in self.checks, at_start)
        if not self.keep_tmp and self.tmp:
            machine.output(["rm", "-rf", self.tmp])


def main(hook: str, args: List[str], env: MutableMapping[str, str], stdin: IO[str], out: IO[str],
         err: IO[str]) -> int:
    for name in GIT_VARIABLES:
        env.pop(name, None)
    code, root = machine.output(["git", "rev-parse", "--show-toplevel"], env=env)
    if code != 0 or not root.strip():
        return 1
    repo, say = Repo(root.strip(), env, Files(LocalDisk(), root.strip())), Terminal(out, err)
    env.pop("PREPUSH_DELEGATED_FROM", None)
    at_start = where.keep_hooks_path_relative(repo, say)
    where.root_warning(repo, say, hook)
    for signum in (signal.SIGTERM, signal.SIGINT, signal.SIGHUP):
        signal.signal(signum, ended_by)
    run = Run(hook, args, repo, say, stdin)
    try:
        return run.go()
    except Ended as ended:
        return ended.code
    finally:
        run.finish(at_start)
