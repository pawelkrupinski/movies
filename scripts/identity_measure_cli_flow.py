"""identity-measure's course (scripts/identity_measure_cli.py, its header): the commit, base and countries measured,
then the first of --take-paths, --run, --plan, THE RESULT CACHE, the footprint measure, --all-recordings, REUSE and a
fresh dispatch that answers; and the clean-up every exit runs (the run cancelled, the measure/ branch deleted)."""
from __future__ import annotations

import signal
import sys
from typing import Optional

import identity_measure_cli_options as options_
from cli import tool_countries
from identity_measure_cli_context import Context, Die, Exit, say
from identity_measure_cli_dispatch import Dispatch, parts_of, what_if_gate
from identity_measure_cli_footprint import Footprint
from identity_measure_cli_run import CollectRun


class Measure:
    def __init__(self, c: Context) -> None:
        self.c = c
        self.dispatcher, self.parts = parts_of(c)
        self.waiter, self.collector, self.paths, self.results, self.recordings = self.parts

    def main(self) -> int:
        o = self.c.o
        self.countries()
        if o.take_paths:  # --take-paths reads HEAD alone
            return self.take_paths()
        if not self.c.git.ok("fetch", "-q", self.c.remote, "main", timeout=120):
            raise Die(f"could not fetch {self.c.remote} main")
        if o.run:
            return CollectRun(self.c, self.waiter, self.collector, self.paths, self.results).main(o.run)
        return self.measure()

    def measure(self) -> int:
        o, s = self.c.o, self.c.state
        s.commit = self.commit()
        s.base = self.base()
        self.footprint()
        self.recordings.narrow()
        if o.plan:
            self.recordings.plan()
            return 0
        mode = "full" if o.full else "takes"
        answered = self.answered(mode)
        if answered is not None:
            return answered
        return self.dispatched(mode)

    def dispatched(self, mode: str) -> int:
        """Every recording at once (--all-recordings), else REUSE, else a fresh dispatch."""
        o, s = self.c.o, self.c.state
        if o.recordings == "all" and not o.no_wait:
            what_if_gate(self.c)
            return self.recordings.fan_out(s.countries, [(s.recording or "pinned", s.countries),
                                                         *self.recordings.others()])
        reused = None if o.fresh or o.full or s.recording else self.reused(mode)
        if reused is not None:
            return reused
        return Dispatch(self.c, self.dispatcher, self.parts).main(mode)

    def countries(self) -> None:
        o, s = self.c.o, self.c.state
        s.countries = ",".join(o.countries or tool_countries.CODES)
        s.named = ",".join(o.countries)
        if s.recording == "prod-latest":  # the freshest complete nightly recording, by the first country's marker
            done = self.c.shell("scripts/tool-conventions.sh", "prod_latest_run", [s.countries.split(",")[0]])
            if done.code != 0:
                raise Exit(2)
            s.recording = done.stdout.strip()
            say(f"--recording prod-latest is run {s.recording}")
        options_.checked_recording(s.recording)

    def take_paths(self) -> int:
        patterns = self.paths.patterns("HEAD")
        if patterns is None:
            raise Die("HEAD has no .github/convergence-paths.txt")
        with open(patterns) as f:
            sys.stdout.write(f.read())
        return 0

    def commit(self) -> str:
        o, git = self.c.o, self.c.git
        commit = (git.out("rev-parse", "--verify", "-q", f"{o.ref or 'HEAD'}^{{commit}}", quiet=True) or "").strip()
        if not commit:
            raise Die(f"no commit {o.ref or 'HEAD'}")
        if not o.ref and (git.out("status", "--porcelain", "--untracked-files=no", quiet=True) or "").strip():
            say(f"WARNING: uncommitted changes are not measured -- only {commit}")
        return commit

    def base(self) -> str:
        """--base, else the commit's merge-base with the remote's main; the commit's parent when it is on main."""
        c, commit = self.c, self.c.state.commit
        base = c.o.base or (c.git.out("merge-base", commit, f"{c.remote}/main", quiet=True) or "").strip()
        if not base:
            raise Die(f"no merge-base of {commit} and {c.remote}/main")
        if base == commit and not c.o.base:
            base = (c.git.out("rev-parse", f"{commit}^", quiet=True) or "").strip()
        verified = (c.git.out("rev-parse", "--verify", "-q", f"{base}^{{commit}}", quiet=True) or "").strip()
        if not verified:
            raise Die(f"no base commit {base}")
        return verified

    def footprint(self) -> None:
        """THE FOOTPRINT: without --countries, only the countries the change can move a take in."""
        o, s = self.c.o, self.c.state
        if s.named or o.all_countries or o.full:
            return
        reach = self.paths.footprint(s.base, s.commit)
        if not reach:
            say("footprint: the change touches no path that can move a take -- measuring every country")
        elif reach != s.countries:
            say(f"footprint: only {reach} can move (--all-countries to measure every country)")
            s.countries = s.named = reach

    def answered(self, mode: str) -> Optional[int]:
        """A verdict that dispatches nothing: THE RESULT CACHE, then the footprint measure; None when the full measure
        must run."""
        cached = self.cached(mode)
        return cached if cached is not None or not self.c.o.footprint else self.footprint_measured()

    def cached(self, mode: str) -> Optional[int]:
        """The full measure's stored verdict answers a footprint request too (it is the stronger), then a footprint's."""
        o = self.c.o
        if o.recordings == "all" and not o.no_wait:
            return None
        for each in [mode, *(["footprint"] if o.footprint else [])]:
            verdict = self.results.lookup(each)
            if verdict is not None and each == "footprint":
                return verdict
            if verdict is not None:
                return self.recordings.then_cross_country(verdict, self.results.cached_run)
        return None

    def footprint_measured(self) -> Optional[int]:
        verdict = Footprint(self.c, self.paths, self.results).measure()
        if verdict is not None:
            return verdict
        if self.c.o.footprint == "only":
            say("--footprint-only: not measured in full (run without it to dispatch the full measure)")
            return 3
        say("measuring in full (measure: full)")
        return None

    def reused(self, mode: str) -> Optional[int]:
        """REUSE: an earlier measurement of the same change, main moved only outside the paths that move a take."""
        s = self.c.state
        found = self.paths.find(s.base, s.commit)
        if found is None:
            return None
        run, b0 = found
        how = "the same base" if b0 == s.base else "main moved only outside the paths that move a take"
        say(f"REUSED measurement of run {run} (base {b0[:9]}, same change, {how}) -- --fresh to dispatch")
        self.c.say_commit_and_base()
        verdict = self.collector.collect(run, s.commit)
        self.results.store(mode, verdict, run, s.collected_table, self.c.measured(run, "moved"))
        return self.recordings.then_cross_country(verdict, run)

    def clean_up(self) -> None:
        """Run to the end: a second Ctrl-C (or the TERM after a first) mid-clean-up would leave the branch behind."""
        for sig in (signal.SIGINT, signal.SIGTERM):
            signal.signal(sig, signal.SIG_IGN)
        s = self.c.state
        if s.measured_run and not s.finished:  # the run would replay a branch about to vanish
            self.cancel(s.measured_run, "run")
        if s.convergence_run and not s.convergence_done and not s.keep:  # --full's, until its conclusion is read
            self.cancel(s.convergence_run, "convergence run")
        if s.branch and not s.keep:
            self.delete_branch()
        elif s.branch:
            say(f"kept branch {s.branch}")

    def cancel(self, run: str, what: str) -> None:
        say(f"cancelling {what} {run}")
        if self.c.gh(["run", "cancel", run]) is None:
            say(f"could not cancel run {run} -- cancel it by hand")

    def delete_branch(self) -> None:
        c, branch = self.c, self.c.state.branch
        if c.git.ok("push", "-q", c.remote, "--delete", f"refs/heads/{branch}", timeout=120, quiet=True):
            say(f"deleted branch {branch}")
            return
        # a push that failed may never have made the branch (ls-remote's 2: no such ref): nothing to delete
        listed = c.git.call("ls-remote", "--exit-code", c.remote, f"refs/heads/{branch}", timeout=60, quiet=True)
        if listed.code != 2:
            say(f"could not delete branch {branch} -- git push {c.remote} --delete {branch}")
