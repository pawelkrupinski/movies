"""`identity-measure --run <id>` (scripts/identity_measure_cli.py): an existing run collected -- waited for while it runs
(at most --timeout; with --no-wait exit 3 at once) -- classified with this checkout's judgements, stored for REUSE when
it measured a commit this clone has, its verdict stored under a --no-wait dispatch's key, and its measure/ branch
deleted."""
from __future__ import annotations

import os
from datetime import datetime
from typing import Optional

from identity_measure_cli_collect import Collector, first_base
from identity_measure_cli_context import JUDGEMENTS, Context, Die, say
from identity_measure_cli_paths import TakePaths
from identity_measure_cli_results import Results, blob, read_stripped
from identity_measure_cli_wait import Waiter

MAIN_CI_OVER = "measure: origin/main's CI over "


class CollectRun:
    def __init__(self, c: Context, waiter: Waiter, collector: Collector, paths: TakePaths, results: Results) -> None:
        self.c, self.waiter, self.collector, self.paths, self.results = c, waiter, collector, paths, results

    def main(self, run: str) -> int:
        c, s = self.c, self.c.state
        view = c.gh(["run", "view", run, "--json", "status,headBranch,headSha", "--jq",
                     "[.status, .headBranch, .headSha] | @tsv"])
        if view is None:
            raise Die(f"could not read run {run}")
        status, branch, sha = (view.rstrip("\n").split("\t") + ["", ""])[:3]
        conclusion = self.waited(run, status) if status != "completed" else ""
        if conclusion is None:
            return 3
        s.branch = branch if branch.startswith("measure/") else ""
        if conclusion == "takes-done":  # its takes rows ended; the run's tail still reads the branch
            say(f"run {run}: every takes row has ended -- collecting now")
        verdict = self.collector.collect(run, sha, checkout=True)
        base = first_base(c, run) or ""
        if verdict <= 1:
            sha = self.store(run, base, sha)
        self.store_result(run, verdict)
        if verdict == 0:
            self.say_cross_country(run, sha, base)
        return verdict

    def say_cross_country(self, run: str, sha: str, base: str) -> None:
        moved = read_stripped(self.c.measured(run, "moved"))
        if "," in moved:
            say(f"CROSS-COUNTRY: takes moved in {moved} -- before landing, measure the other recordings too (THE "
                "RECORDINGS):")
            say(f"  scripts/identity-measure --head {sha[:12]} --base {base[:12]} --countries {moved} --all-recordings")

    def waited(self, run: str, status: str) -> Optional[str]:
        """A run still going: its conclusion once it has one (None: --no-wait, said)."""
        s = self.c.state
        if self.c.o.no_wait:
            s.keep = s.finished = True
            say(f"run {run} is {status}: collect it once it completes")
            return None
        # waited for here, bounded, rather than leave every caller its own poll loop (agents spent ~10 min on one)
        say(f"waiting for run {run} ({status}), at most --timeout {self.c.o.timeout_min} minutes")
        self.waiter.say_eta(run)
        self.waiter.default_timeout()
        conclusion = self.waiter.for_takes(run, self.c.o.timeout_min * 60)
        if conclusion is None:
            self.waiter.timed_out(run)
        return conclusion

    def store(self, run: str, base: str, sha: str) -> str:
        """A takes-only measure run stored for reuse: its base from the artifact, its head the commit (not main's CI
        laid over it); not one whose head this clone lacks. The head it measured."""
        c = self.c
        if not (c.state.branch and base and c.git.ok("cat-file", "-e", f"{sha}^{{commit}}", quiet=True)):
            return sha
        if (c.git.out("log", "-1", "--format=%s", sha, quiet=True) or "").startswith(MAIN_CI_OVER):
            sha = (c.git.out("rev-parse", f"{sha}^", quiet=True) or sha).strip()
        created = c.gh(["run", "view", run, "--json", "createdAt", "--jq", ".createdAt"])
        if created:
            at = datetime.fromisoformat(created.strip().replace("Z", "+00:00")).timestamp()
            self.paths.store(base, sha, run, int(at))
        return sha

    def store_result(self, run: str, verdict: int) -> None:
        """A --no-wait dispatch's verdict, stored under its request's key -- when classified with the head's own
        judgements."""
        c, s = self.c, self.c.state
        key_file = c.measured(run, "result-key")
        if not (os.path.isfile(key_file) and os.path.getsize(key_file)) or not s.collected_judgements:
            return
        key, s.commit, s.base, recording, s.countries = (read_stripped(key_file).split("\t") + [""] * 5)[:5]
        s.recording = "" if recording == "pinned" else recording
        own = (c.git.out("rev-parse", "-q", "--verify", f"{s.commit}:{JUDGEMENTS}", quiet=True) or "").strip()
        if blob(s.collected_judgements) == own:
            self.results.store_as(key, verdict, run, s.collected_table, c.measured(run, "moved"))
        else:
            say(f"cache: not stored -- classified with this checkout's judgements, not {s.commit[:9]}'s")
