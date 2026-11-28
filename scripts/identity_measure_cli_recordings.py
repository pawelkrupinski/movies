"""THE RECORDINGS of identity-measure (scripts/identity_measure_cli.py, its header): which countries a recording holds
(its scrape-fixtures-<cc> artifacts, or the identity-recordings release's archived corpora), the other recordings a
cross-country change is measured over too, each a measurement of its own run in parallel, and the --plan."""
from __future__ import annotations

import concurrent.futures
import os
import re
import sys
from typing import List, Optional, Tuple

from cli import tool_countries
import tool_process
from identity_measure_cli_context import HERE, Context, Die, csv_and, say
from identity_measure_cli_results import read_stripped

Recording = Tuple[str, str]  # (run, the countries it holds)


class Recordings:
    def __init__(self, c: Context) -> None:
        self.c = c

    def slug(self) -> str:
        url = (self.c.git.out("remote", "get-url", self.c.remote, quiet=True) or "").strip()
        url = re.sub(r"\.git$", "", url)
        found = re.match(r"^.*[:/]([^/:]+/[^/]+)$", url)
        return found.group(1) if found else url

    def holds(self, run: str) -> Optional[str]:
        """The countries whose corpus the recording run holds, read from the forge; None when they cannot be read. A
        run this forge never recorded (e.g. 38713274617) holds nothing, and every leg replaying it failed "the pinned
        pair has expired" (runs 884-893, 2026-10-21)."""
        slug = self.slug()
        names = self.c.gh(["api", f"repos/{slug}/actions/runs/{run}/artifacts", "--jq", ".artifacts[]?.name"])
        if names is None:
            return None
        tag = self.c.env.get("RECORDING_ARCHIVE_TAG") or "identity-recordings"
        assets = self.c.gh(["api", f"repos/{slug}/releases/tags/{tag}", "--jq", ".assets[]?.name"], quiet=True) or ""
        held = set(names.split()) | {f"scrape-fixtures-{m.group(1)}" for m in
                                     re.finditer(rf"(?m)^scrapes-([a-z]+)-{run}\.tar\.(gz|zst)$", assets)}
        return ",".join(cc for cc in tool_countries.CODES if f"scrape-fixtures-{cc}" in held)

    def others(self) -> List[Recording]:
        """IDENTITY_MEASURE_RECORDINGS (default 528, all five) less the one measured: `<run>[:<countries>]`, read from
        the forge without them (every country when that cannot be read); one holding none is left out, said."""
        found: List[Recording] = []
        for spec in self.c.env.get("IDENTITY_MEASURE_RECORDINGS", "528").split():
            run, _, holds = spec.partition(":")
            if ":" not in spec:
                holds = self.holds(run) or ",".join(tool_countries.CODES)
            if not holds:
                say(f"recording {run} holds no corpus on this forge: skipped")
            elif run != (self.c.state.recording or "pinned"):
                found.append((run, holds))
        return found

    def narrow(self) -> None:
        """A named recording must hold the countries measured: one it lacks fails every leg of the run, an hour in."""
        s, o = self.c.state, self.c.o
        held = self.holds(s.recording) if s.recording else None
        if held is None:
            return
        if not held:
            raise Die(f"recording {s.recording} holds no corpus on this forge (a github.com-era run?): use the pinned "
                      "pair (no --recording) or one of IDENTITY_MEASURE_RECORDINGS")
        both = csv_and(s.countries, held)
        if s.named and both and both != s.countries and not o.all_countries:
            raise Die(f"recording {s.recording} holds only {held}, not all of --countries {s.countries}")
        if not both:
            raise Die(f"recording {s.recording} holds {held}: none of {s.countries}")
        if both != s.countries:
            say(f"recording {s.recording} holds {held}: measuring {both}")
            s.countries = s.named = both

    def plan(self) -> None:
        s = self.c.state
        print(f"footprint {s.countries}")
        print(f"measure {s.recording or 'pinned'} {s.countries}")
        if self.c.o.recordings == "all":
            for run, holds in self.others():
                on = csv_and(s.countries, holds)
                if on:
                    print(f"measure {run} {on}")
        elif self.c.o.recordings != "one":
            print("then, when LANDABLE and 2+ countries moved: " + ";".join(f"{r} {h}" for r, h in self.others()))

    def then_cross_country(self, verdict: int, run: str) -> int:
        """The pre-diff's follow-up: the other recordings, for the countries that moved, when the one recording was
        LANDABLE and two or more countries moved."""
        if verdict != 0 or self.c.o.recordings != "auto":
            return verdict
        moved = read_stripped(self.c.measured(run, "moved"))
        if "," not in moved:
            return 0
        say(f"CROSS-COUNTRY: takes moved in {moved} -- measuring the other recordings for them (--one-recording skips this)")
        return self.fan_out(moved, self.others())

    def fan_out(self, countries: str, recordings: List[Recording]) -> int:
        """This change measured over each recording at once, each its own measurement: every table printed, and the
        worst verdict -- NOT LANDABLE over could-not-measure over still-running."""
        jobs = [(run, on) for run, holds in recordings for on in [csv_and(countries, holds)] if run and on]
        for run, on in jobs:
            say(f"measuring over recording {run} too ({on}), in parallel")
        logs = [os.path.join(self.c.scratch_dir(), run) for run, _ in jobs]
        with concurrent.futures.ThreadPoolExecutor(max_workers=max(len(jobs), 1)) as pool:
            started = [pool.submit(self.one, run, on, log) for (run, on), log in zip(jobs, logs)]
            codes = [each.result() for each in started]
        worst = 0
        for (run, _), log, code in zip(jobs, logs, codes):
            print(f"\n── recording {run} ──")
            with open(log) as f:
                sys.stdout.write(f.read())
            worst = worse(worst, code)
        sys.stdout.flush()
        return worst

    def one(self, run: str, countries: str, log: str) -> int:
        o, s = self.c.o, self.c.state
        argv = [sys.executable, os.path.join(HERE, "identity_measure_cli.py"), "--head", s.commit, "--base", s.base,
                *([] if run == "pinned" else ["--recording", run]), "--countries", countries, "--one-recording",
                *(["--json"] if o.json else []), *(["--timeout", str(o.timeout_min)] if o.timeout_set else []),
                *(["--cancel-on-timeout"] if o.cancel_on_timeout else []),
                *(["--no-what-if", o.no_what_if] if o.no_what_if else [])]
        return self.c.runner.run(tool_process.Command(argv, self.c.env, log=log))


def worse(worst: int, code: int) -> int:
    """1 (not landable) beats 2 (not made) beats 3 (still running -- no verdict, never a pass) beats 0."""
    rank = {1: 3, 2: 2, 3: 1, 0: 0}
    return code if rank.get(code, 2) > rank.get(worst, 2) else worst
