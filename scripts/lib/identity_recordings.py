"""A RECORDING DOWNLOADED ONCE INTO THE SHARED CACHE (scripts/lib; identity-tools.sh's identity_recording,
identity_recording_cached, identity_newest_recording and identity_recording_run): a "Record scrape fixtures" run's
corpus/ and enrichment-<cc>/ (with `corpus`: the scrape corpus alone), fetched by scripts/identity-fetch-recording.sh,
one download of a run's country at a time across agents. With IDENTITY_FILLS (the fill assets a hermetic leg laid over
its tree, as its log names them), the pair with those fills laid over it, cached apart from the bare pair: the two
answer differently.

  Recordings(ctx, locks).recording(run, cc, what="") -> the fixtures directory holding that run's corpus/ and enrichment-<cc>/
  .cached(run, cc)             that directory when the cache already holds it, else fails: never downloads
  .newest(cc="")               the id of the newest successful run (with a country: the newest holding its corpus, each
                               newer one skipped said on stderr); fails when none of the newest 20 holds it -- or when a
                               run's artifacts could not be read: an unreadable run is never skipped for an older one
  .run_named(spec, cc)         the run id a --recording names; `prod-latest`: the freshest COMPLETE nightly recording of
                               that country, as `prod-latest-<cc>.txt` on the convergence-fixtures release names it"""
from __future__ import annotations

import os
import re
from typing import List

import machine
import tool_python
from identity_context import Context, Failed
from identity_lock import Locks

NEWEST_WITH_COUNTRY = 20
LISTED = 100


class Recordings:
    def __init__(self, ctx: Context, locks: Locks) -> None:
        self.ctx, self.locks = ctx, locks

    def key(self, run: str) -> str:
        """The run's cache key: the run, and with IDENTITY_FILLS a digest of the fills it names."""
        if not self.ctx.fills:
            return run
        words = sorted(re.sub(" +", "\n", self.ctx.fills.replace(",", " ")).split("\n"))
        code, out = machine.output(["cksum"], text_in="\n".join(words) + "\n")
        return f"{run}-fills-{out.split()[0] if code == 0 and out.split() else ''}"

    def fixtures(self, run: str) -> str:
        return os.path.join(self.ctx.cache, "recordings", self.key(run), "pair", "test", "resources", "fixtures")

    @staticmethod
    def recorded(fixtures: str, cc: str, what: str = "") -> bool:
        """Whether the recording is laid out: the corpus is laid out last."""
        return os.path.isfile(os.path.join(fixtures, "corpus", f"cinema-scrapes-{cc}.json.gz")) and (
            what == "corpus" or os.path.isdir(os.path.join(fixtures, f"enrichment-{cc}")))

    def recording(self, run: str, cc: str, what: str = "") -> str:
        fixtures = self.fixtures(run)
        if not self.recorded(fixtures, cc, what):
            with self.locks.held(f"recording-{self.key(run)}-{cc}"):
                self.download(run, cc, what)
        return fixtures

    def cached(self, run: str, cc: str) -> str:
        fixtures = self.fixtures(run)
        if not self.recorded(fixtures, cc):
            self.ctx.log(f"recording {run} holds no {cc} corpus and tree in {fixtures} "
                         "(fetch it: scripts/identity-fetch-recording.sh)")
            raise Failed(1)
        return fixtures

    def download(self, run: str, cc: str, what: str) -> None:
        directory = os.path.join(self.ctx.cache, "recordings", self.key(run))
        if self.recorded(self.fixtures(run), cc, what):
            return
        alone = f", its {what} alone" if what else ""
        self.ctx.log(f"downloading recording {run} ({cc}{alone}{', with its fills' if self.ctx.fills else ''}) into {directory}")
        argv = [os.path.join(self.ctx.repo, "scripts", "identity-fetch-recording.sh"),
                *(["--corpus-only"] if what else []), *(["--fills", self.ctx.fills] if self.ctx.fills else []),
                run, directory, cc]
        if machine.status(argv, out=2) != 0:
            self.ctx.log(f"could not download recording {run} ({cc})")
            raise Failed(1)

    def run_named(self, spec: str, cc: str) -> str:
        if spec != "prod-latest":
            return spec
        script = os.path.join(os.path.dirname(os.path.abspath(__file__)), "gh_download.py")
        status, out = machine.captured(tool_python.isolated(script, "prod-latest", cc), env=self.ctx.env)
        if status != 0:
            raise Failed(1)
        run = out.strip()
        self.ctx.log(f"prod-latest {cc} recording: run {run}")
        return run

    def newest(self, cc: str = "") -> str:
        ids = self.listed(NEWEST_WITH_COUNTRY if cc else 1)
        if not ids:
            self.ctx.log('no successful "Record scrape fixtures" run')
            raise Failed(1)
        if not cc:
            return ids[0]
        for run in ids:
            if self.holds(run, cc):
                self.ctx.log(f"recording {run}: the newest holding a {cc} corpus")
                return run
            self.ctx.log(f"recording {run} skipped: it holds no {cc} corpus (no live scrape-fixtures-{cc} artifact)")
        self.ctx.log(f"none of the newest {NEWEST_WITH_COUNTRY} successful recordings holds a {cc} corpus: name one with --recording")
        raise Failed(1)

    def listed(self, count: int) -> List[str]:
        """The newest runs by CREATION time, not listing order: GitHub's status-filtered listing is not in creation
        order (`--limit 1` named 2026-09-25's run on 2026-10-07, the newest being 2026-10-06's -- 2026-10-17)."""
        status, out = self.ctx.gh(self.ctx.gh_read, "run", "list", "-R", self.ctx.repository, "--workflow",
                                  "Record scrape fixtures", "--status", "success", "-L", str(LISTED), "--json",
                                  "databaseId,createdAt", "-q", '.[] | "\\(.databaseId)\\t\\(.createdAt)"')
        if status != 0:
            self.ctx.log('could not list the "Record scrape fixtures" runs (gh failed or timed out): name one with --recording')
            raise Failed(1)
        rows = [line.split("\t") for line in out.splitlines() if line]
        rows.sort(key=lambda row: row[1] if len(row) > 1 else "", reverse=True)
        return [row[0] for row in rows[:count]]

    def holds(self, run: str, cc: str) -> bool:
        """Whether the run holds the country's corpus: already cached, or a live scrape-fixtures-<cc> artifact."""
        if self.recorded(self.fixtures(run), cc, "corpus"):
            return True
        status, out = self.ctx.gh(self.ctx.gh_read, "api", f"repos/{self.ctx.repository}/actions/runs/{run}/artifacts?per_page=100",
                                  "--paginate", "-q",
                                  f'.artifacts[] | select(.name == "scrape-fixtures-{cc}" and (.expired | not)) | .name')
        if status != 0:
            self.ctx.log(f"could not read recording {run}'s artifacts (gh failed or timed out): name one with --recording")
            raise Failed(1)
        return bool(out.strip())
