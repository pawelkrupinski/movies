"""COLLECTING a run for identity-measure (scripts/identity_measure_cli.py): its identity-takes-<cc> artifacts downloaded
once into the shared cache (under identity-tools.sh's identity_locked: two agents collecting one run never download
into, or read, the same directory at once), its takes rows' conclusions, the classification with the head's
judgements (or the checkout's, for --run), and where its time went."""
from __future__ import annotations

import glob
import json
import os
import shlex
import shutil
from typing import Optional

from cli import tool_countries
from identity_measure_cli_context import IDENTITY_TOOLS, JUDGEMENTS, TAKES_ROW_JQ, Context, Die, say
from identity_measure_cli_results import read_stripped

# Run under the lock, in identity-tools.sh's shell (gh_download: bounded, a failed read asked again; absent, its 3, a
# run without takes; a failed read, 1, no answer).
DOWNLOAD = '''c={cache}; dl="$c/artifacts"
if [ -f "$c/downloaded" ]; then :; else
  rm -rf "$dl"; mkdir -p "$dl"
  gh_download {timeout} run download {run} --pattern 'identity-takes-*' --dir "$dl"
  case $? in
    0) touch "$c/downloaded" ;;
    3) echo "identity-measure: run {run}: no identity-takes artifact to download (a run from before the takes rows uploaded theirs?)" >&2 ;;
    *) echo "identity-measure: could not download run {run}'s identity-takes artifacts (gh failed or timed out)" >&2; false ;;
  esac
fi'''
# Where a measurement's time went: the run's wait for runners (from its first job's start: on the main lane it also
# waits for the lane) and its length, and each takes row's job (and its base row's).
TIMINGS_JQ = '''
    def s: if . == null or . == "" then null else fromdateiso8601 end;
    ([.jobs[].startedAt | s | select(. != null)] | min) as $first |
    "run: created \\(.createdAt), waited for runners \\(($first - (.createdAt|s)) // "?")s, then ran \\(((.updatedAt|s) - $first) // "?")s",
    (.jobs[] | select(.name | test(" / takes(-base)?( \\\\([a-z-]+\\\\))?$")) |
      "  \\(.name): \\(((.completedAt|s) - (.startedAt|s)) // "?")s \\(.conclusion // "unfinished")")'''


class Collector:
    def __init__(self, c: Context) -> None:
        self.c = c

    def collect(self, run: str, commit: str, checkout: bool = False) -> int:
        """The run's takes rows classified: the verdict (its table on stdout). `checkout`: with this checkout's
        judgements, committed or not (--run re-classifies an older run with the judgements made since)."""
        scratch = self.c.scratch_dir()
        rows, judgements, table = (os.path.join(scratch, n) for n in ("rows.json", "identity-judgements.tsv", "table.txt"))
        os.makedirs(self.c.measured(run), exist_ok=True)
        url = self.c.gh(["run", "view", run, "--json", "url", "--jq", ".url"])
        if url is None:
            raise Die(f"could not read run {run}")
        self.write_rows(run, rows)
        if not self.download(run):
            raise Die(f"run {run}'s takes could not be downloaded (above)")
        self.judgements(commit, judgements, checkout)
        dl = self.c.measured(run, "artifacts")
        verdict = self.c.classify(dl, judgements, rows, table, ["--run-url", url.strip(), "--moved-out",
                                                                  self.c.measured(run, "moved"), "--kind", "full"])
        self.c.state.collected_table, self.c.state.collected_judgements = table, judgements
        self.timings(run, dl)
        return verdict

    def write_rows(self, run: str, rows: str) -> None:
        jobs = self.c.gh(["run", "view", run, "--json", "jobs", "--jq",
                          TAKES_ROW_JQ + '(.jobs // [])[] | (.name | takes_country) as $c | [$c, .conclusion] | @tsv'])
        if jobs is None:
            raise Die(f"could not read run {run}'s jobs")
        conclusions = {}
        for line in jobs.splitlines():
            name, _, conclusion = line.partition("\t")
            if name:
                conclusions[tool_countries.code(name)] = conclusion or "unfinished"
        with open(rows, "w") as f:
            json.dump(conclusions, f)

    def download(self, run: str) -> bool:
        script = DOWNLOAD.format(cache=shlex.quote(self.c.measured(run)), timeout=int(self.c.gh_timeout), run=run)
        return self.c.shell(IDENTITY_TOOLS, "identity_locked", [f"measure-{run}", "eval", script]).code == 0

    def judgements(self, commit: str, path: str, checkout: bool) -> None:
        top = (self.c.git.out("rev-parse", "--show-toplevel", quiet=True) or "").strip()
        own = os.path.join(top, JUDGEMENTS)
        if checkout and os.path.isfile(own):
            shutil.copyfile(own, path)
            say(f"classifying with this checkout's judgements ({own})")
        elif not self.c.judgements_into(commit, path):
            raise Die("no judgements to classify with")

    def timings(self, run: str, dl: str) -> None:
        """On stderr (the table, or --json, stays alone on stdout): the run's, each takes row's, and each row's phases
        from identity-takes-check's timings.tsv -- which a stored result spared ("reused") and which were resolved."""
        times = self.c.gh(["run", "view", run, "--json", "createdAt,startedAt,updatedAt,jobs", "--jq", TIMINGS_JQ])
        if times is None:
            say(f"run {run}: could not read its timings")
            return
        say(f"timings of run {run}")
        for line in times.rstrip("\n").split("\n"):
            say(f"  {line}")
        for path in sorted(glob.glob(os.path.join(dl, "identity-takes-*", "timings.tsv"))):
            cc = os.path.basename(os.path.dirname(path))[len("identity-takes-"):]
            say(f"  {cc} phases: {phases(path)}")


def phases(path: str) -> str:
    with open(path) as f:
        rows = [line.rstrip("\n").split("\t") for line in f.readlines()[1:]]
    return ", ".join(f"{r[0]} {r[1]}s ({r[2]})" for r in rows if len(r) >= 3)


def first_base(c: Context, run: str) -> Optional[str]:
    """The base a run's takes rows resolved, from its first artifact's base.txt."""
    for path in sorted(glob.glob(c.measured(run, "artifacts", "identity-takes-*", "base.txt"))):
        return read_stripped(path).split("\n", 1)[0] or None
    return None
