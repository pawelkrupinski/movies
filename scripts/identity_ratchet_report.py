"""The unmatched-cluster ratchet's findings as a clean table — WRONG / UNJUDGED / MOVED / LOST / NEW takes, each with
the decision's explanation — from an "Identity capture" run, the PR it opened, or a local ratchet run.

  scripts/identity-ratchet-report <run id | PR number | ratchet-takes.tsv | --local> [--status S[,S]] [--country cc]
                                  [--explain] [--tsv] [--limit N]

  run id      an identity-capture.yml run: its `identity-ratchet` artifact (ratchet-takes.tsv, written by
              the ratchet, scripts.UnmatchedClustersRatchet). A run from before the artifact existed is read from its pull-request
              job's log instead (no explanations; a LOST take's venue is the text before its first comma).
  PR number   a capture PR (opened only once the ratchet is green): the run its body names ("run 38389526184").
  --local     target/identity-unmatched/ratchet-takes.tsv, written by a local
              `sbt "worker/Test/runMain scripts.UnmatchedClustersRatchet"` (or its spec, UnmatchedClustersRatchetSpec).
  --status    only these (default WRONG,UNJUDGED,MOVED,LOST); ALL for every finding
  --explain   print each take's explanation under it
  --tsv       print the findings as TSV (the artifact's columns) instead of the table, for a script or a spreadsheet

Downloads are kept in $KINOWO_IDENTITY_CACHE/ratchet/<run>/ (default ~/.cache/kinowo-identity): a repeat is instant.
"""

from __future__ import annotations

import io
import json
import os
import re
import shutil
import sys
import tempfile
from collections import Counter
from dataclasses import dataclass
from typing import Callable, Dict, List, Mapping, Optional, Set

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
from processes import machine  # noqa: E402
import identity_ratchet_findings as findings  # noqa: E402
from seams import tool_files  # noqa: E402
from vcs import tool_github  # noqa: E402
import tool_process  # noqa: E402

Row = findings.Row
LOCAL = os.path.join("target", "identity-unmatched", "ratchet-takes.tsv")
DEFAULT_STATUSES = "WRONG,UNJUDGED,MOVED,LOST"
# what gh prints when a run holds no such artifact (scripts/ci/gh-optional.sh): anything else is a read that failed
ABSENT = re.compile(r"no artifact matches|no valid artifacts found|HTTP 404")


class Failed(Exception):
    """The report could not be made: its line on stderr, the exit 1."""


class Usage(Exception):
    """A command line the tool does not take: its line on stderr, the exit 2."""


# ── the command line ────────────────────────────────────────────────────────────────────────────────────────────────

@dataclass
class Options:
    source: str
    statuses: str = DEFAULT_STATUSES
    country: Optional[str] = None
    explain: bool = False
    as_tsv: bool = False
    limit: Optional[int] = None

    def wanted(self) -> Set[str]:
        return set(findings.ORDER) if self.statuses.upper() == "ALL" else set(self.statuses.upper().split(","))


def set_status(o: Options, value: str) -> None:
    o.statuses = value


def set_country(o: Options, value: str) -> None:
    o.country = value


def set_limit(o: Options, value: str) -> None:
    if not value.isdigit():
        raise Usage(f"--limit takes a number, not {value}")
    o.limit = int(value)


def set_explain(o: Options) -> None:
    o.explain = True


def set_tsv(o: Options) -> None:
    o.as_tsv = True


VALUED: Dict[str, Callable[[Options, str], None]] = {"--status": set_status, "--country": set_country,
                                                      "--limit": set_limit}
SWITCHES: Dict[str, Callable[[Options], None]] = {"--explain": set_explain, "--tsv": set_tsv}


def options(argv: List[str]) -> Options:
    o, rest = Options(argv[0]), argv[1:]
    while rest:
        flag = rest.pop(0)
        if flag in SWITCHES:
            SWITCHES[flag](o)
        elif flag in VALUED and rest:
            VALUED[flag](o, rest.pop(0))
        else:
            raise Usage(f"{flag} needs a value" if flag in VALUED else f"unknown argument {flag}")
    return o


# ── the sources ─────────────────────────────────────────────────────────────────────────────────────────────────────

@dataclass
class Sources:
    """Where findings come from: a capture run (its artifact, else its pull-request job's log), cached under `cache`."""
    github: tool_github.GitHub
    files: tool_files.Files
    repo: str
    cache: str
    timeout: int

    def gh(self, *args: str) -> tool_process.Captured:
        """gh, bounded: a timeout an exit status like any other failure, said as one."""
        done = self.github.call(args, self.timeout, quiet=True)
        if done.code == tool_process.TIMED_OUT:
            return tool_process.Captured(done.code, "", f"gh {' '.join(args)} timed out after {self.timeout}s")
        return done

    def read(self, *args: str) -> str:
        done = self.gh(*args)
        if done.code != 0:
            raise Failed(f"[identity-ratchet-report] could not read gh {' '.join(args[:2])}: "
                         f"{done.stderr.strip() or f'exit {done.code}'}")
        return done.stdout

    def of(self, source: str) -> List[Row]:
        if source == "--local":
            return findings.read_tsv(self.files, LOCAL)
        if self.files.is_file(source):
            return findings.read_tsv(self.files, source)
        if re.fullmatch(r"\d{6,}", source):
            return self.of_run(source)
        if re.fullmatch(r"\d{1,5}", source):
            return self.of_run(self.run_of_pr(source))
        raise Usage(f"no run, PR or file {source}")

    def run_of_pr(self, number: str) -> str:
        body = json.loads(self.read("pr", "view", number, "-R", self.repo, "--json", "body"))["body"]
        m = re.search(r"run (\d{6,})", body)
        if not m:
            raise Failed(f"PR {number}'s body names no capture run")
        return m.group(1)

    def of_run(self, run: str) -> List[Row]:
        """The run's findings, cached: its artifact, else its pull-request job's log."""
        folder = os.path.join(self.cache, "ratchet", run)
        cached = os.path.join(folder, "ratchet-takes.tsv")
        if self.files.exists(cached):
            return findings.read_tsv(self.files, cached)
        self.files.make_directories(folder)
        done = self.download(run, folder, cached)
        if done.code == 0 and self.files.exists(cached):
            return findings.read_tsv(self.files, cached)
        # only an artifact gh says is not there sends the read to the log: a failed download is not an absent artifact
        if done.code != 0 and not ABSENT.search(done.stderr):
            raise Failed(f"[identity-ratchet-report] could not download run {run}'s identity-ratchet artifact: "
                         f"{done.stderr.strip() or f'exit {done.code}'}")
        rows = self.of_log(run)
        out = io.StringIO()
        findings.write_tsv(rows, out)
        self.files.write_text(cached, out.getvalue())
        return rows

    def download(self, run: str, folder: str, cached: str) -> tool_process.Captured:
        """The artifact downloaded beside the cache's copy and moved in whole: a download cut short is never read back
        as the findings."""
        part = tempfile.mkdtemp(prefix="dl.", dir=folder)
        try:
            done = self.gh("run", "download", run, "-R", self.repo, "--name", "identity-ratchet", "--dir", part)
            if done.code == 0 and os.path.exists(os.path.join(part, "ratchet-takes.tsv")):
                os.replace(os.path.join(part, "ratchet-takes.tsv"), cached)
            return done
        finally:
            shutil.rmtree(part, ignore_errors=True)

    def of_log(self, run: str) -> List[Row]:
        print(f"[identity-ratchet-report] run {run} has no identity-ratchet artifact: reading its pull-request job's log",
              file=sys.stderr)
        jobs = json.loads(self.read("run", "view", run, "-R", self.repo, "--json", "jobs"))["jobs"]
        job = next((j for j in jobs if j["name"] == "pull-request"), None)
        if job is None:
            raise Failed(f"run {run} has no pull-request job")
        return findings.parse_log(self.read("run", "view", "-R", self.repo, "--job", str(job["databaseId"]), "--log"))


def sources(env: Mapping[str, str], files: tool_files.Files) -> Sources:
    return Sources(tool_github.GitHub(tool_process.SubprocessRunner(), env), files,
                   env.get("KINOWO_REPO", "pawelkrupinski/movies"),
                   env.get("KINOWO_IDENTITY_CACHE", os.path.expanduser("~/.cache/kinowo-identity")),
                   int(env.get("KINOWO_IDENTITY_DOWNLOAD_TIMEOUT", "1800")))


# ── the report ──────────────────────────────────────────────────────────────────────────────────────────────────────

def report(rows: List[Row], o: Options) -> None:
    counts = Counter(r["status"] for r in rows)
    wanted = o.wanted()
    shown = [r for r in rows if r["status"] in wanted and (o.country is None or r["country"] == o.country)]
    shown = shown if o.limit is None else shown[:o.limit]
    if o.as_tsv:
        findings.write_tsv(shown, sys.stdout)
        return
    print("findings: " + ", ".join(f"{s} {counts[s]}" for s in findings.ORDER if counts[s]))
    findings.table(shown, o.explain, sys.stdout)


def main(argv: List[str]) -> int:
    if not argv or argv[0] in ("-h", "--help"):
        print(__doc__)
        return 0 if argv else 2
    try:
        o = options(argv)
        report(sources(machine.environment(), tool_files.Files.here()).of(o.source), o)
    except Usage as e:
        print(e, file=sys.stderr)
        return 2
    except Failed as e:
        print(e, file=sys.stderr)
        return 1
    return 0


def cli() -> int:
    import signal
    signal.signal(signal.SIGPIPE, signal.SIG_DFL)  # `| head` ends the table quietly
    return main(sys.argv[1:])


if __name__ == "__main__":
    sys.exit(cli())
