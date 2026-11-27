#!/usr/bin/env python3
"""Turn a PRODUCTION listing — one a regression alert, a drift-check line or the user names — into a slice case on the
freshest recording, or say exactly why it cannot be one.

  scripts/identity-repro <cc> <venue> <title> [--recording RUN|prod-latest] [--ref REF] [--no-mirror] [--no-emit] [--wait]

  <venue> <title>  substrings, case and accents aside (as identity-slice's --venue/--title); the listing must be one
                 venue's one raw title in the recording, else every candidate is listed and nothing is sliced
  --recording    default `prod-latest`: the freshest complete nightly "Record scrape fixtures" recording — the corpus
                 prod had scraped that night and the TMDB/IMDb answers recorded live over it (prod-latest-<cc>.txt on
                 the convergence-fixtures release). The hermetic pair (573) and 528 are weeks behind prod's listings.
  --ref          decide under that ref's code (default: this checkout's)
  --no-mirror    do not read the local prod mirror (scripts/local-mirror, :28017): by default, when it answers, the
                 slice reads prod's filed state beside the recording (identity-slice --prod-mirror), and a listing the
                 recording lacks is looked up there to say whether prod scraped it after the recording
  --no-emit      slice and explain only; do not pin the case (identity-slice --emit-case)
  --wait         tonight's recording first: when a "Record scrape fixtures" run newer than prod-latest's is in flight on
                 main, say its run id and an ETA (the median wall time of recent recordings), wait for it with
                 ~/bin/agentctl wait's backoff (60 s, x1.5, at most 5 min; IDENTITY_REPRO_WAIT_FIRST seconds first, at most
                 IDENTITY_REPRO_WAIT_HOURS, 6), then reproduce on prod-latest as it then stands; none in flight, or it
                 ends otherwise than success: prod-latest as it is, said so. With --recording prod-latest only. An agent
                 runs it in the background and ends its turn

Verdicts, one line on stdout, and the exit status:
  REPRODUCIBLE  (0)  sliced, every question the slice asked was answered by the recording, case emitted (or --no-emit)
  NEEDS-JUDGEMENT (4) sliced, but identity-judgements.tsv does not judge the listing: --emit-case refused. Judge it
                     (review page / scripts/label-lookup), then run this again
  NEEDS-ANSWERS (5)  sliced, taking otherwise than prod (or prod not compared), and the recording left questions
                     unanswered (listed): the slice reads them as unknown, as
                     a hermetic leg does, so its take may differ from prod's for want of answers, not code. The next
                     nightly recording records them (its identity lookups ask every listing's questions)
  NOT-IN-RECORDING (3) the recording holds no such venue or title; says whether the prod mirror has it now (scraped
                     after the recording: tonight's prod-latest will) or not at all (gone from prod, or misnamed)
  AMBIGUOUS (2)      several venues or titles match: listed, nothing sliced — narrow the substrings
  (1) a recording, a download or the slice failed — the message says which

Every download and compile is identity-slice's, shared through $KINOWO_IDENTITY_CACHE. Run it on the laptop under
the heavy-build throttle like any slice (it starts one JVM). The recording's run and layout are still identity-tools.sh's
(prod_latest_run, identity_recording, through tool_shell); the --wait's ETA and backoff tool_dispatch's."""
from __future__ import annotations

import gzip
import json
import os
import re
import shutil
import sys
import tempfile
import time
import unicodedata
from dataclasses import dataclass
from typing import Any, Callable, Dict, List, Mapping, Optional, Sequence, cast

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "lib"))
import machine  # noqa: E402
import mongo_local  # noqa: E402
import tool_countries  # noqa: E402
import tool_dispatch  # noqa: E402
import tool_github  # noqa: E402
import tool_process  # noqa: E402
import tool_shell  # noqa: E402

TOOL = "identity-repro"
RECORDING_WORKFLOW = "Record scrape fixtures"
EMITTED = "; case emitted under test/resources/fixtures/identity-slices/"
TAKES_WHAT_PROD_SERVES = "; the slice takes what prod serves"


class Usage(Exception):
    """A command line the tool does not take: its message (if any), then the header, on stderr; exit 2."""


class Verdict(Exception):
    def __init__(self, name: str, why: str, code: int) -> None:
        super().__init__(why)
        self.name, self.why, self.code = name, why, code


def log(line: str) -> None:
    """identity-tools.sh's identity_log."""
    print(f"[{TOOL}] {line}", file=sys.stderr, flush=True)


@dataclass
class Options:
    cc: str
    venue: str
    title: str
    recording: str = "prod-latest"
    ref: str = ""
    mirror: bool = True
    emit: bool = True
    wait: bool = False


SWITCHES = {"--no-mirror": ("mirror", False), "--no-emit": ("emit", False), "--wait": ("wait", True)}
VALUED = {"--recording": ("recording", "--recording needs a run or prod-latest"), "--ref": ("ref", "--ref needs a ref")}


def parse(argv: Sequence[str]) -> Options:
    if len(argv) < 3:
        raise Usage("")
    o, rest = Options(argv[0], argv[1], argv[2]), list(argv[3:])
    while rest:
        arg = rest.pop(0)
        if arg in SWITCHES:
            setattr(o, *SWITCHES[arg])
        elif arg in VALUED:
            if not rest or not rest[0]:
                raise Usage(VALUED[arg][1])
            setattr(o, VALUED[arg][0], rest.pop(0))
        else:
            raise Usage(f"unknown argument: {arg}")
    return o


def fold(text: Optional[str]) -> str:
    return "".join(c for c in unicodedata.normalize("NFKD", text or "") if not unicodedata.combining(c)).casefold()


@dataclass(frozen=True)
class Venue:
    id: str
    scraped_at: str
    titles: List[str]


def matching(corpus: str, venue: str, title: str) -> List[Venue]:
    """The corpus's venues (_id) matching `venue`, each with its raw titles matching `title`, as identity-slice's
    selectors match."""
    with gzip.open(corpus, "rt", encoding="utf-8") as f:
        scrapes: List[Dict[str, Any]] = json.load(f)
    found: List[Venue] = []
    for s in scrapes:
        if fold(venue) in fold(s.get("_id")):
            titles = {film["movie"]["title"] for film in s.get("films", []) if fold(title) in fold(film["movie"]["title"])}
            found.append(Venue(s["_id"], str(s.get("scrapedAt", "?")), sorted(titles)))
    return found


def recorded_at(path: str) -> str:
    try:
        with open(path, encoding="utf-8") as f:
            return str(json.load(f).get("recordedAt", ""))
    except Exception:
        return ""


MIRROR_LOOKUP = '''
    const fold = s => (s || "").normalize("NFKD").replace(/[̀-ͯ]/g, "").toLowerCase();
    const v = fold(process.env.REPRO_VENUE), t = fold(process.env.REPRO_TITLE);
    db.cinema_scrapes.find({}, {_id: 1, scrapedAt: 1, "films.movie.title": 1}).forEach(s => {
      if (!fold(s._id).includes(v)) return;
      const ts = (s.films || []).map(f => f.movie.title).filter(x => fold(x).includes(t));
      print(`${s._id}\\t${s.scrapedAt && s.scrapedAt.toISOString ? s.scrapedAt.toISOString() : s.scrapedAt}\\t${[...new Set(ts)].join(" / ")}`);
    })'''


class Mirror:
    """prod's own scrape NOW, from the local prod mirror (mongosh) -- never prod itself."""

    def __init__(self, runner: tool_process.Runner, env: Mapping[str, str], cc: str, wanted: bool) -> None:
        self.runner, self.env = runner, env
        self.usable = wanted and shutil.which("mongosh", path=env.get("PATH")) is not None
        self.url = f"{mongo_local.local_mongo_server(env)}/{mongo_local.mirror_database(cc)}"

    def _eval(self, script: str, env: Mapping[str, str]) -> Optional[str]:
        done = self.runner.capture(tool_process.Command(["mongosh", "--quiet", self.url, "--eval", script], env))
        return done.stdout if done.code == 0 else None

    def scrapes(self, venue: str, title: str) -> Optional[List[List[str]]]:
        """Each matching venue's `id, scrapedAt, titles` (" / " between); None when the mirror was not read."""
        if not self.usable:
            return None
        out = self._eval(MIRROR_LOOKUP, {**self.env, "REPRO_VENUE": venue, "REPRO_TITLE": title})
        return None if out is None else [line.split("\t") for line in out.rstrip("\n").split("\n") if line]

    def answers(self) -> bool:
        return self.usable and self._eval("db.runCommand({ping: 1}).ok", self.env) is not None


def not_in_recording(run: str, o: Options, venues: List[Venue], prod: Optional[List[List[str]]]) -> str:
    if not venues:
        return mirror_says(f'recording {run} holds no {o.cc} venue matching "{o.venue}"', prod)
    scraped = ";".join(f"{v.id} scraped {v.scraped_at}" for v in venues)
    return mirror_says(f'recording {run}\'s venue(s) {scraped} list no title matching "{o.title}"', prod)


def mirror_says(why: str, prod: Optional[List[List[str]]]) -> str:
    """Why the recording lacks the listing, and whether the prod mirror has it now."""
    if prod is None:
        return f"{why} (prod mirror not consulted)"
    if not prod:
        return f"{why}; the prod mirror has no such venue either (misnamed?)"
    if not any(len(row) > 2 and row[2] for row in prod):
        rows = ";".join(" scraped ".join(row[:2]) for row in prod)
        return f"{why}; the prod mirror lists no such title there now either (gone from prod, or misnamed): {rows}"
    rows = ";".join(f"{row[0]} scraped {row[1]}: {row[2] if len(row) > 2 else ''}" for row in prod)
    return (f"{why}; prod HAS it now — {rows} — scraped after the recording: tonight's prod-latest records it "
            f'(or dispatch "{RECORDING_WORKFLOW}")')


def first_line(out: str, pattern: str) -> Optional[str]:
    found = re.search(f"^{pattern}.*$", out, re.MULTILINE)
    return found.group(0) if found else None


def prod_said(out: str) -> str:
    """Beside prod's stored take (--prod-mirror): whether the slice takes what prod serves. A slice that takes otherwise
    on every answer prod's code had is prod's INCREMENTAL state (an order, a split), which no recording replays."""
    otherwise = first_line(out, "PROD TAKES OTHERWISE")
    if otherwise is not None:
        return (f"; PROD TAKES OTHERWISE ({re.sub(r'^.*: ', '', otherwise)}): prod's incremental state, not this "
                "recording's answers, unless the gaps below explain it")
    if first_line(out, "-- prod.s stored state") is not None:
        return TAKES_WHAT_PROD_SERVES
    return "; prod's take not compared (no mirror)"


def judged(out: str, run: str, key: str, emit: bool) -> Verdict:
    """identity-slice's output read: the verdict."""
    gaps = re.sub(r"^UNANSWERED by the recording[^:]*: ", "", first_line(out, "UNANSWERED by the recording") or "")
    prod = prod_said(out)
    if "--emit-case REFUSED" in out:
        also = f" (also unanswered: {gaps})" if gaps else ""
        return Verdict("NEEDS-JUDGEMENT", f"sliced on recording {run}{prod}, but identity-judgements.tsv does not judge "
                                          f'"{key}": judge it, then run again{also}', 4)
    emitted = EMITTED if emit else ""
    # Taking what prod serves, the slice reproduces it whatever it left unanswered (named, for what they may hide).
    if gaps and prod == TAKES_WHAT_PROD_SERVES:
        return Verdict("REPRODUCIBLE", f"sliced on recording {run}{prod} (unanswered, read as unknown: {gaps}){emitted}", 0)
    if gaps:
        return Verdict("NEEDS-ANSWERS", f"sliced on recording {run}{prod}; unanswered: {gaps}", 5)
    return Verdict("REPRODUCIBLE", f"sliced on recording {run}{prod}{emitted}", 0)


def rows(answer: Any) -> List[Dict[str, Any]]:
    """gh's JSON list's objects (none when it is no list)."""
    if not isinstance(answer, list):
        return []
    return [cast(Dict[str, Any], row) for row in cast(List[Any], answer) if isinstance(row, dict)]


class RecordingWait:
    """--wait: a recording newer than prod-latest's, in flight on main, waited for with agentctl wait's backoff, so a
    listing prod scraped today reproduces on tonight's recording, not yesterday's."""

    def __init__(self, github: tool_github.GitHub, env: Mapping[str, str], clock: Callable[[], float],
                 sleep: Callable[[int], None]) -> None:
        self.github, self.clock, self.sleep = github, clock, sleep
        self.settings = tool_dispatch.Settings.of(env, 60)
        self.dispatcher = tool_dispatch.Dispatcher(github, self.settings)
        self.first = int(env.get("IDENTITY_REPRO_WAIT_FIRST") or 60)
        self.hours = env.get("IDENTITY_REPRO_WAIT_HOURS") or "6"

    def _json(self, args: Sequence[str]) -> Any:
        out = self.github.read(args, 60)
        try:
            return None if out is None else json.loads(out)
        except ValueError:
            return None

    def in_flight(self) -> Optional[int]:
        runs = self._json(["run", "list", "--workflow", RECORDING_WORKFLOW, "--branch", "main", "--limit", "10",
                           "--json", "databaseId,status"])
        ids = [int(r["databaseId"]) for r in rows(runs) if r.get("status") != "completed"]
        return max(ids) if ids else None

    def state(self, run: int) -> Optional[str]:
        view = rows([self._json(["run", "view", str(run), "--json", "status,conclusion"])])
        return f"{view[0].get('status')} {view[0].get('conclusion') or ''}" if view else None

    def wait(self, latest: int) -> None:
        flight = self.in_flight()
        if flight is None or flight <= latest:
            log(f"--wait: no recording newer than prod-latest's (run {latest}) in flight -- reproducing on it")
            return
        eta = self.dispatcher.eta(RECORDING_WORKFLOW, "main", ["--branch", "main"])
        log(f"--wait: recording run {flight} in flight (newer than prod-latest's {latest}): "
            f"{f'ETA {eta}' if eta else 'no ETA'} -- waiting with backoff (60 s, x1.5, at most 5 min)")
        delay, misses, end = self.first, 0, self.clock() + float(self.hours) * 3600
        while True:
            state = self.state(flight)
            misses = 0 if state is not None else misses + 1
            said = self.ended(flight, latest, state, misses, end)
            if said:
                log(f"--wait: {said}")
                return
            self.sleep(delay)
            delay = tool_dispatch.backoff_next(delay, self.settings.backoff_cap)

    def ended(self, flight: int, latest: int, state: Optional[str], misses: int, end: float) -> str:
        """Why the wait is over ("" while it is not)."""
        if state == "completed success":
            return f"recording run {flight} finished"
        if state is not None and state.startswith("completed"):
            return f"recording run {flight} ended {state[len('completed '):]}: reproducing on prod-latest's {latest}"
        if misses >= 10:
            return f"run {flight} could not be read ten times in a row: reproducing on prod-latest's {latest}"
        if self.clock() >= end:
            return f"run {flight} not done within {self.hours} h: reproducing on prod-latest's {latest}"
        return ""


class Repro:
    def __init__(self, o: Options, env: Mapping[str, str], runner: tool_process.Runner,
                 clock: Callable[[], float] = time.monotonic) -> None:
        self.o, self.runner, self.clock = o, runner, clock
        self.env = {**env, "IDENTITY_TOOL": TOOL}
        self.github = tool_github.GitHub(runner, self.env, env.get("KINOWO_GH") or "gh")
        self.mirror = Mirror(runner, self.env, o.cc, o.mirror)

    def shell(self, library: str, function: str, args: Sequence[str]) -> Optional[str]:
        done = tool_shell.shell_function(self.runner, self.env, library, function, args)
        return done.stdout.strip() if done.code == 0 else None

    def sleep(self, seconds: int) -> None:
        self.runner.run(tool_process.Command(["sleep", str(seconds)], self.env))

    def main(self) -> Verdict:
        if self.o.wait:
            latest = self.shell("scripts/tool-conventions.sh", "prod_latest_run", [self.o.cc]) or "0"
            RecordingWait(self.github, self.env, self.clock, self.sleep).wait(int(latest))
        run = self.recording_run()
        fixtures = self.shell("scripts/identity-tools.sh", "identity_recording", [run, self.o.cc])
        if fixtures is None:
            return Verdict("FAILED", f"could not lay out recording {run} ({self.o.cc})", 1)
        key = self.listing(run, fixtures)
        return self.sliced(run, key)

    def recording_run(self) -> str:
        if self.o.recording != "prod-latest":
            return self.o.recording
        run = self.shell("scripts/tool-conventions.sh", "prod_latest_run", [self.o.cc])
        if run is None:
            raise Verdict("FAILED", f"no {self.o.recording} recording for {self.o.cc} (above)", 1)
        log(f"prod-latest {self.o.cc} recording: run {run}")
        return run

    def listing(self, run: str, fixtures: str) -> str:
        """The one listing (venue|title) the recording holds, else the verdict why not."""
        corpus = os.path.join(fixtures, "corpus", f"cinema-scrapes-{self.o.cc}.json.gz")
        try:
            venues = matching(corpus, self.o.venue, self.o.title)
        except Exception:
            raise Verdict("FAILED", f"could not read {corpus}", 1) from None
        at = recorded_at(os.path.join(fixtures, "corpus", f"prod-coverage-{self.o.cc}.json"))
        log(f"recording {run} ({self.o.cc}, recorded {at or 'at an unknown time'})")
        listings = [(v.id, t) for v in venues for t in v.titles]
        if not listings:
            prod = self.mirror.scrapes(self.o.venue, self.o.title)
            raise Verdict("NOT-IN-RECORDING", not_in_recording(run, self.o, venues, prod), 3)
        if len(listings) > 1:
            print("several listings match; narrow the substrings:", file=sys.stderr)
            print("\n".join(" | ".join(listing) for listing in listings), file=sys.stderr, flush=True)
            raise Verdict("AMBIGUOUS", f"{len(listings)} listings in recording {run}", 2)
        key = "|".join(listings[0])
        log(f"listing in recording {run}: {key}")
        return key

    def sliced(self, run: str, key: str) -> Verdict:
        args = [self.o.cc, "--key", key, "--recording", run] + (["--ref", self.o.ref] if self.o.ref else [])
        args += ["--prod-mirror", self.o.cc] if self.mirror.answers() else []
        args += ["--emit-case"] if self.o.emit else []
        slicer = self.env.get("IDENTITY_REPRO_SLICE") or os.path.join(os.path.dirname(HERE), "scripts", "identity-slice")
        with tempfile.TemporaryDirectory(prefix=f"{TOOL}.") as scratch:
            out = os.path.join(scratch, "slice.log")
            rc = self.runner.run(tool_process.Command([slicer, *args], self.env, log=out, tee=True))
            with open(out, encoding="utf-8", errors="replace") as f:
                said = f.read()
        if rc != 0:
            return Verdict("FAILED", f"identity-slice {' '.join(args)} exited {rc} (above)", 1)
        return judged(said, run, key, self.o.emit)


def main(argv: List[str], env: Dict[str, str]) -> int:
    try:
        o = parse(argv)
    except Usage as e:
        print("\n".join(filter(None, [str(e), __doc__])), file=sys.stderr)
        return 2
    try:
        if o.cc not in tool_countries.CODES:
            print(f"unknown country {o.cc} ({' '.join(tool_countries.CODES)})", file=sys.stderr)
            return 2
        if o.wait and o.recording != "prod-latest":
            print("--wait waits for tonight's recording: it goes with --recording prod-latest only", file=sys.stderr)
            return 2
        # the slice's output passed through on stderr (`| tee log >&2`): the verdict alone on stdout
        verdict = Repro(o, env, tool_process.SubprocessRunner(stdout=sys.stderr.buffer)).main()
    except Verdict as v:
        verdict = v
    print(f"{verdict.name}: {o.cc} | {o.venue} | {o.title} — {verdict.why}", flush=True)
    return verdict.code


if __name__ == "__main__":
    if sys.argv[1:2] in (["-h"], ["--help"]):
        print(__doc__)
        sys.exit(0)
    sys.exit(main(sys.argv[1:], dict(machine.environment())))
