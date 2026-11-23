#!/usr/bin/env python3
"""ONE venue's listings as the code at one or two git refs reads them, from a recording — never the network. Answers
"is this cinema's client reading a field wrong, and does commit X change that?" in seconds, where a convergence run
takes 10–90 minutes (.claude/skills/identity-investigation/SKILL.md, "which tool").

  scripts/venue-replay <cc> <venue> [--recording RUN] [--ref A [--ref B]] [--title S]... [--fields]
                       [--fixtures DIR]... [--scrape [--date YYYY-MM-DD]] [--prod]

  <venue>       a substring of its name, case and accents aside (an exact name wins)
  --recording   the "Record scrape fixtures" run (default: the newest green one holding the country's corpus, each newer
                one skipped said), downloaded once into the shared cache
                identity-slice uses. Its row for the venue is what PROD's client parsed then; each ref reads that row
                into its slots and clean titles, and re-reads every detail page it fetches for it (a DetailEnricher's,
                or a client's parseDetail(html, url)) from the recording's tree.
  --ref         read under that ref's code (laid out and compiled once into the cache); two refs print both, then
                every listing and field that moved. Without --ref: this checkout. Both refs read the SAME input.
  --title S     only the listings whose raw title holds S (repeatable)
  --fields      every field of every listing, one a line
  --fixtures D  another recorded tree to serve from, after the recording's (a client's checked-in fixture tree,
                e.g. test/resources/fixtures/kinoheld) — repeatable
  --scrape      re-run the venue's scraper itself (fetch()) over the trees, its clock on --date (default the
                recording's scrape day). A recording holds no listing pages — it is prod's PARSED output — so this
                needs --fixtures holding them.
  --prod        beside the replay, the venue's CURRENT prod state from the local prod mirror (:28017, read-only):
                its cinema_scrapes row (read by the same code, held to the same rules), each listing's venue_pages
                read, and whether identity_listings carries the same listings — after scripts/local-mirror/freshness

Prints, per listing: raw title → clean title, the search query, format tags, year, runtime, director, synopsis head,
feed ids, detail URL, showtimes count, where its detail fields came from (and what a re-read page moved), every
CinemaSlotInvariantsSpec / SearchQueryMarkersSpec finding (the specs' allowlists, in e2e, may excuse one), and the
scripts/title-trace line for a title a rule moved. A request no recorded tree holds FAILS (exit 3) and is named —
whatever the client did with the failure.

Code or data? Both refs here read ONE recording, so what moves is the code's. Whether the VENUE's data moved between
two recordings is `scripts/recording-diff <runA> <runB> --country cc --venue S`; one listing's titles and searches
rule by rule, `scripts/title-trace` (each moved title prints its line).

The tool's files dropped into each ref (as identity-slice does): VenueReplay and the recording reader it opens a
recording with. The trees, classpath and recording come from identity-tools.sh through scripts/lib/identity_ref_trees.py
until that library moves. Tested by scripts/venue_replay_test.py."""
from __future__ import annotations

import json
import os
import shutil
import sys
import tempfile
from dataclasses import dataclass, field
from typing import List, Mapping

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import identity_ref_trees  # noqa: E402
import tool_cli  # noqa: E402
import tool_process  # noqa: E402
import tool_shell  # noqa: E402

TOOL = "venue-replay"
REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TOOL_FILES = ("worker/src/test/scala/scripts/VenueReplay.scala", "worker/src/fixtures/scala/tools/RecordedCorpus.scala",
              "worker/src/fixtures/scala/tools/CinemaSlotInvariants.scala",
              "worker/src/fixtures/scala/tools/SearchQueryMarkers.scala",
              "worker/src/fixtures/scala/services/identity/measures/OlderRefsPackage.scala",
              "worker/src/fixtures/scala/services/identityadapters/OlderRefsPackage.scala")
PROD_NEEDS = "cinema_scrapes,identity_listings,venue_pages"


def usage() -> str:
    return (__doc__ or "").split("\n\nThe tool's files dropped")[0] + "\n"


class Exit(Exception):
    def __init__(self, code: int) -> None:
        super().__init__(code)
        self.code = code


@dataclass
class Options:
    country: str
    venue: str
    refs: List[str] = field(default_factory=lambda: [])
    run: str = ""
    passed: List[str] = field(default_factory=lambda: [])
    prod: bool = False

    def word(self, argv: List[str], i: int) -> int:
        flag = argv[i]
        if flag in ("--fields", "--scrape"):
            self.passed.append(flag)
            return i
        if flag == "--prod":
            self.prod = True
            return i
        if flag not in ("--ref", "--recording", "--title", "--date", "--fixtures"):
            raise tool_cli.Refused(f"unknown argument {flag}", with_usage=True)
        value = tool_cli.value_at(argv, i)
        if flag == "--ref":
            self.refs.append(value)
        elif flag == "--recording":
            self.run = value
        elif flag == "--fixtures":
            if not os.path.isdir(value):
                raise tool_cli.UsageError(f"no directory {value}")
            self.passed += ["--fixtures", os.path.abspath(value)]
        else:
            self.passed += [flag, value]
        return i + 1


def options(argv: List[str]) -> Options:
    o = Options(argv[0].lower(), argv[1])
    rest = argv[2:]
    i = 0
    while i < len(rest):
        i = o.word(rest, i) + 1
    if len(o.refs) > 2:
        raise tool_cli.Refused("at most two --ref")
    return o


class Replay:
    def __init__(self, o: Options, env: Mapping[str, str], runner: tool_process.Runner, work: str) -> None:
        self.o, self.env, self.runner, self.work = o, env, runner, work
        self.refs = identity_ref_trees.RefTrees(env, runner, TOOL)

    def answer(self, function: str, *args: str) -> str:
        """`x=$(function args)` under errexit: its answer, or the tool ends with its status."""
        done = self.refs.call(function, args)
        if done.code != 0:
            raise Exit(done.code)
        return done.stdout.rstrip("\n")

    def recording(self) -> str:
        run = self.o.run or self.answer("identity_newest_recording", self.o.country)
        recording = self.answer("identity_recording", run, self.o.country)
        self.refs.log(f"recording {run}: {recording}")
        return recording

    def prod(self) -> List[str]:
        """The venue's current prod rows from the local mirror, read beside the replay (how current the mirror is
        first: a stale mirror is shown, but said to be stale); none, said, with no mirror."""
        cc = self.o.country
        fresh = self.runner.run(tool_process.Command(
            ["sh", "-c", 'exec "$@" >&2', "sh", os.path.join(REPO, "scripts", "local-mirror", "freshness"), cc,
             "--need", PROD_NEEDS, "--quiet"], self.env))
        if fresh == 2:
            self.refs.log(f"no local prod mirror for {cc}: --prod skipped")
            return []
        if fresh != 0:
            self.refs.log("the freshness check WARNed (above): read prod's rows below with it in mind")
        db = "kinowo" if cc == "pl" else f"kinowo_{cc}"
        uri = tool_shell.shell_function(self.runner, self.env, "scripts/tool-conventions.sh", "local_mongo_uri",
                                        [f"{db}_prod_mirror"]).stdout.strip()
        seconds = self.env.get("KINOWO_VENUE_REPLAY_MONGOSH_SECONDS") or "120"
        # bounded: a mongod that stops answering must not hold the tool forever
        read = self.runner.capture(tool_process.Command(
            ["mongosh", "--quiet", uri, "--eval", f"var SELECTOR={json.dumps(self.o.venue)}",
             "--file", os.path.join(REPO, "scripts", "venue-replay-prod.js")], self.env, timeout=float(seconds),
            stderr_through=True))
        if read.code != 0:
            print(f"{TOOL}: the prod mirror's mongosh read failed or took over {seconds}s "
                  "(KINOWO_VENUE_REPLAY_MONGOSH_SECONDS)", file=sys.stderr)
            raise Exit(1)
        path = os.path.join(self.work, "prod.json")
        with open(path, "w") as f:
            f.write(read.stdout)
        return ["--prod", path]

    def run_at(self, tree: str, ref: str, recording: str, out: str, printed: str, passed: List[str]) -> int:
        """That tree's VenueReplay, in one JVM, its stdout into `printed`."""
        cc = self.o.country
        return self.refs.run_java(tree, ["scripts.VenueReplay", cc, "--venue", self.o.venue,
                                         "--corpus", f"{recording}/corpus/cinema-scrapes-{cc}.json.gz",
                                         "--recording", recording, "--ref", ref, "--out", out, *passed],
                                  ["-Dlogback.statusListenerClass=ch.qos.logback.core.status.NopStatusListener"],
                                  printed)

    def tree_of(self, ref: str) -> str:
        if ref == ".":
            return REPO
        tree = self.refs.tree(ref, TOOL_FILES)
        if tree is None:
            raise Exit(1)
        return tree

    def replay(self) -> int:
        recording = self.recording()
        passed = self.o.passed + (self.prod() if self.o.prod else [])
        refs = self.o.refs or ["."]
        status = 0
        for i, ref in enumerate(refs):
            tree = self.tree_of(ref)
            printed = os.path.join(self.work, f"{i}.txt")
            code = self.run_at(tree, ref, recording, os.path.join(self.work, f"{i}.tsv"), printed, passed)
            with open(printed, encoding="utf-8", errors="replace") as f:
                sys.stdout.write(f.read())
            sys.stdout.flush()
            status = code or status
            if code not in (0, 3):
                return code
        if len(refs) == 2:
            diff = self.runner.run(tool_process.Command(
                ["python3", "-I", "-B", os.path.join(REPO, "scripts", "venue_replay_diff.py"),
                 os.path.join(self.work, "0.tsv"), os.path.join(self.work, "1.tsv"), refs[0], refs[1]], self.env))
            if diff != 0:
                return diff
        return status


def main(argv: List[str], env: Mapping[str, str]) -> int:
    if argv and argv[0] in ("-h", "--help"):
        sys.stdout.write(usage())
        return 0
    if len(argv) < 2:
        sys.stderr.write(usage())
        return 2
    try:
        o = options(argv)
    except tool_cli.UsageError as error:
        return tool_cli.usage_failed(TOOL, error)
    except tool_cli.Refused as refusal:
        return tool_cli.refused(refusal, usage())
    work = tempfile.mkdtemp(prefix="venue-replay.", dir=env.get("TMPDIR") or "/tmp")
    try:
        return Replay(o, env, tool_process.SubprocessRunner(), work).replay()
    except Exit as end:
        return end.code
    finally:
        shutil.rmtree(work, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], os.environ))
