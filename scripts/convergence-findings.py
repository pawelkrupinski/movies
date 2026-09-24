#!/usr/bin/env python3
"""The films a convergence leg's log names as divergent, churning or split.

Reads one or more convergence logs (the tee'd `convergence*.log` a leg writes, or a
`gh run view --log-failed` dump) and prints `code<TAB>title` per film, deduplicated —
the input `scripts.BuildHardClusters extend` takes, so a cluster a full leg caught is
added to the hard-cluster fixture the itAll layer replays on every push (see
`tools.HardClusters` and `scripts/hard-clusters.sh`).

    scripts/convergence-findings.py uk convergence.log > findings.tsv

The shapes it reads are the clues `CountryConvergenceBehaviour`, `CorpusDiff` and
`MixedFilmSplitter` print:

    Mixed-film split: 'It' (2017) — Bright Star Cinemas screens … → re-diverted to staging as 'It (1990)' …
    record keys differ: onlypass1=HashSet((The Hunger Games: Mockingjay - Part 2,Some(2015))) onlypass0=…
    tick 1: keys APPEARED: (Blood & Sinners,Some(2026)), …
    record 'Lalka' (Some(2026)):
    tick 1: 5 known film(s) RE-DIVERTED to staging: (Freiluftkino Hasenheide,bloodisinners), …
    pass2=Map(Cinema1␟avengerskoniecgryrerelease -> 14)
    …ResolvedMovie(avengerskoniecgryrerelease|2026,Avengers: Koniec Gry (re-release),…

The last two are the order-dependence report ("SCREENINGS differ" / "RENDERED ROWS
differ"). A RE-DIVERTED entry and a screening key name only the sanitized key; it is emitted as-is and matches the
fixture's listings by containment.
"""
import re
import sys

ANSI = re.compile(r"\x1b\[[0-9;]*m")
SPLIT = re.compile(r"Mixed-film split(?: deferred for|:) '([^']+)'")
SPLIT_AS = re.compile(r"re-diverted to staging as '([^']+)'")
RECORD = re.compile(r"record '([^']+)' \((?:Some\(\d{4}\)|None)\):")
# `(Title,Some(2015))` / `(Title,None)` inside key sets, APPEARED/VANISHED lists and
# only-in sets. The title may itself hold commas and parentheses ("It (1990)"), so match
# lazily up to the LAST comma before the year.
KEY_TUPLE = re.compile(r"\(([^()]+(?:\([^()]*\)[^()]*)*?),(?:Some\(\d{4}\)|None)\)")
KEY_CONTEXT = re.compile(r"(only[-a-z0-9]*=|APPEARED|VANISHED|keys differ)")
REDIVERTED = re.compile(r"RE-DIVERTED to staging: (.*)")
CINEMA_KEY = re.compile(r"\(([^,()]+),([a-z0-9]+)\)")
# A screening count keyed `cinema␟sanitizedTitle`, and a rendered read-model row.
SCREENING_KEY = re.compile(r"\u241f([a-z0-9]+) ->")
RESOLVED_MOVIE = re.compile(r"ResolvedMovie\([^|,()]+\|(?:\d{4})?,([^,]+),")


def titles(line: str):
    line = ANSI.sub("", line)
    yield from SPLIT.findall(line)
    yield from SPLIT_AS.findall(line)
    yield from RECORD.findall(line)
    if KEY_CONTEXT.search(line):
        yield from KEY_TUPLE.findall(line)
    yield from SCREENING_KEY.findall(line)
    yield from RESOLVED_MOVIE.findall(line)
    m = REDIVERTED.search(line)
    if m:
        yield from (key for _, key in CINEMA_KEY.findall(m.group(1)))


def main(argv):
    if len(argv) < 3:
        sys.stderr.write(__doc__)
        return 2
    code, logs = argv[1].lower(), argv[2:]
    seen = []
    for path in logs:
        with open(path, encoding="utf-8", errors="replace") as log:
            for line in log:
                for title in titles(line):
                    title = title.strip()
                    if title and title not in seen:
                        seen.append(title)
    for title in seen:
        print(f"{code}\t{title}")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
