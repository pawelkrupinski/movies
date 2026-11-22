"""READING A SPEC GROUP'S sbt LOG, for scripts/identity-landing-check (identity_landing_check.py): whether sbt passed,
which tests failed and in which suite, and the hint for each failure's usual cause.

  verdict(log) -> 1 when sbt failed a command (a failed test, an error, a compile error), 0 when it ended with
                  success, 2 when it never ended (a log cut short: no answer)
  failures(log) -> "<Spec><TAB><test><TAB><its first detail line>" per failed test; a suite that failed with no test
                  named (aborted, an error before its tests) as "<Spec><TAB>(the suite failed: see the log)<TAB>"; a
                  compile error as "compile<TAB><its first error><TAB>"
  hint(spec, test, detail) -> what usually makes it fail, and the way out

Suites run in parallel and their output interleaves, so the "<Spec>:" header above a failure need not be its suite's.
A failure is the suite its own location names ("(XSpec.scala:56)"); else the suite a failure worded alike was given;
else the one suite sbt's summary lists failed that no failure has claimed yet; else the newest such unclaimed suite
whose header came before it; else the newest failed suite's header above it."""
from __future__ import annotations

import fnmatch
import re
from dataclasses import dataclass, field
from typing import Dict, List, Sequence, Set, Tuple

ANSI = re.compile(r"\x1b\[[0-9;]*m")
FAILED_COMMAND = re.compile(r"\[error\] (Total time|Failed tests:|Error during tests:|.*Compilation failed)"
                            r"|\*\*\* (FAILED|ABORTED) \*\*\*")
SUCCEEDED = re.compile(r"\[success\] Total time")
COMPILATION_FAILED = re.compile(r"\[error\] .*Compilation failed")
SOURCE_ERROR = re.compile(r"\[error\] [^ ]+\.scala:[0-9]+:[0-9]+:")
SUMMARY = re.compile(r"\[error\] (Failed tests|Error during tests):")
SUMMARY_SUITE = re.compile(r"\[error\][ \t]+[a-z][A-Za-z0-9_.$]*$")
TEST_LINE = re.compile(r"\[info\] +- ")
HEADER = re.compile(r"\[info\] ([A-Z][A-Za-z0-9_$]*):$")
FAILURE = re.compile(r"\*\*\* (FAILED|ABORTED) \*\*\*")
LOCATION = re.compile(r"\(([A-Z][A-Za-z0-9_$]*\.scala:[0-9]+)\)")
DETAIL_BYTES = 400  # of a failure's first detail line
DETAIL_LINES = 40  # a failure's detail and location are read from at most this many lines below it
WORDED_ALIKE = 40  # two failures whose test names start with this many characters alike are one suite's


def lines_of(log: str) -> List[str]:
    """The log's lines, colours dropped."""
    with open(log, "rb") as f:
        text = ANSI.sub("", f.read().decode(errors="replace"))
    lines = text.split("\n")
    return lines[:-1] if lines and lines[-1] == "" else lines


def verdict(log: str) -> int:
    lines = lines_of(log)
    if any(FAILED_COMMAND.search(line) for line in lines):
        return 1
    return 0 if any(SUCCEEDED.search(line) for line in lines) else 2


@dataclass
class Failure:
    test: str
    header: str  # the newest "<Spec>:" header above it
    before: List[str]  # every header above it, oldest first
    detail: str = ""
    location: str = ""
    who: str = ""


@dataclass
class Reading:
    """The log read line by line (the shell tool's awk): its failures, sbt's summary of failed suites, its compile
    error."""
    failures: List[Failure] = field(default_factory=lambda: [])
    failed: Set[str] = field(default_factory=lambda: set())
    order: List[str] = field(default_factory=lambda: [])
    seen: List[str] = field(default_factory=lambda: [])
    spec: str = ""
    compile: bool = False
    first_error: str = ""
    summary: bool = False
    open: bool = False
    lines: int = 0

    def read(self, line: str) -> None:
        if COMPILATION_FAILED.search(line):
            self.compile = True
        if SOURCE_ERROR.search(line) and not self.first_error:
            self.first_error = re.sub(r"^.*\[error\] ", "", line, count=1)
        if SUMMARY.search(line):
            self.open, self.summary = False, True
        elif self.summary and SUMMARY_SUITE.search(line):
            self.summary_suite(line)
        else:
            self.summary = False
            self.read_body(line)

    def summary_suite(self, line: str) -> None:
        suite = line.split()[-1].split(".")[-1]
        self.failed.add(suite)
        self.order.append(suite)

    def read_body(self, line: str) -> None:
        header = HEADER.search(line)
        if TEST_LINE.search(line) or header:
            self.open = False
        if header:
            self.spec = header.group(1)
            self.seen.append(self.spec)
        if FAILURE.search(line):
            self.failure(line)
        elif self.open:
            self.lines += 1
            if self.lines > DETAIL_LINES:
                self.open = False
            else:
                self.detail_line(re.sub(r"^.*\[info\] +", "", line, count=1))

    def failure(self, line: str) -> None:
        test = re.sub(r"^.*\[info\] +(- )?", "", line, count=1)
        test = re.sub(r" \*\*\* (FAILED|ABORTED) \*\*\*.*$", "", test, count=1)
        if "ABORTED" in line:
            test = "(aborted) " + test
        self.failures.append(Failure(test, self.spec, list(self.seen)))
        self.open, self.lines = True, 0

    def detail_line(self, text: str) -> None:
        failure = self.failures[-1]
        if not failure.detail and text:
            failure.detail = text.encode()[:DETAIL_BYTES].decode(errors="ignore")  # awk's substr: bytes
        located = LOCATION.search(text)
        if not failure.location and located:
            failure.location = located.group(1)


def failures(log: str) -> List[str]:
    reading = Reading()
    for line in lines_of(log):
        reading.read(line)
    claimed = attribute(reading.failures, reading.failed, reading.order)
    found = [f"{f.who}\t{f.test}\t{f.detail}" for f in reading.failures]
    found += [f"{suite}\t(the suite failed: see the log)\t" for suite in reading.order if suite not in claimed]
    if reading.compile:
        found.append(f"compile\t{reading.first_error or 'Compilation failed'}\t")
    return found


def attribute(found: Sequence[Failure], failed: Set[str], order: Sequence[str]) -> Dict[str, bool]:
    """Each failure's suite (the module's header says in which order of evidence); the suites claimed."""
    claimed: Dict[str, bool] = {}
    for f in found:
        suite = re.sub(r"\.scala:.*", "", f.location, count=1)
        f.who = suite if suite in failed else ""
        if f.who:
            claimed[f.who] = True
    for f in found:
        if not f.who:
            f.who = guessed(f, found, failed, order, claimed)
            claimed[f.who] = True
    return claimed


def guessed(f: Failure, found: Sequence[Failure], failed: Set[str], order: Sequence[str],
            claimed: Dict[str, bool]) -> str:
    """A failure its location does not place: the suite a failure worded alike was given; else the one failed suite
    no failure claimed yet; else the newest unclaimed failed header above it; else the newest failed header above it;
    else the header above it ("?" for none)."""
    return (worded_alike(f, found) or only_unclaimed(order, claimed) or newest_header(f, failed, claimed)
            or newest_header(f, failed, {}) or f.header or "?")


def worded_alike(f: Failure, found: Sequence[Failure]) -> str:
    return next((o.who for o in found if o is not f and o.who and o.test[:WORDED_ALIKE] == f.test[:WORDED_ALIKE]), "")


def only_unclaimed(order: Sequence[str], claimed: Dict[str, bool]) -> str:
    unclaimed = [suite for suite in order if suite not in claimed]
    return unclaimed[0] if len(unclaimed) == 1 else ""


def newest_header(f: Failure, failed: Set[str], claimed: Dict[str, bool]) -> str:
    return next((h for h in reversed(f.before) if h in failed and h not in claimed), "")


# (spec<TAB>test<TAB>detail pattern, hint): the first that matches
HINTS: List[Tuple[str, str]] = [
    ("MatchedTakesLabelSpec*known-wrong*",
     "judgement rows stale -> known-wrong scope rows: a judgement now denies a take the unified rows "
     "(IdentityUnifiedFit.Takes) hold; add it to identity-judgements.tsv as known-wrong in scope `rows` with why, or "
     "re-emit the rows (scripts/identity-calibrate.sh --takes)"),
    ("MatchedTakesLabelSpec*",
     "the unified rows' takes against the judgements: re-emit the rows (scripts/identity-calibrate.sh --takes) or fix "
     "the judgement rows the spec names"),
    ("IdentitySliceCasesSpec*unpinned*|IdentitySliceCasesSpec*not pinned*|IdentitySliceCasesSpec*lacks answers*",
     "slice case asks unpinned questions -> re-pin from the recording: scripts/identity-slice --emit-case over the "
     "case's listings"),
    ("IdentitySliceCasesSpec*",
     "a pinned slice case no longer takes its right film: the change regresses that listing (scripts/identity-slice "
     "--explain), or the case's expected.tsv is stale / asks unpinned questions -> re-pin from the recording "
     "(scripts/identity-slice --emit-case)"),
    ("UnmatchedClustersRatchetSpec*",
     "the capture ratchet: a new right take -> copy target/identity-unmatched/expected-matches.tsv over the checked-in "
     "one; a wrong one -> fix it, or known-wrong in scope `capture` with why (scripts/identity-ratchet-report)"),
    ("TitleRule*|PlaceholderExpanderSpec*|RuleScopeSpec*|SearchOnlyAffixesSpec*|PolishRewriteScopingSpec*"
     "|ProdTitlesNormalizationSpec*|FestivalTagSpec*|DiscoveredDecorationsSpec*",
     "title rules: the golden/examples/mutation tables hold every rule's readings -- a reading changed on purpose -> "
     "regenerate the golden/examples the spec names; otherwise the rule change reaches titles it should not"),
    ("SearchQueryMarkersSpec*",
     "a screening marker survives into a listing's title search: a title rule misses that spelling (add it to "
     "title-rules.tsv), or the marker vocabulary needs the word"),
    ("HardCluster*",
     "the hard clusters: a cluster's outage recovery or convergence lost its film -- fix it; an exemption "
     "(HardClusterExemptions) only with the user's OK, they only shrink"),
    ("FilmScheduleEndToEndSpec*|IdentityCutoverEndToEndSpec*",
     "end to end over the fixture corpus: the change moves a film the e2e corpus pins -- read the spec's diff in the "
     "log"),
    ("DocCommentPlacementSpec*",
     "a Scaladoc no longer sits directly on the member it documents (a blank line, a statement or a second doc "
     "between): move it"),
    ("compile*", "the commit does not compile at its tree"),
]


def hint(spec: str, test: str, detail: str = "") -> str:
    subject = f"{spec}\t{test}\t{detail}"
    for patterns, why in HINTS:
        if any(fnmatch.fnmatchcase(subject, p) for p in patterns.split("|")):
            return why
    return "read its failure in the log"
