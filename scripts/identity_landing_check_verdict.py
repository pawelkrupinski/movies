"""THE VERDICT of scripts/identity-landing-check (identity_landing_check.py), last on stdout, from what the measure, the
drift group and each spec group left under the change's out directory: `LANDABLE` (0), `BLOCKED` with each failing spec,
its test and a hint for its usual cause (1), or `NO VERDICT` when a group or the measure could not run (2).

The blocked rows are "<what><TAB><test><TAB><hint>" lines, each read back as the shell tool's `IFS=$'\\t' read -r` read
it (`read_fields`): a run of tabs is one separator, and the last field takes the rest."""
from __future__ import annotations

import os
import re
from typing import List, Optional, cast

import identity_landing_check_sbt as sbt

NOT_LANDABLE_HINT = ("a take moved wrong or a right one was lost: the measure's table, {out}/measure.txt "
                     "(scripts/identity-slice --explain its listings)")
DRIFT_HINT = ("the incremental state ends elsewhere than a whole resolve, or a take flips on the way: each row's "
              "verdict and report lines in {out}/drift.txt; a fix, never an allow-list entry (a list takes rows only "
              "at its check's birth: scripts/ci/commit_rules.py)")


def lines(path: str) -> List[str]:
    """The file's lines as sed and grep read them ([] when it is missing)."""
    try:
        with open(path, "rb") as f:
            text = f.read().decode(errors="replace")
    except OSError:
        return []
    split = text.split("\n")
    return split[:-1] if split and split[-1] == "" else split


def last_line(path: str) -> str:
    """`tail -1`."""
    found = lines(path)
    return found[-1] if found else ""


def why_not(log: str, tool: str) -> str:
    """": <the tool's last word on why it stopped>" (e.g. identity-ci-run's "could not push measure/…: remote rejected
    (unable to migrate objects to permanent storage)"), or "" when the log has none."""
    said = [line for line in lines(log) if line.startswith(f"{tool}: ")]
    return f": {said[-1][len(tool) + 2:]}" if said else ""


def read_fields(line: str, count: int) -> List[str]:
    """`IFS=$'\\t' read -r a b c`: leading and trailing tabs dropped, a run of tabs one separator, the last field the
    rest of the line."""
    rest, fields = line.strip("\t"), cast(List[str], [])
    while len(fields) < count - 1 and rest:
        head, _, rest = rest.partition("\t")
        fields.append(head)
        rest = rest.lstrip("\t")
    fields.append(rest)
    return fields + [""] * (count - len(fields))


def measure_reasons(text: List[str]) -> str:
    """The three lines after the measure's NOT LANDABLE, their "- " dropped, joined by ';'."""
    start = next((i for i, line in enumerate(text) if line.startswith("NOT LANDABLE")), None)
    if start is None:
        return ""
    return ";".join(re.sub(r"^ *- ", "", line, count=1) for line in text[start + 1:start + 4])


class Verdict:
    def __init__(self, commit: str, base: str, out: str) -> None:
        self.commit, self.base, self.out = commit, base, out
        self.blocked: List[str] = []
        self.unknown: List[str] = []

    def of(self, measure_rc: Optional[int], drift_rc: Optional[int], groups: Optional[List[str]]) -> int:
        """Every check's line, then the verdict; its exit. None: not run."""
        print(f"identity landing check: {self.commit[:12]} over {self.base[:12]}")
        if measure_rc is not None:
            self.measure(measure_rc)
        if drift_rc is not None:
            self.drift(drift_rc)
        for name in groups or []:
            self.group(name)
        print()
        return self.final()

    def measure(self, rc: int) -> None:
        text = lines(os.path.join(self.out, "measure.txt"))
        log = os.path.join(self.out, "measure.log")
        if rc == 0:
            said = next((line for line in text if re.match(r"(LANDABLE|NOT LANDABLE)", line)), "")
            print(f"  measure     {said or 'LANDABLE'}")
        elif rc == 1:
            print("  measure     NOT LANDABLE")
            self.blocked.append(f"measure\tNOT LANDABLE: {measure_reasons(text)}\t"
                                + NOT_LANDABLE_HINT.format(out=self.out))
        elif rc == 3 and "STILL RUNNING" in "\n".join(lines(log)):
            print(f"  measure     still running at --timeout, left running: collect with identity-measure --run <id> "
                  f"({log})")
            self.unknown.append("measure")
        elif rc == 3:
            print("  measure     not answered by the footprint (--footprint-only)")
            self.blocked.append("measure\t--footprint-only could not answer\trun without --footprint-only for the full "
                                "measure")
        else:
            print(f"  measure     could not measure{why_not(log, 'identity-measure')} ({log})")
            self.unknown.append("measure")

    def drift(self, rc: int) -> None:
        said = last_line(os.path.join(self.out, "drift.txt"))
        if rc in (0, 1):
            print(f"  drift       {said}")
        if rc == 1:
            self.blocked.append(f"drift\t{said}\t" + DRIFT_HINT.format(out=self.out))
        elif rc != 0:
            log = os.path.join(self.out, "drift.log")
            print(f"  drift       could not check{why_not(log, 'identity-drift-ci')} ({log})")
            self.unknown.append("drift")

    def group(self, name: str) -> None:
        folder = os.path.join(self.out, name)
        exit_file = os.path.join(folder, "exit")
        rc = "\n".join(lines(exit_file)).rstrip("\n") if os.path.isfile(exit_file) else "2"  # `$(cat exit || echo 2)`
        log = os.path.join(folder, "sbt.log")
        found = sbt.failures(log) if os.path.isfile(log) else []
        if rc == "0" and not found:
            print(f"  {name:<11} passed")
        elif rc == "2" and not found:
            print(f"  {name:<11} could not run{why_not(os.path.join(folder, 'ci-run.log'), 'identity-ci-run')} "
                  f"({folder}/)")
            self.unknown.append(name)
        else:
            print(f"  {name:<11} FAILED ({log})")
            for line in found or [f"{name}\t(sbt failed, no test named)"]:
                spec, test, detail = read_fields(line, 3)
                self.blocked.append(f"{spec}\t{test}\t{sbt.hint(spec, test, detail)}")

    def final(self) -> int:
        if self.blocked:
            print("BLOCKED")
            for row in self.blocked:
                spec, test, why = read_fields(row, 3)
                print(f"  - {spec}: {test}")
                print(f"    hint: {why}")
            if self.unknown:
                print(f"  (and could not check: {' '.join(self.unknown)})")
            return 1
        if self.unknown:
            print(f"NO VERDICT: could not check {' '.join(self.unknown)} -- see {self.out}")
            return 2
        print("LANDABLE")
        return 0
