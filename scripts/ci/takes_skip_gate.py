#!/usr/bin/env python3
"""THE TAKES SKIP GATE: whether a convergence `takes` row may leave out its takes diff -- the base's and the head's
resolve and final takes over the recording -- because the commit PROVABLY moves no take. FAIL-CLOSED: it answers SKIP
only when every condition below holds and was positively checked; anything else -- a condition that fails, a read or a
command that errors or hangs, an input missing, an answer it cannot parse -- is RUN. It never decides a verdict itself.

SKIP needs ALL of:
  1. TAKES_SKIP_MODE is shadow or enforce (off, unset, anything else: RUN);
  2. base and head are distinct full commit names, and git can diff them (an error: RUN), and the diff is not empty;
  3. every changed file is either footprint-eligible -- a title-rule file the resolve reads only through the title
     normaliser's readings (title-rules.tsv, services/titles/rules/*.scala but TitleRulePlaceholders.scala, which
     FormatTags reads directly, and TitleNormalizer.scala: identity-measure's footprint_rule_path) -- or inert (docs/,
     .claude/, a Markdown file). Any other file -- a spec, a judgement, a capture, a workflow, a script -- is RUN;
  4. no changed path names identity, matcher, resolver or projection code (checked on its own, whatever 3 said);
  5. the trace tooling (NormalizerTrace, NormalizerTraceCheck, this gate, the takes check) is the same at base and head;
  6. the base's trace exists, is a readable gzip with at least one reading, and was stored under the base's EXACT
     inputs (its identity code, the recording, its tree, its fills and the capture: the caller names that file);
  7. the proof -- the base's every title reading read again under the head's rules (services.movies.NormalizerTraceCheck,
     the command the caller hands in) -- exits 0 within TAKES_SKIP_PROOF_TIMEOUT seconds (default 900), its last
     `[normalizer-trace]` line reading N > 0 readings made again, 0 read otherwise, 0 could not be made again.

usage: takes_skip_gate.py <base sha> <head sha> <base trace .jsonl.gz> -- <proof command...>
  (the proof command is run with `--out <differ.tsv> <trace>` appended)
Prints SKIP or RUN on stdout, then -- on SKIP -- `measure: footprint, 0 readings differ (<N> readings)`; the reasons on
stderr. Exit 0 for SKIP, 1 for RUN. Git, files and the proof's process come to `Gate` from `main()` (tool_git, tool_files, tool_process): tested by
takes_skip_gate_test.py beside it (black-box), takes_skip_gate_unit_test.py (over fakes) and takes_skip_gate_mutation_test.py,
which proves that removing any one check lets a RUN case through (each mutant run in a sandbox with the adapters)."""
from __future__ import annotations

import fnmatch
import gzip
import os
import re
import sys
import zlib
from typing import List, Mapping, NoReturn, Optional, Sequence

sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), "lib"))
import machine  # noqa: E402
import tool_git  # noqa: E402
import tool_process  # noqa: E402
from tool_files import Files  # noqa: E402

TOOLING = ("testkit/src/main/scala/services/movies/NormalizerTrace.scala",
           "testkit/src/main/scala/services/movies/NormalizerTraceCheck.scala",
           "scripts/ci/takes_skip_gate.py", "scripts/ci/identity-takes-check.sh", "scripts/ci/identity_takes_check*.py")
PLACEHOLDERS = "common/src/main/scala/services/titles/rules/TitleRulePlaceholders.scala"  # FormatTags reads it directly
ELIGIBLE = ("common/src/main/resources/title-rules.tsv", "common/src/main/scala/services/titles/rules/*.scala",
            "common/src/main/scala/services/titles/TitleNormalizer.scala")
DENIED = re.compile(r"(^|/)(identity|matcher|resolver|projection)|identity[^/]*\.|matcher[^/]*\.|resolver[^/]*\.|"
                    r"projection[^/]*\.", re.IGNORECASE)
VERDICT = re.compile(r"^\[normalizer-trace\] ([0-9]+) reading\(s\) made again: 0 read otherwise, 0 could not be made "
                     r"again")
FULL_NAME = re.compile(r"^[0-9a-f]{40}$")


class Run(Exception):
    """The answer RUN, with its reason."""


def require(holds: bool, reason: str) -> None:
    if not holds:
        raise Run(reason)


def eligible(path: str) -> bool:
    if path == PLACEHOLDERS:
        return False
    return any(fnmatch.fnmatchcase(path, pattern) for pattern in ELIGIBLE)


def inert(path: str) -> bool:
    return path.startswith(("docs/", ".claude/")) or path.endswith(".md")


def only_rules(changed: Sequence[str]) -> int:
    """3 and 4: every file eligible or inert, at least one a rule, and none identity code -> how many rules."""
    rules = 0
    for f in changed:
        if eligible(f):
            rules += 1
            continue
        require(inert(f), f"{f} is neither a title-rule file nor inert")
    require(rules > 0, "no title-rule file changed: nothing for the proof to prove")
    denied = [f for f in changed if DENIED.search(f) and not inert(f)]
    require(not denied, f"identity, matcher, resolver or projection code changed: {(denied + [''])[0]}")
    return rules


def proof_verdict(text: str) -> int:
    """7: the readings the proof's last `[normalizer-trace]` line says it made again, all alike."""
    verdicts = [line for line in text.split("\n") if line.startswith("[normalizer-trace] ")]
    verdict = verdicts[-1] if verdicts else ""
    matched = VERDICT.match(verdict)
    require(matched is not None,
            f"the proof said no '0 read otherwise, 0 could not be made again' ({verdict or 'nothing'})")
    readings = int(matched.group(1)) if matched else 0
    require(readings > 0, "the proof read nothing again")
    return readings


def proof_timeout(env: Mapping[str, str]) -> float:
    return float(env.get("TAKES_SKIP_PROOF_TIMEOUT") or "900")


def gzip_lines(data: bytes) -> Optional[int]:
    """How many non-empty lines a gzip holds; None when it is not a whole one."""
    try:
        return sum(1 for line in gzip.decompress(data).split(b"\n") if line.strip(b"\n"))
    except (OSError, EOFError, zlib.error):
        return None


class Gate:
    """What a takes row's skip is decided from: the repository's diffs (git), the base trace and the proof's output
    (files), the proof itself (a runner: bounded, its process group ended with the gate)."""

    def __init__(self, git: tool_git.Git, files: Files, runner: tool_process.Runner, env: Mapping[str, str]) -> None:
        self.git, self.files, self.runner, self.env = git, files, runner, env

    def changed_files(self, base: str, head: str) -> List[str]:
        """2: what changed (an error is RUN, not "nothing")."""
        diff = self.git.call("diff", "--name-only", "--no-renames", base, head, "--", quiet=True)
        require(diff.code == 0, f"git could not diff {base} and {head}")
        changed = [f for f in diff.stdout.split("\n") if f]
        require(bool(changed), "no file changed: nothing for a proof to stand for")
        return changed

    def tooling_unchanged(self, base: str, head: str) -> None:
        """5: asked first -- its files are no rule, and 3 would refuse them under another name."""
        diff = self.git.call("diff", "--name-only", base, head, "--", *TOOLING, quiet=True)
        require(diff.code == 0, "git could not diff the trace tooling")
        require(not diff.stdout.strip(), f"the trace tooling changed: {diff.stdout.split(chr(10))[0]}")
        for f in TOOLING[:2]:
            require(self.git.ok("cat-file", "-e", f"{head}:{f}", quiet=True), f"the head has no {f}")

    def trace_readings(self, trace: str) -> None:
        """6: the base's trace, under its exact inputs (the caller names it), readable and not empty."""
        require(self.files.is_file(trace) and self.files.size(trace) > 0,
                f"no base trace at {trace} (none stored under the base's exact inputs)")
        lines = gzip_lines(self.files.read_bytes(trace))
        require(lines is not None, f"the base trace {trace} is not a whole gzip")
        require(bool(lines), f"the base trace {trace} holds no reading")

    def proof_readings(self, command: Sequence[str], trace: str) -> int:
        """7: the proof, bounded -> the readings it made again, all alike. It runs in a process group of its own, ended
        past TAKES_SKIP_PROOF_TIMEOUT seconds (124) and when the gate itself is ended (INT, TERM): a caller's bound
        strands no proof (the runner's own: tool_process.LiveGroups)."""
        with self.files.scratch("takes-skip-gate-") as out:
            differ, said = os.path.join(out, "differ.tsv"), os.path.join(out, "proof.txt")
            rc = self.runner.run(tool_process.Command(
                [*command, "--out", differ, trace], self.env, log=said,
                timeout=proof_timeout(self.env)))
            text = self.files.read_bytes(said).decode("utf-8", "replace")
            sys.stderr.write(text)
            require(rc == 0, f"the proof exited {rc} (1: readings differ, 3: some cannot be made again, 124: timed out)")
            readings = proof_verdict(text)
            require(not self.files.read_text_or(differ, ""), f"the proof listed readings in {differ} though it exited 0")
            return readings

    def decide(self, argv: Sequence[str]) -> str:
        """The SKIP line's reasons, or Run."""
        mode = self.env.get("TAKES_SKIP_MODE") or "off"
        require(mode in ("shadow", "enforce"), f"TAKES_SKIP_MODE is {self.env.get('TAKES_SKIP_MODE') or 'unset'}")
        require(len(argv) >= 5 and argv[3] == "--", "usage: <base> <head> <trace> -- <proof command...>")
        base, head, trace, command = argv[0], argv[1], argv[2], argv[4:]
        require(bool(FULL_NAME.match(base) and FULL_NAME.match(head)), "base and head must be full commit names")
        require(base != head, "base and head are one commit")
        changed = self.changed_files(base, head)
        self.tooling_unchanged(base, head)
        rules = only_rules(changed)
        self.trace_readings(trace)
        readings = self.proof_readings(command, trace)
        print(f"takes-skip-gate: SKIP -- {rules} title-rule file(s) changed, nothing else that can move a take; "
              f"{readings} readings alike", file=sys.stderr)
        return f"measure: footprint, 0 readings differ ({readings} readings)"


def main(argv: Sequence[str], env: Mapping[str, str]) -> NoReturn:
    runner = tool_process.SubprocessRunner()
    gate = Gate(tool_git.Git(runner, env, "."), Files.here(), runner, env)
    try:
        line = gate.decide(argv)
    except Run as run:
        print(f"takes-skip-gate: RUN -- {run}", file=sys.stderr)
        print("RUN")
        sys.exit(1)
    print("SKIP")
    print(line)
    sys.exit(0)


if __name__ == "__main__":
    main(sys.argv[1:], machine.environment())
