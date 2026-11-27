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
stderr. Exit 0 for SKIP, 1 for RUN. Standard library only, so a mutant copied anywhere runs: tested by
takes_skip_gate_test.py beside it, whose mutation test (takes_skip_gate_mutation_test.py) proves that removing any one
check lets a RUN case through."""
from __future__ import annotations

import fnmatch
import gzip
import os
import re
import signal
import subprocess
import sys
import tempfile
import zlib
from typing import List, Mapping, NoReturn, Sequence

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


def git(*args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(["git", *args], stdout=subprocess.PIPE, stderr=subprocess.DEVNULL, text=True)


def eligible(path: str) -> bool:
    if path == PLACEHOLDERS:
        return False
    return any(fnmatch.fnmatchcase(path, pattern) for pattern in ELIGIBLE)


def inert(path: str) -> bool:
    return path.startswith(("docs/", ".claude/")) or path.endswith(".md")


def changed_files(base: str, head: str) -> List[str]:
    """2: what changed (an error is RUN, not "nothing")."""
    diff = git("diff", "--name-only", "--no-renames", base, head, "--")
    require(diff.returncode == 0, f"git could not diff {base} and {head}")
    changed = [f for f in diff.stdout.split("\n") if f]
    require(bool(changed), "no file changed: nothing for a proof to stand for")
    return changed


def tooling_unchanged(base: str, head: str) -> None:
    """5: asked first -- its files are no rule, and 3 would refuse them under another name."""
    diff = git("diff", "--name-only", base, head, "--", *TOOLING)
    require(diff.returncode == 0, "git could not diff the trace tooling")
    require(not diff.stdout.strip(), f"the trace tooling changed: {diff.stdout.split(chr(10))[0]}")
    for f in TOOLING[:2]:
        require(git("cat-file", "-e", f"{head}:{f}").returncode == 0, f"the head has no {f}")


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


def trace_readings(trace: str) -> None:
    """6: the base's trace, under its exact inputs (the caller names it), readable and not empty."""
    require(os.path.isfile(trace) and os.path.getsize(trace) > 0,
            f"no base trace at {trace} (none stored under the base's exact inputs)")
    try:
        with gzip.open(trace, "rb") as f:
            lines = sum(1 for line in f if line.strip(b"\n"))
    except (OSError, EOFError, zlib.error):
        raise Run(f"the base trace {trace} is not a whole gzip") from None
    require(lines > 0, f"the base trace {trace} holds no reading")


def proof_exit(command: Sequence[str], out: str, timeout: float) -> int:
    """The proof's exit, its output (both streams) in `out`. It runs in a process group of its own, ended past
    `timeout` (124) and when the gate itself is ended (INT, TERM): a caller's bound strands no proof. The handlers are
    in place BEFORE the proof is spawned -- an end arriving while Popen returns is held and acted on once the process
    is in hand (Tool self-tests #632: a TERM in that window killed the gate and left its proof running)."""
    held: List[int] = []
    started: List[subprocess.Popen[bytes]] = []

    def end(signum: int, _frame: object) -> None:
        if not started:
            held.append(signum)
            return
        end_group(started[0])
        sys.exit(128 + signum)

    for signum in (signal.SIGINT, signal.SIGTERM):
        signal.signal(signum, end)
    with open(out, "w") as sink:
        try:
            proc = subprocess.Popen(list(command), stdout=sink, stderr=subprocess.STDOUT, start_new_session=True)
        except PermissionError:
            return 126
        except OSError:
            return 127
        started.append(proc)
        if held:
            end(held[0], None)
        try:
            return proc.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            end_group(proc)
            return 124


def end_group(proc: subprocess.Popen[bytes]) -> None:
    try:
        os.killpg(proc.pid, signal.SIGKILL)
    except OSError:
        pass
    proc.wait()


def proof_readings(command: Sequence[str], trace: str, env: Mapping[str, str]) -> int:
    """7: the proof, bounded -> the readings it made again, all alike."""
    with tempfile.TemporaryDirectory() as out:
        differ, said = os.path.join(out, "differ.tsv"), os.path.join(out, "proof.txt")
        rc = proof_exit([*command, "--out", differ, trace], said, float(env.get("TAKES_SKIP_PROOF_TIMEOUT") or "900"))
        with open(said, errors="replace") as f:
            text = f.read()
        sys.stderr.write(text)
        require(rc == 0, f"the proof exited {rc} (1: readings differ, 3: some cannot be made again, 124: timed out)")
        verdicts = [line for line in text.split("\n") if line.startswith("[normalizer-trace] ")]
        verdict = verdicts[-1] if verdicts else ""
        matched = VERDICT.match(verdict)
        require(matched is not None,
                f"the proof said no '0 read otherwise, 0 could not be made again' ({verdict or 'nothing'})")
        readings = int(matched.group(1)) if matched else 0
        require(readings > 0, "the proof read nothing again")
        require(not (os.path.isfile(differ) and os.path.getsize(differ) > 0),
                f"the proof listed readings in {differ} though it exited 0")
        return readings


def decide(argv: Sequence[str], env: Mapping[str, str]) -> str:
    """The SKIP line's reasons, or Run."""
    mode = env.get("TAKES_SKIP_MODE") or "off"
    require(mode in ("shadow", "enforce"), f"TAKES_SKIP_MODE is {env.get('TAKES_SKIP_MODE') or 'unset'}")
    require(len(argv) >= 5 and argv[3] == "--", "usage: <base> <head> <trace> -- <proof command...>")
    base, head, trace, command = argv[0], argv[1], argv[2], argv[4:]
    require(bool(FULL_NAME.match(base) and FULL_NAME.match(head)), "base and head must be full commit names")
    require(base != head, "base and head are one commit")
    changed = changed_files(base, head)
    tooling_unchanged(base, head)
    rules = only_rules(changed)
    trace_readings(trace)
    readings = proof_readings(command, trace, env)
    print(f"takes-skip-gate: SKIP -- {rules} title-rule file(s) changed, nothing else that can move a take; "
          f"{readings} readings alike", file=sys.stderr)
    return f"measure: footprint, 0 readings differ ({readings} readings)"


def main(argv: Sequence[str], env: Mapping[str, str]) -> NoReturn:
    try:
        line = decide(argv, env)
    except Run as run:
        print(f"takes-skip-gate: RUN -- {run}", file=sys.stderr)
        print("RUN")
        sys.exit(1)
    print("SKIP")
    print(line)
    sys.exit(0)


if __name__ == "__main__":
    main(sys.argv[1:], os.environ)
