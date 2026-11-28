#!/usr/bin/env python3
"""SLICE ANSWER COVERAGE: every pinned slice -- the identity slice cases (test/resources/fixtures/identity-slices/,
IdentitySliceCasesSpec) and the drift slices (test/resources/fixtures/drift-slices/<cc>/, IdentityDriftSlicesSpec) --
decided again at this checkout's code from its own answers alone, and EVERY request one asks that its fixture lacks
named, all slices at once (scripts.IdentitySliceCoverage). A resolver, title-rule or shape change asking a new search
otherwise fails IdentitySliceCasesSpec with "lacks answers", one case and five requests at a time: Unmatched C turned
main red twice that way (#2180, the Halstead and Keswick cases).

  scripts/identity-slice-coverage [--repin] [SLICE_DIR...]

  (none)    the check: exit 0 when every slice holds every answer it asks, 1 naming each slice and request it lacks
            (and the --repin line), 2 when it could not run (a compile failure: sbt's log tail shown)
  --repin   each lacking slice topped up from ITS OWN recording (the one it was cut from): its http.jsonl.gz gains the
            answers it asks, round after round until it decides alone lacking nothing; listings, filed answers and
            expected.tsv stay as pinned. A recording not in the shared cache is downloaded first (a plain run id; a
            recording with fills is named for scripts/identity-fetch-recording.sh --fills). Commit the http.jsonl.gz.
  SLICE_DIR a slice directory or a root holding them (default both fixture roots)

Runs in this checkout's warm sbt (scripts/sbt-warm; IDENTITY_SLICE_COVERAGE_SBT names another, for its test): ~30 s
over the ~110 slices once compiled (~2.5 min on a loaded laptop). The pre-push hook's identity area runs it before the area's specs
(scripts/hooks/local-tests.txt's `identity first` line), so every missing answer is named before a push, not by main.
The recordings come from identity-tools.sh (identity_recording_cached, identity_recording) through scripts/lib/processes/tool_shell.py
until that library moves. Tested by scripts/identity_slice_coverage_test.py."""
from __future__ import annotations

import os
import re
import shutil
import sys
import tempfile
from typing import List, Mapping, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from processes import machine  # noqa: E402
import tool_process  # noqa: E402
from processes import tool_shell  # noqa: E402

TOOL = "identity-slice-coverage"
HERE = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(HERE)
TALLY = re.compile(r"^identity-slice-coverage: [0-9]* slice\(s\) replayed", re.M)
ROOTS = ("test/resources/fixtures/identity-slices", "test/resources/fixtures/drift-slices")


def usage() -> str:
    return (__doc__ or "").split("\nThe recordings come from")[0] + "\n"


def say(line: str) -> None:
    print(f"{TOOL}: {line}", file=sys.stderr, flush=True)


def short(text: str) -> str:
    """The checkout's paths as the fixtures name them."""
    return text.replace(f"{REPO}/", "")


class Coverage:
    def __init__(self, env: Mapping[str, str], runner: tool_process.Runner, slices: List[str], work: str) -> None:
        self.env, self.runner, self.slices, self.work = env, runner, slices, work
        self.sbt = env.get("IDENTITY_SLICE_COVERAGE_SBT") or os.path.join(HERE, "sbt-warm")
        self.report = os.path.join(work, "report")
        self.log = os.path.join(work, "sbt.log")

    def run(self, *more: str) -> int:
        """The tool in the checkout's sbt: its own lines in the report, sbt's in the log. 0, 1, or 2 (it did not run)."""
        open(self.report, "w").close()
        command = " ".join(["worker/Test/runMain scripts.IdentitySliceCoverage", *self.slices, "--report", self.report,
                            *more])
        rc = self.runner.run(tool_process.Command([self.sbt, command], self.env, log=self.log, cwd=REPO))
        # the tool's first line is its tally: none means it never ran (a compile failure, sbt itself)
        if not TALLY.search(read(self.report)):
            sys.stderr.write("".join(read(self.log).splitlines(keepends=True)[-40:]))
            say(f"the coverage did not run (sbt exit {rc}) -- no verdict")
            return 2
        return 0 if rc == 0 else 1

    def shown(self) -> None:
        sys.stdout.write(short(read(self.report)))
        sys.stdout.flush()

    def repin(self) -> int:
        """Which recordings the lacking slices were cut from, each laid out in the shared cache, then the top-up."""
        needs = os.path.join(self.work, "needs")
        rc = self.run("--needs", needs)
        if rc == 2:
            return 2
        if rc == 0:
            self.shown()
            say("nothing to re-pin")
            return 0
        if os.path.exists(needs):
            self.lay_out(needs)
        rc = self.run("--repin", "--recordings", os.path.join(self.identity_cache(), "recordings"))
        if rc != 2:
            self.shown()
        return rc

    def lay_out(self, needs: str) -> None:
        for line in read(needs).splitlines():
            key, _, cc = line.partition("\t")
            if not key or self.identity("identity_recording_cached", key, cc, quiet=True) == 0:
                continue
            if not key.isdigit():
                cache = self.identity_cache()
                say(f"recording {key} ({cc}) is not in {cache}/recordings and carries fills: fetch it with "
                    f"scripts/identity-fetch-recording.sh --fills <its fill assets> <run> {cache}/recordings/{key} {cc}")
            elif self.identity("identity_recording", key, cc) != 0:
                say(f"could not download recording {key} ({cc})")

    def identity(self, function: str, key: str, cc: str, quiet: bool = False) -> int:
        command = tool_shell.shell_command(self.env, "scripts/identity-tools.sh", function, [key, cc])
        if quiet:
            return self.runner.capture(command).code
        return self.runner.capture(tool_process.Command(command.argv, command.env, stderr_through=True)).code

    def identity_cache(self) -> str:
        return self.env.get("KINOWO_IDENTITY_CACHE") or os.path.expanduser("~/.cache/kinowo-identity")


def read(path: str) -> str:
    try:
        with open(path, encoding="utf-8", errors="replace") as f:
            return f.read()
    except OSError:
        return ""


def parsed(argv: List[str]) -> Optional[List[str]]:
    """[repin flag, slices...]: "1"/"0" first, then each `--slices DIR`; None for --help (printed). SystemExit(2) on a
    usage error."""
    repin = "0"
    slices: List[str] = []
    for arg in argv:
        if arg == "--repin":
            repin = "1"
        elif arg in ("-h", "--help"):
            sys.stdout.write(usage())
            return None
        elif arg.startswith("-"):
            print(f"{TOOL}: {TOOL}: unknown option {arg}\n{TOOL}: see {TOOL} --help", file=sys.stderr)
            raise SystemExit(2)
        else:
            # absolute: sbt's forked run starts in worker/
            slices += ["--slices", os.path.join(os.getcwd(), arg[:-1] if arg.endswith("/") else arg)]
    return [repin, *(slices or [s for root in ROOTS for s in ("--slices", os.path.join(REPO, root))])]


def main(argv: List[str], env: Mapping[str, str]) -> int:
    call = parsed(argv)
    if call is None:
        return 0
    work = tempfile.mkdtemp(prefix=f"{TOOL}.", dir=env.get("TMPDIR") or None)
    try:
        coverage = Coverage(env, tool_process.SubprocessRunner(), call[1:], work)
        if call[0] == "1":
            return coverage.repin()
        rc = coverage.run()
        if rc != 2:
            coverage.shown()
        return rc
    finally:
        shutil.rmtree(work, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], machine.environment()))
