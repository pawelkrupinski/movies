#!/usr/bin/env python3
"""Re-pin identity slice cases on this checkout's code, in place, keeping each case's expected.tsv.

  scripts/identity-slice-refresh [--recording RUN] <case dir>...

A pinned case (test/resources/fixtures/identity-slices/<dir>, IdentitySliceCasesSpec) holds every answer its listings
read when it was emitted. A resolver change that makes them ask more -- a listing it leaves unmatched asks the
agreement's lookups, a new banner a new search -- fails the case with "lacks answers" though nothing it pins moved.
This re-emits each case (scripts/identity-slice --emit-case) with every listing its expected.tsv names, on the
recording given (default identity-slice's), into a scratch root, then puts the new slice files into the case's own
directory and keeps its expected.tsv as it was: a hand expectation ("not:<film>", "pending:…") survives, the case keeps
its name, and no other case is touched.

The country is the directory name's prefix (de-…, uk-…). A case is left as it was when its re-emit fails, is refused
(the judgements judge none of its listings) or pins its listings as more than one slice (they fall in several
clusters: re-emit them by hand, or with --widen). Exit 1 when
any case was not re-pinned, 2 on a usage error."""
from __future__ import annotations

import os
import re
import shutil
import sys
import tempfile
from typing import List, Mapping, Optional, Tuple

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
import tool_cli  # noqa: E402
import tool_process  # noqa: E402

TOOL = "identity-slice-refresh"
HERE = os.path.dirname(os.path.abspath(__file__))
PURPOSE = "Re-pin identity slice cases on this checkout's code, in place, keeping each case's expected.tsv"
SLICE_FILES = ("slice.json.gz", "rows.json.gz", "http.jsonl.gz", "filed.json.gz", "filed.source")
# identity-slice names each directory it pinned: "--emit-case: N listing(s) pinned in <path> (…"
PINNED = re.compile(r"^--emit-case: .* pinned in (.*) \(IdentitySliceCasesSpec.*$", re.M)


class LeftAsItWas(Exception):
    """A case not re-pinned: why, said after its directory."""


def usage() -> str:
    """The header as the shell tool's usage_from_header printed it: its Purpose line first."""
    return f"Purpose: {PURPOSE}\n{__doc__}\n"


def say(line: str) -> None:
    print(f"{TOOL}: {line}", file=sys.stderr, flush=True)


def tail(text: str, lines: int) -> str:
    return "".join(text.splitlines(keepends=True)[-lines:])


def listing_keys(expected: str) -> List[str]:
    """`venue|raw title` of every listing expected.tsv names, once each (a `not:` case names one listing per film it
    must not take); a last line without its newline too. Its fields split as the shell's `read` split them: empty
    ones dropped."""
    keys: List[str] = []
    for line in expected.splitlines()[1:]:
        fields = [f for f in line.split("\t") if f] + ["", ""]
        key = f"{fields[0]}|{fields[1]}"
        if key != "|" and key not in keys:
            keys.append(key)
    return keys


class Refresh:
    def __init__(self, env: Mapping[str, str], runner: tool_process.Runner, recording: List[str], scratch: str) -> None:
        self.env, self.runner, self.recording, self.scratch = env, runner, recording, scratch
        self.slice = env.get("IDENTITY_SLICE") or os.path.join(HERE, "identity-slice")

    def case(self, case: str) -> bool:
        """The case re-pinned (True), or said why it was left as it was."""
        try:
            listings = self.repin(case)
        except LeftAsItWas as e:
            say(f"{case}: {e}")
            return False
        say(f"{case}: re-pinned ({listings} listing(s))")
        return True

    def repin(self, case: str) -> int:
        expected = os.path.join(case, "expected.tsv")
        if not os.path.isfile(expected):
            raise LeftAsItWas("no expected.tsv -- not a slice case")
        keys = listing_keys(read(expected))
        if not keys:
            raise LeftAsItWas("its expected.tsv names no listing")
        name = os.path.basename(case)
        pinned = pinned_directory(self.emit(name, name.split("-", 1)[0], keys))
        for f in SLICE_FILES:
            if os.path.lexists(os.path.join(case, f)):
                os.remove(os.path.join(case, f))
            if os.path.isfile(os.path.join(pinned, f)):
                shutil.copyfile(os.path.join(pinned, f), os.path.join(case, f))
        return len(keys)

    def emit(self, name: str, cc: str, keys: List[str]) -> str:
        """identity-slice's output (stdout and stderr as they came) re-emitting the case's listings into its own
        scratch root; LeftAsItWas when it failed."""
        root = os.path.join(self.scratch, name)
        os.makedirs(root, exist_ok=True)
        log = os.path.join(self.scratch, f"{name}.log")
        argv = [self.slice, cc, *self.recording, *[a for key in keys for a in ("--key", key)], "--emit-case"]
        env = {**self.env, "KINOWO_IDENTITY_SLICE_EMIT_ROOT": root}
        code = self.runner.run(tool_process.Command(argv, env, log=log))
        out = read(log)
        if code != 0:
            sys.stderr.write(tail(out, 5))
            raise LeftAsItWas("re-emit failed, left as it was")
        return out


def pinned_directory(out: str) -> str:
    """The one directory identity-slice says it pinned; LeftAsItWas when it refused, pinned none or several."""
    pinned = [p for p in PINNED.findall(out) if p]
    # emitCase refuses a slice none of whose listings the judgements judge, and pins nothing
    if not pinned and "--emit-case REFUSED" in out:
        sys.stderr.write(tail("".join(line for line in out.splitlines(keepends=True) if "--emit-case" in line), 5))
        raise LeftAsItWas("identity-slice refused to pin it (the judgements judge none of its listings), left as it was")
    if not pinned:
        raise LeftAsItWas("identity-slice pinned nothing, left as it was")
    if len(pinned) != 1:
        raise LeftAsItWas(f"its listings were pinned as {len(pinned)} slices (they fall in several clusters), "
                          "left as it was")
    if not os.path.isdir(pinned[0]):
        raise LeftAsItWas("identity-slice named no pinned directory, left as it was")
    return pinned[0]


def read(path: str) -> str:
    with open(path, encoding="utf-8", errors="replace") as f:
        return f.read()


def parsed(argv: List[str]) -> Optional[Tuple[List[str], List[str]]]:
    """(the --recording words, the case directories); None for --help (printed). tool_cli.Refused on a usage error."""
    recording: List[str] = []
    cases: List[str] = []
    i = 0
    while i < len(argv):
        arg = argv[i]
        if arg == "--recording":
            recording = ["--recording", tool_cli.value_at(argv, i)]
            i += 1
        elif arg in ("-h", "--help"):
            sys.stdout.write(usage())
            return None
        elif arg.startswith("-"):
            raise tool_cli.Refused(f"{TOOL}: unknown option {arg}")
        else:
            cases.append(arg[:-1] if arg.endswith("/") else arg)
        i += 1
    if not cases:
        raise tool_cli.Refused("", with_usage=True)
    return recording, cases


def main(argv: List[str], env: Mapping[str, str]) -> int:
    try:
        call = parsed(argv)
    except tool_cli.Refused as e:
        return tool_cli.refused(e, usage())
    except tool_cli.UsageError as e:
        return tool_cli.usage_failed(TOOL, e)
    if call is None:
        return 0
    recording, cases = call
    scratch = tempfile.mkdtemp(prefix=f"{TOOL}.", dir=env.get("TMPDIR") or None)
    try:
        refresh = Refresh(env, tool_process.SubprocessRunner(), recording, scratch)
        results = [refresh.case(case) for case in cases]
        return 0 if all(results) else 1
    finally:
        shutil.rmtree(scratch, ignore_errors=True)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:], os.environ))
