"""identity-measure's command line (scripts/identity_measure_cli.py, whose header is the usage): the options, checked --
a usage error is the tool's exit 2 (tool-conventions.sh's usage_error)."""
from __future__ import annotations

import os
import sys
from dataclasses import dataclass, field
from typing import List, Optional

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "lib"))
from cli import tool_cli  # noqa: E402
from cli import tool_countries  # noqa: E402
from cli.tool_cli import UsageError  # noqa: E402


@dataclass
class Options:
    ref: str = ""
    base: str = ""
    recording: str = ""
    countries: List[str] = field(default_factory=lambda: [])
    keep: bool = False
    no_wait: bool = False
    wait: bool = False
    full: bool = False
    fresh: bool = False
    take_paths: bool = False
    all_countries: bool = False
    recordings: str = "auto"  # all | one | auto
    plan: bool = False
    footprint: str = ""  # "", "measure" (--footprint) or "only" (--footprint-only)
    traces: str = ""
    no_store: bool = False
    no_cache: bool = False
    json: bool = False
    run: str = ""
    no_what_if: str = ""
    timeout_min: int = 120
    timeout_set: bool = False
    cancel_on_timeout: bool = False


VALUED = {"--base": "base", "--head": "ref", "--recording": "recording", "--traces": "traces", "--run": "run",
          "--no-what-if": "no_what_if", "--timeout": "timeout"}
SWITCHES = {"--keep-branch": ("keep", True), "--no-wait": ("no_wait", True), "--wait": ("wait", True),
            "--full": ("full", True), "--fresh": ("fresh", True), "--take-paths": ("take_paths", True),
            "--all-countries": ("all_countries", True), "--all-recordings": ("recordings", "all"),
            "--one-recording": ("recordings", "one"), "--plan": ("plan", True), "--footprint": ("footprint", "measure"),
            "--footprint-only": ("footprint", "only"), "--no-cache": ("no_cache", True), "--json": ("json", True),
            "--cancel-on-timeout": ("cancel_on_timeout", True)}


def parse(argv: List[str], usage: str) -> Optional[Options]:
    """The options, checked; None for --help (`usage` said)."""
    o = tool_cli.read_options(Options(), argv, usage, word)
    if o is not None and o.wait and o.no_wait:
        raise UsageError("--wait and --no-wait contradict each other")
    return o


def word(o: Options, argv: List[str], i: int) -> int:
    """argv[i] read into the options: the index of the last word it took."""
    arg = argv[i]
    if arg in SWITCHES or arg == "--no-store":
        switch(o, arg)
    elif arg in VALUED:
        valued(o, VALUED[arg], tool_cli.value_at(argv, i))
        i += 1
    elif arg.split("=", 1)[0] in ("--country", "--countries"):
        o.countries, i = tool_countries.flag_at(argv, i, o.countries)
    elif arg.startswith("-"):
        raise UsageError(f"unknown option {arg}")
    else:
        o.ref = arg
    return i


def switch(o: Options, arg: str) -> None:
    if arg == "--no-store":  # implies --fresh
        o.no_store = o.fresh = True
    else:
        setattr(o, *SWITCHES[arg])


def valued(o: Options, name: str, value: str) -> None:
    if name == "timeout":
        o.timeout_min, o.timeout_set = tool_cli.minutes("--timeout", value), True
    else:
        setattr(o, name, value)


def checked_recording(recording: str) -> None:
    if recording and not recording.isdigit():
        raise UsageError(f"--recording takes a run id or prod-latest, not {recording}")
